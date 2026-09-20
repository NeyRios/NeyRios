# --- GDAL streaming clip v0.5.8 ----------------------------------------------
# Sustituye los recortes/máscaras de resolución completa de terra por gdalwarp
# con memoria acotada. El GeoTIFF científico permanece a 10 m, EPSG:5367 y
# alineado a la malla del DEM IGN original.
APP_VERSION <- "0.5.10"

.gdalwarp_bin <- Sys.getenv("GDALWARP_BIN", unset = "/usr/bin/gdalwarp")

log_dem_stage <- function(stage, detail = "") {
  message(
    "DEM_STAGE | ", stage,
    if (nzchar(detail)) paste0(" | ", detail) else ""
  )
}

snap_extent_to_dem_grid <- function(poly, dem) {
  pe <- terra::ext(poly)
  de <- terra::ext(dem)
  rr <- terra::res(dem)
  rx <- rr[1]
  ry <- rr[2]

  xmin <- terra::xmin(de) + floor((terra::xmin(pe) - terra::xmin(de)) / rx) * rx
  xmax <- terra::xmin(de) + ceiling((terra::xmax(pe) - terra::xmin(de)) / rx) * rx

  ymax <- terra::ymax(de) - floor((terra::ymax(de) - terra::ymax(pe)) / ry) * ry
  ymin <- terra::ymax(de) - ceiling((terra::ymax(de) - terra::ymin(pe)) / ry) * ry

  c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
}

validate_grid_alignment <- function(x, dem, tol = 1e-6) {
  rr <- terra::res(x)
  dr <- terra::res(dem)

  if (any(abs(rr - dr) > tol)) {
    stop(
      sprintf(
        "El raster recortado no conserva resolución 10 m: %.9f x %.9f",
        rr[1], rr[2]
      )
    )
  }

  ex <- terra::ext(x)
  de <- terra::ext(dem)

  dx <- (terra::xmin(ex) - terra::xmin(de)) / dr[1]
  dy <- (terra::ymax(de) - terra::ymax(ex)) / dr[2]

  if (abs(dx - round(dx)) > 1e-5 || abs(dy - round(dy)) > 1e-5) {
    stop("El raster recortado no quedó alineado a la malla original del MDE IGN.")
  }

  invisible(TRUE)
}

gdal_clip_to_polygon <- function(
  source_path,
  polygon,
  dem_template,
  output_tif,
  work_dir,
  tag = "clip"
) {
  if (!file.exists(.gdalwarp_bin)) {
    stop("gdalwarp no está disponible en el contenedor.")
  }
  if (!file.exists(source_path)) {
    stop("La copia operacional local del MDE no está disponible para gdalwarp.")
  }

  cutline <- file.path(work_dir, paste0(tag, "_cutline.geojson"))
  terra::writeVector(
    polygon,
    cutline,
    filetype = "GeoJSON",
    overwrite = TRUE
  )

  bounds <- snap_extent_to_dem_grid(polygon, dem_template)
  rr <- terra::res(dem_template)

  args <- c(
    "--config", "GDAL_CACHEMAX", "64",
    "-wm", "64",
    "-wo", "NUM_THREADS=1",
    "-overwrite",
    "-of", "GTiff",
    "-cutline", shQuote(cutline),
    "-te_srs", "EPSG:5367",
    "-te",
    format(bounds["xmin"], scientific = FALSE, trim = TRUE, digits = 16),
    format(bounds["ymin"], scientific = FALSE, trim = TRUE, digits = 16),
    format(bounds["xmax"], scientific = FALSE, trim = TRUE, digits = 16),
    format(bounds["ymax"], scientific = FALSE, trim = TRUE, digits = 16),
    "-tr",
    format(rr[1], scientific = FALSE, trim = TRUE, digits = 16),
    format(rr[2], scientific = FALSE, trim = TRUE, digits = 16),
    "-r", "near",
    # El MDE IGN usa un valor de relleno Float32 cercano a 3.4e38 en zonas
    # sin datos (p. ej. mar). Debe convertirse a NA; de lo contrario sesga
    # max, media, sd y mediana.
    "-srcnodata", "3.4e38",
    "-dstnodata", "nan",
    "-ot", "Float32",
    "-co", "TILED=YES",
    "-co", "COMPRESS=DEFLATE",
    "-co", "PREDICTOR=3",
    "-co", "BIGTIFF=IF_SAFER",
    shQuote(source_path),
    shQuote(output_tif)
  )

  log_dem_stage(
    paste0("gdalwarp_", tag, "_start"),
    paste0(
      "bounds=", paste(round(bounds, 3), collapse = ","),
      " | res=", paste(rr, collapse = "x")
    )
  )

  out <- tryCatch(
    system2(
      .gdalwarp_bin,
      args = args,
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(
      paste("No fue posible ejecutar gdalwarp:", conditionMessage(e)),
      status = 1L
    )
  )

  status <- attr(out, "status")
  if (is.null(status)) status <- 0L

  if (status != 0L || !file.exists(output_tif) || file.info(output_tif)$size <= 0) {
    tail_log <- if (length(out)) paste(tail(out, 12), collapse = " | ") else "sin salida"
    stop(
      paste0(
        "gdalwarp falló durante ", tag,
        ". exit=", status,
        ". Detalle: ", tail_log
      )
    )
  }

  result <- terra::rast(output_tif)
  validate_grid_alignment(result, dem_template)

  log_dem_stage(
    paste0("gdalwarp_", tag, "_done"),
    paste0(
      "bytes=", file.info(output_tif)$size,
      " | rows=", terra::nrow(result),
      " | cols=", terra::ncol(result)
    )
  )

  result
}

process_with_dem <- function(
  dem,
  aoi,
  buffer_m,
  source_mode,
  output_tif,
  work_dir
) {
  log_dem_stage("process_start", paste0("buffer_m=", buffer_m))

  aoi_dem <- terra::project(aoi, terra::crs(dem))

  if (!extents_overlap(terra::ext(aoi_dem), terra::ext(dem))) {
    stop_dem("El AOI no intersecta la cobertura del MDE IGN 2017.", 422)
  }

  aoi_work <- if (buffer_m > 0) {
    terra::buffer(aoi_dem, width = buffer_m)
  } else {
    aoi_dem
  }

  if (!extent_inside(
    terra::ext(aoi_work),
    terra::ext(dem),
    tolerance = max(terra::res(dem)) * 2
  )) {
    stop_dem(
      "El buffer solicitado extiende el análisis fuera de la cobertura del MDE. Reduzca el buffer.",
      422
    )
  }

  source_path <- get_dem_source()
  if (startsWith(source_path, "/vsicurl/")) {
    stop(
      "El procesamiento GDAL requiere la copia operacional local del MDE; la fuente activa sigue siendo remota."
    )
  }

  dem_aoi <- gdal_clip_to_polygon(
    source_path = source_path,
    polygon = aoi_work,
    dem_template = dem,
    output_tif = output_tif,
    work_dir = work_dir,
    tag = "buffer"
  )

  list(
    aoi_dem = aoi_dem,
    dem_aoi = dem_aoi,
    source_mode = source_mode,
    source_path = source_path
  )
}

process_dem_aoi <- function(geojson, buffer_m = 0, aoi_name = "AOI") {
  if (!is.finite(buffer_m) || buffer_m < 0 || buffer_m > PUBLIC_LIMITS$max_buffer_m) {
    stop_dem(
      sprintf("El buffer debe estar entre 0 y %.0f m.", PUBLIC_LIMITS$max_buffer_m),
      422
    )
  }

  cleanup_old_outputs()

  output_dir <- getOption("dem_explorer_output_dir")
  if (is.null(output_dir) || !dir.exists(output_dir)) {
    stop_dem("El servicio no tiene configurada la carpeta temporal de resultados.", 503)
  }

  job_id <- paste0(
    format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
    sprintf("%06d", sample.int(999999, 1))
  )

  base_name <- safe_name(aoi_name)
  tif_name <- paste0("DEM_IGN_2017_10m_", base_name, "_", job_id, ".tif")
  csv_name <- paste0("Estadisticas_DEM_", base_name, "_", job_id, ".csv")
  png_name <- paste0("Vista_DEM_", base_name, "_", job_id, ".png")
  hill_name <- paste0("Hillshade_DEM_", base_name, "_", job_id, ".png")

  tif_path <- file.path(output_dir, tif_name)
  csv_path <- file.path(output_dir, csv_name)
  png_path <- file.path(output_dir, png_name)
  hill_path <- file.path(output_dir, hill_name)

  work_dir <- file.path(.memory_work_root, paste0("job_", job_id))
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  completed <- FALSE
  on.exit({
    unlink(work_dir, recursive = TRUE, force = TRUE)
    if (!completed) {
      unlink(c(tif_path, csv_path, png_path, hill_path), force = TRUE)
    }
    invisible(gc())
  }, add = TRUE)

  log_dem_stage("job_start", paste0("job=", job_id))

  dem <- get_dem()
  source_path <- get_dem_source()
  inspected <- inspect_aoi(geojson, dem = dem, enforce_limits = TRUE)
  aoi <- inspected$aoi

  log_dem_stage(
    "aoi_validated",
    paste0(
      "area_km2=", round(inspected$area_km2, 3),
      " | source_mode=", source_label(source_path)
    )
  )

  processed <- tryCatch(
    process_with_dem(
      dem = dem,
      aoi = aoi,
      buffer_m = buffer_m,
      source_mode = source_label(source_path),
      output_tif = tif_path,
      work_dir = work_dir
    ),
    error = function(e) {
      if (inherits(e, "dem_explorer_error") && http_status_for_error(e) < 500) stop(e)
      stop_dem(
        paste0(
          "No fue posible completar el recorte del MDE con procesamiento GDAL de memoria acotada. ",
          "Detalle técnico: ", conditionMessage(e)
        ),
        503
      )
    }
  )

  aoi_dem <- processed$aoi_dem
  active_mode <- processed$source_mode
  source_local <- processed$source_path

  # Raster independiente SIN buffer para estadísticas y visualización.
  display_file <- file.path(work_dir, "dem_display_aoi.tif")

  log_dem_stage("display_clip_start")
  dem_display <- gdal_clip_to_polygon(
    source_path = source_local,
    polygon = aoi_dem,
    dem_template = dem,
    output_tif = display_file,
    work_dir = work_dir,
    tag = "aoi"
  )
  log_dem_stage("display_clip_done")

  rm(processed)
  invisible(gc())

  log_dem_stage("stats_start")
  stats <- compute_stats(dem_display, aoi_dem, buffer_m)
  utils::write.csv(
    stats,
    csv_path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  log_dem_stage("stats_done")
  invisible(gc())

  log_dem_stage("preview_start")
  web_visual <- prepare_web_visual_layers(
    dem_display,
    aoi_dem,
    work_dir = work_dir
  )

  dem_web <- web_visual$dem
  hill_web <- web_visual$hillshade
  rm(web_visual, dem_display)
  invisible(gc())

  preview_coordinates <- extent_corners_lonlat(dem_web)
  preview_rows <- terra::nrow(dem_web)
  preview_cols <- terra::ncol(dem_web)

  log_dem_stage(
    "preview_ready",
    paste0("rows=", preview_rows, " | cols=", preview_cols)
  )

  render_elevation_preview(dem_web, png_path)
  rm(dem_web)
  invisible(gc())

  render_hillshade_preview(hill_web, hill_path)
  rm(hill_web)
  invisible(gc())

  final_dem <- terra::rast(tif_path)
  final_resolution <- unname(terra::res(final_dem))
  final_rows <- terra::nrow(final_dem)
  final_cols <- terra::ncol(final_dem)
  rm(final_dem)
  invisible(gc())

  result <- list(
    ok = TRUE,
    job_id = job_id,
    aoi_name = base_name,
    source_mode = active_mode,
    stats = as.list(stats[1, , drop = FALSE]),
    preview_url = paste0("/outputs/", png_name),
    hillshade_url = paste0("/outputs/", hill_name),
    preview_coordinates = preview_coordinates,
    dem_url = paste0("/outputs/", tif_name),
    statistics_url = paste0("/outputs/", csv_name),
    expires_in_minutes = PUBLIC_LIMITS$output_ttl_min,
    metadata = list(
      source = "MDE IGN 2017",
      provider = "Instituto Geográfico Nacional / SNIT",
      source_mode = active_mode,
      resolution_m = final_resolution,
      crs = "CR05 / CRTM05 (EPSG:5367)",
      raster_rows = final_rows,
      raster_cols = final_cols,
      preview_crs = "EPSG:3857",
      preview_rows = preview_rows,
      preview_cols = preview_cols,
      preview_max_cells = .preview_max_cells,
      processing_engine = "GDAL streaming + terra preview",
      aoi_features = inspected$feature_count,
      aoi_vertices = inspected$vertex_count,
      aoi_area_km2 = inspected$area_km2
    )
  )

  completed <- TRUE
  log_dem_stage("job_done", paste0("job=", job_id))
  result
}
# --- end GDAL streaming clip v0.5.8 ------------------------------------------
