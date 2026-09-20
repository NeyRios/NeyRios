# --- Memory-safe processing v0.5.7 -------------------------------------------
# Objetivo: procesar AOI grandes en Railway con limite ~1 GB RAM sin degradar
# el producto cientifico (GeoTIFF 10 m EPSG:5367 ni estadisticas).
#
# Estrategia:
# - operaciones raster pesadas respaldadas en disco temporal;
# - estadisticas por bloques (incluida mediana nativa de terra);
# - preview web reducido antes de hillshade/reproyeccion;
# - DEM + hillshade web siguen compartiendo exactamente la misma malla EPSG:3857.
APP_VERSION <- "0.5.7"

.memory_work_root <- Sys.getenv(
  "DEM_WORK_DIR",
  unset = "/tmp/dem_explorer_work"
)
dir.create(.memory_work_root, recursive = TRUE, showWarnings = FALSE)

.preview_max_cells <- suppressWarnings(as.integer(
  Sys.getenv("DEM_PREVIEW_MAX_CELLS", unset = "300000")
))
if (!is.finite(.preview_max_cells) || .preview_max_cells < 100000L) {
  .preview_max_cells <- 300000L
}

# Fuerza a terra a preferir procesamiento respaldado en disco.
try(
  terra::terraOptions(
    memfrac = 0.15,
    todisk = TRUE,
    tempdir = .memory_work_root
  ),
  silent = TRUE
)
Sys.setenv(GDAL_CACHEMAX = "64")

.memory_wopt <- function(compress = TRUE) {
  gdal_opts <- c("TILED=YES", "BIGTIFF=IF_SAFER")
  if (isTRUE(compress)) {
    gdal_opts <- c(gdal_opts, "COMPRESS=DEFLATE", "PREDICTOR=3")
  }
  list(
    datatype = "FLT4S",
    gdal = gdal_opts
  )
}

compute_stats <- function(x, aoi_projected, buffer_m) {
  n_valid <- safe_global_scalar(x, "notNA")
  if (!is.finite(n_valid) || n_valid <= 0) {
    stop_dem(
      "El AOI no contiene celdas válidas de elevación en el MDE disponible.",
      422
    )
  }

  # Estas funciones nativas de terra se evalúan por bloques.
  basic <- terra::global(
    x,
    fun = c("min", "max", "mean", "sd"),
    na.rm = TRUE
  )

  # Importante: usar la mediana nativa evita materializar decenas de millones
  # de valores en un vector R.
  q50 <- safe_global_scalar(x, "median")

  n_total <- terra::ncell(x)
  n_nodata <- n_total - n_valid
  r <- terra::res(x)
  cell_area_m2 <- abs(r[1] * r[2])
  area_valid_km2 <- n_valid * cell_area_m2 / 1e6
  aoi_area_km2 <- sum(terra::expanse(aoi_projected, unit = "km"), na.rm = TRUE)

  zmin <- as.numeric(basic$min[1])
  zmax <- as.numeric(basic$max[1])
  zmean <- as.numeric(basic$mean[1])
  zsd <- as.numeric(basic$sd[1])

  data.frame(
    source = "MDE IGN 2017",
    provider = "Instituto Geográfico Nacional / SNIT",
    resolution_m = r[1],
    crs = "EPSG:5367",
    buffer_m = buffer_m,
    aoi_area_km2 = aoi_area_km2,
    valid_area_km2 = area_valid_km2,
    elevation_min_m = zmin,
    elevation_max_m = zmax,
    elevation_mean_m = zmean,
    elevation_median_m = q50,
    elevation_sd_m = zsd,
    elevation_range_m = zmax - zmin,
    valid_cells = n_valid,
    nodata_cells = n_nodata,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
}

prepare_preview_raster <- function(
  x,
  max_preview_cells = .preview_max_cells,
  work_dir = .memory_work_root
) {
  preview <- x

  if (terra::ncell(preview) > max_preview_cells) {
    fact <- ceiling(sqrt(terra::ncell(preview) / max_preview_cells))
    preview_file <- file.path(work_dir, "preview_native.tif")

    preview <- terra::aggregate(
      preview,
      fact = fact,
      fun = "mean",
      na.rm = TRUE,
      filename = preview_file,
      overwrite = TRUE,
      wopt = .memory_wopt(compress = TRUE)
    )
  }

  preview
}

compute_native_hillshade <- function(
  x,
  work_dir = .memory_work_root
) {
  slope_file <- file.path(work_dir, "preview_slope.tif")
  aspect_file <- file.path(work_dir, "preview_aspect.tif")
  hill_file <- file.path(work_dir, "preview_hillshade_native.tif")

  slope <- terra::terrain(
    x,
    v = "slope",
    unit = "radians",
    neighbors = 8,
    filename = slope_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = TRUE)
  )

  aspect <- terra::terrain(
    x,
    v = "aspect",
    unit = "radians",
    neighbors = 8,
    filename = aspect_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = TRUE)
  )

  hill <- terra::shade(
    slope,
    aspect,
    angle = 45,
    direction = 315,
    normalize = TRUE,
    filename = hill_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = TRUE)
  )

  rm(slope, aspect)
  invisible(gc())

  hill
}

prepare_web_visual_layers <- function(
  dem_display,
  aoi_dem,
  work_dir = .memory_work_root
) {
  dem_native_preview <- prepare_preview_raster(
    dem_display,
    max_preview_cells = .preview_max_cells,
    work_dir = work_dir
  )

  hill_native <- compute_native_hillshade(
    dem_native_preview,
    work_dir = work_dir
  )

  dem_web_raw_file <- file.path(work_dir, "preview_dem_3857_raw.tif")
  dem_web_file <- file.path(work_dir, "preview_dem_3857.tif")
  hill_web_raw_file <- file.path(work_dir, "preview_hill_3857_raw.tif")
  hill_web_file <- file.path(work_dir, "preview_hill_3857.tif")

  dem_web_raw <- terra::project(
    dem_native_preview,
    "EPSG:3857",
    method = "bilinear",
    filename = dem_web_raw_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = TRUE)
  )

  aoi_web <- terra::project(aoi_dem, "EPSG:3857")

  dem_web <- terra::mask(
    dem_web_raw,
    aoi_web,
    filename = dem_web_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = TRUE)
  )
  rm(dem_web_raw)
  invisible(gc())

  # Se usa dem_web como template exacto para preservar el alineamiento
  # cartográfico validado entre DEM e hillshade.
  hill_web_raw <- terra::project(
    hill_native,
    dem_web,
    method = "bilinear",
    filename = hill_web_raw_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = TRUE)
  )

  hill_web <- terra::mask(
    hill_web_raw,
    aoi_web,
    filename = hill_web_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = TRUE)
  )
  rm(hill_web_raw, hill_native, dem_native_preview)
  invisible(gc())

  same_geom <- terra::compareGeom(
    dem_web,
    hill_web,
    stopOnError = FALSE,
    crs = TRUE,
    ext = TRUE,
    rowcol = TRUE,
    res = TRUE
  )

  if (!isTRUE(same_geom)) {
    stop("DEM e hillshade de visualización no comparten la misma malla EPSG:3857.")
  }

  list(
    dem = dem_web,
    hillshade = hill_web,
    crs = "EPSG:3857"
  )
}

process_with_dem <- function(
  dem,
  aoi,
  buffer_m,
  source_mode,
  output_tif,
  work_dir
) {
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

  crop_file <- file.path(work_dir, "dem_crop_buffer.tif")

  dem_crop <- terra::crop(
    dem,
    aoi_work,
    snap = "out",
    filename = crop_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = FALSE)
  )

  # El GeoTIFF final se genera directamente durante la máscara, evitando
  # mantener otra copia de 10 m en memoria.
  dem_aoi <- terra::mask(
    dem_crop,
    aoi_work,
    filename = output_tif,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = TRUE)
  )

  rm(dem_crop)
  invisible(gc())

  list(
    aoi_dem = aoi_dem,
    dem_aoi = dem_aoi,
    source_mode = source_mode
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

  dem <- get_dem()
  source_path <- get_dem_source()
  inspected <- inspect_aoi(geojson, dem = dem, enforce_limits = TRUE)
  aoi <- inspected$aoi

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
      if (inherits(e, "dem_explorer_error") && http_status_for_error(e) < 500) {
        stop(e)
      }
      stop_dem(
        paste0(
          "No fue posible completar el recorte del MDE. ",
          "El procesamiento fue configurado para usar disco temporal y limitar RAM. ",
          "Detalle técnico: ",
          conditionMessage(e)
        ),
        503
      )
    }
  )

  aoi_dem <- processed$aoi_dem
  active_mode <- processed$source_mode

  dem_saved <- terra::rast(tif_path)

  # Estadísticas y visualización deben excluir el buffer. Se genera una
  # versión de trabajo respaldada en disco con el AOI original.
  display_crop_file <- file.path(work_dir, "dem_display_crop.tif")
  display_file <- file.path(work_dir, "dem_display_aoi.tif")

  display_crop <- terra::crop(
    dem_saved,
    aoi_dem,
    snap = "out",
    filename = display_crop_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = FALSE)
  )

  dem_display <- terra::mask(
    display_crop,
    aoi_dem,
    filename = display_file,
    overwrite = TRUE,
    wopt = .memory_wopt(compress = TRUE)
  )

  rm(display_crop, dem_saved, processed)
  invisible(gc())

  stats <- compute_stats(dem_display, aoi_dem, buffer_m)
  utils::write.csv(
    stats,
    csv_path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  invisible(gc())

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
      aoi_features = inspected$feature_count,
      aoi_vertices = inspected$vertex_count,
      aoi_area_km2 = inspected$area_km2
    )
  )

  completed <- TRUE
  result
}
# --- end Memory-safe processing v0.5.7 ---------------------------------------
