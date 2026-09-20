# --- Statistics compatibility v0.5.9 -----------------------------------------
# Evita el paso indirecto de `fun` a terra::global(), que en la version de
# terra del contenedor puede ser evaluado como simbolo y producir:
# "could not find function \"fun\"".
APP_VERSION <- "0.5.9"

.compute_global_scalar <- function(x, stat_name) {
  out <- switch(
    stat_name,
    notNA = terra::global(x, "notNA", na.rm = TRUE),
    min = terra::global(x, "min", na.rm = TRUE),
    max = terra::global(x, "max", na.rm = TRUE),
    mean = terra::global(x, "mean", na.rm = TRUE),
    sd = terra::global(x, "sd", na.rm = TRUE),
    median = terra::global(x, "median", na.rm = TRUE),
    stop(paste0("Estadistico no soportado: ", stat_name))
  )
  as.numeric(out[1, 1])
}

compute_stats <- function(x, aoi_projected, buffer_m) {
  log_dem_stage("stats_notna_start")
  n_valid <- .compute_global_scalar(x, "notNA")
  log_dem_stage("stats_notna_done", paste0("valid_cells=", format(n_valid, scientific = FALSE)))

  if (!is.finite(n_valid) || n_valid <= 0) {
    stop_dem(
      "El AOI no contiene celdas válidas de elevación en el MDE disponible.",
      422
    )
  }

  log_dem_stage("stats_min_start")
  zmin <- .compute_global_scalar(x, "min")
  log_dem_stage("stats_min_done", paste0("min=", zmin))

  log_dem_stage("stats_max_start")
  zmax <- .compute_global_scalar(x, "max")
  log_dem_stage("stats_max_done", paste0("max=", zmax))

  log_dem_stage("stats_mean_start")
  zmean <- .compute_global_scalar(x, "mean")
  log_dem_stage("stats_mean_done", paste0("mean=", zmean))

  log_dem_stage("stats_sd_start")
  zsd <- .compute_global_scalar(x, "sd")
  log_dem_stage("stats_sd_done", paste0("sd=", zsd))

  log_dem_stage("stats_median_start")
  q50 <- .compute_global_scalar(x, "median")
  log_dem_stage("stats_median_done", paste0("median=", q50))

  n_total <- terra::ncell(x)
  n_nodata <- n_total - n_valid
  r <- terra::res(x)
  cell_area_m2 <- abs(r[1] * r[2])
  area_valid_km2 <- n_valid * cell_area_m2 / 1e6
  aoi_area_km2 <- sum(terra::expanse(aoi_projected, unit = "km"), na.rm = TRUE)

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
# --- end Statistics compatibility v0.5.9 -------------------------------------
