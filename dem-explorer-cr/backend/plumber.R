project_dir <- Sys.getenv("DEM_PROJECT_DIR", unset = "")

if (!nzchar(project_dir)) {
  stop("DEM_PROJECT_DIR no está definido. Inicie la aplicación con source('run.R').")
}

core_file <- file.path(project_dir, "backend", "R", "dem_core.R")
if (!file.exists(core_file)) {
  stop(paste0("No se encontró el motor DEM en: ", core_file))
}
source(core_file, local = TRUE)

#* Cabeceras y límites básicos para la beta pública.
#* @filter security
function(req, res) {
  res$setHeader("X-Content-Type-Options", "nosniff")
  res$setHeader("Referrer-Policy", "strict-origin-when-cross-origin")
  res$setHeader("X-Frame-Options", "SAMEORIGIN")
  res$setHeader("Permissions-Policy", "geolocation=(), camera=(), microphone=()")

  if (startsWith(req$PATH_INFO %||% "", "/api/") || identical(req$PATH_INFO, "/health")) {
    res$setHeader("Cache-Control", "no-store")
  }

  if (identical(req$REQUEST_METHOD, "POST")) {
    content_length <- suppressWarnings(as.numeric(req$HTTP_CONTENT_LENGTH %||% NA_real_))
    max_bytes <- PUBLIC_LIMITS$max_request_mb * 1024^2
    if (is.finite(content_length) && content_length > max_bytes) {
      res$status <- 413
      return(list(
        ok = FALSE,
        error = sprintf(
          "La solicitud supera el tamaño máximo permitido de %.0f MB.",
          PUBLIC_LIMITS$max_request_mb
        )
      ))
    }
  }

  plumber::forward()
}

#* Entrada amigable: redirige al visor.
#* @get /
function(res) {
  res$status <- 302
  res$setHeader("Location", "/app/index.html")
  ""
}

#* Estado del servicio.
#* @get /health
function() {
  cleanup_old_outputs()
  list(
    ok = TRUE,
    service = "DEM Explorer CR",
    version = APP_VERSION,
    stage = "Cloud Beta",
    author = "Ney Ríos",
    institution = "Unidad de Cuencas - CATIE",
    study = "Estudio de Recarga Potencial en la Península de Nicoya (CATIE-SENARA)",
    technical_collaboration = "ChatGPT (OpenAI)",
    dem = "MDE IGN 2017",
    source_preference = "remote",
    resolution_m = 10,
    crs = "EPSG:5367",
    limits = get_public_limits()
  )
}

#* Metadatos de la fuente DEM activa.
#* @get /api/dem/metadata
function(res) {
  tryCatch(
    dem_metadata(),
    error = function(e) {
      res$status <- http_status_for_error(e, 500L)
      list(ok = FALSE, error = conditionMessage(e))
    }
  )
}

#* Valida el AOI antes de ejecutar el recorte del DEM.
#* @post /api/aoi/validate
#* @parser json
function(req, res) {
  tryCatch({
    body <- req$body
    if (is.null(body) || is.null(body$geojson)) {
      res$status <- 400
      return(list(ok = FALSE, error = "No se recibió la geometría del Shapefile."))
    }
    validate_aoi_request(body$geojson)
  }, error = function(e) {
    res$status <- http_status_for_error(e, 422L)
    list(ok = FALSE, error = conditionMessage(e))
  })
}

#* Recorta el DEM con el AOI derivado del Shapefile cargado en el navegador.
#* @post /api/dem/analyze
#* @parser json
function(req, res) {
  tryCatch({
    body <- req$body

    if (is.null(body) || is.null(body$geojson)) {
      res$status <- 400
      return(list(ok = FALSE, error = "No se recibió la geometría del Shapefile."))
    }

    buffer_m <- suppressWarnings(as.numeric(body$buffer_m %||% 0))
    if (!is.finite(buffer_m) || buffer_m < 0 || buffer_m > PUBLIC_LIMITS$max_buffer_m) {
      res$status <- 422
      return(list(
        ok = FALSE,
        error = sprintf("El buffer debe estar entre 0 y %.0f m.", PUBLIC_LIMITS$max_buffer_m)
      ))
    }

    aoi_name <- as.character(body$aoi_name %||% "AOI")

    process_dem_aoi(
      geojson = body$geojson,
      buffer_m = buffer_m,
      aoi_name = aoi_name
    )
  }, error = function(e) {
    res$status <- http_status_for_error(e, 500L)
    list(ok = FALSE, error = conditionMessage(e))
  })
}
