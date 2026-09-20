# --- Cloud fallback v0.5.3 ----------------------------------------------------
# Conserva EXACTAMENTE la URL remota activa del DEM.
# 1) Intenta lectura aleatoria GDAL /vsicurl/.
# 2) Si la URL entrega una pagina HTML/intersticial a GDAL, usa gdown sobre
#    LA MISMA URL directa para resolver la descarga del mismo archivo.
# 3) Guarda el GeoTIFF en cache efimera del contenedor y lo reutiliza.
APP_VERSION <- "0.5.4"

get_remote_cache_path <- function() {
  cache_dir <- trimws(Sys.getenv(
    "DEM_REMOTE_CACHE_DIR",
    unset = "/tmp/dem_explorer_cr_cache"
  ))
  if (!nzchar(cache_dir)) cache_dir <- "/tmp/dem_explorer_cr_cache"
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(cache_dir, "MDE_IGN_2017_10m_CRTM05.tif")
}

is_tiff_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  size <- file.info(path)$size
  if (!is.finite(size) || size < 100 * 1024^2) return(FALSE)

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = "raw", n = 4L)
  if (length(sig) < 4L) return(FALSE)
  hex <- paste(sprintf("%02X", as.integer(sig)), collapse = " ")
  hex %in% c("49 49 2A 00", "4D 4D 00 2A", "49 49 2B 00", "4D 4D 00 2B")
}

download_remote_dem_cache <- function() {
  dest <- get_remote_cache_path()
  if (is_tiff_file(dest)) return(dest)

  lock_dir <- paste0(dest, ".lock")
  acquired <- dir.create(lock_dir, recursive = FALSE, showWarnings = FALSE)

  if (!acquired) {
    deadline <- Sys.time() + 1800
    while (Sys.time() < deadline) {
      if (is_tiff_file(dest)) return(dest)
      Sys.sleep(2)
    }
    stop_dem(
      "La descarga temporal del MDE sigue en curso o no pudo completarse. Intente nuevamente en unos minutos.",
      503
    )
  }

  on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)

  part <- paste0(dest, ".part")
  unlink(part, force = TRUE)

  gdown_bin <- Sys.getenv("GDOWN_BIN", unset = "/opt/gdown/bin/gdown")
  if (!file.exists(gdown_bin)) {
    stop_dem(
      "El mecanismo de descarga robusta del MDE no esta disponible en el contenedor.",
      503
    )
  }

  message(
    "/vsicurl/ no pudo abrir la URL directa del MDE. ",
    "Se intentara descargar EL MISMO archivo con gdown a cache temporal: ",
    dest
  )

  cmd_out <- tryCatch(
    system2(
      command = gdown_bin,
      args = c(
        "--quiet",
        "-O", shQuote(part),
        shQuote(get_remote_url())
      ),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(
      paste("Error ejecutando gdown:", conditionMessage(e)),
      status = 1L
    )
  )

  exit_status <- attr(cmd_out, "status")
  if (is.null(exit_status)) exit_status <- 0L

  if (exit_status != 0L || !is_tiff_file(part)) {
    size_part <- if (file.exists(part)) file.info(part)$size else 0
    tail_log <- if (length(cmd_out)) {
      paste(tail(cmd_out, 8), collapse = " | ")
    } else {
      "sin salida adicional"
    }

    unlink(part, force = TRUE)

    stop_dem(
      paste0(
        "La URL directa del MDE no pudo resolverse como GeoTIFF desde Railway. ",
        "gdown exit=", exit_status,
        "; bytes recibidos=", size_part,
        ". Detalle: ", tail_log,
        ". Se mantiene exactamente la misma URL remota configurada."
      ),
      503
    )
  }

  if (!file.rename(part, dest)) {
    unlink(part, force = TRUE)
    stop_dem("No fue posible finalizar la cache temporal del MDE en el servidor.", 503)
  }

  message(
    "MDE remoto descargado correctamente desde la URL directa configurada. Cache: ",
    dest,
    " | bytes: ",
    file.info(dest)$size
  )

  dest
}

source_label <- function(path) {
  if (startsWith(path, "/vsicurl/")) return("remote-range")
  if (identical(
    normalizePath(path, winslash = "/", mustWork = FALSE),
    normalizePath(get_remote_cache_path(), winslash = "/", mustWork = FALSE)
  )) return("remote-downloaded-cache")
  "local"
}

get_dem <- function(force_local = FALSE) {
  cache_name <- if (force_local) "dem_local" else "dem"
  source_name <- if (force_local) "source_path_local" else "source_path"

  if (!exists(cache_name, envir = .dem_cache, inherits = FALSE)) {
    remote_path <- get_remote_vsi_url()
    local_path <- get_local_fallback()

    if (force_local) {
      if (!nzchar(local_path)) {
        stop_dem("No existe un DEM local de respaldo configurado.", 503)
      }
      opened <- list(dem = open_dem_source(local_path), path = local_path)
    } else {
      opened <- tryCatch(
        list(dem = open_dem_source(remote_path), path = remote_path),
        error = function(e) {
          remote_error <- conditionMessage(e)

          cached <- tryCatch(
            download_remote_dem_cache(),
            error = function(cache_error) cache_error
          )

          if (is.character(cached) && length(cached) == 1L && file.exists(cached)) {
            warning(
              "No fue posible abrir el DEM por /vsicurl/. Se usara una copia temporal ",
              "del MISMO GeoTIFF descargado desde la URL directa configurada. ",
              "Detalle /vsicurl/: ", remote_error
            )
            return(list(dem = open_dem_source(cached), path = cached))
          }

          if (nzchar(local_path)) {
            warning(
              "No fue posible abrir ni descargar el DEM remoto. Se usara el respaldo local. Detalle: ",
              remote_error
            )
            return(list(dem = open_dem_source(local_path), path = local_path))
          }

          cache_detail <- if (inherits(cached, "error")) {
            conditionMessage(cached)
          } else {
            "sin detalle adicional"
          }

          stop_dem(
            paste0(
              "El servicio no pudo abrir el MDE por lectura remota ni descargar una copia temporal ",
              "desde la misma URL directa configurada. Detalle /vsicurl/: ", remote_error,
              ". Detalle de cache: ", cache_detail
            ),
            503
          )
        }
      )
    }

    assign(cache_name, opened$dem, envir = .dem_cache)
    assign(source_name, opened$path, envir = .dem_cache)
  }

  get(cache_name, envir = .dem_cache, inherits = FALSE)
}
# --- end Cloud fallback v0.5.3 -----------------------------------------------
