# --- Cloud fallback v0.5.2 ----------------------------------------------------
# Conserva la URL remota activa. Si GDAL /vsicurl/ no puede leerla por rangos,
# descarga el MISMO GeoTIFF a almacenamiento efimero del contenedor y reutiliza
# esa copia mientras la instancia siga viva.
APP_VERSION <- "0.5.2"

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
    deadline <- Sys.time() + 900
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

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  timeout_value <- suppressWarnings(as.numeric(old_timeout))
  if (!length(timeout_value) || !is.finite(timeout_value)) timeout_value <- 60
  options(timeout = max(3600, timeout_value))

  message(
    "/vsicurl/ no pudo abrir el MDE; descargando la misma URL remota a cache temporal: ",
    dest
  )

  ok <- tryCatch({
    utils::download.file(
      url = get_remote_url(),
      destfile = part,
      mode = "wb",
      method = "libcurl",
      quiet = FALSE
    )
    TRUE
  }, error = function(e) {
    warning("Fallo la descarga temporal del MDE: ", conditionMessage(e))
    FALSE
  })

  if (!ok || !is_tiff_file(part)) {
    unlink(part, force = TRUE)
    stop_dem(
      paste0(
        "La URL remota del MDE respondio, pero no entrego un GeoTIFF valido para la cache temporal. ",
        "Se mantiene la misma fuente remota configurada."
      ),
      503
    )
  }

  if (!file.rename(part, dest)) {
    unlink(part, force = TRUE)
    stop_dem("No fue posible finalizar la cache temporal del MDE en el servidor.", 503)
  }

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
              "descargada desde la misma URL remota. Detalle /vsicurl/: ",
              remote_error
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
              "desde la misma URL configurada. Detalle /vsicurl/: ", remote_error,
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
# --- end Cloud fallback v0.5.2 -----------------------------------------------
