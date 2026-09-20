# --- Cloud fallback v0.5.5 ----------------------------------------------------
# Conserva EXACTAMENTE la URL remota activa del DEM para la lectura primaria.
# Si /vsicurl/ recibe HTML, extrae el file ID de ESA MISMA URL y usa gdown
# por ID para descargar el mismo archivo a cache efimera del contenedor.
APP_VERSION <- "0.5.5"

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
  if (!is.finite(size) || size < 1024^3) return(FALSE)

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = "raw", n = 4L)
  if (length(sig) < 4L) return(FALSE)
  hex <- paste(sprintf("%02X", as.integer(sig)), collapse = " ")
  hex %in% c("49 49 2A 00", "4D 4D 00 2A", "49 49 2B 00", "4D 4D 00 2B")
}

extract_drive_file_id <- function(url) {
  m <- regexec("[?&]id=([^&]+)", url, perl = TRUE)
  hit <- regmatches(url, m)[[1]]
  if (length(hit) >= 2L && nzchar(hit[[2]])) {
    return(utils::URLdecode(hit[[2]]))
  }

  # Compatibilidad adicional con /file/d/FILE_ID/ si en el futuro la URL cambia.
  m2 <- regexec("/file/d/([^/?&]+)", url, perl = TRUE)
  hit2 <- regmatches(url, m2)[[1]]
  if (length(hit2) >= 2L && nzchar(hit2[[2]])) {
    return(hit2[[2]])
  }

  ""
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

  remote_url <- get_remote_url()
  drive_id <- extract_drive_file_id(remote_url)

  if (!nzchar(drive_id)) {
    stop_dem(
      "No fue posible extraer el identificador del archivo desde la URL directa configurada del MDE.",
      503
    )
  }

  message(
    "/vsicurl/ no pudo abrir la URL directa del MDE. ",
    "Se descargara EL MISMO archivo usando el file ID extraido de esa URL: ",
    drive_id,
    " | cache temporal: ",
    dest
  )

  cmd_out <- character()
  exit_status <- 1L

  for (attempt in seq_len(3L)) {
    cmd_out <- tryCatch(
      system2(
        command = gdown_bin,
        args = c(
          "--quiet",
          "--continue",
          "-O", shQuote(part),
          shQuote(drive_id)
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

    if (exit_status == 0L && is_tiff_file(part)) break

    message(
      "Intento gdown ", attempt, "/3 no produjo aun un GeoTIFF completo. ",
      "exit=", exit_status,
      " | bytes=", if (file.exists(part)) file.info(part)$size else 0
    )
  }

  if (exit_status != 0L || !is_tiff_file(part)) {
    size_part <- if (file.exists(part)) file.info(part)$size else 0
    tail_log <- if (length(cmd_out)) {
      paste(tail(cmd_out, 12), collapse = " | ")
    } else {
      "sin salida adicional"
    }

    unlink(part, force = TRUE)

    stop_dem(
      paste0(
        "La URL directa del MDE no pudo resolverse como GeoTIFF desde Railway. ",
        "Se uso el file ID extraido de la misma URL. ",
        "gdown exit=", exit_status,
        "; bytes recibidos=", size_part,
        ". Detalle: ", tail_log,
        ". Se mantiene exactamente la misma fuente remota configurada."
      ),
      503
    )
  }

  if (!file.rename(part, dest)) {
    unlink(part, force = TRUE)
    stop_dem("No fue posible finalizar la cache temporal del MDE en el servidor.", 503)
  }

  message(
    "MDE remoto descargado correctamente desde el mismo archivo configurado. Cache: ",
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
              "del MISMO GeoTIFF descargado usando el file ID de la URL directa configurada. ",
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
              "del mismo archivo configurado. Detalle /vsicurl/: ", remote_error,
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
# --- end Cloud fallback v0.5.5 -----------------------------------------------
