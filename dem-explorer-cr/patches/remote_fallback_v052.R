# --- Cloud storage v0.5.6 ----------------------------------------------------
# Preferencia operativa:
# 1) Railway Bucket privado (misma copia operacional del MDE)
# 2) URL remota directa validada mediante /vsicurl/
# 3) gdown sobre el file ID extraido de la MISMA URL remota
#
# La fuente cientifica no cambia: MDE IGN 2017, 10 m, EPSG:5367.
APP_VERSION <- "0.5.6"

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

bucket_configured <- function() {
  all(nzchar(c(
    Sys.getenv("DEM_BUCKET_NAME", unset = ""),
    Sys.getenv("DEM_BUCKET_ENDPOINT", unset = ""),
    Sys.getenv("DEM_BUCKET_KEY", unset = ""),
    Sys.getenv("AWS_ACCESS_KEY_ID", unset = ""),
    Sys.getenv("AWS_SECRET_ACCESS_KEY", unset = "")
  )))
}

download_bucket_dem_cache <- function() {
  if (!bucket_configured()) {
    stop("Railway Bucket no configurado.")
  }

  dest <- get_remote_cache_path()
  if (is_tiff_file(dest)) return(dest)

  aws_bin <- Sys.getenv("AWS_BIN", unset = "/opt/gdown/bin/aws")
  if (!file.exists(aws_bin)) stop("AWS CLI no disponible en el contenedor.")

  part <- paste0(dest, ".bucket.part")
  unlink(part, force = TRUE)

  bucket <- Sys.getenv("DEM_BUCKET_NAME")
  endpoint <- Sys.getenv("DEM_BUCKET_ENDPOINT")
  key <- Sys.getenv("DEM_BUCKET_KEY")
  region <- Sys.getenv("AWS_DEFAULT_REGION", unset = "auto")

  args <- c(
    "s3", "cp",
    shQuote(paste0("s3://", bucket, "/", key)),
    shQuote(part),
    "--endpoint-url", shQuote(endpoint),
    "--region", shQuote(region),
    "--no-progress"
  )

  message(
    "Intentando obtener el MDE desde Railway Bucket: s3://",
    bucket, "/", key
  )

  out <- tryCatch(
    system2(aws_bin, args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )

  status <- attr(out, "status")
  if (is.null(status)) status <- 0L

  if (status != 0L || !is_tiff_file(part)) {
    bytes <- if (file.exists(part)) file.info(part)$size else 0
    unlink(part, force = TRUE)
    stop(
      paste0(
        "Railway Bucket no contiene aun un GeoTIFF valido en la clave configurada. ",
        "exit=", status, "; bytes=", bytes,
        if (length(out)) paste0("; detalle=", paste(tail(out, 8), collapse = " | ")) else ""
      )
    )
  }

  if (!file.rename(part, dest)) {
    unlink(part, force = TRUE)
    stop("No fue posible finalizar la cache local del MDE descargado del Railway Bucket.")
  }

  message(
    "MDE obtenido correctamente desde Railway Bucket. bytes=",
    file.info(dest)$size
  )

  dest
}

extract_drive_file_id <- function(url) {
  m <- regexec("[?&]id=([^&]+)", url, perl = TRUE)
  hit <- regmatches(url, m)[[1]]
  if (length(hit) >= 2L && nzchar(hit[[2]])) {
    return(utils::URLdecode(hit[[2]]))
  }

  m2 <- regexec("/file/d/([^/?&]+)", url, perl = TRUE)
  hit2 <- regmatches(url, m2)[[1]]
  if (length(hit2) >= 2L && nzchar(hit2[[2]])) return(hit2[[2]])

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
    stop_dem("El mecanismo gdown no esta disponible en el contenedor.", 503)
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
    "Se intentara descargar EL MISMO archivo usando el file ID extraido de esa URL: ",
    drive_id
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
        "gdown exit=", exit_status,
        "; bytes recibidos=", size_part,
        ". Detalle: ", tail_log
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
  )) {
    if (bucket_configured()) return("railway-bucket-cache")
    return("remote-downloaded-cache")
  }
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
      # Preferir Railway Bucket cuando este configurado y el objeto exista.
      if (bucket_configured()) {
        bucket_try <- tryCatch(
          download_bucket_dem_cache(),
          error = function(e) e
        )

        if (is.character(bucket_try) && file.exists(bucket_try)) {
          opened <- list(dem = open_dem_source(bucket_try), path = bucket_try)
          assign(cache_name, opened$dem, envir = .dem_cache)
          assign(source_name, opened$path, envir = .dem_cache)
          return(opened$dem)
        }

        warning(
          "Railway Bucket configurado, pero el objeto DEM aun no esta disponible o no es valido. ",
          if (inherits(bucket_try, "error")) conditionMessage(bucket_try) else ""
        )
      }

      opened <- tryCatch(
        list(dem = open_dem_source(remote_path), path = remote_path),
        error = function(e) {
          remote_error <- conditionMessage(e)

          cached <- tryCatch(
            download_remote_dem_cache(),
            error = function(cache_error) cache_error
          )

          if (is.character(cached) && length(cached) == 1L && file.exists(cached)) {
            return(list(dem = open_dem_source(cached), path = cached))
          }

          if (nzchar(local_path)) {
            return(list(dem = open_dem_source(local_path), path = local_path))
          }

          cache_detail <- if (inherits(cached, "error")) {
            conditionMessage(cached)
          } else {
            "sin detalle adicional"
          }

          stop_dem(
            paste0(
              "El MDE no esta disponible en Railway Bucket y la URL remota tampoco pudo abrirse. ",
              "Detalle /vsicurl/: ", remote_error,
              ". Detalle de fallback: ", cache_detail
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
# --- end Cloud storage v0.5.6 -----------------------------------------------
