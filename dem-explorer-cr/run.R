if (!requireNamespace("plumber", quietly = TRUE)) {
  stop("Falta el paquete 'plumber'. Ejecute primero: source('install_packages.R')")
}

# -----------------------------------------------------------------------------
# DEM Explorer CR v0.5.1 - Cloud Beta / Railway Ready
# Fuente principal: COG remoto por URL.
# Respaldo opcional: DEM local, solo durante desarrollo o contingencia.
# -----------------------------------------------------------------------------

.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) NA_character_
)

if (is.na(.this_file)) {
  stop(
    paste0(
      "No fue posible determinar la ubicación de run.R.\n",
      "Abra dem-explorer-cr.Rproj y ejecute de nuevo: source('run.R')"
    )
  )
}

.project_dir <- dirname(.this_file)
Sys.setenv(DEM_PROJECT_DIR = .project_dir)

remote_default <- paste0(
  "https://drive.usercontent.google.com/download?",
  "id=1nqYFsRDveZOnyVSu6Gg1Xd1clz8NAVMM",
  "&export=download&confirm=t"
)

if (!nzchar(Sys.getenv("DEM_REMOTE_URL", unset = ""))) {
  Sys.setenv(DEM_REMOTE_URL = remote_default)
}

local_fallback_default <- "D:/DEM_CostaRica/MDE_IGN_2017_10m_CRTM05.tif"
if (!nzchar(Sys.getenv("DEM_LOCAL_PATH", unset = "")) && file.exists(local_fallback_default)) {
  Sys.setenv(DEM_LOCAL_PATH = local_fallback_default)
}

# Ajustes GDAL para lectura HTTP Range del COG remoto.
Sys.setenv(
  GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
  GDAL_HTTP_MAX_RETRY = "3",
  GDAL_HTTP_RETRY_DELAY = "1",
  VSI_CACHE = "TRUE",
  VSI_CACHE_SIZE = "67108864"
)

outputs_dir <- file.path(.project_dir, "outputs")
frontend_dir <- file.path(.project_dir, "frontend")
plumber_file <- file.path(.project_dir, "backend", "plumber.R")
core_file <- file.path(.project_dir, "backend", "R", "dem_core.R")

required_paths <- c(
  plumber_file,
  core_file,
  file.path(frontend_dir, "index.html"),
  file.path(frontend_dir, "app.js"),
  file.path(frontend_dir, "styles.css")
)

missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop(
    paste0(
      "El proyecto está incompleto. Faltan estos archivos:\n",
      paste(missing_paths, collapse = "\n"),
      "\n\nVuelva a extraer el ZIP completo conservando la estructura de carpetas."
    )
  )
}

if (!dir.exists(outputs_dir)) {
  dir.create(outputs_dir, recursive = TRUE)
}

options(
  dem_explorer_output_dir = normalizePath(
    outputs_dir,
    winslash = "/",
    mustWork = TRUE
  )
)

# Depuración inicial de resultados temporales antiguos.
source(core_file, local = TRUE)
cleanup_old_outputs()

pr <- plumber::plumb(plumber_file)
pr <- plumber::pr_static(
  pr,
  "/app",
  normalizePath(frontend_dir, winslash = "/", mustWork = TRUE)
)
pr <- plumber::pr_static(
  pr,
  "/outputs",
  getOption("dem_explorer_output_dir")
)

app_host <- Sys.getenv("APP_HOST", unset = "127.0.0.1")

# Railway y otros proveedores cloud inyectan el puerto público en PORT.
# En desarrollo local se mantiene APP_PORT=8000.
port_value <- Sys.getenv(
  "PORT",
  unset = Sys.getenv("APP_PORT", unset = "8000")
)
app_port <- suppressWarnings(as.integer(port_value))
if (!is.finite(app_port) || app_port < 1 || app_port > 65535) app_port <- 8000L

cat("\n============================================\n")
cat(" DEM Explorer CR - Nivel 1 Cloud Beta v0.5.1\n")
cat("============================================\n")
cat("\nCarpeta del proyecto:\n")
cat(.project_dir, "\n")
cat("\nFuente principal DEM: COG remoto\n")

local_fb <- Sys.getenv("DEM_LOCAL_PATH", unset = "")
if (nzchar(local_fb)) {
  cat("Respaldo local disponible: sí\n")
} else {
  cat("Respaldo local disponible: no\n")
}

cat("\nServidor:\n")
cat(sprintf("host = %s | port = %s\n", app_host, app_port))
cat("\nAcceso local (desarrollo):\n")
cat(sprintf("http://127.0.0.1:%s/\n\n", app_port))
cat("En Railway use el dominio público generado por la plataforma.\n")
cat("El contenedor escucha en APP_HOST=0.0.0.0 y usa PORT cuando Railway lo inyecta.\n\n")

pr$run(host = app_host, port = app_port)
