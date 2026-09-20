FROM rocker/geospatial:latest

RUN apt-get update &&     apt-get install -y --no-install-recommends       unzip       coreutils       libsodium-dev       python3       python3-venv &&     rm -rf /var/lib/apt/lists/*

RUN python3 -m venv /opt/gdown &&     /opt/gdown/bin/pip install --no-cache-dir --upgrade pip gdown awscli

RUN R -q -e "install.packages(c('sodium','plumber','jsonlite','png'), repos='https://cloud.r-project.org'); stopifnot(requireNamespace('sodium', quietly=TRUE), requireNamespace('plumber', quietly=TRUE), requireNamespace('jsonlite', quietly=TRUE), requireNamespace('png', quietly=TRUE))"

WORKDIR /src
COPY dem-explorer-cr/archive /src/archive
COPY dem-explorer-cr/patches /src/patches

RUN cat /src/archive/chunk*.b64 > /src/app.b64 &&     base64 -d /src/app.b64 > /src/app.zip &&     mkdir -p /tmp/appzip /app &&     unzip -q /src/app.zip -d /tmp/appzip &&     cp -a /tmp/appzip/dem_explorer_cr/. /app/ &&     cat /src/patches/remote_fallback_v052.R >> /app/backend/R/dem_core.R &&     mkdir -p /app/outputs

WORKDIR /app

ENV APP_HOST=0.0.0.0     APP_PORT=8000     DEM_OUTPUT_TTL_MIN=60     DEM_MAX_BUFFER_M=10000     DEM_MAX_AOI_AREA_KM2=20000     DEM_MAX_VERTICES=250000     DEM_MAX_FEATURES=2000     DEM_MAX_REQUEST_MB=30     DEM_REMOTE_CACHE_DIR=/tmp/dem_explorer_cr_cache     GDOWN_BIN=/opt/gdown/bin/gdown

EXPOSE 8000

CMD ["R", "--vanilla", "-e", "source('/app/run.R')"]
