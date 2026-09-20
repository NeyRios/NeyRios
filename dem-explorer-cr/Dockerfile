FROM rocker/geospatial:latest

RUN apt-get update &&     apt-get install -y --no-install-recommends unzip coreutils &&     rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages(c('plumber','jsonlite','png'), repos='https://cloud.r-project.org')"

WORKDIR /src
COPY dem-explorer-cr/archive /src/archive

RUN cat /src/archive/chunk*.b64 > /src/app.b64 &&     base64 -d /src/app.b64 > /src/app.zip &&     mkdir -p /tmp/appzip /app &&     unzip -q /src/app.zip -d /tmp/appzip &&     cp -a /tmp/appzip/dem_explorer_cr/. /app/ &&     mkdir -p /app/outputs

WORKDIR /app

ENV APP_HOST=0.0.0.0     APP_PORT=8000     DEM_OUTPUT_TTL_MIN=60     DEM_MAX_BUFFER_M=10000     DEM_MAX_AOI_AREA_KM2=20000     DEM_MAX_VERTICES=250000     DEM_MAX_FEATURES=2000     DEM_MAX_REQUEST_MB=30

EXPOSE 8000

CMD ["R", "--vanilla", "-e", "source('/app/run.R')"]
