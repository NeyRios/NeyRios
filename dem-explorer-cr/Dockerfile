FROM rocker/geospatial:latest

RUN R -q -e "install.packages(c('plumber','jsonlite','png'), repos='https://cloud.r-project.org')"

WORKDIR /app
COPY . /app
RUN mkdir -p /app/outputs

ENV APP_HOST=0.0.0.0     APP_PORT=8000     DEM_OUTPUT_TTL_MIN=60     DEM_MAX_BUFFER_M=10000     DEM_MAX_AOI_AREA_KM2=20000     DEM_MAX_VERTICES=250000     DEM_MAX_FEATURES=2000     DEM_MAX_REQUEST_MB=30

# EXPOSE es informativo. En Railway run.R prioriza la variable PORT
# que la plataforma inyecta automáticamente.
EXPOSE 8000

CMD ["R", "--vanilla", "-e", "source('/app/run.R')"]
