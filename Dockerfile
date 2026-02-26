# Basis-Image mit R und Shiny
FROM rocker/shiny:latest

# Systemabhängige Pakete installieren (falls nötig)
RUN apt-get update && \
    apt-get install -y \
        make \
        zlib1g-dev \
        pandoc \
        libicu-dev \
        libpng-dev \
        libwebpmux3 \
        libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

# R-Package aus GitHub installieren
RUN R -e "install.packages('remotes'); remotes::install_github('janlisec/eCerto')"

# Arbeitsverzeichnis
WORKDIR /app

# Startkommando: ruft die Funktion run_app() aus dem Package auf
CMD ["R", "-e", "eCerto::run_app(options = list(\"port\" = 3838, \"host\" = \"0.0.0.0\"))"]
