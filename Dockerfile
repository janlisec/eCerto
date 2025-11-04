# Basis-Image mit R und Shiny
FROM rocker/shiny:latest

# Systemabhängige Pakete installieren (falls nötig)
RUN apt-get update && \
    [ $(which google-chrome) ] || apt-get install -y gnupg curl && \
    [ $(which google-chrome) ] || curl -fsSL -o /tmp/google-chrome.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb && \
    [ $(which google-chrome) ] || DEBIAN_FRONTEND='noninteractive' apt-get install -y /tmp/google-chrome.deb && \
    apt-get install -y \
        libcurl4-openssl-dev \
        libssl-dev \
        make \
        zlib1g-dev \
        pandoc \
        libicu-dev \
        libpng-dev \
        libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

# R-Package aus GitHub installieren
RUN R -e "install.packages('remotes'); remotes::install_github('janlisec/eCerto')"

# Arbeitsverzeichnis
WORKDIR /app

# Startkommando: ruft die Funktion run_app() aus dem Package auf
CMD ["R", "-e", "PKGNAME::run_app()"]
