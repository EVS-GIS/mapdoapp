FROM rocker/verse:4.4.1

LABEL org.opencontainers.image.authors="Samuel Dunesme <samuel.dunesme@ens-lyon.fr>"
LABEL org.opencontainers.image.url="https://github.com/evs-gis/mapdoapp"
LABEL org.opencontainers.image.documentation="https://evs-gis.github.io/mapdowebsite/"
LABEL org.opencontainers.image.version="2.0.1"
LABEL org.opencontainers.image.description="Application de description des caractéristiques \
morphologiques des réseaux hydrographiques."

RUN apt-get update && apt-get install -y  gdal-bin libcurl4-openssl-dev libgdal-dev libgeos-dev libicu-dev libpng-dev libpq-dev libproj-dev libsqlite3-dev libssl-dev libudunits2-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN locale-gen fr_FR.UTF-8

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'

# RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.4")'
# RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.7.0")'
# RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
# RUN Rscript -e 'remotes::install_version("cachem",upgrade="never", version = "1.1.0")'
# RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.7.0")'
# RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.8.1")'
# RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.8.8")'
# RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.1")'
# RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.1")'
# RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.2")'
# RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
# RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-3")'
# RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.1.1")'
# RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.2.3")'
# RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.1")'
# RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.7")'
# RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1.1")'
# RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.4.7")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.2.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2")'
RUN Rscript -e 'remotes::install_version("sparkline",upgrade="never", version = "2.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.8.7")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "1.0-17")'
RUN Rscript -e 'remotes::install_version("reactable",upgrade="never", version = "0.4.4")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.4")'
RUN Rscript -e 'remotes::install_version("leaflet.extras",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("colourpicker",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("cicerone",upgrade="never", version = "1.0.4")'
RUN Rscript -e 'remotes::install_version("bsicons",upgrade="never", version = "0.1.2")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.2.5")' #remove?

RUN mkdir /app
ADD . /app
WORKDIR /app

RUN R -e 'remotes::install_local(upgrade="never")'

EXPOSE 3838

RUN groupadd -g 1001 shiny && useradd -c 'shiny' -u 1001 -g 1001 -m -d /home/shiny -s /sbin/nologin shiny
USER shiny

CMD  ["R", "-f", "app.R"]
