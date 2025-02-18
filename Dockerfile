FROM rocker/r2u:jammy

LABEL org.opencontainers.image.authors="Samuel Dunesme <samuel.dunesme@ens-lyon.fr>"
LABEL org.opencontainers.image.source="https://github.com/evs-gis/mapdoapp"
LABEL org.opencontainers.image.documentation="https://evs-gis.github.io/mapdowebsite/"
LABEL org.opencontainers.image.version="2.0.1"
LABEL org.opencontainers.image.description="Application de description des caractéristiques morphologiques des réseaux hydrographiques."

RUN locale-gen fr_FR.UTF-8

RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'install.packages("rlang")'
RUN Rscript -e 'install.packages("glue")'
RUN Rscript -e 'install.packages("magrittr")'
RUN Rscript -e 'install.packages("cachem")'
RUN Rscript -e 'install.packages("bslib")'
RUN Rscript -e 'install.packages("htmltools")'
RUN Rscript -e 'install.packages("jsonlite")'
RUN Rscript -e 'install.packages("tibble")'
RUN Rscript -e 'install.packages("stringr")'
RUN Rscript -e 'install.packages("purrr")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("RColorBrewer")'
RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'install.packages("DBI")'
RUN Rscript -e 'install.packages("tidyr")'
RUN Rscript -e 'install.packages("httr")'
RUN Rscript -e 'install.packages("testthat")'
RUN Rscript -e 'install.packages("RPostgres")'
RUN Rscript -e 'install.packages("leaflet")'
RUN Rscript -e 'install.packages("config")'
RUN Rscript -e 'install.packages("shinyjs")'
RUN Rscript -e 'install.packages("spelling")'
RUN Rscript -e 'install.packages("sparkline")'
RUN Rscript -e 'install.packages("shinyWidgets")'
RUN Rscript -e 'install.packages("sf")'
RUN Rscript -e 'install.packages("reactable")'
RUN Rscript -e 'install.packages("plotly")'
RUN Rscript -e 'install.packages("leaflet.extras")'
RUN Rscript -e 'install.packages("golem")'
RUN Rscript -e 'install.packages("colourpicker")'
RUN Rscript -e 'install.packages("cicerone")'
RUN Rscript -e 'install.packages("bsicons")'
RUN Rscript -e 'install.packages("shinycssloaders")'

RUN mkdir /app
ADD . /app
WORKDIR /app

RUN R -e 'remotes::install_local()'

EXPOSE 3840

RUN groupadd -g 1010 app && useradd -c 'app' -u 1010 -g 1010 -m -d /home/app -s /sbin/nologin app
USER app

CMD  ["R", "-f", "app.R"]
