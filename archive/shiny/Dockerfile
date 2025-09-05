FROM --platform=linux/amd64  rocker/r-ver:4.3.2

LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
      org.opencontainers.image.source="https://github.com/rocker-org/rocker-versioned2" \
      org.opencontainers.image.vendor="Rocker Project" \
      org.opencontainers.image.authors="Carl Boettiger <cboettig@ropensci.org>"

ENV S6_VERSION=v2.1.0.2
ENV SHINY_SERVER_VERSION=latest
ENV PANDOC_VERSION=default

# hk add the geospatial stuff first
RUN /rocker_scripts/install_geospatial.sh

# shiny stuff later
RUN /rocker_scripts/install_shiny_server.sh

# Copy custom index.html and shiny-server.conf
# COPY /config/index.html /srv/shiny-server/index.html
COPY /config/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Install WildCat dependencies
RUN install2.r --skipinstalled --error renv \
    jsonlite \
    leaflet.extras \
    CircStats \
    httr \
    data.tree \
    devtools \
    dplyr \
    DT \
    geojsonsf \
    getPass \
    ggplot2 \
    KernSmooth \
    shiny \
    shiny.i18n \
    shinyjs \
    shinybusy \
    shinyWidgets \
    shinyTree \
    sp \
    sf \
    stringr \
    leaflet \
    leaflet.extras \
    loggit \
    lubridate \
    plotly \
    ipc \
    future \
    promises \
    colorRamps \
    raster \
    readxl \
    remotes \
    reshape2 \
    RColorBrewer

# Make the directory writable by all users
RUN chmod -R a+w /usr/local/lib/R/site-library
#RUN chmod 777 /usr/local/lib/R/site-library
# Optionally, you can also change the ownership if needed (e.g., user:group)
# RUN chown -R root:root /usr/local/lib/R/site-library

# Add home dir for the 'shiny' user, which is needed by renv
RUN mkdir -p /home/shiny
RUN chown shiny /home/shiny/
RUN chmod -R 755 /srv/shiny-server/

#RUN R -e "remotes::install_github('sensingclues/sensingcluesr')"
EXPOSE 3838

CMD ["/init"]
