#!/bin/bash

# Install development packages (jupyter notebooks) along with project dependencies 
# for the Sensing Clues challenge with Correlaid

set -e

# Update and install system dependencies
echo "[Correlaid] Updating system and installing dependencies..."
apt-get update && \
apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libzmq3-dev \
    libzmq5 \
    python3 \
    python3-pip \
    shiny-server && \
apt-get clean && \
rm -rf /var/lib/apt/lists/*

# Install Python libraries for Jupyter
echo "[Correlaid] Installing Python libraries for Jupyter..."
pip3 install \
    typer \
    rich \
    earthengine-api \
    geemap \
    jupyter \
    jupyter-client --break-system-packages

# Install R packages from CRAN
echo "[Correlaid] Installing R packages..."
R -e "install.packages(c('devtools', 'leaflet', 'shiny-server', 'shiny', 'shinyjs', 'sf', 'terra', 'ggplot2', 'patchwork', 'lubridate', 'rasterVis', 'mapview', 'tmap', 'plotly'))"

# Install IRkernel and register it with Jupyter
echo "[Correlaid] Installing IRkernel and registering it with Jupyter..."
R -e "devtools::install_github('IRkernel/IRkernel')" && \
R -e "IRkernel::installspec(user = FALSE)"

# Create a directory for Jupyter notebooks
echo "[Correlaid] Creating directory for Jupyter notebooks..."
mkdir -p /home/rstudio/notebooks

# Create a sample Shiny app directory (optional)
echo "[Correlaid] Creating directory for Shiny apps..."
mkdir -p /srv/shiny-server

# Set permissions for Shiny and notebooks (optional)
echo "[Correlaid] Setting permissions..."
chmod -R 755 /srv/shiny-server /home/rstudio/notebooks
