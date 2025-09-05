# Use the rocker/shiny image as a base for R and Shiny
FROM --platform=linux/amd64 rocker/shiny:latest

# Install necessary system dependencies and Python for Jupyter support
RUN apt-get update && \
    apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
                       libzmq3-dev libzmq5 python3 python3-pip shiny-server && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Install Jupyter and jupyter-client via pip
RUN pip3 install typer rich earthengine-api geemap jupyter jupyter-client --break-system-packages

# Install devtools and IRkernel from GitHub using devtools
RUN R -e "install.packages(c('devtools', 'leaflet', 'shiny-server', 'shiny', 'shinyjs', 'sf', 'terra', 'ggplot2', 'patchwork', 'lubridate', 'rasterVis', 'mapview', 'tmap', 'plotly', 'nasapower'))" && \
    R -e "devtools::install_github('IRkernel/IRkernel')" && \
    R -e "IRkernel::installspec(user = FALSE)"

WORKDIR /home/user

# Expose ports for Jupyter (8888)
EXPOSE 8888

# Start both Shiny and Jupyter
CMD /bin/bash -c "jupyter notebook --ip=0.0.0.0 --port=8888 --no-browser --allow-root --NotebookApp.token='' --NotebookApp.password='' "

