# Shiny and Jupyter Project in Docker

This project uses Docker to deploy a Shiny app and Jupyter Notebook environment. It requires Docker and Docker Compose to be installed on your system. Below are instructions for installing Docker on macOS and launching the application using Docker Compose.

## Requirements

- **Docker**: Docker is required to build and run the containerized environment.
- **Docker Compose**: Docker Compose is used to manage multiple services within the Docker container.

### Installing Docker on macOS

To install Docker on macOS using Homebrew, run:

```bash
brew install --cask docker-compose
```

Once installed, open the Docker application to start the Docker daemon.

### Folder Structure

The project follows this folder structure:



```
project-root/
├── data/                      # Raw GIS data and processed files
│   ├── raw/                   # Original GIS files (e.g., shapefiles)
│   └── processed/             # Cleaned and transformed data files
├── scripts/                   # R scripts for data processing, analysis, etc.
│   ├── data_preprocessing.R   # Preprocessing steps for GIS data
│   └── analysis.R             # Analysis and visualization scripts
├── shiny_app/                 # Shiny app directory
│   ├── ui.R                   # UI component of the Shiny app
│   ├── server.R               # Server logic for the Shiny app
│   ├── pages/                 # Includes .R files corresponding to individual pages 
│   ├── pages/*.R              # R script shiny pages 
│   └── www/                   # Assets (CSS, JavaScript, images)
├── notebooks/                 # Jupyter notebooks for exploratory work
│   └── example_notebook.ipynb # Example notebook in R
├── shiny/                     # Docker configuration for the shiny app
│   └── Dockerfile
├── jupyter/                   # Docker configuration for the jupyter app
│   └── Dockerfile
├── .env                       # Environment variables
├── .gitignore                 # Git ignore file
└── README.md                  # Project overview and instructions
```

### Shiny Server

This Shiny app is designed for nature conservation projects, providing an interactive interface to monitor environmental data. It uses modular functions to organize different pages, making the codebase easy to extend and maintain. Each page is built using a dedicated UI and server function to handle content and interactivity.

#### Page Structure
The app consists of modular pages, each with a unique UI and server function:

* projectOverviewUI and projectOverviewServer: Displays an overview of the project, including details about the partner organization, project goals, and timeline.
* sensorDeploymentUI and sensorDeploymentServer: Provides a page with content focused on sensor deployment strategies and visualizations.

Each page module is structured in its own file for better organization and includes:

* UI Function: Defines the layout and static content.
* Server Function: Contains dynamic elements, such as data rendering or interactive plots.

#### Creating your own Shiny pages & scripts

To add or modify a page, follow these steps:

Create or Edit UI and Server Functions: Define a new UI function (e.g., newPageUI) and server function (newPageServer) for each page. Use tagList to organize content and elements within the UI.
Add to Main App: Include the new page in the ui and server sections of the main Shiny app. For example:

```r 
ui = fluidPage(
  newPageUI("newPageId")
)

server = function(input, output, session) {
  newPageServer("newPageId")
}
```

Look at the existing pages (for example pages/project_overview.R) to understand the basics.

### Launching the Project

To start both the Shiny app and Jupyter Notebook, use Docker Compose. This will build and launch the services specified in docker-compose.yml.

### Command to Launch
From the project root directory, run:

```
docker-compose up --build
```

This command will:

Build the Docker image and install all necessary dependencies.
Start Shiny Server on port localhost:3838.
Start Jupyter Notebook on port localhost:8888.

### Laching a specific service

Note that in this project there are multiple docker containers. One for development, including jupyter notebooks, and another for running a shiny app. It is possible to build and launch a single service at a time. For example, to launch jupyter;

```
docker-compose up --build jupyter
```

and similarly, to launch the shiny local endpoint,

```
docker-compose up --build shiny
```

### Stopping the Project
To stop the running containers, use:

docker-compose down
Links to Services

Shiny Server: http://localhost:3838
Jupyter Notebook: http://localhost:8888

### Notes

Jupyter Notebook authentication is disabled for easy access.
Modify shiny-server.conf in docker/ if you need to customize the Shiny app settings.
This setup provides a convenient environment for developing and running both Shiny applications and Jupyter notebooks with GIS and data science capabilities.

Currently, the local execution of notebooks depends on the data folder structure to be as follows:

```
data
├── AoI
├── LandUse
│   └── Zambia
│       └── S2_10m_LULC_2023
│           ├── Zambia_Bare_ground_2023.geojson
│           ├── Zambia_Crops_2023.geojson
│           ├── ...
└── NDVI
    ├── Bulgaria
    │   ├── 1000m_resolution
    │   ├── 100m_resolution
    │   └── 10m_resolution
    ├── Kenya
    │   ├── 1000m_resolution
    │   ├── 100m_resolution
    │   └── 10m_resolution
    ├── Spain
    │   ├── 1000m_resolution
    │   ├── 100m_resolution
    │   └── 10m_resolution
    └── Zambia
        ├── 1000m_resolution
        ├── 100m_resolution
        └── 10m_resolution


