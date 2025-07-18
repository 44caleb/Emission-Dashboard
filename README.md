# Exploring Global Emission Data Dashboard

## Overview
This project involves the creation of an interactive **Shiny** dashboard that allows users to explore and visualize global greenhouse gas emissions (specifically CO₂ and methane) from 1990 to 2020. The dashboard offers several interactive features, including choropleth maps, area plots, and trend line visualizations by country and emission type.

## Features
- **Dataset Viewer**: View raw CO₂ and methane emissions data.
- **Choropleth Map Visualization**: Explore global emissions data for specific years and emission types (CO₂ or methane) on an interactive map.
- **Area Plot Visualization**: Analyze the overall global emissions trends for CO₂ and methane from 1990 to 2020.
- **Trend Line Plot Visualization**: Visualize the emission trends of specific countries for both CO₂ and methane.

## Technologies Used
- **R** programming language
- **Shiny** for building the interactive dashboard
- **ggplot2** for data visualization
- **shinydashboard** for UI design
- **bslib** for custom styling
- **dplyr** for data manipulation
- **readxl** for loading Excel data
- **ggpattern** for advanced patterning in plots
- **hrbrthemes** for attractive theme settings
- **magick** for image manipulation
- **sf** for handling spatial data
- **DT** for interactive tables

## Installation
The Renv package in R was used to create a reproducible environment for the project. To set up the application including all the files and dependencies in a local machine with R use the following the steps:
-	In Rstudio start a new project by cloning the repo url: https://github.com/44caleb/Emission-Dashboard.git
-	Ensure you have the Renv package installed
-	In the Rstudio console run renv::restore() to replicate the project’s environment on your machine and load application dependencies
-	Finally run shiny::runApp("shiny_app”) to start the application
-	Note: If an application window does not open, copy the URL generated in the console and paste in a browser to use the application

## Dataset
The dataset used for this project contains global emissions data for CO₂ and methane from 1990 to 2020. Data is sourced from the World Bank Group. You can view the datasets directly in the application and explore their content using the Dataset tab.
Files included:
- data/co2_data.xlsx – Contains CO₂ emissions data.
- data/meth_data.xlsx – Contains methane emissions data.

The dataset can be viewed under the "Dataset" tab, where you can see both CO₂ and methane emissions data.


Example Preview of Dashboard
Choropleth Map:

Shows emissions for a selected year and greenhouse gas type.

Area Plot:

Displays total emissions for CO₂ or methane from 1990 to 2020.

Trend Line Plot:

Shows emission trends for a selected country over the years.
