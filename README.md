# Project Title
Compassion and Happiness Interactive Dashboard

# Overview
This project explores the relationship between compassion and national happiness using global socioeconomic data. An interactive R Shiny dashboard allows users to investigate how compassion interacts with economic factors such as GDP, inequality, and social spending.

# Dataset Sources
* World Happiness Report
* World Values Survey
* World Bank
* OECD Social Expenditure Database

# Tools Used
* R
* R Shiny
* Plotly
* ggplot2
* Leaflet
* Python (Pandas)

# Key Features
* Interactive world maps
* Bubble scatter visualization
* Parallel coordinate multivariate analysis
* Correlation matrix visualization
* Continent filtering

# Key Insight
GDP per capita shows the strongest relationship with happiness (r ≈ 0.67), while compassion demonstrates a moderate relationship influenced by inequality and social spending.

# Dashboard Preview
View the "Dashboard Layout.pdf" file in the images folder

# How to Run
1. Open RStudio and create an R Shiny Application directory with multiple files 
2. Place the files "server.R", "ui.R", and "wrangled_data_source.csv" in the newly created R Shiny Application directory
3. Click Run App

Note: Install these packages if you don't have them

```
install.packages(c("shiny","leaflet","plotly","ggplot2","dplyr"))
```