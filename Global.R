
# Chargement des packages

library(shiny)
library(bs4Dash)
library(plotly)
library(car)
library(multcomp)
library(DT)
library(rstatix)
library(dplyr)
# limiter la taille des ficheirs a 300MB
options(shiny.maxRequestSize = 300*1024^2)


source("ui.R")
source("server.R")


shinyApp(ui = ui, server = server)