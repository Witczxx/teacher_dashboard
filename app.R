### Teacher Dashboard App Running Script -----

### Loading Packages -----
library(tidyverse)
library(RPostgres)
library(DBI)
library(showtext)
library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)

### Loading R Scripts -----
source("helper.R")
source("ui.R")
source("server.R")

# Run App -----
shinyApp(ui, server)