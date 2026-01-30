### Teacher Dashboard App Running Script -----

### Loading Packages -----
library(tidyverse)
library(tibble)
library(DBI)
library(showtext)
library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)

### Loading R Scripts -----
source("Dashboard/helper.R")
source("Dashboard/ui.R")
source("Dashboard/server.R")

# Run App -----
shinyApp(ui, server)