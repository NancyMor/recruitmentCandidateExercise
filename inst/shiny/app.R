# Load required packages -------------------------------
library(shiny)
library(shinythemes)
library(magrittr)
library(highcharter)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(plyr)
library(lmerTest)
library(lme4)
library(shinydashboard)

# Load data -------------------------------
load("../R/sysdata.Rda")
#original_data <- read_csv("./data.csv") %>% as.data.table()
#usethis::use_data(original_data, internal = TRUE

# Loading ui and server
source("ui.R")
source("server.R")


# Run the application
shinyApp(ui = ui, server = server)
