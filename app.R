# app.R

library(shiny)
library(bslib)
library(plotly)
library(data.table) # Will be needed later
library(qs)         # Will be needed later
library(htmltools)
library(bsicons)

# Source all R files from the R/ directory
source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE, ignore.case = TRUE)
sapply(source_files, source)

# Define UI using the main UI function
ui <- ui_main()

# Define server using the main server function
server <- srv_main

# Run the application
shinyApp(ui = ui, server = server)
