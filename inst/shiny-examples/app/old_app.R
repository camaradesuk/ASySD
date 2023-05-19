# Load packages ----
require(shiny)
require(readr)
require(DT)
require(stringr)
require(dplyr)
library(shinyalert)
library(progressr)
library(networkD3)
library(rsconnect)
library(RCurl)
library(shiny)
library(ASySD)
library(shinythemes)
library(knitr)
library(shinycssloaders)
library(htmlwidgets)
library(shinyWidgets)
library(bslib)

options(shiny.maxRequestSize=1000*1024^2, timeout = 40000000)

# UI ----
ui <- navbarPage(

  shinyjs::useShinyjs(),

  tags$head(includeHTML(("google-analytics.html"))),

  title="ASySD",

  theme = bs_theme(bg = "rgb(251, 251, 251)",
                   secondary = "#754E9B",
                   success = "#8894B6",
                   info = "#82D173",
                   warning = "#FFC07F",
                   danger = "#C1666B",
                   font_scale = NULL,
                   bootswatch = "flatly",
                   fg = "#000")
)

server <- function(input, output, session){

  shinyalert("This is an old link!",
             "To use ASySD, please go to https://camarades.shinyapps.io/ASySD/", type = "warning")

}

# Run the application
shinyApp(ui = ui, server = server)



