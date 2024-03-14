library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
require(DT)
library(dada2)
library(dplyr)
library(GSVA)
library(Seurat)

options(shiny.maxRequestSize=300*1024^2) 


htmltags<- tags
ui <- dashboardPage(
  dashboardHeader(title = "gsvaShiny"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Input Data", tabName = "input_tab", icon = icon("upload"))
      
      
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    extendShinyjs(script = "custom.js", functions = c("addStatusIcon", "collapse")),
    
    htmltags$head(
      htmltags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
      htmltags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"),
      htmltags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      
    ),
    tabItems(
      source("ui-tab-inputdata.R", local = TRUE)$value
      
      
      
    )
  )
)
