library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
require(DT)
library(dada2)
library(dplyr)

library(phyloseq)
library(Biostrings)
library(ggplot2)
htmltags <- tags
ui <- dashboardPage(
    
    dashboardHeader(title = tags$div(
            tags$img(src = "CGSB-Logo.png", height = "35px", style = "margin-right: 10px;"), 
            "DADA2"
        )),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("User Guide", tabName = "userGuideTab", icon = icon("book")),
            menuItem("Input Data", tabName = "input_tab", icon = icon("upload")),
            menuItem("Quality Profile", tabName = "qualityprofile_tab", icon = icon("chart-line")),
            menuItem("Filter & Trim", tabName = "filter_and_trim_tab", icon = icon("cut")),
            menuItem("Error Rate", tabName = "errorRatesTab", icon = icon("exclamation-triangle")),
            menuItem("Merged Paired Reads", tabName = "margePairedReadsTab", icon = icon("compress-arrows-alt")),
            menuItem("Track Reads", tabName = "trackReadsTab", icon = icon("stream")),
            menuItem("Taxonomy", tabName = "taxanomyTab", icon = icon("sitemap")),
            menuItem("Diversity & Abundance", tabName = "alphaDiversityTab", icon = icon("chart-pie"))
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        extendShinyjs(script = "custom.js", functions = c("addStatusIcon", "collapse")),
        htmltags$head(
            htmltags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            htmltags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
            htmltags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"),
        ),
        tabItems(
            source("ui-tab-userguide.R", local = TRUE)$value,
            source("ui-tab-inputdata.R", local = TRUE)$value,
            source("ui-tab-qualityprofile.R", local = TRUE)$value,
            source("ui-tab-filter_and_trim.R", local = TRUE)$value,
            source("ui-tab-errorRates.R", local = TRUE)$value,
            source("ui-tab-mergePairedReads.R", local = TRUE)$value,
            source("ui-tab-trackReads.R", local = TRUE)$value,
            source("ui-tab-alphaDiversity.R", local = TRUE)$value,
            source("ui-tab-taxonomy.R", local = TRUE)$value
        )
    )
)
