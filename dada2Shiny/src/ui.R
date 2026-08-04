library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
require(DT)
library(dada2)
library(dplyr)

library(phyloseq)
library(Biostrings)
library(ggplot2)
library(rmarkdown)

publication_downloads <- function(id) {
    tags$div(
        class = "publication-downloads",
        tags$span(class = "publication-downloads__label", "Download figure"),
        downloadButton(paste0(id, "_png"), "PNG (300 DPI)", class = "btn btn-default btn-sm"),
        downloadButton(paste0(id, "_pdf"), "PDF", class = "btn btn-default btn-sm"),
        downloadButton(paste0(id, "_svg"), "SVG", class = "btn btn-default btn-sm")
    )
}

# library(future)
# library(promises)
# plan(multisession)  # Enable parallel processing with separate R sessions


# library(RcppParallel)
# setThreadOptions(numThreads = 4)



htmltags <- tags

options(shiny.timeout = 300000 * 1600) # Set timeout to 5 * 3 minutes (in milliseconds)


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
            menuItem("Run DADA2", tabName = "filter_and_trim_tab", icon = icon("cogs")),
            menuItem("Error Rate", tabName = "errorRatesTab", icon = icon("chart-bar")),
            menuItem("Merged Paired Reads", tabName = "margePairedReadsTab", icon = icon("compress-arrows-alt")),
            menuItem("Track Reads", tabName = "trackReadsTab", icon = icon("stream")),
            menuItem("Taxonomy", tabName = "taxanomyTab", icon = icon("sitemap")),
            menuItem("Diversity & Abundance", tabName = "alphaDiversityTab", icon = icon("chart-pie"))
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$style(HTML("
        .selectize-control .selectize-input {
            max-width: 100%; /* Adjust the width of selectize input */
            overflow: hidden; 
            text-overflow: ellipsis; /* This will truncate long text */
        }
        .publication-downloads {
            display: flex;
            align-items: center;
            flex-wrap: wrap;
            gap: 8px;
            padding: 10px 0 4px;
            border-top: 1px solid #d9dee3;
        }
        .publication-downloads__label {
            color: #4f5b66;
            font-weight: 600;
            margin-right: 4px;
        }
        ")),

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
        ),
         # Add a heartbeat function to prevent session from disconnecting
        tags$script(HTML(
           'setInterval(function(){ Shiny.onInputChange("keepAlive", new Date()); }, 10000);'
         ))
    )
)
