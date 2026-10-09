library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(ggplot2)
source("nasqar-project.R")
suppressPackageStartupMessages({
    library(VariantAnnotation)
    library(GenomicFeatures)
})

publication_downloads <- function(id) {
    tags$div(
        class = "publication-downloads",
        tags$span(class = "publication-downloads__label", "Download figure"),
        downloadButton(paste0(id, "_png"), "PNG (300 DPI)", class = "btn btn-default btn-sm"),
        downloadButton(paste0(id, "_pdf"), "PDF", class = "btn btn-default btn-sm"),
        downloadButton(paste0(id, "_svg"), "SVG", class = "btn btn-default btn-sm")
    )
}

htmltags <- tags

options(shiny.timeout = 300000 * 1600)

ui <- dashboardPage(
    dashboardHeader(title = tags$div(
        tags$img(src = "CGSB-Logo.png", height = "35px", style = "margin-right: 10px;"),
        "Variant Annotation"
    )),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("User Guide", tabName = "userGuideTab", icon = icon("book")),
            menuItem("Input Data", tabName = "input_tab", icon = icon("upload")),
            menuItem("VCF Summary", tabName = "summary_tab", icon = icon("list")),
            menuItem("Filter Variants", tabName = "filter_tab", icon = icon("filter")),
            menuItem("Annotate", tabName = "annotate_tab", icon = icon("cogs")),
            menuItem("Summary Plots", tabName = "plots_tab", icon = icon("chart-bar")),
            menuItem("Downloads", tabName = "downloads_tab", icon = icon("download")),
            menuItem("Enrichment Handoff", tabName = "handoff_tab", icon = icon("share-square"))
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$style(HTML("
        .selectize-control .selectize-input {
            max-width: 100%;
            overflow: hidden;
            text-overflow: ellipsis;
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
        .variant-report { white-space: pre-wrap; font-family: monospace; font-size: 13px; }
        ")),
        extendShinyjs(script = "custom.js", functions = c("addStatusIcon", "collapse")),
        htmltags$head(
            # same icon as the NASQAR2 landing page
            htmltags$link(rel = "icon", type = "image/png", href = "CGSB-Logo.png"),
            htmltags$link(rel = "shortcut icon", type = "image/png", href = "CGSB-Logo.png"),
            htmltags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            htmltags$style(HTML(" .shiny-output-error-validation {color: darkred; } "))
        ),
        tabItems(
            source("ui-tab-userguide.R", local = TRUE)$value,
            source("ui-tab-inputdata.R", local = TRUE)$value,
            source("ui-tab-summary.R", local = TRUE)$value,
            source("ui-tab-filter.R", local = TRUE)$value,
            source("ui-tab-annotate.R", local = TRUE)$value,
            source("ui-tab-plots.R", local = TRUE)$value,
            source("ui-tab-downloads.R", local = TRUE)$value,
            source("ui-tab-handoff.R", local = TRUE)$value
        ),
        tags$script(HTML(
            'setInterval(function(){ Shiny.onInputChange("keepAlive", new Date()); }, 10000);'
        ))
    )
)
