library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(Rsamtools)
library(MotifDb)
library(ATACseqQC)
library(ChIPpeakAnno)
library(GenomicAlignments)
require(DT)


htmltags<- tags

analysis_panel <- function(title, caption, ..., class = NULL) {
    tags$section(
        class = paste("analysis-panel", class),
        tags$header(
            class = "analysis-panel__header",
            tags$h3(title)
        ),
        tags$div(class = "analysis-panel__body", ...),
        tags$footer(class = "analysis-panel__caption", caption)
    )
}

publication_downloads <- function(id, vector = TRUE) {
    controls <- list(
        tags$span(class = "publication-downloads__label", "Download figure"),
        downloadButton(paste0(id, "_png"), "PNG (300 DPI)", class = "btn btn-default btn-sm")
    )
    if (vector) {
        controls <- c(
            controls,
            list(
                downloadButton(paste0(id, "_pdf"), "PDF", class = "btn btn-default btn-sm"),
                downloadButton(paste0(id, "_svg"), "SVG", class = "btn btn-default btn-sm")
            )
        )
    }
    tags$div(class = "publication-downloads", controls)
}

ui <- dashboardPage(
    dashboardHeader(title = "ATACseqQC"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Input Data", tabName = "input_tab", icon = icon("upload")),
            menuItem("Heatmap", tabName = "heatmap_tab"),
            menuItem("Library Complexity", tabName = "librarycomplexity_tab"),
            menuItem("Fragment size", tabName = "fragmentsize_tab"),
            menuItem(
                "Core QC",
                tabName = "nucleosomepositioning_tab",
                icon = icon("check-circle")
            ),
            menuItem(
                "Nucleosome Profiles",
                tabName = "nucleosomeprofiles_tab",
                icon = icon("area-chart")
            ),
            menuItem(
                "TF Footprinting",
                tabName = "footprinting_tab",
                icon = icon("bullseye")
            )
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        extendShinyjs(script = "custom.js", functions = c("addStatusIcon", "collapse")),

        htmltags$head(
            htmltags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
            htmltags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"),
            htmltags$link(
                rel = "stylesheet",
                type = "text/css",
                href = "custom.css?v=20260729-scientific-panels"
            )
        
        ),
        tabItems(
            source("ui-tab-inputdata.R", local = TRUE)$value,
            source("ui-tab-heatmap.R", local = TRUE)$value,
            source("ui-tab-librarycomplexity.R", local = TRUE)$value,
            source("ui-tab-fragmentsize.R", local = TRUE)$value,
            source("ui-tab-nucleosomepositioning.R", local = TRUE)$value,
            source("ui-tab-nucleosomeprofiles.R", local = TRUE)$value,
            source("ui-tab-footprinting.R", local = TRUE)$value
            
        )
    )
)
