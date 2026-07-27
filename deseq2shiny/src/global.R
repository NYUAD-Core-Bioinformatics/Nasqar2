# Global Shiny configuration
options(shiny.maxRequestSize = 600 * 1024^2)

# Configure Shiny for reverse proxy with subpath
options(shiny.port = NULL)  # Let Shiny auto-detect
options(shiny.host = "0.0.0.0")

# Ensure proper handling of selectize inputs behind proxy
options(shiny.sanitize.errors = TRUE)

# Load pure validation and persistence helpers.
source("core-functions.R", local = FALSE)
source("server-export-code.R", local = FALSE)
source("server-modules.R", local = FALSE)

# Export helpers to avoid NULL input issues in download handlers
get_export_mode <- function(input) {
  mode <- tryCatch(input$export_mode_global, error = function(e) NULL)
  if (is.null(mode) || length(mode) == 0 || !nzchar(as.character(mode))) {
    return("full")
  }
  as.character(mode)
}

get_export_format <- function(input) {
  # Only .R format is supported (R Markdown removed)
  return("r")
}

zip_export_dir <- function(export_dir, zip_file) {
  files <- list.files(export_dir, full.names = FALSE)
  if (length(files) == 0) {
    stop("No files to zip.")
  }
  current_wd <- getwd()
  on.exit(setwd(current_wd), add = TRUE)
  setwd(export_dir)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(zipfile = zip_file, files = files)
  } else {
    utils::zip(zipfile = zip_file, files = files)
  }
}

# Load all required libraries at startup
suppressPackageStartupMessages({
  library(shinydashboard)
  library(shinyjs)
  library(shinyBS)
  library(shinycssloaders)
  library(DT)
  library(shiny)
  library(rhandsontable)
  library(readr)
  library(RColorBrewer)
  library(DESeq2)
  library(heatmaply)
  library(ggplot2)
  library(ggthemes)
  library(plotly)
  library(BiocParallel)
  library(sodium)
  library(NMF)
  library(tidyr)
  library(dplyr)
  library(sva)
  library(uuid)
  library(colourpicker)
  library(VennDiagram)
  library(smplot2)
  library(EnhancedVolcano)
  library(rclipboard)
  library(ComplexHeatmap)
  library(InteractiveComplexHeatmap)
  library(GetoptLong)
  library(gridExtra)
})
