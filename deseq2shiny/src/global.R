# Global Shiny configuration
options(shiny.maxRequestSize = 600 * 1024^2)

# Configure Shiny for reverse proxy with subpath
options(shiny.port = NULL)  # Let Shiny auto-detect
options(shiny.host = "0.0.0.0")

# Ensure proper handling of selectize inputs behind proxy
options(shiny.sanitize.errors = FALSE)  # For debugging

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
