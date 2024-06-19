options(shiny.maxRequestSize = 30 * 1024^4)

server <- function(input, output,session) {
  # Function to check if a Bioconductor package is installed, and install it if not
  check_and_load_bioc_package <- function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      BiocManager::install(pkg, update = FALSE)
      library(pkg, character.only = TRUE)
    }
  }

  source("server-inputdata.R", local = TRUE)
  source("server-heatmap.R", local = TRUE)
  source("server-librarycomplexity.R", local = TRUE)
  source("server-fragmentsize.R", local = TRUE)
  source("server-nucleosomepositioning.R", local = TRUE)
}