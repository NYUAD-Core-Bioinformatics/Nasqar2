options(shiny.maxRequestSize = 30 * 1024^4)

server <- function(input, output,session) {
  check_and_load_bioc_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(
            paste(
                "Required genome package is not installed in this image:",
                pkg
            ),
            call. = FALSE
        )
    }
    suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
    )
    invisible(TRUE)
  }

  source("plot-downloads.R", local = TRUE)
  source("server-inputdata.R", local = TRUE)
  source("server-heatmap.R", local = TRUE)
  source("server-librarycomplexity.R", local = TRUE)
  source("server-fragmentsize.R", local = TRUE)
  source("server-nucleosomepositioning.R", local = TRUE)
}
