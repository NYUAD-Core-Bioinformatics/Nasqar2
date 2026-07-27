# Executable contract test for the DESeq2Shiny server modules.

options(shiny.legacy.datatable = FALSE)

source("global.R")
source("ui.R")
source("server.R")

expected_modules <- c(
  "input", "design", "sva", "deseq", "results",
  "venn", "volcano", "boxplot", "heatmap"
)

expected_contracts <- list(
  input = c("input_file", "submitted_data"),
  design = c("dds", "metadata", "update_formula"),
  sva = "sva",
  deseq = "dds",
  results = c("comparison", "files", "contrast_specs"),
  venn = c("selected_matrix", "expression_data", "heatmap_matrix"),
  volcano = c("selected_data", "significant_genes"),
  boxplot = c("expression", "colors"),
  heatmap = "heatmap"
)

cat("Running DESeq2Shiny module contract test\n")

shiny::testServer(server, {
  api <- session$returned

  stopifnot(
    identical(names(api), c("state", "session_dir", "modules")),
    inherits(api$state, "reactivevalues"),
    dir.exists(api$session_dir),
    identical(names(api$modules), expected_modules)
  )

  for (module_name in expected_modules) {
    stopifnot(identical(
      names(api$modules[[module_name]]),
      expected_contracts[[module_name]]
    ))
  }

  session$setInputs(
    data_file_type = "examplecountsfactors",
    gene_alias = "notincluded",
    no_replicates = FALSE
  )
  example_counts <- api$modules$input$input_file()
  stopifnot(
    is.matrix(example_counts),
    nrow(example_counts) > 0L,
    ncol(example_counts) > 1L
  )
})

cat("All DESeq2Shiny module contracts passed\n")
