# Server module boundaries.
#
# The current UI uses historical, root-level input/output IDs. Each module
# receives those root objects explicitly while its server lifecycle and local
# symbols are isolated by moduleServer(). This compatibility bridge allows the
# server architecture to be modularized without changing saved UI state or
# breaking existing links. New UI work should use matching NS()-based module
# UIs and remove the root bridge.

inputDataServer <- function(
    id, root_input, root_output, root_session, state, design_api, js_api) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- state
    updateDesignFormula <- design_api$update_formula
    js <- js_api

    source("server-inputdata.R", local = TRUE)

    list(
      input_file = inputFileReactive,
      submitted_data = csvDataReactive
    )
  })
}

designServer <- function(
    id, root_input, root_output, root_session, state, factor_api, js_api) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- state
    isCategoricalFactor <- factor_api$is_categorical
    getValidCategoricalFactors <- factor_api$get_valid
    js <- js_api

    source("server-conditions.R", local = TRUE)

    list(
      dds = ddsInitReactive,
      metadata = metadataFileReactive,
      update_formula = updateDesignFormula
    )
  })
}

svaServer <- function(
    id, root_input, root_output, root_session, state, design_api, js_api,
    goto_tab) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- state
    ddsInitReactive <- design_api$dds
    js <- js_api
    GotoTab <- goto_tab

    source("server-svaseq.R", local = TRUE)

    list(sva = svaReactive)
  })
}

deseqServer <- function(
    id, root_input, root_output, root_session, state, session_dir, factor_api,
    js_api, goto_tab) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- state
    isCategoricalFactor <- factor_api$is_categorical
    getValidCategoricalFactors <- factor_api$get_valid
    js <- js_api
    GotoTab <- goto_tab

    source("server-runDeseq.R", local = TRUE)

    list(dds = ddsReactive)
  })
}

resultsServer <- function(id, root_input, root_output, root_session, state,
                          session_dir, js_api) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- state
    js <- js_api

    source("server-analysisRes.R", local = TRUE)

    list(
      comparison = compareReactive,
      files = filelist,
      contrast_specs = contrast_specs
    )
  })
}

vennServer <- function(id, root_input, root_output, root_session, state,
                       session_dir, results_api) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- state
    filelist <- results_api$files

    source("server-venndiagram.R", local = TRUE)

    list(
      selected_matrix = selected_matrix,
      expression_data = expression_set_data,
      heatmap_matrix = heatmap_matrix
    )
  })
}

volcanoServer <- function(id, root_input, root_output, root_session, state,
                          session_dir, results_api) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- state
    filelist <- results_api$files

    source("server-volcanoplot.R", local = TRUE)

    list(
      selected_data = avo_data,
      significant_genes = sig_genes
    )
  })
}

boxplotServer <- function(id, root_input, root_output, root_session, state,
                          session_dir, color_generator) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- state
    generate_random_colors <- color_generator

    source("server-boxplot.R", local = TRUE)

    list(
      expression = geneExrReactive,
      colors = custom_colors
    )
  })
}

heatmapServer <- function(id, root_input, root_output, root_session, state,
                          session_dir) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- state

    source("server-heatmap.R", local = TRUE)

    list(heatmap = heatmapReactive)
  })
}
