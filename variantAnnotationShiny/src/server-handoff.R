handoff_genes <- reactive({
    req(my_values$annotation)
    annotated_gene_list(
        my_values$annotation$genes,
        id_type = input$handoff_id_type %||% "SYMBOL",
        coding_only = isTRUE(input$handoff_coding_only)
    )
})

output$handoff_gene_count <- renderText({
    genes <- handoff_genes()
    sprintf("%s gene(s) will be transferred.", format(length(genes), big.mark = ","))
})

output$handoff_link <- renderUI({
    genes <- handoff_genes()
    if (!length(genes)) {
        return(helpText("No genes match the current selection."))
    }
    exchange_path <- write_gene_list_csv(genes)
    token <- encryptUrlParam(exchange_path)
    handoff_path <- paste0(
        Sys.getenv("NASQAR_BASE_PATH", unset = ""),
        "/ClusterProfShinyORA/?gene_names=", token
    )
    tags$a(
        icon("external-link-alt"),
        "Open genes in ClusterProfShinyORA",
        href = handoff_path,
        `data-handoff-path` = handoff_path,
        onclick = paste(
            "this.href = window.location.origin +",
            "this.getAttribute('data-handoff-path');"
        ),
        target = "_blank",
        class = "btn btn-success",
        style = "width: 100%; margin-top: 10px;"
    )
})
