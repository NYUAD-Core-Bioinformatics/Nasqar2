myValues <- reactiveValues()

observe({
    gseGoReactive()
})

gseGoReactive <- eventReactive(input$initGo, {
    withProgress(message = "Processing , please wait", {
        isolate({
            lapply(c("errorNotify", "errorNotify1", "errorNotify2", "warnNotify",
                     "warnNotify2", "goGseError", "keggGseError"), removeNotification)

            go_gse <- NULL
            kegg_gse <- NULL

            prepared <- tryCatch({
                df <- inputDataReactive()$data
                original_gene_list <- df[[input$log2fcColumn]]
                names(original_gene_list) <- df[[input$geneColumn]]
                gene_list <- sort(na.omit(original_gene_list), decreasing = TRUE)
                myValues$gene_list <- gene_list
                list(df = df, original_gene_list = original_gene_list, gene_list = gene_list)
            }, error = function(e) {
                showNotification(
                    id = "errorNotify",
                    paste("Input data error:", conditionMessage(e)),
                    type = "error", duration = NULL
                )
                NULL
            })

            if (!is.null(prepared)) {
                df <- prepared$df
                original_gene_list <- prepared$original_gene_list
                gene_list <- prepared$gene_list
                orgDb.obj <- eval(parse(text = input$organismDb, keep.source = FALSE))

                setProgress(value = 0.3, detail = "Performing GO gene set enrichment analysis ...")
                go_gse <- tryCatch(
                    gseGO(
                        geneList = gene_list,
                        ont = input$ontology,
                        keyType = input$keytype,
                        nPerm = input$nPerm,
                        minGSSize = input$minGSSize,
                        maxGSSize = input$maxGSSize,
                        pvalueCutoff = input$pvalCuttoff,
                        verbose = TRUE,
                        OrgDb = orgDb.obj,
                        pAdjustMethod = input$pAdjustMethod
                    ),
                    error = function(e) {
                        showNotification(
                            id = "goGseError",
                            paste("GO GSEA failed:", conditionMessage(e)),
                            type = "error", duration = NULL
                        )
                        NULL
                    }
                )

                if (!is.null(go_gse) && nrow(go_gse@result) > 0) {
                    if (isTRUE(input$simplifyGo)) {
                        go_gse <- tryCatch(
                            simplify(go_gse, cutoff = input$simplifyCutoff),
                            error = function(e) {
                                showNotification(
                                    paste("GO simplification was skipped:", conditionMessage(e)),
                                    type = "warning", duration = 10
                                )
                                go_gse
                            }
                        )
                    }
                    n_go <- nrow(go_gse@result)
                    updateNumericInput(session, "showCategory_go_global", max = n_go, value = min(5L, n_go))
                    updateSelectizeInput(session, "pubmedTerms", choices = go_gse@result$Description)
                } else {
                    go_gse <- NULL
                    showNotification("GO GSEA returned no terms. KEGG GSEA will still be attempted.", type = "warning", duration = 10)
                }

                setProgress(value = 0.6, detail = "Performing KEGG gene set enrichment analysis ...")
                kegg_gse <- tryCatch({
                    ids <- NULL
                    myValues$convWarningMessage <- capture.output(
                        ids <- bitr(names(original_gene_list), fromType = input$keytype,
                                    toType = "ENTREZID", OrgDb = input$organismDb),
                        type = "message"
                    )
                    dedup_ids <- ids[!duplicated(ids[[input$keytype]]), , drop = FALSE]
                    mapped_entrez <- dedup_ids$ENTREZID[
                        match(df[[input$geneColumn]], dedup_ids[[input$keytype]])
                    ]
                    mapped <- !is.na(mapped_entrez)
                    df2 <- df[mapped, , drop = FALSE]
                    df2$Y <- mapped_entrez[mapped]

                    kegg_gene_list <- df2[[input$log2fcColumn]]
                    names(kegg_gene_list) <- df2$Y
                    kegg_gene_list <- sort(na.omit(kegg_gene_list), decreasing = TRUE)
                    myValues$kegg_gene_list <- kegg_gene_list

                    organismsDbKegg <- c(
                        "org.Hs.eg.db" = "hsa", "org.Mm.eg.db" = "mmu", "org.Rn.eg.db" = "rno",
                        "org.Sc.sgd.db" = "sce", "org.Dm.eg.db" = "dme", "org.At.tair.db" = "ath",
                        "org.Dr.eg.db" = "dre", "org.Bt.eg.db" = "bta", "org.Ce.eg.db" = "cel",
                        "org.Gg.eg.db" = "gga", "org.Cf.eg.db" = "cfa", "org.Ss.eg.db" = "ssc",
                        "org.Mmu.eg.db" = "mcc", "org.EcK12.eg.db" = "eck", "org.Xl.eg.db" = "xla",
                        "org.Pt.eg.db" = "ptr", "org.Ag.eg.db" = "aga", "org.Pf.plasmo.db" = "pfa",
                        "org.EcSakai.eg.db" = "ecs"
                    )
                    myValues$organismKegg <- unname(organismsDbKegg[input$organismDb])

                    gseKEGG(
                        geneList = kegg_gene_list,
                        organism = myValues$organismKegg,
                        nPerm = input$nPerm,
                        minGSSize = input$minGSSize,
                        maxGSSize = input$maxGSSize,
                        pvalueCutoff = input$pvalCuttoff,
                        pAdjustMethod = input$pAdjustMethod,
                        keyType = "ncbi-geneid"
                    )
                }, error = function(e) {
                    showNotification(
                        id = "keggGseError",
                        paste("KEGG GSEA failed:", conditionMessage(e)),
                        type = "error", duration = NULL
                    )
                    NULL
                })

                if (!is.null(kegg_gse) && nrow(kegg_gse@result) > 0) {
                    n_kegg <- nrow(kegg_gse@result)
                    updateNumericInput(session, "showCategory_kegg_global", max = n_kegg, value = min(5L, n_kegg))
                    pathway_choices <- setNames(
                        kegg_gse@result$ID,
                        paste0(kegg_gse@result$ID, " \u2014 ", kegg_gse@result$Description)
                    )
                    updateSelectizeInput(session, "pathwayIds", choices = pathway_choices)
                } else {
                    kegg_gse <- NULL
                    updateSelectizeInput(session, "pathwayIds", choices = character(0))
                    showNotification("KEGG GSEA returned no pathways. GO results remain available.", type = "warning", duration = 10)
                }
            }
        })

        if (!is.null(go_gse)) {
            shinyjs::show(selector = "a[data-value=\"pubmedTab\"]")
            shinyjs::show(selector = "a[data-value=\"goplotsTab\"]")
            shinyjs::show(selector = "a[data-value=\"gseGoTab\"]")
        } else {
            shinyjs::hide(selector = "a[data-value=\"pubmedTab\"]")
            shinyjs::hide(selector = "a[data-value=\"goplotsTab\"]")
            shinyjs::hide(selector = "a[data-value=\"gseGoTab\"]")
        }
        if (!is.null(kegg_gse)) {
            shinyjs::show(selector = "a[data-value=\"pathviewTab\"]")
            shinyjs::show(selector = "a[data-value=\"keggPlotsTab\"]")
            shinyjs::show(selector = "a[data-value=\"gseKeggTab\"]")
        } else {
            shinyjs::hide(selector = "a[data-value=\"pathviewTab\"]")
            shinyjs::hide(selector = "a[data-value=\"keggPlotsTab\"]")
            shinyjs::hide(selector = "a[data-value=\"gseKeggTab\"]")
        }

        return(list("go_gse" = go_gse, "kegg_gse" = kegg_gse))
    })
})


# ── Selected-term pills for gseGO ────────────────────────────────────────────
output$go_selected_pills <- renderUI({
    gse_data <- gseGoReactive()
    s <- input$go_checked_rows

    if (!is.null(gse_data) && !is.null(gse_data$go_gse) && length(s) > 0) {
        go_gse    <- gse_data$go_gse
        term_ids  <- go_gse@result$ID[s]
        term_desc <- go_gse@result$Description[s]

        pill_tags <- mapply(function(tid, tdesc) {
            label <- paste0(tid, " — ", substr(tdesc, 1, 40))
            tags$span(
                style = paste0(
                    "display:inline-flex; align-items:center; background:#27ae60; color:white;",
                    "border-radius:12px; padding:4px 12px; margin:3px 4px 6px 0;",
                    "font-size:13px; font-weight:600; cursor:default;"
                ),
                label,
                tags$button(
                    type = "button",
                    style = paste0(
                        "background:none; border:none; color:white; cursor:pointer;",
                        "font-size:16px; font-weight:bold; line-height:1;",
                        "padding:0 0 0 7px; margin:0; vertical-align:middle;"
                    ),
                    HTML("&times;"),
                    onclick = sprintf(
                        "Shiny.setInputValue('go_deselect_term','%s',{priority:'event'});",
                        tid
                    )
                )
            )
        }, term_ids, term_desc, SIMPLIFY = FALSE)

        tags$div(
            style = "padding:6px 0 4px 0; min-height:38px;",
            tags$strong("Selected: ", style = "font-size:13px; color:#555; margin-right:4px;"),
            pill_tags
        )
    }
})

# ── Deselect a GO term via pill × button ─────────────────────────────────────
observeEvent(input$go_deselect_term, {
    tid      <- input$go_deselect_term
    gse_data <- gseGoReactive()
    req(!is.null(gse_data), !is.null(gse_data$go_gse), nchar(tid) > 0)

    all_ids    <- gse_data$go_gse@result$ID
    idx_1based <- which(all_ids == tid)
    if (length(idx_1based) > 0) {
        session$sendCustomMessage("go_uncheck_row",
                                  list(dataIdx = as.integer(idx_1based[1]) - 1L))
    }
}, ignoreInit = TRUE)


output$gseGoTable <- renderDataTable({
    gse_data <- gseGoReactive()

    if (!is.null(gse_data) && !is.null(gse_data$go_gse)) {
        resultDF <- gse_data$go_gse@result

        if (!isTRUE(input$showAllColumns)) {
            resultDF <- resultDF[, seq_len(min(9, ncol(resultDF))), drop = FALSE]
        }

        # Prepend a checkbox column for row selection
        resultDF <- cbind(
            " " = '<input type="checkbox" class="go-row-cb" style="cursor:pointer;width:15px;height:15px;">',
            resultDF
        )

        DT::datatable(
            resultDF,
            escape    = FALSE,
            selection = "none",
            options   = list(
                scrollX    = TRUE,
                columnDefs = list(
                    list(orderable  = FALSE,
                         searchable = FALSE,
                         targets    = 0,
                         width      = "30px")
                )
            )
        )
    }
})

output$downloadgseGoCSV <- downloadHandler(
    filename = function() {
        paste0("gsego", ".csv")
    },
    content = function(file) {
        write.csv(gseGoReactive()$go_gse@result, file, row.names = TRUE)
    }
)

output$gseGoAvailable <-
    reactive({
        return(!is.null(gseGoReactive()$go_gse))
    })
outputOptions(output, "gseGoAvailable", suspendWhenHidden = FALSE)


# ── Selected-pathway pills for gseKEGG ───────────────────────────────────────
output$kegg_selected_pills <- renderUI({
    gse_data <- gseGoReactive()
    s <- input$kegg_checked_rows

    if (!is.null(gse_data) && !is.null(gse_data$kegg_gse) && length(s) > 0) {
        kegg_gse    <- gse_data$kegg_gse
        pathway_ids <- kegg_gse@result$ID[s]

        pill_tags <- lapply(pathway_ids, function(pid) {
            tags$span(
                style = paste0(
                    "display:inline-flex; align-items:center; background:#3c8dbc; color:white;",
                    "border-radius:12px; padding:4px 12px; margin:3px 4px 6px 0;",
                    "font-size:13px; font-weight:600; cursor:default;"
                ),
                pid,
                tags$button(
                    type = "button",
                    style = paste0(
                        "background:none; border:none; color:white; cursor:pointer;",
                        "font-size:16px; font-weight:bold; line-height:1;",
                        "padding:0 0 0 7px; margin:0; vertical-align:middle;"
                    ),
                    HTML("&times;"),
                    onclick = sprintf(
                        "Shiny.setInputValue('kegg_deselect_pathway','%s',{priority:'event'});",
                        pid
                    )
                )
            )
        })

        tags$div(
            style = "padding:6px 0 4px 0; min-height:38px;",
            tags$strong("Selected: ", style = "font-size:13px; color:#555; margin-right:4px;"),
            pill_tags
        )
    }
})

# ── Deselect a KEGG pathway via pill × button ─────────────────────────────────
observeEvent(input$kegg_deselect_pathway, {
    pid      <- input$kegg_deselect_pathway
    gse_data <- gseGoReactive()
    req(!is.null(gse_data), !is.null(gse_data$kegg_gse), nchar(pid) > 0)

    all_ids    <- gse_data$kegg_gse@result$ID
    idx_1based <- which(all_ids == pid)
    if (length(idx_1based) > 0) {
        session$sendCustomMessage("kegg_uncheck_row",
                                  list(dataIdx = as.integer(idx_1based[1]) - 1L))
    }
}, ignoreInit = TRUE)


output$gseKEGGTable <- renderDataTable(
    {
        gse_data <- gseGoReactive()

        if (!is.null(gse_data) && !is.null(gse_data$kegg_gse)) {
            resultDF <- gse_data$kegg_gse@result
            org_code  <- myValues$organismKegg

            # Batch-convert all unique ENTREZ IDs → KO IDs (e.g. "38742" → "K08513")
            all_entrez <- unique(unlist(strsplit(paste(resultDF$core_enrichment, collapse = "/"), "/")))
            gene_map   <- build_ko_map(all_entrez, org_code)

            # Make ID column clickable: map pathway + KO IDs + log2FC colour coding
            make_kegg_link <- function(pathway_id, gene_ids_str) {
                map_id   <- sub("^[a-z]+", "map", pathway_id)   # dme04130 → map04130
                genes    <- unlist(strsplit(gene_ids_str, "/"))
                ko_genes <- gene_map[as.character(genes)]

                # Drop genes with no KO assignment or empty KO string
                valid    <- !is.na(ko_genes) & nzchar(ko_genes)
                ko_genes <- ko_genes[valid]
                genes_v  <- genes[valid]
                if (length(genes_v) == 0) return(pathway_id)  # no KO → plain text

                # log2FC → green-lightgrey-red colour per gene
                lfc      <- myValues$kegg_gene_list[genes_v]
                lfc[is.na(lfc)] <- 0
                max_lfc  <- if (length(lfc) > 0) max(abs(lfc), na.rm = TRUE) else 1
                if (!is.finite(max_lfc) || max_lfc == 0) max_lfc <- 1
                col_ramp <- colorRamp(c("#1a9641", "#d3d3d3", "#d7191c"))
                norm     <- pmax(0, pmin(1, (lfc / max_lfc + 1) / 2))
                rgb_mat  <- col_ramp(norm)
                hex_cols <- apply(rgb_mat, 1, function(r) {
                    sprintf("%%23%02x%02x%02x", round(r[1]), round(r[2]), round(r[3]))
                })

                gene_parts <- paste0(ko_genes, "%09", hex_cols)
                url <- paste0("https://www.kegg.jp/kegg-bin/show_pathway?", map_id, "/",
                              paste(gene_parts, collapse = "/"))
                paste0('<a href="', url, '" target="_blank">', pathway_id, '</a>')
            }
            resultDF$ID <- mapply(make_kegg_link, resultDF$ID, resultDF$core_enrichment)

            # Convert core_enrichment ENTREZ → symbols for display (links already built above)
            readable <- tryCatch(
                setReadable(gse_data$kegg_gse, OrgDb = input$organismDb, keyType = "ENTREZID")@result,
                error = function(e) NULL
            )
            if (!is.null(readable))
                resultDF$core_enrichment <- readable[rownames(resultDF), "core_enrichment"]

            if (!isTRUE(input$showAllColumns_kegg)) {
                resultDF <- resultDF[, seq_len(min(9, ncol(resultDF))), drop = FALSE]
            }

            # Prepend a checkbox column for row selection
            resultDF <- cbind(
                " " = '<input type="checkbox" class="kegg-row-cb" style="cursor:pointer;width:15px;height:15px;">',
                resultDF
            )

            DT::datatable(
                resultDF,
                escape    = FALSE,
                selection = "none",
                options   = list(
                    scrollX    = TRUE,
                    columnDefs = list(
                        list(orderable  = FALSE,
                             searchable = FALSE,
                             targets    = 0,
                             width      = "30px")
                    )
                )
            )
        }
    },
    options = list(scrollX = TRUE)
)

output$downloadgseKEGGCSV <- downloadHandler(
    filename = function() {
        paste0("gseKEGG", ".csv")
    },
    content = function(file) {
        write.csv(gseGoReactive()$kegg_gse@result, file, row.names = TRUE)
    }
)

output$gseKEGGAvailable <-
    reactive({
        return(!is.null(gseGoReactive()$kegg_gse))
    })
outputOptions(output, "gseKEGGAvailable", suspendWhenHidden = FALSE)


output$warningText <- renderText({
    outputText <- myValues$convWarningMessage
    if (length(outputText) == 3) {
        outputText[3] <- paste0("<strong>", outputText[3], "</strong>")
    }

    paste("<p>", outputText, "</p>")
})


# ── KEGG UpSet plot (interactive) ─────────────────────────────────────────────
output$genesInKeggPathway <- plotly::renderPlotly({
    gse_data <- gseGoReactive()
    req(!is.null(gse_data))
    kegg_gse <- gse_data$kegg_gse
    req(!is.null(kegg_gse), nrow(kegg_gse@result) > 0)

    s <- input$kegg_checked_rows
    if (is.null(s) || length(s) == 0) s <- seq_len(min(input$showCategory_kegg_global, nrow(kegg_gse@result)))
    validate(need(length(s) >= 2, "Please select at least 2 pathways to view the gene intersection plot."))

    kegg_gse_readable <- tryCatch(
        setReadable(kegg_gse, OrgDb = input$organismDb, keyType = "ENTREZID"),
        error = function(e) kegg_gse
    )
    selected_results <- kegg_gse_readable@result[s, , drop = FALSE]

    set_labels <- substr(selected_results$Description, 1, 35)
    gene_sets  <- setNames(
        lapply(selected_results$core_enrichment, function(g) unlist(strsplit(g, "/"))),
        set_labels
    )
    gene_sets  <- gene_sets[order(sapply(gene_sets, length))]
    set_labels <- names(gene_sets)
    n_sets     <- length(gene_sets)
    all_genes  <- unique(unlist(gene_sets))

    membership_mat  <- sapply(gene_sets, function(gs) as.integer(all_genes %in% gs))
    rownames(membership_mat) <- all_genes
    patterns        <- apply(membership_mat, 1, paste, collapse = "")
    by_pattern      <- split(all_genes, patterns)
    sizes           <- sapply(by_pattern, length)
    ord             <- order(sizes, decreasing = TRUE)
    sorted_patterns <- names(by_pattern)[ord]
    sorted_sizes    <- sizes[ord]
    sorted_genes    <- by_pattern[ord]
    n_inter         <- length(sorted_patterns)
    x_pos           <- seq_len(n_inter)

    get_label <- function(pat) {
        bits <- as.integer(strsplit(pat, "")[[1]])
        paste(set_labels[which(bits == 1)], collapse = " \u2229 ")
    }
    inter_labels <- sapply(sorted_patterns, get_label)

    myValues$kegg_sorted_genes <- sorted_genes
    myValues$kegg_inter_labels <- inter_labels

    p_top <- plotly::plot_ly(
        x = x_pos, y = sorted_sizes,
        type = "bar",
        marker = list(color = "#2c3e50"),
        text = paste0(sorted_sizes, " genes — click to see"),
        hoverinfo = "text", textposition = "none"
    ) %>% plotly::layout(
        xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE, range = c(0.5, n_inter + 0.5)),
        yaxis = list(title = "Intersection Size"),
        showlegend = FALSE, bargap = 0.35
    )

    dot_df <- expand.grid(x = x_pos, y = seq_len(n_sets))
    dot_df$filled <- mapply(function(xi, yi) {
        as.integer(strsplit(sorted_patterns[xi], "")[[1]])[yi] == 1
    }, dot_df$x, dot_df$y)

    p_bottom <- plotly::plot_ly(hoverinfo = "none") %>%
        plotly::add_markers(
            data = dot_df[!dot_df$filled, ], x = ~x, y = ~y,
            marker = list(color = "#cccccc", size = 10), showlegend = FALSE
        ) %>%
        plotly::add_markers(
            data = dot_df[dot_df$filled, ], x = ~x, y = ~y,
            marker = list(color = "#2c3e50", size = 12), showlegend = FALSE
        ) %>%
        plotly::layout(
            xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE, range = c(0.5, n_inter + 0.5)),
            yaxis = list(ticktext = set_labels, tickvals = seq_len(n_sets), tickmode = "array", zeroline = FALSE, showgrid = FALSE),
            showlegend = FALSE
        )

    for (xi in x_pos) {
        bits     <- as.integer(strsplit(sorted_patterns[xi], "")[[1]])
        filled_y <- which(bits == 1)
        if (length(filled_y) >= 2) {
            p_bottom <- p_bottom %>%
                plotly::add_segments(
                    x = xi, xend = xi, y = min(filled_y), yend = max(filled_y),
                    line = list(color = "#2c3e50", width = 2),
                    hoverinfo = "none", showlegend = FALSE
                )
        }
    }

    plotly::subplot(p_top, p_bottom, nrows = 2, heights = c(0.55, 0.45),
                    shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
                       margin = list(l = 220), autosize = TRUE) %>%
        publication_plotly_config("gsea-kegg-gene-membership") %>%
        htmlwidgets::onRender("function(el) { setTimeout(function() { Plotly.Plots.resize(el); }, 300); }")
})

observeEvent(plotly::event_data("plotly_click", source = "A"), {
    click    <- plotly::event_data("plotly_click", source = "A")
    s_genes  <- myValues$kegg_sorted_genes
    s_labels <- myValues$kegg_inter_labels
    req(!is.null(click), !is.null(s_genes))
    req(isTRUE(click$curveNumber == 0))

    bar_idx <- round(click$x)
    req(bar_idx >= 1, bar_idx <= length(s_genes))
    myValues$clicked_inter_genes <- sort(s_genes[[bar_idx]])
    myValues$clicked_inter_label <- s_labels[bar_idx]
})

observeEvent(input$kegg_checked_rows, {
    myValues$clicked_inter_genes <- NULL
    myValues$clicked_inter_label <- NULL
}, ignoreNULL = FALSE)

output$kegg_clicked_genes_inline <- renderUI({
    req(myValues$clicked_inter_genes)
    tagList(
        tags$hr(),
        h4(strong(paste0(myValues$clicked_inter_label, "  \u2014  ", length(myValues$clicked_inter_genes), " genes"))),
        DT::dataTableOutput("kegg_clicked_genes_table")
    )
})

output$kegg_clicked_genes_table <- DT::renderDataTable({
    req(myValues$clicked_inter_genes)
    DT::datatable(data.frame(Gene = myValues$clicked_inter_genes),
                  options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
})

output$genesKeggMembershipTable <- renderDataTable({
    gse_data <- gseGoReactive()
    req(!is.null(gse_data), !is.null(gse_data$kegg_gse))
    kegg_gse <- gse_data$kegg_gse
    s <- input$kegg_checked_rows
    if (is.null(s) || length(s) == 0) s <- seq_len(min(input$showCategory_kegg_global, nrow(kegg_gse@result)))
    req(nrow(kegg_gse@result) > 0)

    kegg_gse_readable <- tryCatch(
        setReadable(kegg_gse, OrgDb = input$organismDb, keyType = "ENTREZID"),
        error = function(e) kegg_gse
    )
    selected_results <- kegg_gse_readable@result[s, , drop = FALSE]
    all_genes <- unique(unlist(lapply(selected_results$core_enrichment, function(g) unlist(strsplit(g, "/")))))

    membership <- data.frame(Gene = sort(all_genes), stringsAsFactors = FALSE)
    for (i in seq_len(nrow(selected_results))) {
        pathway_genes <- unlist(strsplit(selected_results$core_enrichment[i], "/"))
        col_name <- substr(selected_results$Description[i], 1, 35)
        membership[[col_name]] <- ifelse(membership$Gene %in% pathway_genes, "\u2713", "")
    }
    DT::datatable(membership, options = list(scrollX = TRUE, pageLength = 25), rownames = FALSE)
})

observeEvent(input$gotoGoPlots, {
    GotoTab("goplotsTab")
})

observeEvent(input$gotoKeggPlots, {
    GotoTab("keggPlotsTab")
})

observeEvent(input$gotoPathview, {
    GotoTab("pathviewTab")
})

observeEvent(input$gotoPubmed, {
    GotoTab("pubmedTab")
})
