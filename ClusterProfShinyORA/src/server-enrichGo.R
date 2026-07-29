myValues <- reactiveValues()

observe({
    enrichGoReactive()
})

enrichGoReactive <- eventReactive(input$initGo, {
    query <- parseQueryString(session$clientData$url_search)

    withProgress(message = "Processing , please wait", {
        isolate({
            if (!is.null(query[["gene_names"]])) {
                print(inputDataReactive()$data)
                genes <- inputDataReactive()$data
                go_enrich <- enrichGO(
                    gene = genes$x,
                    OrgDb = input$organismDb,
                    keyType = input$keytype,
                    minGSSize = input$minGSSize,
                    maxGSSize = input$maxGSSize,
                    readable = T,
                    ont = input$ontology,
                    pool = isTRUE(input$poolGo),
                    pvalueCutoff = input$pvalCuttoff,
                    qvalueCutoff = input$qvalCuttoff,
                    pAdjustMethod = input$pAdjustMethod
                )

                if (isTRUE(input$simplifyGo) && nrow(go_enrich) > 0) {
                    go_enrich <- tryCatch(
                        simplify(go_enrich, cutoff = input$simplifyCutoff),
                        error = function(e) go_enrich
                    )
                }

                if (nrow(go_enrich) < 1) {
                    showNotification(id = "warnNotify", "No gene can be mapped ...", type = "warning", duration = NULL)
                    showNotification(id = "warnNotify2", "Tune the parameters and try again.", type = "warning", duration = NULL)
                    return(NULL)
                }
                n <- nrow(go_enrich@result)
                updateNumericInput(session, "showCategory_bar", max = n, min = 0, value = ifelse(n > 0, 5, 0))
                updateNumericInput(session, "showCategory_dot", max = n, min = 0, value = ifelse(n > 0, 5, 0))
                updateNumericInput(session, "showCategory_enrichmap", max = n, min = 0, value = ifelse(n > 0, 5, 0))
                updateNumericInput(session, "showCategory_goplot", max = n, min = 0, value = ifelse(n > 0, 5, 0))
                updateNumericInput(session, "showCategory_cnet", max = n, min = 0, value = ifelse(n > 0, 5, 0))
                updateNumericInput(session, "showCategory_tree", max = n, min = 0, value = ifelse(n > 0, min(30, n), 0))
                updateNumericInput(session, "showCategory_heat", max = n, min = 0, value = ifelse(n > 0, min(10, n), 0))

                shinyjs::show(selector = "a[data-value=\"goplotsTab\"]")
                shinyjs::show(selector = "a[data-value=\"enrichGoTab\"]")

                return(list("go_enrich" = go_enrich, "kegg_enrich" = NULL))
            } else {
                # remove notifications if they exist
                removeNotification("errorNotify")
                removeNotification("errorNotify1")
                removeNotification("errorNotify2")
                removeNotification("warnNotify")
                removeNotification("warnNotify2")
                removeNotification("keggEnrichError")

                go_enrich <- NULL
                kegg_enrich <- NULL

                validate(need(
                    tryCatch(
                        {
                            df <- inputDataReactive()$data
                            # we want the log2 fold change
                            original_gene_list <- df[[input$log2fcColumn]]

                            # name the vector
                            names(original_gene_list) <- df[[input$geneColumn]]

                            # omit any NA values
                            gene_list <- na.omit(original_gene_list)

                            # sort the list in decreasing order (required for clusterProfiler)
                            gene_list <- sort(gene_list, decreasing = TRUE)

                            myValues$gene_list <- gene_list

                            # Exctract significant results
                            # ALLOW USERS TO EDIT 0.05 AS A PARAMETER
                            # sig_genes_df = subset(df, padj < input$padjCutoff)
                            sig_genes_df <- df[df[, input$padjColumn] < input$padjCutoff, ]
                            sig_genes_df <- na.omit(sig_genes_df)

                            # From significant results, we want to filter on log2fold change
                            genes <- sig_genes_df[[input$log2fcColumn]]

                            # Name the vector
                            names(genes) <- sig_genes_df[[input$geneColumn]]

                            # omit NA values
                            genes <- na.omit(genes)

                            # filter on min log2fold change (PARAMETER)
                            genes <- names(genes)[abs(genes) > input$logfcCuttoff]

                            # Optionally intersect with preferred gene list
                            if (isTRUE(input$usePreferredGenes) && nchar(trimws(input$preferredGeneList)) > 0) {
                                preferred <- unique(trimws(strsplit(input$preferredGeneList, "[\n,;\t]+")[[1]]))
                                preferred <- preferred[nchar(preferred) > 0]
                                genes_before <- length(genes)
                                genes <- intersect(genes, preferred)
                                showNotification(
                                    ui = paste0("Preferred gene list applied: ", length(genes), " of ", genes_before,
                                                " threshold-filtered genes retained."),
                                    type = "message", duration = 8
                                )
                            }

                            setProgress(value = 0.3, detail = "Performing Go enrichment analysis, please wait ...")

                            go_enrich <- enrichGO(
                                gene = genes,
                                universe = names(gene_list),
                                OrgDb = input$organismDb,
                                keyType = input$keytype,
                                minGSSize = input$minGSSize,
                                maxGSSize = input$maxGSSize,
                                readable = T,
                                ont = input$ontology,
                                pool = isTRUE(input$poolGo),
                                pvalueCutoff = input$pvalCuttoff,
                                qvalueCutoff = input$qvalCuttoff,
                                pAdjustMethod = input$pAdjustMethod
                            )

                            if (isTRUE(input$simplifyGo) && nrow(go_enrich) > 0) {
                                go_enrich <- tryCatch(
                                    simplify(go_enrich, cutoff = input$simplifyCutoff),
                                    error = function(e) go_enrich
                                )
                            }

                            if (nrow(go_enrich) < 1) {
                                showNotification(id = "warnNotify", "No gene can be mapped ...", type = "warning", duration = NULL)
                                showNotification(id = "warnNotify2", "Tune the parameters and try again.", type = "warning", duration = NULL)
                                return(NULL)
                            }

                            nGo <- nrow(go_enrich@result)
                            updateNumericInput(session, "showCategory_go_global", max = nGo, value = min(5, nGo))

                            ## KEGG enrich

                            # Convert gene IDs for enrichKEGG function
                            # We will lose some genes here because not all IDs will be converted
                            myValues$convWarningMessage <- capture.output(ids <- bitr(names(original_gene_list), fromType = input$keytype, toType = "ENTREZID", OrgDb = input$organismDb), type = "message")

                            # remove duplicate IDS (here I use "ENSEMBL", but it should be whatever was selected as keyType)
                            dedup_ids <- ids[!duplicated(ids[c(input$keytype)]), ]
                            myValues$dedup_ids <- dedup_ids  # store for ENTREZ → original ID reverse lookup

                            # Match by the selected identifier. Filtering and assigning by
                            # position can attach an ENTREZ ID to the wrong input row.
                            mapped_entrez <- dedup_ids$ENTREZID[
                                match(df[[input$geneColumn]], dedup_ids[[input$keytype]])
                            ]
                            mapped <- !is.na(mapped_entrez)
                            df2 <- df[mapped, , drop = FALSE]
                            df2$Y <- mapped_entrez[mapped]

                            # Create a vector of the gene unuiverse
                            kegg_gene_list <- df2[[input$log2fcColumn]]

                            # Name vector with ENTREZ ids
                            names(kegg_gene_list) <- df2$Y

                            # omit any NA values
                            kegg_gene_list <- na.omit(kegg_gene_list)

                            # sort the list in decreasing order (required for clusterProfiler)
                            kegg_gene_list <- sort(kegg_gene_list, decreasing = TRUE)

                            myValues$kegg_gene_list <- kegg_gene_list

                            # Exctract significant results from df2
                            # ALLOW USERS TO EDIT 0.05 AS A PARAMETER
                            # kegg_sig_genes_df = subset(df2, padj < input$padjCutoff)
                            kegg_sig_genes_df <- df2[df2[, input$padjColumn] < input$padjCutoff, ]
                            kegg_sig_genes_df <- na.omit(kegg_sig_genes_df)

                            # From significant results, we want to filter on log2fold change
                            kegg_genes <- kegg_sig_genes_df[[input$log2fcColumn]]

                            # Name the vector with the CONVERTED ID!
                            names(kegg_genes) <- kegg_sig_genes_df$Y

                            # omit NA values
                            kegg_genes <- na.omit(kegg_genes)

                            # filter on log2fold change (PARAMETER)
                            kegg_genes <- names(kegg_genes)[abs(kegg_genes) > input$logfcCuttoff]

                            # Optionally intersect with preferred gene list (using ENTREZ IDs mapped from preferred names)
                            if (isTRUE(input$usePreferredGenes) && nchar(trimws(input$preferredGeneList)) > 0) {
                                preferred <- unique(trimws(strsplit(input$preferredGeneList, "[\n,;\t]+")[[1]]))
                                preferred <- preferred[nchar(preferred) > 0]
                                # Map preferred names to ENTREZ IDs via dedup_ids
                                preferred_entrez <- dedup_ids$ENTREZID[dedup_ids[[input$keytype]] %in% preferred]
                                kegg_genes <- intersect(kegg_genes, preferred_entrez)
                            }

                            setProgress(value = 0.6, detail = "Performing KEGG enrichment analysis, please wait ...")

                            organismsDbKegg <- c(
                                "org.Hs.eg.db" = "hsa", "org.Mm.eg.db" = "mmu", "org.Rn.eg.db" = "rno",
                                "org.Sc.sgd.db" = "sce", "org.Dm.eg.db" = "dme", "org.At.tair.db" = "ath",
                                "org.Dr.eg.db" = "dre", "org.Bt.eg.db" = "bta", "org.Ce.eg.db" = "cel",
                                "org.Gg.eg.db" = "gga", "org.Cf.eg.db" = "cfa", "org.Ss.eg.db" = "ssc",
                                "org.Mmu.eg.db" = "mcc", "org.EcK12.eg.db" = "eck", "org.Xl.eg.db" = "xla",
                                "org.Pt.eg.db" = "ptr", "org.Ag.eg.db" = "aga", "org.Pf.plasmo.db" = "pfa",
                                "org.EcSakai.eg.db" = "ecs"
                            )

                            kegg_enrich <- tryCatch(
                                enrichKEGG(
                                    gene = kegg_genes,
                                    universe = names(kegg_gene_list),
                                    organism = organismsDbKegg[input$organismDb],
                                    pvalueCutoff = input$pvalCuttoff,
                                    qvalueCutoff = input$qvalCuttoff,
                                    pAdjustMethod = input$pAdjustMethod,
                                    keyType = "ncbi-geneid",
                                    minGSSize = input$minGSSize,
                                    maxGSSize = input$maxGSSize
                                ),
                                error = function(e) {
                                    showNotification(
                                        id = "keggEnrichError",
                                        paste("KEGG enrichment failed:", conditionMessage(e),
                                              "GO results remain available."),
                                        type = "warning", duration = NULL
                                    )
                                    NULL
                                }
                            )

                            myValues$organismKegg <- organismsDbKegg[input$organismDb]


                            if (!is.null(kegg_enrich) && nrow(kegg_enrich@result) > 0) {
                                nKegg <- nrow(kegg_enrich@result)
                                updateNumericInput(session, "showCategory_kegg_global",
                                                   max = nKegg, value = min(5, nKegg))
                                pathway_choices <- setNames(
                                    kegg_enrich@result$ID,
                                    paste0(kegg_enrich@result$ID, " \u2014 ",
                                           kegg_enrich@result$Description)
                                )
                                updateSelectizeInput(session, "pathwayIds", choices = pathway_choices)
                            } else {
                                kegg_enrich <- NULL
                                updateSelectizeInput(session, "pathwayIds", choices = character(0))
                                showNotification(
                                    "KEGG enrichment returned no pathways. GO results remain available.",
                                    type = "warning", duration = 10
                                )
                            }
                        },
                        error = function(e) {
                            myValues$status <- paste("Error: ", e$message)

                            showNotification(id = "errorNotify", myValues$status, type = "error", duration = NULL)
                            showNotification(id = "errorNotify1", "Make sure the right organism was selected", type = "error", duration = NULL)
                            showNotification(id = "errorNotify2", "Make sure the corresponding required columns are correctly selected", type = "error", duration = NULL)
                            return(NULL)
                        }
                    ),
                    "Error. Check!"
                ))
            }
        })

        # if()


        if (!is.null(go_enrich)) {
            shinyjs::show(selector = "a[data-value=\"goplotsTab\"]")
            shinyjs::show(selector = "a[data-value=\"enrichGoTab\"]")
        } else {
            shinyjs::hide(selector = "a[data-value=\"goplotsTab\"]")
            shinyjs::hide(selector = "a[data-value=\"enrichGoTab\"]")
        }
        if (!is.null(kegg_enrich)) {
            shinyjs::show(selector = "a[data-value=\"pathviewTab\"]")
            shinyjs::show(selector = "a[data-value=\"keggPlotsTab\"]")
            shinyjs::show(selector = "a[data-value=\"enrichKeggTab\"]")
        } else {
            shinyjs::hide(selector = "a[data-value=\"pathviewTab\"]")
            shinyjs::hide(selector = "a[data-value=\"keggPlotsTab\"]")
            shinyjs::hide(selector = "a[data-value=\"enrichKeggTab\"]")
        }
        if (!is.null(go_enrich) || !is.null(kegg_enrich)) {
            shinyjs::show(selector = "a[data-value=\"wordcloudTab\"]")
        } else {
            shinyjs::hide(selector = "a[data-value=\"wordcloudTab\"]")
        }

        return(list("go_enrich" = go_enrich, "kegg_enrich" = kegg_enrich))
    })
})

# output$x4 = renderPrint({
#     s = input$enrichGoTable_rows_selected
#     if (length(s)) {
#       cat('These rows were selected:\n\n')
#       cat(s, sep = ', ')
#     }
# })
output$enrich_kegg_selected <- renderDataTable(
    {
        #enrichGo <- enrichGoReactive()
        enrichKEGG <- enrichGoReactive()
        s <- input$kegg_checked_rows
    
 
        if (!is.null(enrichKEGG) && length(s) > 0) {
            kegg_enrich <- tryCatch(
                setReadable(enrichKEGG$kegg_enrich, OrgDb = input$organismDb, keyType = "ENTREZID"),
                error = function(e) enrichKEGG$kegg_enrich
            )
            # Directly subset @result to avoid S4 filter() issues with multiple rows
            resultDF <- kegg_enrich@result[s, , drop = FALSE]

            if (!isTRUE(input$showGeneidKegg)) {
                resultDF <- resultDF[, setdiff(names(resultDF), "geneID"), drop = FALSE]
            }

            if (nrow(resultDF) > 0) {
                resultDF$KEGG_Link <- sprintf(
                    '<a href="https://www.kegg.jp/pathway/%s" target="_blank">%s</a>',
                    as.character(resultDF$ID), as.character(resultDF$ID)
                )
            }
            DT::datatable(resultDF, escape = FALSE, options = list(scrollX = TRUE))
        }
    }
)
# ── Selected-pathway pills ──────────────────────────────────────────────────
output$kegg_selected_pills <- renderUI({
    enrichKEGG <- enrichGoReactive()
    s <- input$kegg_checked_rows

    if (!is.null(enrichKEGG) && length(s) > 0) {
        kegg_enrich  <- enrichKEGG$kegg_enrich
        pathway_ids  <- kegg_enrich@result$ID[s]

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

# ── Deselect a pathway via pill × button ─────────────────────────────────────
observeEvent(input$kegg_deselect_pathway, {
    pid        <- input$kegg_deselect_pathway
    enrichKEGG <- enrichGoReactive()
    req(!is.null(enrichKEGG), nchar(pid) > 0)

    all_ids      <- enrichKEGG$kegg_enrich@result$ID
    idx_1based   <- which(all_ids == pid)
    if (length(idx_1based) > 0) {
        # JS uses 0-based data indices; send the first match
        session$sendCustomMessage("kegg_uncheck_row",
                                  list(dataIdx = as.integer(idx_1based[1]) - 1L))
    }
}, ignoreInit = TRUE)

# UpSet plot for genes in selected KEGG pathways
output$genesInKeggPathway <- plotly::renderPlotly({
    enrichKEGG <- enrichGoReactive()
    req(!is.null(enrichKEGG))
    req(nrow(enrichKEGG$kegg_enrich@result) > 0)
    s <- input$kegg_checked_rows
    if (is.null(s) || length(s) == 0) s <- seq_len(min(input$showCategory_kegg_global, nrow(enrichKEGG$kegg_enrich@result)))
    validate(need(length(s) >= 2, "Please select at least 2 pathways to view the gene intersection plot."))

    kegg_enrich <- enrichKEGG$kegg_enrich
    kegg_enrich_readable <- tryCatch(
        setReadable(kegg_enrich, OrgDb = input$organismDb, keyType = "ENTREZID"),
        error = function(e) kegg_enrich
    )
    selected_results <- kegg_enrich_readable@result[s, , drop = FALSE]

    # Order sets largest → smallest (bottom of matrix = largest, like UpSetR)
    set_labels <- substr(selected_results$Description, 1, 35)
    gene_sets  <- setNames(
        lapply(selected_results$geneID, function(g) unlist(strsplit(g, "/"))),
        set_labels
    )
    gene_sets <- gene_sets[order(sapply(gene_sets, length))]  # ascending for y-axis
    set_labels <- names(gene_sets)
    n_sets <- length(gene_sets)

    all_genes <- unique(unlist(gene_sets))

    # Compute intersections (always 2+ pathways guaranteed by validate above)
    membership_mat <- sapply(gene_sets, function(gs) as.integer(all_genes %in% gs))
    rownames(membership_mat) <- all_genes
    patterns    <- apply(membership_mat, 1, paste, collapse = "")
    by_pattern  <- split(all_genes, patterns)
    sizes       <- sapply(by_pattern, length)
    ord         <- order(sizes, decreasing = TRUE)
    sorted_patterns <- names(by_pattern)[ord]
    sorted_sizes    <- sizes[ord]
    sorted_genes    <- by_pattern[ord]
    n_inter <- length(sorted_patterns)
    x_pos   <- seq_len(n_inter)

    get_label <- function(pat) {
        bits <- as.integer(strsplit(pat, "")[[1]])
        paste(set_labels[which(bits == 1)], collapse = " \u2229 ")
    }
    inter_labels <- sapply(sorted_patterns, get_label)

    # Store for click handler
    myValues$kegg_sorted_genes  <- sorted_genes
    myValues$kegg_inter_labels  <- inter_labels

    # ── Top panel: intersection size bars ──────────────────────────────
    p_top <- plotly::plot_ly(
        x = x_pos, y = sorted_sizes,
        type = "bar",
        marker = list(color = "#2c3e50"),
        text = paste0(sorted_sizes, " genes — click to see"),
        hoverinfo = "text",
        textposition = "none"
    ) %>% plotly::layout(
        xaxis = list(showticklabels = FALSE, zeroline = FALSE,
                     showgrid = FALSE, range = c(0.5, n_inter + 0.5)),
        yaxis = list(title = "Intersection Size"),
        showlegend = FALSE, bargap = 0.35
    )

    # ── Bottom panel: set membership dot matrix ────────────────────────
    dot_df <- expand.grid(x = x_pos, y = seq_len(n_sets))
    dot_df$filled <- mapply(function(xi, yi) {
        as.integer(strsplit(sorted_patterns[xi], "")[[1]])[yi] == 1
    }, dot_df$x, dot_df$y)

    p_bottom <- plotly::plot_ly(hoverinfo = "none") %>%
        plotly::add_markers(
            data = dot_df[!dot_df$filled, ], x = ~x, y = ~y,
            marker = list(color = "#cccccc", size = 10),
            showlegend = FALSE
        ) %>%
        plotly::add_markers(
            data = dot_df[dot_df$filled, ], x = ~x, y = ~y,
            marker = list(color = "#2c3e50", size = 12),
            showlegend = FALSE
        ) %>%
        plotly::layout(
            xaxis = list(showticklabels = FALSE, zeroline = FALSE,
                         showgrid = FALSE, range = c(0.5, n_inter + 0.5)),
            yaxis = list(ticktext = set_labels, tickvals = seq_len(n_sets),
                         tickmode = "array", zeroline = FALSE, showgrid = FALSE),
            showlegend = FALSE
        )

    # Add vertical connecting lines between filled dots in same intersection
    for (xi in x_pos) {
        bits     <- as.integer(strsplit(sorted_patterns[xi], "")[[1]])
        filled_y <- which(bits == 1)
        if (length(filled_y) >= 2) {
            p_bottom <- p_bottom %>%
                plotly::add_segments(
                    x = xi, xend = xi,
                    y = min(filled_y), yend = max(filled_y),
                    line = list(color = "#2c3e50", width = 2),
                    hoverinfo = "none", showlegend = FALSE
                )
        }
    }

    # subplot() resets source to default "A" — don't override it; listen on "A"
    plotly::subplot(p_top, p_bottom, nrows = 2, heights = c(0.55, 0.45),
                    shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(
            plot_bgcolor  = "#ffffff",
            paper_bgcolor = "#ffffff",
            margin = list(l = 220),
            autosize = TRUE
        ) %>%
        publication_plotly_config("ora-kegg-gene-membership") %>%
        htmlwidgets::onRender("function(el) { setTimeout(function() { Plotly.Plots.resize(el); }, 300); }")
})

# Genes shown on bar click — listen on default source "A" (subplot resets source)
observeEvent(plotly::event_data("plotly_click", source = "A"), {
    click    <- plotly::event_data("plotly_click", source = "A")
    s_genes  <- myValues$kegg_sorted_genes
    s_labels <- myValues$kegg_inter_labels
    req(!is.null(click), !is.null(s_genes))
    req(isTRUE(click$curveNumber == 0))   # top-panel bars only, not dot matrix

    bar_idx <- round(click$x)
    req(bar_idx >= 1, bar_idx <= length(s_genes))

    myValues$clicked_inter_genes <- sort(s_genes[[bar_idx]])
    myValues$clicked_inter_label <- s_labels[bar_idx]
})

# Reset inline gene list whenever pathway selection changes
observeEvent(input$kegg_checked_rows, {
    myValues$clicked_inter_genes <- NULL
    myValues$clicked_inter_label <- NULL
}, ignoreNULL = FALSE)

output$kegg_clicked_genes_inline <- renderUI({
    req(myValues$clicked_inter_genes)
    tagList(
        tags$hr(),
        h4(strong(paste0(
            myValues$clicked_inter_label,
            "  \u2014  ", length(myValues$clicked_inter_genes), " genes"
        ))),
        DT::dataTableOutput("kegg_clicked_genes_table")
    )
})

output$kegg_clicked_genes_table <- DT::renderDataTable({
    req(myValues$clicked_inter_genes)
    DT::datatable(
        data.frame(Gene = myValues$clicked_inter_genes),
        options  = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
    )
})

# Gene-pathway membership table below UpSet plot
output$genesKeggMembershipTable <- renderDataTable({
    enrichKEGG <- enrichGoReactive()
    req(!is.null(enrichKEGG))
    s <- input$kegg_checked_rows
    if (is.null(s) || length(s) == 0) s <- seq_len(min(input$showCategory_kegg_global, nrow(enrichKEGG$kegg_enrich@result)))
    req(nrow(enrichKEGG$kegg_enrich@result) > 0)

    kegg_enrich <- enrichKEGG$kegg_enrich
    kegg_enrich_readable <- tryCatch(
        setReadable(kegg_enrich, OrgDb = input$organismDb, keyType = "ENTREZID"),
        error = function(e) kegg_enrich
    )

    selected_results <- kegg_enrich_readable@result[s, , drop = FALSE]
    all_genes <- unique(unlist(lapply(selected_results$geneID, function(g) unlist(strsplit(g, "/")))))

    membership <- data.frame(Gene = sort(all_genes), stringsAsFactors = FALSE)
    for (i in seq_len(nrow(selected_results))) {
        pathway_genes <- unlist(strsplit(selected_results$geneID[i], "/"))
        col_name <- substr(selected_results$Description[i], 1, 35)
        membership[[col_name]] <- ifelse(membership$Gene %in% pathway_genes, "\u2713", "")
    }

    DT::datatable(membership, options = list(scrollX = TRUE, pageLength = 25),
                  rownames = FALSE)
})

# browseKEGG button for selected pathway
output$browseKEGGLink <- renderUI({
    enrichKEGG <- enrichGoReactive()
    s <- input$kegg_checked_rows
    if (!is.null(enrichKEGG) && length(s) > 0) {
        kegg_enrich <- enrichKEGG$kegg_enrich
        pathway_id <- kegg_enrich@result$ID[s[1]]
        gene_str   <- kegg_enrich@result$geneID[s[1]]
        genes      <- unlist(strsplit(gene_str, "/"))
        org_code <- myValues$organismKegg   # e.g. "dme", "hsa", "mmu"
        map_id   <- sub("^[a-z]+", "map", pathway_id)   # dme04080 → map04080

        # ── ENTREZ → KO IDs  (e.g. "38742" → "K08513") ───────────────────
        ko_map   <- build_ko_map(genes, org_code)
        ko_genes <- ko_map[as.character(genes)]

        # Drop genes with no KO assignment or empty KO string
        valid    <- !is.na(ko_genes) & nzchar(ko_genes)
        ko_genes <- ko_genes[valid]
        genes_v  <- genes[valid]
        if (length(genes_v) == 0) return(NULL)   # no KO mappings → hide button

        # ── Log2FC → colour (green–lightgrey–red) ─────────────────────────
        lfc <- myValues$kegg_gene_list[genes_v]
        lfc[is.na(lfc)] <- 0
        max_lfc <- if (length(lfc) > 0) max(abs(lfc), na.rm = TRUE) else 1
        if (!is.finite(max_lfc) || max_lfc == 0) max_lfc <- 1
        col_ramp <- colorRamp(c("#1a9641", "#d3d3d3", "#d7191c"))
        norm     <- pmax(0, pmin(1, (lfc / max_lfc + 1) / 2))
        rgb_mat  <- col_ramp(norm)
        hex_cols <- apply(rgb_mat, 1, function(r) {
            sprintf("%%23%02x%02x%02x", round(r[1]), round(r[2]), round(r[3]))
        })

        gene_parts <- paste0(ko_genes, "%09", hex_cols)
        url <- paste0(
            "https://www.kegg.jp/kegg-bin/show_pathway?", map_id, "/",
            paste(gene_parts, collapse = "/")
        )
        tags$a(
            href = url, target = "_blank",
            class = "btn btn-success",
            style = "margin-bottom: 10px; width: 100%;",
            icon("external-link-alt"),
            paste0(" Browse KEGG Pathway: ", pathway_id)
        )
    }
})

# ── Selected-term pills for enrichGO ─────────────────────────────────────────
output$go_selected_pills <- renderUI({
    enrichGo <- enrichGoReactive()
    s <- input$go_checked_rows

    if (!is.null(enrichGo) && length(s) > 0) {
        go_enrich <- enrichGo$go_enrich
        term_ids  <- go_enrich@result$ID[s]
        term_desc <- go_enrich@result$Description[s]

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

# ── Deselect a GO term via pill × button ──────────────────────────────────────
observeEvent(input$go_deselect_term, {
    tid      <- input$go_deselect_term
    enrichGo <- enrichGoReactive()
    req(!is.null(enrichGo), nchar(tid) > 0)

    all_ids    <- enrichGo$go_enrich@result$ID
    idx_1based <- which(all_ids == tid)
    if (length(idx_1based) > 0) {
        session$sendCustomMessage("go_uncheck_row",
                                  list(dataIdx = as.integer(idx_1based[1]) - 1L))
    }
}, ignoreInit = TRUE)



output$enrichGoTable <- renderDataTable(
    {
        enrichGo <- enrichGoReactive()

        if (!is.null(enrichGo)) {
            resultDF <- enrichGo$go_enrich@result
            if (!isTRUE(input$showGeneidGo)) {
                resultDF <- resultDF[, setdiff(names(resultDF), "geneID"), drop = FALSE]
            }

            # Prepend a checkbox column for row selection
            resultDF <- cbind(
                " " = '<input type="checkbox" class="go-row-cb" style="cursor:pointer;width:15px;height:15px;">',
                resultDF
            )

            DT::datatable(
                resultDF,
                escape    = FALSE,
                selection = "none",          # checkboxes drive selection via JS
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
    }
)

output$downloadEnrichGoCSV <- downloadHandler(
    filename = function() {
        paste0("enrichgo", ".csv")
    },
    content = function(file) {
        write.csv(enrichGoReactive()$go_enrich@result, file, row.names = TRUE)
    }
)

output$enrichGoAvailable <-
    reactive({
        return(!is.null(enrichGoReactive()$go_enrich))
    })
outputOptions(output, "enrichGoAvailable", suspendWhenHidden = FALSE)


output$enrichKEGGTable <- renderDataTable(
    {
        enrichKEGG <- enrichGoReactive()

        if (!is.null(enrichKEGG)) {
            resultDF <- enrichKEGG$kegg_enrich@result
            org_code  <- myValues$organismKegg

            # Batch-convert all unique ENTREZ IDs → KO IDs (e.g. "38742" → "K08513")
            all_entrez <- unique(unlist(strsplit(paste(resultDF$geneID, collapse = "/"), "/")))
            gene_map   <- build_ko_map(all_entrez, org_code)

            # Make ID column clickable: map pathway + KO IDs + log2FC colour coding
            make_kegg_link <- function(pathway_id, gene_ids_str) {
                map_id   <- sub("^[a-z]+", "map", pathway_id)   # dme04080 → map04080
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
            resultDF$ID <- mapply(make_kegg_link, resultDF$ID, resultDF$geneID)

            # Convert geneID ENTREZ → symbols for display (links already built above)
            readable <- tryCatch(
                setReadable(enrichKEGG$kegg_enrich, OrgDb = input$organismDb, keyType = "ENTREZID")@result,
                error = function(e) NULL
            )
            if (!is.null(readable))
                resultDF$geneID <- readable[rownames(resultDF), "geneID"]

            if (!isTRUE(input$showGeneidKegg)) {
                resultDF <- resultDF[, setdiff(names(resultDF), "geneID"), drop = FALSE]
            }

            # Prepend a checkbox column for row selection
            resultDF <- cbind(
                " " = '<input type="checkbox" class="kegg-row-cb" style="cursor:pointer;width:15px;height:15px;">',
                resultDF
            )

            DT::datatable(
                resultDF,
                escape    = FALSE,
                selection = "none",          # checkboxes drive selection via JS
                options   = list(
                    scrollX     = TRUE,
                    columnDefs  = list(
                        list(orderable  = FALSE,
                             searchable = FALSE,
                             targets    = 0,
                             width      = "30px")
                    )
                )
            )
        }
    }
)

output$downloadEnrichKEGGCSV <- downloadHandler(
    filename = function() {
        paste0("enrichKEGG", ".csv")
    },
    content = function(file) {
        write.csv(enrichGoReactive()$kegg_enrich@result, file, row.names = TRUE)
    }
)

output$enrichKEGGAvailable <-
    reactive({
        return(!is.null(enrichGoReactive()$kegg_enrich))
    })
outputOptions(output, "enrichKEGGAvailable", suspendWhenHidden = FALSE)


output$warningText <- renderText({
    outputText <- myValues$convWarningMessage
    if (length(outputText) == 3) {
        outputText[3] <- paste0("<strong>", outputText[3], "</strong>")
    }

    paste("<p>", outputText, "</p>")
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

observeEvent(input$gotoWordcloud, {
    GotoTab("wordcloudTab")
})

observeEvent(input$gotoWordcloud1, {
    GotoTab("wordcloudTab")
})
