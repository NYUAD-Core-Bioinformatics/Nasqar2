### GO ───────────────────────────────────────────────────────────────────────

# Info banner shown at the top of the GO Plots tab
output$goPlots_selectionBanner <- renderUI({
    go_enrich <- enrichGoReactive()$go_enrich
    req(!is.null(go_enrich))
    s <- input$go_checked_rows
    if (!is.null(s) && length(s) > 0) {
        descs <- go_enrich@result$Description[s]
        tags$div(
            class = "alert alert-info",
            style = "margin-bottom:10px;",
            icon("filter"),
            strong(paste0(" Plotting ", length(s), " selected term(s): ")),
            paste(descs, collapse = ", ")
        )
    } else {
        n_avail <- nrow(go_enrich@result)
        tags$div(
            class = "alert alert-warning",
            style = "margin-bottom:10px;",
            icon("info-circle"),
            strong(" No terms selected — "),
            paste0("showing top ", input$showCategory_go_global, " of ", n_avail,
                   " terms. Select rows in the enrichGO tab to customise.")
        )
    }
})

# Reactive: go_enrich filtered to checkbox-selected rows.
# Falls back to top N (global input) when nothing is checked.
goEnrichForPlots <- reactive({
    go_enrich <- enrichGoReactive()$go_enrich
    req(!is.null(go_enrich), nrow(go_enrich@result) > 0)

    s <- input$go_checked_rows
    idx <- if (!is.null(s) && length(s) > 0) {
        s                                                                  # user selection (1-based row indices)
    } else {
        seq_len(min(input$showCategory_go_global, nrow(go_enrich@result))) # global default
    }

    # Directly subset @result — avoids filter() dispatch ambiguity across versions
    ge <- go_enrich
    ge@result <- go_enrich@result[idx, , drop = FALSE]
    ge
})

# Keep the global GO category input in sync with available term count,
# and disable it when the user has manually selected terms
observe({
    s       <- input$go_checked_rows
    ge_full <- enrichGoReactive()$go_enrich
    n       <- if (!is.null(ge_full)) nrow(ge_full@result) else 1L

    if (!is.null(s) && length(s) > 0) {
        shinyjs::disable("showCategory_go_global")
    } else {
        shinyjs::enable("showCategory_go_global")
        cur_val <- isolate(input$showCategory_go_global)
        if (!is.null(cur_val) && cur_val > n) {
            updateNumericInput(session, "showCategory_go_global", max = n, value = n)
        } else {
            updateNumericInput(session, "showCategory_go_global", max = n)
        }
    }
})

# ── Interactive plotly UpSet plot for GO terms ────────────────────────────────
output$genesInGoTerm <- plotly::renderPlotly({
    enrichGO_data <- enrichGoReactive()
    req(!is.null(enrichGO_data))
    go_enrich <- enrichGO_data$go_enrich
    req(!is.null(go_enrich), nrow(go_enrich@result) > 0)

    s <- input$go_checked_rows
    if (is.null(s) || length(s) == 0) s <- seq_len(min(input$showCategory_go_global, nrow(go_enrich@result)))
    validate(need(length(s) >= 2, "Please select at least 2 GO terms to view the gene intersection plot."))

    go_enrich_readable <- tryCatch(
        setReadable(go_enrich, OrgDb = input$organismDb, keyType = "ENTREZID"),
        error = function(e) go_enrich
    )
    selected_results <- go_enrich_readable@result[s, , drop = FALSE]

    set_labels <- substr(selected_results$Description, 1, 35)
    gene_sets  <- setNames(
        lapply(selected_results$geneID, function(g) unlist(strsplit(g, "/"))),
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

    myValues$go_sorted_genes <- sorted_genes
    myValues$go_inter_labels <- inter_labels

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
        publication_plotly_config("ora-go-gene-membership") %>%
        htmlwidgets::onRender("function(el) { setTimeout(function() { Plotly.Plots.resize(el); }, 300); }")
})

# subplot() resets source to "A" — listen on "A", guard with go_sorted_genes
observeEvent(plotly::event_data("plotly_click", source = "A"), {
    click    <- plotly::event_data("plotly_click", source = "A")
    s_genes  <- myValues$go_sorted_genes
    s_labels <- myValues$go_inter_labels
    req(!is.null(click), !is.null(s_genes))
    req(isTRUE(click$curveNumber == 0))

    bar_idx <- round(click$x)
    req(bar_idx >= 1, bar_idx <= length(s_genes))
    myValues$go_clicked_inter_genes <- sort(s_genes[[bar_idx]])
    myValues$go_clicked_inter_label <- s_labels[bar_idx]
})

observeEvent(input$go_checked_rows, {
    myValues$go_clicked_inter_genes <- NULL
    myValues$go_clicked_inter_label <- NULL
}, ignoreNULL = FALSE)

output$go_clicked_genes_inline <- renderUI({
    req(myValues$go_clicked_inter_genes)
    tagList(
        tags$hr(),
        h4(strong(paste0(myValues$go_clicked_inter_label, "  \u2014  ", length(myValues$go_clicked_inter_genes), " genes"))),
        DT::dataTableOutput("go_clicked_genes_table")
    )
})

output$go_clicked_genes_table <- DT::renderDataTable({
    req(myValues$go_clicked_inter_genes)
    DT::datatable(data.frame(Gene = myValues$go_clicked_inter_genes),
                  options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
})

output$genesGoMembershipTable <- renderDataTable({
    enrichGO_data <- enrichGoReactive()
    req(!is.null(enrichGO_data))
    go_enrich <- enrichGO_data$go_enrich
    s <- input$go_checked_rows
    if (is.null(s) || length(s) == 0) s <- seq_len(min(input$showCategory_go_global, nrow(go_enrich@result)))
    req(nrow(go_enrich@result) > 0)

    go_enrich_readable <- tryCatch(
        setReadable(go_enrich, OrgDb = input$organismDb, keyType = "ENTREZID"),
        error = function(e) go_enrich
    )
    selected_results <- go_enrich_readable@result[s, , drop = FALSE]
    all_genes <- unique(unlist(lapply(selected_results$geneID, function(g) unlist(strsplit(g, "/")))))

    membership <- data.frame(Gene = sort(all_genes), stringsAsFactors = FALSE)
    for (i in seq_len(nrow(selected_results))) {
        term_genes <- unlist(strsplit(selected_results$geneID[i], "/"))
        col_name   <- substr(selected_results$Description[i], 1, 35)
        membership[[col_name]] <- ifelse(membership$Gene %in% term_genes, "\u2713", "")
    }
    DT::datatable(membership, options = list(scrollX = TRUE, pageLength = 25), rownames = FALSE)
})

# ── GO plot reactives (shared between renderPlot and downloadHandlers) ─────────

plot_barPlot <- reactive({
    ge <- goEnrichForPlots(); n <- nrow(ge@result); req(n > 0)
    barplot(ge, showCategory = n, title = "GO Biological Pathways", font.size = 8)
})
plot_dotPlot <- reactive({
    ge <- goEnrichForPlots(); n <- nrow(ge@result); req(n > 0)
    dotplot(ge, showCategory = n, font.size = 8)
})
plot_enrichPlotMap <- reactive({
    ge <- goEnrichForPlots(); n <- nrow(ge@result); req(n > 0)
    validate(need(n >= 2, "Enrichment map requires at least 2 terms. Please select 2 or more GO terms."))
    tryCatch(emapplot(pairwise_termsim(ge), showCategory = n),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
})
plot_goInducedGraph <- reactive({
    ge <- goEnrichForPlots(); n <- nrow(ge@result); req(n > 0)
    goplot(ge, showCategory = n)
})
plot_cnetplot <- reactive({
    ge <- goEnrichForPlots(); n <- nrow(ge@result); req(n > 0)
    cnetplot(ge, showCategory = n, categorySize = "pvalue",
             color.params = list(foldChange = myValues$gene_list))
})
plot_treePlot <- reactive({
    ge <- goEnrichForPlots(); n <- nrow(ge@result); req(n > 0)
    validate(need(n >= 2, "Tree plot requires at least 2 terms. Please select 2 or more GO terms."))
    n_clust <- max(1L, min(input$nCluster_tree, n - 1L))
    tryCatch(
        treeplot(pairwise_termsim(ge), showCategory = n, cluster.params = list(n = n_clust)),
        error = function(e) {
            validate(need(FALSE, paste0(
                "Tree plot failed: ", conditionMessage(e), "\n",
                "This may be due to a tidytree version incompatibility. ",
                "Try updating the enrichplot and tidytree packages."
            )))
        }
    )
})
plot_heatPlot <- reactive({
    ge <- goEnrichForPlots(); n <- nrow(ge@result); req(n > 0)
    fc <- if (!is.null(myValues$gene_list) && length(myValues$gene_list) > 0)
        myValues$gene_list else NULL
    heatplot(ge, showCategory = n, foldChange = fc)
})

output$barPlot       <- renderPlot({ withProgress(message = "Plotting barplot ...",               plot_barPlot()) })
output$dotPlot       <- renderPlot({ withProgress(message = "Plotting dotplot ...",               plot_dotPlot()) })
output$enrichPlotMap <- renderPlot({ withProgress(message = "Plotting enrichment map ...",        plot_enrichPlotMap()) })
output$goInducedGraph <- renderPlot({ withProgress(message = "Plotting induced GO DAG ...",       plot_goInducedGraph()) })
output$cnetplot      <- renderPlot({ withProgress(message = "Plotting Gene-Concept Network ...",  plot_cnetplot()) })
output$treePlot      <- renderPlot({ withProgress(message = "Plotting tree plot ...",             plot_treePlot()) })
output$heatPlot      <- renderPlot({ withProgress(message = "Plotting heat plot ...",             plot_heatPlot()) })

make_downloader(output, "barPlot",        plot_barPlot,        width = 10, height = 7)
make_downloader(output, "dotPlot",        plot_dotPlot,        width = 10, height = 7)
make_downloader(output, "enrichPlotMap",  plot_enrichPlotMap,  width = 10, height = 8)
make_downloader(output, "goInducedGraph", plot_goInducedGraph, width = 10, height = 8)
make_downloader(output, "cnetplot",       plot_cnetplot,       width = 10, height = 8)
make_downloader(output, "treePlot",       plot_treePlot,       width = 14, height = 8)
make_downloader(output, "heatPlot",       plot_heatPlot,       width = 14, height = 6)

output$wordcloud <- renderWordcloud2({
    withProgress(message = "Plotting Wordcloud ...", {
        if (input$plotWordcloud) {
            isolate({
                if (input$wordcloudGoOrKegg == "enrichGo") {
                    go_enrich <- enrichGoReactive()$go_enrich
                    
                } else {
                    go_enrich <- enrichGoReactive()$kegg_enrich
                }

                wcdf <- read.table(text = go_enrich$GeneRatio, sep = "/")[1]
                wcdf$term <- go_enrich[, 2]
                # wc = wordcloud(words = wcdf$term, freq = wcdf$V1, scale=(c(2, .5)), colors=brewer.pal(8, "Dark2"), max.words = input$maxWords)

                wordsDF <- data.frame(word = go_enrich[, 2], freq = wcdf$V1)

                wordsDF <- wordsDF[order(-wordsDF$freq), ]

                wordsDF <- head(wordsDF, input$maxWords)

                wordcloud2(wordsDF, color = input$wordsColor, shape = input$wordShape, size = 0.1)
            })
        }
    })
})


### KEGG ─────────────────────────────────────────────────────────────────────

# Info banner shown at the top of the KEGG Plots tab
output$keggPlots_selectionBanner <- renderUI({
    kegg_enrich <- enrichGoReactive()$kegg_enrich
    req(!is.null(kegg_enrich))
    s <- input$kegg_checked_rows
    if (!is.null(s) && length(s) > 0) {
        ids <- kegg_enrich@result$ID[s]
        tags$div(
            class = "alert alert-info",
            style = "margin-bottom:10px;",
            icon("filter"),
            strong(paste0(" Plotting ", length(ids), " selected pathway(s): ")),
            paste(ids, collapse = ", ")
        )
    } else {
        n_avail <- nrow(kegg_enrich@result)
        tags$div(
            class = "alert alert-warning",
            style = "margin-bottom:10px;",
            icon("info-circle"),
            strong(" No pathways selected — "),
            paste0("showing top ", input$showCategory_kegg_global, " of ", n_avail,
                   " pathways. Select rows in the enrichKEGG tab to customise.")
        )
    }
})

# Reactive: kegg_enrich filtered to checkbox-selected rows.
# Falls back to top N (global input) when nothing is checked.
keggEnrichForPlots <- reactive({
    kegg_enrich <- enrichGoReactive()$kegg_enrich
    req(!is.null(kegg_enrich), nrow(kegg_enrich@result) > 0)

    s            <- input$kegg_checked_rows
    user_selected <- !is.null(s) && length(s) > 0

    idx <- if (user_selected) {
        s                                                                        # explicit row selection
    } else {
        seq_len(min(input$showCategory_kegg_global, nrow(kegg_enrich@result)))  # top-N fallback
    }

    print(idx)
    ke <- kegg_enrich
    ke@result <- kegg_enrich@result[idx, , drop = FALSE]

    if (user_selected) {
        # p-value is already enforced by enrichKEGG on @result (all table rows passed it).
        # Only bypass q-value so plot functions don't silently drop user-selected pathways.
        ke@qvalueCutoff <- 1
    }
    # No manual selection: keep original cutoffs — q-value filter applies in plot functions
    ke
})

# Reset to default 5 whenever fresh KEGG results arrive
observeEvent(enrichGoReactive()$kegg_enrich, {
    ke_full <- enrichGoReactive()$kegg_enrich
    req(!is.null(ke_full))
    n <- nrow(ke_full@result)
    updateNumericInput(session, "showCategory_kegg_global", max = n, value = min(5L, n))
}, ignoreNULL = TRUE)

# Keep the global category input in sync with available pathway count,
# and disable it when the user has manually selected pathways
observe({
    s        <- input$kegg_checked_rows
    ke_full  <- enrichGoReactive()$kegg_enrich
    n        <- if (!is.null(ke_full)) nrow(ke_full@result) else 1L

    if (!is.null(s) && length(s) > 0) {
        shinyjs::disable("showCategory_kegg_global")
    } else {
        shinyjs::enable("showCategory_kegg_global")
        cur_val <- isolate(input$showCategory_kegg_global)
        default_val <- min(5L, n)
        if (is.null(cur_val) || cur_val < 1) {
            updateNumericInput(session, "showCategory_kegg_global", max = n, value = default_val)
        } else if (cur_val > n) {
            updateNumericInput(session, "showCategory_kegg_global", max = n, value = n)
        } else {
            updateNumericInput(session, "showCategory_kegg_global", max = n)
        }
    }
})

# ── KEGG plot reactives ────────────────────────────────────────────────────────

plot_barPlot_kegg <- reactive({
    ke <- keggEnrichForPlots(); n <- nrow(ke@result); req(n > 0)
    barplot(ke, showCategory = n, title = "Enriched Pathways", font.size = 8)
})
plot_dotPlot_kegg <- reactive({
    ke <- keggEnrichForPlots(); n <- nrow(ke@result); req(n > 0)
    dotplot(ke, showCategory = n, title = "Enriched Pathways", font.size = 8)
})
plot_enrichPlotMap_kegg <- reactive({
    ke <- keggEnrichForPlots(); n <- nrow(ke@result); req(n > 0)
    validate(need(n >= 2, "Enrichment map requires at least 2 pathways. Please select 2 or more KEGG pathways."))
    tryCatch(emapplot(pairwise_termsim(ke), showCategory = n),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
})
plot_cnetplot_kegg <- reactive({
    ke <- keggEnrichForPlots(); n <- nrow(ke@result); req(n > 0)
    ke_readable <- tryCatch(
        setReadable(ke, OrgDb = input$organismDb, keyType = "ENTREZID"),
        error = function(e) ke
    )
    cnetplot(ke_readable, showCategory = n, categorySize = "pvalue",
             color.params = list(foldChange = myValues$kegg_gene_list))
})
plot_treePlot_kegg <- reactive({
    ke <- keggEnrichForPlots(); n <- nrow(ke@result); req(n > 0)
    validate(need(n >= 2, "Tree plot requires at least 2 pathways. Please select 2 or more KEGG pathways."))
    ke_readable <- tryCatch(
        setReadable(ke, OrgDb = input$organismDb, keyType = "ENTREZID"),
        error = function(e) ke
    )
    n_clust <- max(1L, min(input$nCluster_tree_kegg, n - 1L))
    tryCatch(
        treeplot(pairwise_termsim(ke_readable), showCategory = n, cluster.params = list(n = n_clust)),
        error = function(e) {
            validate(need(FALSE, paste0(
                "Tree plot failed: ", conditionMessage(e), "\n",
                "This may be due to a tidytree version incompatibility. ",
                "Try updating the enrichplot and tidytree packages."
            )))
        }
    )
})
plot_heatPlot_kegg <- reactive({
    ke <- keggEnrichForPlots(); n <- nrow(ke@result); req(n > 0)
    ke_readable <- tryCatch(
        setReadable(ke, OrgDb = input$organismDb, keyType = "ENTREZID"),
        error = function(e) ke
    )
    foldChange <- NULL
    if (!is.null(myValues$kegg_gene_list) && length(myValues$kegg_gene_list) > 0) {
        fc      <- myValues$kegg_gene_list
        sym_map <- tryCatch(
            bitr(names(fc), fromType = "ENTREZID", toType = "SYMBOL", OrgDb = input$organismDb),
            error = function(e) NULL
        )
        if (!is.null(sym_map) && nrow(sym_map) > 0) {
            matched    <- sym_map$SYMBOL[match(names(fc), sym_map$ENTREZID)]
            names(fc)  <- ifelse(is.na(matched), names(fc), matched)
        }
        foldChange <- fc
    }
    heatplot(ke_readable, showCategory = n, foldChange = foldChange)
})

output$barPlot_kegg       <- renderPlot({ withProgress(message = "Plotting barplot ...",               plot_barPlot_kegg()) })
output$dotPlot_kegg       <- renderPlot({ withProgress(message = "Plotting dotplot ...",               plot_dotPlot_kegg()) })
output$enrichPlotMap_kegg <- renderPlot({ withProgress(message = "Plotting Enrichment Map ...",        plot_enrichPlotMap_kegg()) })
output$cnetplot_kegg      <- renderPlot({ withProgress(message = "Plotting Gene-Concept Network ...",  plot_cnetplot_kegg()) })
output$treePlot_kegg      <- renderPlot({ withProgress(message = "Plotting tree plot ...",             plot_treePlot_kegg()) })
output$heatPlot_kegg      <- renderPlot({ withProgress(message = "Plotting heat plot ...",             plot_heatPlot_kegg()) })

make_downloader(output, "barPlot_kegg",       plot_barPlot_kegg,       width = 10, height = 7)
make_downloader(output, "dotPlot_kegg",       plot_dotPlot_kegg,       width = 10, height = 7)
make_downloader(output, "enrichPlotMap_kegg", plot_enrichPlotMap_kegg, width = 10, height = 8)
make_downloader(output, "cnetplot_kegg",      plot_cnetplot_kegg,      width = 10, height = 8)
make_downloader(output, "treePlot_kegg",      plot_treePlot_kegg,      width = 14, height = 8)
make_downloader(output, "heatPlot_kegg",      plot_heatPlot_kegg,      width = 14, height = 6)

# ── KEGG pathway color URL ────────────────────────────────────────────────────

# Green-white-red palette: green (down) → lightgrey → red (up)
fc_to_hex <- function(fc_values, max_abs_fc = NULL) {
    if (length(fc_values) == 0) return(character(0))
    if (is.null(max_abs_fc)) max_abs_fc <- max(abs(fc_values), na.rm = TRUE)
    if (!is.finite(max_abs_fc) || max_abs_fc == 0) return(rep("#d3d3d3", length(fc_values)))
    pal <- grDevices::colorRampPalette(c("#1a9641", "#d3d3d3", "#d7191c"))
    colors <- pal(101)   # index 1 = full green, 51 = white, 101 = full red
    sapply(fc_values, function(fc) {
        idx <- round(50 + 50 * max(min(fc / max_abs_fc, 1), -1))
        colors[idx + 1]
    })
}


# Reactive: build the organism-specific KEGG coloring URL.
# Organism gene IDs avoid a second REST request for KO conversion.
keggColorUrl <- reactive({
    req(input$pathwayIds, myValues$kegg_gene_list)
    kegg_enrich <- enrichGoReactive()$kegg_enrich
    req(!is.null(kegg_enrich))

    pathway_row <- kegg_enrich@result[kegg_enrich@result$ID == input$pathwayIds, , drop = FALSE]
    req(nrow(pathway_row) > 0, nzchar(pathway_row$geneID[1]))

    gene_ids <- unlist(strsplit(pathway_row$geneID[1], "/"))
    gene_fcs <- myValues$kegg_gene_list[gene_ids]
    gene_fcs <- na.omit(gene_fcs)
    req(length(gene_fcs) > 0)

    max_abs_fc <- max(abs(myValues$kegg_gene_list), na.rm = TRUE)
    bg_colors  <- fc_to_hex(gene_fcs, max_abs_fc)

    entrez_ids <- names(gene_fcs)
    org        <- myValues$organismKegg
    kegg_ids   <- paste0(org, ":", entrez_ids)
    parts      <- paste0(kegg_ids, "%09%23", substring(bg_colors, 2))
    paste0("https://www.kegg.jp/kegg-bin/show_pathway?",
           input$pathwayIds, "/",
           paste(parts, collapse = "/"))
})

output$keggColorUrl <- renderUI({
    url <- tryCatch(keggColorUrl(), error = function(e) NULL)
    req(!is.null(url))
    tagList(
        tags$a(
            href   = url,
            target = "_blank",
            class  = "btn btn-success",
            style  = "margin-top:6px;",
            icon("external-link"), " View on KEGG (colored by fold change)"
        ),
        tags$p(
            class = "text-muted",
            style = "font-size:11px; margin-top:4px;",
            "Genes colored: red = up-regulated, green = down-regulated. ",
            "Color intensity scales with log2 fold change magnitude."
        )
    )
})

# ── Pathway gene table ────────────────────────────────────────────────────────
# Reactive: build gene table for selected pathway (ENTREZ → Symbol + LFC)
pathwayGenesDf <- reactive({
    req(input$pathwayIds)
    kegg_enrich <- enrichGoReactive()$kegg_enrich
    req(!is.null(kegg_enrich))

    pathway_row <- kegg_enrich@result[kegg_enrich@result$ID == input$pathwayIds, , drop = FALSE]
    req(nrow(pathway_row) > 0, nzchar(pathway_row$geneID[1]))

    entrez_ids <- unlist(strsplit(pathway_row$geneID[1], "/"))
    lfc_vals   <- myValues$kegg_gene_list[entrez_ids]
    org        <- myValues$organismKegg
    keytype    <- input$keytype   # original input ID type, e.g. "ENSEMBL"

    # ENTREZ → gene symbol
    # as.data.frame() forces collection of any lazy-table returned by bitr
    # so that nrow() / $ access below don't throw "Failed to collect lazy table"
    sym_map <- tryCatch(
        as.data.frame(bitr(entrez_ids, fromType = "ENTREZID", toType = "SYMBOL",
             OrgDb = input$organismDb)),
        error = function(e) NULL
    )
    symbols <- if (!is.null(sym_map) && nrow(sym_map) > 0)
        sym_map$SYMBOL[match(entrez_ids, sym_map$ENTREZID)]
    else
        rep(NA_character_, length(entrez_ids))

    # ENTREZ → original input ID (only when keytype differs from what's already shown)
    orig_ids <- NULL
    if (!is.null(keytype) && !keytype %in% c("ENTREZID", "SYMBOL")) {
        orig_map <- tryCatch(
            as.data.frame(bitr(entrez_ids, fromType = "ENTREZID", toType = keytype,
                 OrgDb = input$organismDb)),
            error = function(e) NULL
        )
        if (!is.null(orig_map) && nrow(orig_map) > 0)
            orig_ids <- orig_map[[keytype]][match(entrez_ids, orig_map$ENTREZID)]
    }

    # Build data frame — insert original ID column right after Symbol when present
    df <- data.frame(Symbol = symbols, stringsAsFactors = FALSE)
    if (!is.null(orig_ids))
        df[[keytype]] <- orig_ids
    df$KEGG_ID <- paste0(org, ":", entrez_ids)
    df$ENTREZ <- entrez_ids
    df$Log2FC <- round(as.numeric(lfc_vals), 3)

    df[order(abs(df$Log2FC), decreasing = TRUE, na.last = TRUE), ]
})

output$pathwayGenesTableUI <- renderUI({
    req(input$pathwayIds)
    # Progress lives here (render context), not inside the reactive.
    # withProgress interacts with the Shiny session via the render domain;
    # putting it inside reactive() confuses tryCatch error handling.
    df <- withProgress(
        message = paste0("Loading genes \u2014 ", input$pathwayIds),
        value   = 0,
    {
        setProgress(0.3, detail = if (kegg_is_cached(myValues$organismKegg))
            "Loading KEGG data from cache \u2026"
        else
            "Downloading KEGG ortholog table (first use \u2014 will be cached) \u2026")
        result <- tryCatch(pathwayGenesDf(), error = function(e) NULL)
        setProgress(1.0)
        result
    })
    req(!is.null(df))
    tagList(
        h4(strong(paste0("Genes in pathway: ", input$pathwayIds,
                         " (", nrow(df), " enriched)"))),
        DT::dataTableOutput("pathwayGenesTable")
    )
})

output$pathwayGenesTable <- DT::renderDataTable({
    df       <- pathwayGenesDf()
    fname    <- paste0("pathway_genes_", input$pathwayIds)
    DT::datatable(
        df, rownames = FALSE,
        extensions = "Buttons",
        options = list(
            pageLength = 15,
            scrollX    = TRUE,
            dom        = "Bfrtip",
            buttons    = list(
                list(extend = "copy",  text = "Copy"),
                list(extend = "csv",   text = "CSV",   filename = fname),
                list(extend = "excel", text = "Excel", filename = fname)
            )
        ),
        class = "compact stripe"
    ) %>%
    DT::formatStyle(
        "Log2FC",
        color      = DT::styleInterval(0, c("#1a9641", "#d7191c")),
        fontWeight = "bold"
    )
})


pathviewReactive <- eventReactive(input$generatePathview, {
    withProgress(message = "Generating Pathview ...", {
        isolate({
            req(myValues$kegg_gene_list)

            tryCatch({
                pathview_dir <- file.path(tempdir(check = TRUE), "pathview")
                dir.create(pathview_dir, recursive = TRUE, showWarnings = FALSE)

                setProgress(0.3, detail = paste0("Downloading pathway map — ", input$pathwayIds, " \u2026"))
                dme <- pathview(
                    gene.data   = myValues$kegg_gene_list,
                    pathway.id  = input$pathwayIds,
                    species     = myValues$organismKegg,
                    gene.idtype = "ENTREZ",
                    kegg.dir    = pathview_dir
                )
                # Copy to a unique name so the UI preview refreshes each run
                native_img <- file.path(pathview_dir, paste0(input$pathwayIds, ".pathview.png"))
                unique_img <- file.path(pathview_dir, paste0("testimage_", input$pathwayIds, ".png"))
                file.copy(native_img, unique_img, overwrite = TRUE)

                setProgress(0.7, detail = paste0("Generating PDF \u2014 ", input$pathwayIds, " \u2026"))
                dmePdf <- pathview(
                    gene.data   = myValues$kegg_gene_list,
                    pathway.id  = input$pathwayIds,
                    species     = myValues$organismKegg,
                    gene.idtype = "ENTREZ",
                    kegg.native = FALSE,
                    kegg.dir    = pathview_dir
                )

                myValues$imagePath <- file.path(
                    pathview_dir,
                    paste0(input$pathwayIds, ".pathview.")
                )
                list(
                    src = normalizePath(unique_img, mustWork = TRUE),
                    filetype = "image/png",
                    alt = paste("Pathview overlay for", input$pathwayIds)
                )

            }, error = function(e) {
                msg <- conditionMessage(e)
                is_network <- grepl(
                    "curl|connection|reset|peer|timeout|could not resolve|network|recv failure",
                    msg, ignore.case = TRUE
                )
                if (is_network) {
                    showNotification(
                        ui       = tagList(
                            icon("exclamation-triangle"), strong(" KEGG network error"),
                            tags$br(),
                            "Connection to rest.kegg.jp was interrupted.",
                            tags$br(),
                            "Please click \u201cGenerate Pathview\u201d again to retry."
                        ),
                        type     = "error",
                        duration = 15
                    )
                } else {
                    showNotification(
                        ui       = tagList(icon("times-circle"), strong(" Pathview error: "), msg),
                        type     = "error",
                        duration = 15
                    )
                }
                NULL
            })
        })
    })
})

output$pathview_plot <- renderImage({
    img <- pathviewReactive()
    req(!is.null(img))
    img
}, deleteFile = FALSE)

output$pathviewPlotsAvailable <-
    reactive({
        return(!is.null(pathviewReactive()))
    })
outputOptions(output, "pathviewPlotsAvailable", suspendWhenHidden = FALSE)

output$downloadPathviewPng <- downloadHandler(
    filename = function() {
        basename(paste0(myValues$imagePath, "png"))
    },
    content = function(file) {
        file.copy(paste0(myValues$imagePath, "png"), file)
    }
)

output$downloadPathviewPdf <- downloadHandler(
    filename = function() {
        basename(paste0(myValues$imagePath, "pdf"))
    },
    content = function(file) {
        file.copy(paste0(myValues$imagePath, "pdf"), file)
    }
)

output$wordcloud_kegg <- renderWordcloud2({
    withProgress(message = "Plotting Wordcloud ...", {
        kegg_enrich <- enrichGoReactive()$kegg_enrich


        wcdf <- read.table(text = kegg_enrich$GeneRatio, sep = "/")[1]
        wcdf$term <- kegg_enrich[, 2]
        # wc = wordcloud(words = wcdf$term, freq = wcdf$V1, scale=(c(2, .5)), colors=brewer.pal(8, "Dark2"), max.words = input$maxWords)

        wordsDF <- data.frame(word = kegg_enrich[, 2], freq = wcdf$V1)

        wordsDF <- wordsDF[order(-wordsDF$freq), ]

        wordsDF <- head(wordsDF, input$maxWords_kegg)
        # browser()
        wordcloud2(wordsDF, color = input$wordsColor_kegg, shape = input$wordShape_kegg, size = 0.1)
    })
})
