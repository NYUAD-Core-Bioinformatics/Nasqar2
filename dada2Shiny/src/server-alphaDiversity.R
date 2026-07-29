qiimeData <- reactive({
    req(input$sampleData)
    file <- input$sampleData$datapath
    print(file)
    import_qiime_sample_data(file)
    # Read the uploaded file based on the extension
    # if (grepl(".csv$", input$sampleData$name)) {
    #   read.csv(file, stringsAsFactors = FALSE)
    # } else if (grepl(".tsv$", input$sampleData$name) || grepl(".txt$", input$sampleData$name)) {
    #   read.delim(file, stringsAsFactors = FALSE)
    # } else {
    #   stop("Unsupported file format")
    # }
})

metadataColumns <- reactive({
    metadata <- as.data.frame(qiimeData(), check.names = FALSE)
    names(metadata)
})

output$alphaDiversityMappings <- renderUI({
    req(input$sampleData)
    columns <- metadataColumns()
    req(length(columns) > 0L)
    taxonomy <- reactiveTaxonomyData$taxa
    taxonomy_ranks <- if (is.null(taxonomy)) character() else colnames(taxonomy)

    tagList(
        selectInput(
            "alpha_x",
            "Sample grouping (x-axis)",
            choices = columns,
            selected = columns[[1L]]
        ),
        selectInput(
            "alpha_color",
            "Sample color",
            choices = c("None" = "", columns),
            selected = ""
        ),
        selectInput(
            "alpha_facet",
            "Composition panels",
            choices = c("None" = "", columns),
            selected = ""
        ),
        selectInput(
            "alpha_tax_rank",
            "Taxonomy rank",
            choices = taxonomy_ranks,
            selected = if ("Family" %in% taxonomy_ranks) {
                "Family"
            } else {
                utils::tail(taxonomy_ranks, 1L)
            }
        )
    )
})



# Reactive expression to load the sample metadata



# Reactive values to store alpha diversity results
alphaDiversityResults <- reactiveValues(
    ps = NULL,
    ps.prop = NULL,
    ord.nmds.bray = NULL,
    ps.top20 = NULL
)

# Run Alpha Diversity analysis when the button is clicked
observeEvent(input$runAlphaDiversity, {

    withProgress(message = "Running AlphaDiversity , ..please wait", {
    print("runAlphaDiversity")
    divergen_done(FALSE)
    
    # Clear previous results
    alphaDiversityResults$ps <- NULL
    alphaDiversityResults$ps.prop <- NULL
    alphaDiversityResults$ord.nmds.bray <- NULL
    alphaDiversityResults$ps.top20 <- NULL
    
    req(qiimeData())
    req(input$alpha_x, input$alpha_tax_rank)

    seqtab.nochim <- reactiveInputData()$seqtab.nochim
    taxa <- reactiveTaxonomyData$taxa
    sample_metadata <- as.data.frame(qiimeData(), check.names = FALSE)
    sample_metadata <- tryCatch(
        align_sample_metadata(sample_metadata, rownames(seqtab.nochim)),
        error = function(error) {
            shiny::validate(need(FALSE, conditionMessage(error)))
        }
    )
    shiny::validate(
        need(nrow(seqtab.nochim) > 0L && ncol(seqtab.nochim) > 0L,
             "The DADA2 count table is empty."),
        need(all(rowSums(seqtab.nochim) > 0),
             "Every sample must contain at least one non-chimeric read."),
        need(nrow(taxa) > 0L,
             "Assign taxonomy before running diversity analysis."),
        need(
            input$alpha_tax_rank %in% colnames(taxa),
            "Select an available taxonomy rank."
        )
    )

    # print(qiimeData())

    print("tax_table(taxa)")
    # print(tax_table(taxa))

    print("otu_table(seqtab.nochim, taxa_are_rows=FALSE)")
    # print(otu_table(seqtab.nochim, taxa_are_rows=FALSE))

    # samples.out <- rownames(seqtab.nochim)
    #     print('samples.out')
    #     print(samples.out)
    #     subject <- sapply(strsplit(samples.out, "D"), '[', 1)
    #     gender <- substr(subject,1,1)
    #     subject <- substr(subject,2,999)
    #     # day <- as.integer(sapply(strsplit(samples.out, "D"), '[', 2))
    #     day <- as.integer(sapply(strsplit(sapply(strsplit(samples.out, "D"), '[', 2), '_'), '[', 1))
    #     samdf <- data.frame(Subject=subject, Gender=gender, Day=day)
    #     samdf$When <- "Early"
    #     samdf$When[samdf$Day>100] <- "Late"
    #     rownames(samdf) <- samples.out
    #     print('samdf')
    #     print(samdf)

    print("qiimeData()")

    # print(qiimeData())

    shiny::setProgress(value = 0.1, detail = "...creating phyloseq object")

    ps <- phyloseq(
        otu_table(seqtab.nochim, taxa_are_rows = FALSE),
        sample_data(sample_metadata),
        tax_table(taxa)
    )
    if ("Mock" %in% sample_names(ps)) {
        ps <- prune_samples(sample_names(ps) != "Mock", ps)
    }


    dna <- Biostrings::DNAStringSet(taxa_names(ps))
    names(dna) <- taxa_names(ps)
    ps <- merge_phyloseq(ps, dna)
    taxa_names(ps) <- paste0("ASV", seq(ntaxa(ps)))
    # print(ps)
    # Transform data to proportions as appropriate for Bray-Curtis distances
    ps.prop <- transform_sample_counts(ps, function(otu) otu / sum(otu))
    ord.nmds.bray <- ordinate(ps.prop, method = "NMDS", distance = "bray")


    top20 <- top_taxa_names(taxa_sums(ps), 20L)
    shiny::validate(need(
        length(top20) > 0L,
        "No taxa are available for the composition plot."
    ))
    ps.top20 <- transform_sample_counts(ps, function(OTU) OTU / sum(OTU))
    ps.top20 <- prune_taxa(top20, ps.top20)
    
    # Store results in reactive values
    alphaDiversityResults$ps <- ps
    alphaDiversityResults$ps.prop <- ps.prop
    alphaDiversityResults$ord.nmds.bray <- ord.nmds.bray
    alphaDiversityResults$ps.top20 <- ps.top20
    
    shiny::setProgress(value = 1.0, detail = "done")

    })

    # Example: Calculate alpha diversity indices and create a plot (assuming a phyloseq object is available)
    # Replace 'physeq_object' with your actual phyloseq object that includes the necessary data
    # alphaDiv <- estimate_richness(physeq_object, measures = c("Shannon", "Simpson"))

    # Plot alpha diversity (example)
    shinyjs::show(selector = "a[data-value=\"alphaDiversityTab\"]")
    js$addStatusIcon("alphaDiversityTab", "done")
    divergen_done(TRUE)
})

# Reactive plot outputs that depend on stored results
alpha_diversity_plot <- reactive({
    req(alphaDiversityResults$ps)
    req(divergen_done())
    color_column <- if (nzchar(input$alpha_color)) input$alpha_color else NULL
    plot_richness(
        alphaDiversityResults$ps,
        x = input$alpha_x,
        measures = c("Shannon", "Simpson"),
        color = color_column
    )
})

ordination_plot <- reactive({
    req(alphaDiversityResults$ps.prop)
    req(alphaDiversityResults$ord.nmds.bray)
    req(divergen_done())
    color_column <- if (nzchar(input$alpha_color)) input$alpha_color else NULL
    plot_ordination(
        alphaDiversityResults$ps.prop,
        alphaDiversityResults$ord.nmds.bray,
        color = color_column,
        title = "Bray-Curtis NMDS"
    )
})

composition_plot <- reactive({
    req(alphaDiversityResults$ps.top20)
    req(divergen_done())
    plot <- plot_bar(
        alphaDiversityResults$ps.top20,
        x = input$alpha_x,
        fill = input$alpha_tax_rank
    )
    if (nzchar(input$alpha_facet)) {
        plot <- plot + facet_wrap(
            stats::as.formula(paste("~", input$alpha_facet)),
            scales = "free_x"
        )
    }
    plot
})

output$plotAlphaDiversity <- renderPlot(alpha_diversity_plot())
output$plotOrdination <- renderPlot(ordination_plot())
output$plotBar <- renderPlot(composition_plot())

register_publication_downloads(
    output, "download_alpha_diversity",
    function() "dada2-alpha-diversity",
    alpha_diversity_plot, width = 8.5, height = 5.5
)
register_publication_downloads(
    output, "download_ordination",
    function() "dada2-bray-curtis-nmds",
    ordination_plot, width = 7.5, height = 6.5
)
register_publication_downloads(
    output, "download_composition",
    function() "dada2-taxonomic-composition",
    composition_plot, width = 10, height = 6.5
)


# output$plotAlphaDiversity <-  renderPlot({

#     ps<- reactiveInputData()$ps
#     print('plotAlphaDiversity')


#     plot_richness(ps, x="Day", measures=c("Shannon", "Simpson"), color="When")
# })


# output$plotOrdination<-  renderPlot({

#     ps.prop<- reactiveInputData()$ps.prop
#     ord.nmds.bray <- reactiveInputData()$ord.nmds.bray

#     plot_ordination(ps.prop, ord.nmds.bray, color="When", title="Bray NMDS")
# })


# output$plotBar<-  renderPlot({

#     ps.top20<- reactiveInputData()$ps.top20


#     plot_bar(ps.top20, x="Day", fill="Family") + facet_wrap(~When, scales="free_x")
# })
