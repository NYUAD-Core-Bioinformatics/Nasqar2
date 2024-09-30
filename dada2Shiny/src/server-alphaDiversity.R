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



# Reactive expression to load the sample metadata



# Run Alpha Diversity analysis when the button is clicked
observeEvent(input$runAlphaDiversity, {

    withProgress(message = "Running AlphaDiversity , ..please wait", {
    print("runAlphaDiversity")
    divergen_done(FALSE)
    req(qiimeData())

    seqtab.nochim <- reactiveInputData()$seqtab.nochim
    taxa <- reactiveTaxonomyData$taxa

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
        sample_data(qiimeData()),
        tax_table(taxa)
    )
    ps <- prune_samples(sample_names(ps) != "Mock", ps) # Remove mock sample


    dna <- Biostrings::DNAStringSet(taxa_names(ps))
    names(dna) <- taxa_names(ps)
    ps <- merge_phyloseq(ps, dna)
    taxa_names(ps) <- paste0("ASV", seq(ntaxa(ps)))
    # print(ps)
    # Transform data to proportions as appropriate for Bray-Curtis distances
    ps.prop <- transform_sample_counts(ps, function(otu) otu / sum(otu))
    ord.nmds.bray <- ordinate(ps.prop, method = "NMDS", distance = "bray")


    top20 <- names(sort(taxa_sums(ps), decreasing = TRUE))[1:20]
    ps.top20 <- transform_sample_counts(ps, function(OTU) OTU / sum(OTU))
    ps.top20 <- prune_taxa(top20, ps.top20)
    shiny::setProgress(value = 1,0, detail = "done")

    })

    # Example: Calculate alpha diversity indices and create a plot (assuming a phyloseq object is available)
    # Replace 'physeq_object' with your actual phyloseq object that includes the necessary data
    # alphaDiv <- estimate_richness(physeq_object, measures = c("Shannon", "Simpson"))

    # Plot alpha diversity (example)
    shinyjs::show(selector = "a[data-value=\"alphaDiversityTab\"]")
    js$addStatusIcon("alphaDiversityTab", "done")
    divergen_done(TRUE)

    output$plotAlphaDiversity <- renderPlot({
        plot_richness(ps, x = "Day", measures = c("Shannon", "Simpson"), color = "When")
    })

    # Placeholder for ordination and bar plot (replace with actual analysis)
    output$plotOrdination <- renderPlot({
        ps.prop <- ps.prop
        # ord.nmds.bray <- ord.nmds.bray

        plot_ordination(ps.prop, ord.nmds.bray, color = "When", title = "Bray NMDS")
    })

    output$plotBar <- renderPlot({
        # ps.top20<- ps.top20
        plot_bar(ps.top20, x = "Day", fill = "Family") + facet_wrap(~When, scales = "free_x")
    })
})




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
