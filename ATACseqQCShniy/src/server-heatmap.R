
output$multiple_bamfiles <- reactive({
 
    req(my_values$base_dir)

    path <- my_values$base_dir
    bamfiles <- dir(path, "*.bam$", full.name=TRUE)

    if(length(bamfiles) > 1){
        return(TRUE)
    } else {
         return(FALSE)
    }

    
})
outputOptions(output, "multiple_bamfiles", suspendWhenHidden = FALSE)

replicate_heatmap_plot <- reactive({
    req(
        my_values$base_dir,
        input$sel_chromosome_heatmap_tab,
        input$bs_genome_input
    )
    chromosome <- trimws(input$sel_chromosome_heatmap_tab)
    validate(need(nzchar(chromosome), "Select a chromosome to generate the heatmap."))

    # path <- system.file("extdata", package="ATACseqQC", mustWork=TRUE)
    path <- my_values$base_dir
    bamfiles <- dir(path, "*.bam$", full.name=TRUE)
    req(length(bamfiles) > 1)
    available_chromosomes <- names(scanBamHeader(bamfiles[[1]])[[1]]$targets)
    validate(need(
        chromosome %in% available_chromosomes,
        "The selected chromosome is not present in the BAM files."
    ))
    
    gals <- lapply(bamfiles, function(bamfile){
                param <- ScanBamParam(
                    tag = character(0),
                    which = GRanges(chromosome, IRanges(1, 1e6))
                )
                # bfile <- readBamFile(bamFile=bamfile, tag=character(0), 
                #             which=GRanges(input$sel_chromosome_heatmap_tab, IRanges(1, 1e6)), 
                #             asMates=TRUE, bigFile=TRUE)

                readGAlignments(bamfile, use.names=TRUE, param=param)
            })

    #library(input$bs_genome_input, character.only = T)
    check_and_load_bioc_package(input$bs_genome_input)
    # library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    tx_db <- tx_db_list[[input$bs_genome_input]]
    check_and_load_bioc_package(tx_db)
    txs <- transcripts(get(tx_db))
    plotCorrelation(GAlignmentsList(gals), txs, seqlev = chromosome)
})

output$plot_heatmap <- renderPlot(replicate_heatmap_plot())
register_publication_downloads(
    output, "download_replicate_heatmap",
    function() paste0("atacseq-replicate-correlation-", input$sel_chromosome_heatmap_tab),
    replicate_heatmap_plot, width = 8, height = 7
)
