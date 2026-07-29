library_complexity_plot <- reactive({
    req(input$sample_librarycomplexity, my_values$base_dir)
    bamfile <- file.path(my_values$base_dir, my_values$samples_df[input$sample_librarycomplexity, "BamFile"])
    estimateLibComplexity(readsDupFreq(bamfile))
})

output$plot_libcomplexity <- renderPlot(library_complexity_plot())
register_publication_downloads(
    output, "download_library_complexity",
    function() paste0("atacseq-library-complexity-", input$sample_librarycomplexity),
    library_complexity_plot, width = 8, height = 6
)
