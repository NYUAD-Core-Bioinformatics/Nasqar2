fragment_size_plot <- reactive({
    req(input$sample_fragmentsize, my_values$base_dir)
    bamfile <- file.path(my_values$base_dir,my_values$samples_df[input$sample_fragmentsize, 'BamFile'])
    fragSizeDist(bamfile, input$sample_fragmentsize)
})

output$plot_fragmentsize <- renderPlot(fragment_size_plot())
register_publication_downloads(
    output, "download_fragment_size",
    function() paste0("atacseq-fragment-size-", input$sample_fragmentsize),
    fragment_size_plot, width = 8.5, height = 6
)
