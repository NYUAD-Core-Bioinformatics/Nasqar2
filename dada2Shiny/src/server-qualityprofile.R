quality_profile_forward <- reactive({
    req(input$sel_sample_qualityprofile_tab)
    fastq_file <- file.path(my_values$base_dir, my_values$samples_df[input$sel_sample_qualityprofile_tab, "FASTQ_Fs"])
    plotQualityProfile(fastq_file)
})

quality_profile_reverse <- reactive({
    req(input$sel_sample_qualityprofile_tab)
    fastq_file <- file.path(my_values$base_dir, my_values$samples_df[input$sel_sample_qualityprofile_tab, "FASTQ_Rs"])
    plotQualityProfile(fastq_file)
})

output$plot_qualityprofile_fs <- renderPlot(quality_profile_forward())
output$plot_qualityprofile_rs <- renderPlot(quality_profile_reverse())

register_publication_downloads(
    output, "download_quality_forward",
    function() paste0("dada2-forward-quality-", input$sel_sample_qualityprofile_tab),
    quality_profile_forward, width = 8.5, height = 5.5
)
register_publication_downloads(
    output, "download_quality_reverse",
    function() paste0("dada2-reverse-quality-", input$sel_sample_qualityprofile_tab),
    quality_profile_reverse, width = 8.5, height = 5.5
)
