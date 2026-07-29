error_plot_forward <- reactive({
    errF <- reactiveInputData()$errF
    plotErrors(errF, nominalQ = TRUE)
})

error_plot_reverse <- reactive({
    errR <- reactiveInputData()$errR
    plotErrors(errR, nominalQ = TRUE)
})

output$plotErrors_errF <- renderPlot(error_plot_forward())
output$plotErrors_errR <- renderPlot(error_plot_reverse())

register_publication_downloads(
    output, "download_error_forward",
    function() "dada2-forward-error-model",
    error_plot_forward, width = 8.5, height = 6.5
)
register_publication_downloads(
    output, "download_error_reverse",
    function() "dada2-reverse-error-model",
    error_plot_reverse, width = 8.5, height = 6.5
)
