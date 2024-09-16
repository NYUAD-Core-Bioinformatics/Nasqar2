output$mergerTable <- DT::renderDataTable(
    {
        mergers <- reactiveInputData()$mergers
        print("mergerTable")
        # print(mergers)
        sample <- input$selSample4margePairedReadsTab
        merger <- mergers[[sample]]
          # Server logic to download the track reads table
        output$download_merger_table <- downloadHandler(
          
          filename = function() {
            paste("abundance_estimation_table", Sys.Date(), ".csv", sep = "")
          },
          content = function(file) {
           
            # Assuming 'track' is your data frame containing the track reads summary
            write.csv(merger, file, row.names = FALSE)
          }
        )
        # merger %>% relocate(sequence, .after = accept)
        merger
        
    },
    options = list(scrollX = TRUE, pageLength = 10)
)






