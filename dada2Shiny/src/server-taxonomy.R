

output$taxonomyTable <- DT::renderDataTable(
    {
        taxonomy <- reactiveTaxonomyData$taxa
        

        output_table_wth_sequence <- cbind(Sequence = rownames(taxonomy), taxonomy)

        output$download_taxonomy_table <- downloadHandler(
          
          filename = function() {
            paste("taxonomy_table", Sys.Date(), ".csv", sep = "")
          },
          content = function(file) {

            
        
            
            output_table_wth_sequence
           
            # Assuming 'track' is your data frame containing the track reads summary
            write.csv(output_table_wth_sequence, file, row.names = FALSE)
          }
        )


        output_table_wth_sequence
    },
    rownames = FALSE, 
    options = list(scrollX = TRUE, pageLength = 10)
)

reactiveTaxonomyData <- reactiveValues()

observeEvent(input$assignTaxonomy, {

    js$addStatusIcon("trackReadsTab", "loading")
    seqtab.nochim <- reactiveInputData()$seqtab.nochim

    

    withProgress(message = "Assigning Taxonomy , please wait", {
        shiny::setProgress(value = 0.1, detail = "...assignTaxonomy")
        taxa <- assignTaxonomy(seqtab.nochim, "./www/taxonomy/silva_nr99_v138.1_train_set.fa.gz", multithread = TRUE)
        shiny::setProgress(value = 0.7, detail = "...addSpecies")
        taxa <- addSpecies(taxa, "./www/taxonomy/silva_species_assignment_v138.1.fa.gz")
        shiny::setProgress(value = 1.0, detail = "...done")
    })






    # shinyjs::show(selector = "a[data-value=\"alphaDiversityTab\"]")

     
      shinyjs::show(selector = "a[data-value=\"alphaDiversityTab\"]")
      shinyjs::show(selector = "a[data-value=\"taxanomyTab\"]")
        js$addStatusIcon("taxanomyTab", "done")
        js$addStatusIcon("trackReadsTab", "done")
        js$addStatusIcon("alphaDiversityTab", "next")

    reactiveTaxonomyData$taxa <- taxa
    return(taxa)
})





