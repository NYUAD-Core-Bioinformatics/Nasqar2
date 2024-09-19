# Define reactive values to hold the selected databases
reactiveTaxonomyData <- reactiveValues()

observeEvent(input$assignTaxonomy, {

    # Show loading icon while assigning taxonomy
    js$addStatusIcon("taxanomyTab", "loading")
    seqtab.nochim <- reactiveInputData()$seqtab.nochim

    # Assign default database paths
    reference_db <- if(input$database_choice == "silva") {
        "./www/taxonomy/silva_nr99_v138.1_train_set.fa.gz"
    } else if(input$database_choice == "custom") {
        input$custom_silva_nr99$datapath  # Path to the uploaded custom file
    }

    species_db <- if(input$species_assignment_choice == "silva_species") {
        "./www/taxonomy/silva_species_assignment_v138.1.fa.gz"
    } else if(input$species_assignment_choice == "custom_species") {
        input$custom_silva_species$datapath  # Path to the uploaded custom species assignment file
    } else {
        NULL  # No species assignment
    }

    # Assign taxonomy using the selected reference database
    withProgress(message = "Assigning Taxonomy, please wait...", {
        shiny::setProgress(value = 0.1, detail = "...assigning taxonomy")
        taxa <- assignTaxonomy(seqtab.nochim, reference_db, multithread = TRUE)
        
        # Assign species if a species-level database is selected
        if (!is.null(species_db)) {
            shiny::setProgress(value = 0.7, detail = "...assigning species")
            taxa <- addSpecies(taxa, species_db)
        }
        
        shiny::setProgress(value = 1.0, detail = "...done")
    })

    # Show the taxonomy and species assignment results
    reactiveTaxonomyData$taxa <- taxa

    # Update icons and tabs
    shinyjs::show(selector = "a[data-value=\"alphaDiversityTab\"]")
    shinyjs::show(selector = "a[data-value=\"taxanomyTab\"]")
    js$addStatusIcon("taxanomyTab", "done")
    js$addStatusIcon("trackReadsTab", "done")
    js$addStatusIcon("alphaDiversityTab", "next")
    
    # return(taxa)
})

# Render the taxonomy table and download handler
output$taxonomyTable <- DT::renderDataTable({
    taxonomy <- reactiveTaxonomyData$taxa
    output_table_wth_sequence <- cbind(Sequence = rownames(taxonomy), taxonomy)

    output$download_taxonomy_table <- downloadHandler(
        filename = function() {
            paste("taxonomy_table", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(output_table_wth_sequence, file, row.names = FALSE)
        }
    )

    output_table_wth_sequence
}, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))


output$taxonomy_ready <- reactive({
  !is.null(reactiveTaxonomyData$taxa)
})
outputOptions(output, "taxonomy_ready", suspendWhenHidden = FALSE)