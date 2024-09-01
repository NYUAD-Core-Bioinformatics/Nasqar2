reactiveTaxonomyData <- eventReactive(input$assignTaxonomy, {
    seqtab.nochim <- reactiveInputData()$seqtab.nochim

    withProgress(message = "Assigning Taxonomy , please wait", {
        shiny::setProgress(value = 0.1, detail = "...assignTaxonomy")
        taxa <- assignTaxonomy(seqtab.nochim, "./www/taxonomy/silva_nr99_v138.1_train_set.fa.gz", multithread = TRUE)
        shiny::setProgress(value = 0.7, detail = "...addSpecies")
        taxa <- addSpecies(taxa, "./www/taxonomy/silva_species_assignment_v138.1.fa.gz")
        shiny::setProgress(value = 1.0, detail = "...done")
    })






    shinyjs::show(selector = "a[data-value=\"alphaDiversityTab\"]")

    js$addStatusIcon("taxanomyTab", "done")

    return(taxa)
})

output$taxonomyTable <- DT::renderDataTable(
    {
        taxonomy <- reactiveTaxonomyData()
        taxonomy
    },
    options = list(scrollX = TRUE, pageLength = 15)
)
