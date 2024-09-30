# Define reactive values to hold the selected databases and selected taxonomy rows
reactiveTaxonomyData <- reactiveValues()
selectedTaxonomyRows <- reactiveVal(NULL)

# Event to assign taxonomy
observeEvent(input$assignTaxonomy, {

    # Show loading icon while assigning taxonomy
    js$addStatusIcon("taxanomyTab", "loading")
    
    # Ensure that seqtab.nochim is available and assigned properly
    seqtab.nochim <- reactiveInputData()$seqtab.nochim
    req(seqtab.nochim)  # Make sure seqtab.nochim is available

    # Assign default database paths
    reference_db <- if (input$database_choice == "silva") {
        "./www/taxonomy/silva_nr99_v138.1_train_set.fa.gz"
    } else if (input$database_choice == "custom") {
        input$custom_silva_nr99$datapath  # Path to the uploaded custom file
    }

    species_db <- if (input$species_assignment_choice == "silva_species") {
        "./www/taxonomy/silva_species_assignment_v138.1.fa.gz"
    } else if (input$species_assignment_choice == "custom_species") {
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

    # Store taxonomy and species assignment results
    taxa[is.na(taxa)] <- 'unknown'
    reactiveTaxonomyData$taxa <- taxa

    # Update icons and tabs
    shinyjs::show(selector = "a[data-value=\"alphaDiversityTab\"]")
    shinyjs::show(selector = "a[data-value=\"taxanomyTab\"]")
    js$addStatusIcon("taxanomyTab", "done")
    js$addStatusIcon("trackReadsTab", "done")
    js$addStatusIcon("alphaDiversityTab", "next")
})

# Render the taxonomy table and download handler
output$taxonomyTable <- DT::renderDataTable({
    req(reactiveTaxonomyData$taxa)  # Ensure taxa is available

    taxonomy <- reactiveTaxonomyData$taxa
    output_table_wth_sequence <- cbind(Sequence = rownames(taxonomy), taxonomy)

    # Populate the grouping column select input based on available columns
    updateSelectInput(session, "grouping_column", choices = colnames(taxonomy))

    # Enable row selection for multiple row selection
    datatable(output_table_wth_sequence, 
              rownames = FALSE, 
              options = list(scrollX = TRUE, pageLength = 10), 
              selection = 'multiple')  # Specify multiple row selection
})

# Create a DataTable proxy
proxy <- dataTableProxy('taxonomyTable')

# Reactive to handle row selection in the taxonomy table and plot generation
observe({
    req(reactiveTaxonomyData$taxa)
    req(input$filter_values)
    selected_rows <- input$taxonomyTable_rows_selected

    # If no rows are selected, select all rows by default
    if (is.null(selected_rows) || length(selected_rows) == 0) {
        selected_rows <- 1:nrow(reactiveTaxonomyData$taxa)  # Select all rows by default
    }

    # Ensure seqtab.nochim is available
    seqtab.nochim <- reactiveInputData()$seqtab.nochim
    req(seqtab.nochim)

    # Get the sequences corresponding to the selected rows
    selected_sequences <- rownames(reactiveTaxonomyData$taxa)[selected_rows]

    # If no sequences are found in seqtab.nochim, show a message and return
    if (length(selected_sequences) == 0) {
        showNotification("No selected sequences found in the dataset.", type = "error")
        return()
    }

    # Reactive for the selected grouping column
    grouping_column <- reactive(input$grouping_column)

    if (is.null(grouping_column())) {
        showNotification("Group not selected.", type = "error")
        return()
    }

    # Update the bar plot for the selected sequences
    output$sequenceDistributionBarChart <- renderPlot({
        req(input$filter_values)
        taxonomy <- reactiveTaxonomyData$taxa
        taxonomy_column_name <- grouping_column()

        # Extract abundance of the selected sequences across samples
        seq_abundance <- (seqtab.nochim[, selected_sequences, drop = FALSE] > 0) * 1  # Presence/Absence matrix

        # Initialize an empty data frame for storing plot data
        plot_data <- data.frame(Sample = character(), Abundance = numeric(), stringsAsFactors = FALSE)

        # Loop through each row of seq_abundance and add frequency information to plot_data
        for (i in 1:nrow(seq_abundance)) {
            cols_with_one <- which(seq_abundance[i, selected_sequences] == 1)  # Find columns with 1 in the current row

            seq_with_one <- colnames(seq_abundance)[cols_with_one]  # Get column names (sequences) with 1

            selected_column_values <- taxonomy[seq_with_one, input$grouping_column, drop = TRUE]
            frequency_table <- table(selected_column_values)

            if (sum(seq_abundance[i, ]) > 0){
                
            

                # Add row to plot_data with the total abundance (sum of 1's) and frequency table
                new_row <- data.frame(
                    Sample = rownames(seq_abundance)[i],  # Current sample name
                    Abundance = sum(seq_abundance[i, ]),
                    stringsAsFactors = FALSE
                )

                for (name in input$filter_values) {
                    new_row[[name]] <- ifelse(name %in% names(frequency_table), frequency_table[name], 0)
                }

                # Bind new_row to plot_data
                plot_data <- rbind(plot_data, new_row)
            }
        }

        # Convert the data into a long format for plotting
        plot_data_long <- tidyr::pivot_longer(
            plot_data,
            cols = input$filter_values,
            names_to = grouping_column(),  # New column to represent grouping
            values_to = "Value"  # 1 or 0 based on the values
        )

        # Create the bar plot with grouping
        ggplot(plot_data_long, aes(x = Sample, y = Value, fill = .data[[grouping_column()]])) +
            geom_bar(stat = "identity") +
            labs(title = paste("Distribution of Selected Sequences Across Samples (Grouped by", grouping_column(), ")"),
                 x = "Sample",
                 y = "Abundance") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_discrete(name = grouping_column())
    })
})

# Function to update the grouping column values and filter
updateGroupingAndFilter <- function(selected_rows, grouping_column) {
    req(reactiveTaxonomyData$taxa)

    # If no rows are selected, select all rows by default
    if (is.null(selected_rows) || length(selected_rows) == 0) {
        selected_rows <- 1:nrow(reactiveTaxonomyData$taxa)
    }

    # Get the sequences corresponding to the selected rows
    selected_sequences <- rownames(reactiveTaxonomyData$taxa)[selected_rows]
    taxonomy <- reactiveTaxonomyData$taxa
    selected_column_values <- taxonomy[selected_sequences, grouping_column, drop = TRUE]

    group_values <- unique(selected_column_values)

    # Update the multi-select input for filtering based on the unique values
    updateSelectInput(session, "filter_values", choices = group_values, selected = group_values)
}

# Update filter options based on selected grouping column
# Observe event for grouping column change
observeEvent(input$grouping_column, {
    req(input$grouping_column)
    selected_rows <- input$taxonomyTable_rows_selected

    # Trigger the update function with the current selection of rows and the grouping column
    updateGroupingAndFilter(selected_rows, input$grouping_column)
})

# Observe event for row selection in taxonomy table
observe({
    req(reactiveTaxonomyData$taxa)
    req(input$grouping_column)

    selected_rows <- input$taxonomyTable_rows_selected
    updateGroupingAndFilter(selected_rows, input$grouping_column)
})

# Taxonomy is ready when there is data in reactiveTaxonomyData$taxa
output$taxonomy_ready <- reactive({
  !is.null(reactiveTaxonomyData$taxa)
})
outputOptions(output, "taxonomy_ready", suspendWhenHidden = FALSE)

# Clear row selection functionality
observeEvent(input$clearSelection, {
    selectedTaxonomyRows(NULL)
    selectRows(proxy, NULL)  # Clear selection in the table using proxy
})

# Render the number of rows selected
output$numSelectedRows <- renderText({
    selected_rows <- input$taxonomyTable_rows_selected
    if (is.null(selected_rows)) {
        ""
    } else {
        paste(length(selected_rows), "rows selected.")
    }
})

# Download taxonomy table handler
output$download_taxonomy_table <- downloadHandler(
    filename = function() {
        paste("taxonomy_table", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        taxonomy <- reactiveTaxonomyData$taxa
        output_table_wth_sequence <- cbind(Sequence = rownames(taxonomy), taxonomy)
        write.csv(output_table_wth_sequence, file, row.names = FALSE)
    }
)

# Download taxonomy table as FASTA
output$download_taxonomy_fasta <- downloadHandler(
    filename = function() {
        paste("taxonomy_table", Sys.Date(), ".fasta", sep = "")
    },
    content = function(file) {
        selected_rows <- input$taxonomyTable_rows_selected
            # If no rows are selected, select all rows by default
        if (is.null(selected_rows) || length(selected_rows) == 0) {
            selected_rows <- 1:nrow(reactiveTaxonomyData$taxa)
        }

        # Get the sequences corresponding to the selected rows
        taxonomy <- reactiveTaxonomyData$taxa[selected_rows,]

        output_table_with_sequence <- cbind(Sequence = rownames(taxonomy), taxonomy)
        
        # Open a connection to write to the file
        fileConn <- file(file, open = "w")

        print('taxonomy_table fasta')
        
        for (i in 1:nrow(output_table_with_sequence)) {
            header <- paste0(">seq_",i,'_','taxonomy_table','_', paste(
                colnames(output_table_with_sequence)[-1], 
                output_table_with_sequence[i, -1], 
                sep = ":", collapse = "; "
            ))

            # print(i)
            # print(header)
            # print(output_table_with_sequence[i, "Sequence"])

            writeLines(header, fileConn)
            writeLines(output_table_with_sequence[i, "Sequence"], fileConn)
        }
        close(fileConn)
    }
)
