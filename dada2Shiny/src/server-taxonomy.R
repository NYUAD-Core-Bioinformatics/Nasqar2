# Define reactive values to hold the selected databases and selected taxonomy rows
reactiveTaxonomyData <- reactiveValues()
selectedTaxonomyRows <- reactiveVal(NULL)
animalculesExchangeToken <- reactiveVal(NULL)

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
        taxa <- assignTaxonomy(seqtab.nochim, reference_db, multithread = hpc_workers)
        
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
    if (!is.null(my_values$work_dir)) {
        taxonomy_output <- cbind(Sequence = rownames(taxa), taxa)
        write.csv(
            taxonomy_output,
            file.path(my_values$work_dir, "taxonomy-table.csv"),
            row.names = FALSE
        )
    }

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

output$animalculesTransferUI <- renderUI({
    req(reactiveTaxonomyData$taxa)
    dada_result <- reactiveInputData()
    req(dada_result$seqtab.nochim)

    token <- animalculesExchangeToken()
    if (is.null(token)) {
        sequences <- colnames(dada_result$seqtab.nochim)
        asv_ids <- sprintf("ASV%04d", seq_along(sequences))

        counts <- t(dada_result$seqtab.nochim)
        rownames(counts) <- asv_ids

        taxonomy <- reactiveTaxonomyData$taxa[sequences, , drop = FALSE]
        rownames(taxonomy) <- asv_ids
        taxonomy <- data.frame(
            Sequence = sequences,
            taxonomy,
            check.names = FALSE,
            stringsAsFactors = FALSE
        )

        sample_names <- rownames(dada_result$seqtab.nochim)
        metadata <- data.frame(
            sample = sample_names,
            source = "DADA2Shiny",
            row.names = sample_names,
            stringsAsFactors = FALSE
        )
        if (!is.null(input$sampleData)) {
            uploaded_metadata <- tryCatch(
                as.data.frame(qiimeData(), check.names = FALSE),
                error = function(error) NULL
            )
            if (!is.null(uploaded_metadata) &&
                all(sample_names %in% rownames(uploaded_metadata))) {
                metadata <- uploaded_metadata[sample_names, , drop = FALSE]
                metadata$source <- "DADA2Shiny"
            }
        }

        exchange_dir <- nasqar_exchange_dir(create = TRUE)
        token <- uuid::UUIDgenerate()
        exchange_path <- file.path(exchange_dir, paste0(token, ".rds"))
        pending_path <- tempfile(
            pattern = paste0(token, "-"),
            tmpdir = exchange_dir,
            fileext = ".pending"
        )
        saveRDS(
            list(counts = counts, taxonomy = taxonomy, metadata = metadata),
            pending_path
        )
        if (!file.rename(pending_path, exchange_path)) {
            unlink(pending_path)
            stop("Could not publish the animalcules exchange file.",
                 call. = FALSE)
        }
        animalculesExchangeToken(token)
    }

    handoffPath <- paste0(
        "/animalcules/?exchange=",
        URLencode(token, reserved = TRUE)
    )

    tags$a(
        icon("external-link-alt"),
        "Open directly in animalcules",
        href = handoffPath,
        `data-handoff-path` = handoffPath,
        onclick = paste(
            "this.href = window.location.origin +",
            "this.getAttribute('data-handoff-path');"
        ),
        target = "_blank",
        class = "btn btn-success",
        style = "width: 100%; margin-top: 10px;"
    )
})

# Create a DataTable proxy
proxy <- dataTableProxy('taxonomyTable')

# Build the selected-sequence distribution once for both display and export.
sequence_distribution_plot <- reactive({
    req(reactiveTaxonomyData$taxa)
    req(input$filter_values)
    req(input$grouping_column)
    selected_rows <- input$taxonomyTable_rows_selected
    if (is.null(selected_rows) || length(selected_rows) == 0) {
        selected_rows <- seq_len(nrow(reactiveTaxonomyData$taxa))
    }
    seqtab.nochim <- reactiveInputData()$seqtab.nochim
    req(seqtab.nochim)
    selected_sequences <- rownames(reactiveTaxonomyData$taxa)[selected_rows]
    validate(need(length(selected_sequences) > 0, "Select at least one sequence."))
    validate(need(
        all(selected_sequences %in% colnames(seqtab.nochim)),
        "Selected taxonomy rows are not present in the sequence table."
    ))

    taxonomy <- reactiveTaxonomyData$taxa
    seq_abundance <- (seqtab.nochim[, selected_sequences, drop = FALSE] > 0) * 1
    samples_vec <- character()
    filter_data <- stats::setNames(
        replicate(length(input$filter_values), numeric(), simplify = FALSE),
        input$filter_values
    )

    for (i in seq_len(nrow(seq_abundance))) {
        if (sum(seq_abundance[i, ]) > 0) {
            seq_with_one <- colnames(seq_abundance)[which(seq_abundance[i, ] == 1)]
            frequency_table <- table(
                taxonomy[seq_with_one, input$grouping_column, drop = TRUE]
            )
            samples_vec <- c(samples_vec, rownames(seq_abundance)[i])
            for (name in input$filter_values) {
                filter_data[[name]] <- c(
                    filter_data[[name]],
                    if (name %in% names(frequency_table)) {
                        as.numeric(frequency_table[[name]])
                    } else {
                        0
                    }
                )
            }
        }
    }
    validate(need(length(samples_vec) > 0, "No selected sequences occur in any sample."))

    plot_data <- data.frame(Sample = samples_vec, check.names = FALSE)
    for (name in input$filter_values) plot_data[[name]] <- filter_data[[name]]
    plot_data_long <- tidyr::pivot_longer(
        plot_data,
        cols = tidyselect::all_of(input$filter_values),
        names_to = input$grouping_column,
        values_to = "Value"
    )

    ggplot(plot_data_long, aes(
        x = Sample,
        y = Value,
        fill = .data[[input$grouping_column]]
    )) +
        geom_col() +
        labs(
            x = "Sample",
            y = "Sequence occurrence",
            fill = input$grouping_column
        ) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

output$sequenceDistributionBarChart <- renderPlot(sequence_distribution_plot())
register_publication_downloads(
    output, "download_sequence_distribution",
    function() "dada2-selected-sequence-distribution",
    sequence_distribution_plot, width = 10, height = 6
)

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
