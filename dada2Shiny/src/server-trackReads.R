output$trackTable <- DT::renderDataTable(
  {
    track <- reactiveInputData()$track
    # Add row names as a column called "Sample"
    track_with_samples <- cbind(Sample = rownames(track), track)
    dadaFs <- reactiveInputData()$dadaFs
    dadaRs <- reactiveInputData()$dadaRs
    mergers <- reactiveInputData()$mergers
    seqtab.nochim <- reactiveInputData()$seqtab.nochim
    sample.names <- reactiveInputData()$sample.names
    print(sample.names)

    # Function to create a FASTA file for each sample
    createFasta <- function(sequences, filename, samplename) {
      seqs <- names(getUniques(sequences))
      # seqs <- colnames(sequences)
      print(seqs)
      index_list <- 1:length(seqs)
      fastaContent <- paste0(">seq", index_list, '_', samplename, "\n", seqs)
      writeLines(fastaContent, con = filename)
    }

    # Create a function to write FASTA files and zip them for download
    createZip <- function(file_prefix, unique_sequences_list, file) {
      # Temporary folder to store the FASTA files
      temp_dir <- tempfile()
      dir.create(temp_dir)
      print(unique_sequences_list)

      # Create individual FASTA files for each sample in the category
      for (i in sample.names) {
        fasta_filename <- file.path(temp_dir, paste0(file_prefix, "_", i, ".fasta"))
        # print(unique_sequences_list[i])
        createFasta(unique_sequences_list[[i]], fasta_filename, i)
      }

      # Create the ZIP file containing all the FASTA files
      zip_filename <- file

      print(list.files(temp_dir, full.names = TRUE))
      zip::zipr(zip_filename, files = list.files(temp_dir, full.names = TRUE))
    }



    # Download handler for denoisedF category
    output$download_zip_denoisedF <- downloadHandler(
      filename = function() {
        "denoisedF.zip"
      },
      content = function(file) {
        createZip("denoisedF", dadaFs, file)
      }
    )

    # Download handler for denoisedR category
    output$download_zip_denoisedR <- downloadHandler(
      filename = function() {
        "denoisedR.zip"
      },
      content = function(file) {
        createZip("denoisedR", dadaRs, file)
      }
    )

    # Download handler for merged category
    # output$download_zip_merged <- downloadHandler(
    #   filename = function() { "merged.zip" },
    #   content = function(file) {
    #     createZip("merged", mergers, file)
    #   }
    # )

    # Download handler for non-chimeric category
    output$download_nonchim <- downloadHandler(
      filename = function() {
        "nonchim.fasta"
      },
      content = function(file) {
        seqs <- colnames(seqtab.nochim)
        print(seqs)
        counts <- 1:length(colnames(seqtab.nochim))
        fastaContent <- paste0(">seq", counts, '_', file ,"\n", seqs)
        writeLines(fastaContent, con = file)
        # createZip("nonchim", seqtab.nochim, file)
      }
    )

    # Download handler for non-chimeric category (one FASTA per sample)
    output$download_zip_nonchim <- downloadHandler(
      filename = function() {
        "nonchim.zip"
      },
      content = function(file) {
        # Function to create per-sample non-chimeric FASTA
        createFastaNonchim <- function(sample_name, sequences, filename) {
          seqs <- colnames(sequences)
          # Extract abundance for each sequence
          counts <- sequences[sample_name, ]
          fastaContent <- character()

          # Only include sequences that have non-zero counts
          for (i in seq_along(seqs)) {
            if (counts[i] > 0) {
              fastaContent <- c(
                fastaContent,
                paste0(">seq", i, "_", sample_name, " count=", counts[i]),
                seqs[i]
              )
            }
          }
          writeLines(fastaContent, con = filename)
        }

        # Temporary folder to store non-chimeric FASTA files per sample
        temp_dir <- tempfile()
        dir.create(temp_dir)

        # Loop over each sample in the sequence table and create FASTA files
        sample_names <- rownames(seqtab.nochim) # Extract sample names
        for (sample in sample_names) {
          fasta_filename <- file.path(temp_dir, paste0("nonchim_", sample, ".fasta"))
          createFastaNonchim(sample, seqtab.nochim, fasta_filename)
        }

        # Zip the individual non-chimeric FASTA files
        zip::zipr(file, files = list.files(temp_dir, full.names = TRUE))
      }
    )


    # Server logic to download the track reads table
    output$download_track_table <- downloadHandler(
      filename = function() {
        paste("track_reads_summary", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        # Assuming 'track' is your data frame containing the track reads summary
        write.csv(track_with_samples, file, row.names = FALSE)
      }
    )



    # print(track)
    track_with_samples
  },
  rownames = FALSE,
  options = list(scrollX = TRUE, pageLength = 5)
)

# Populate the selectInput with truncated sequence names
observe({
    seqtab.nochim <- reactiveInputData()$seqtab.nochim
    available_sequences <- colnames(seqtab.nochim)  # Get the sequences from column names

    # Add sequence number as a prefix and truncate sequences for display (show first 20 characters)
    truncated_sequences <- paste0("Seq", seq_along(available_sequences), ": ", substr(available_sequences, 1, 20), "...")

    # Use the full sequences as values and truncated ones as display names
    display_choices <- setNames(available_sequences, truncated_sequences)
    
    # Update the selectInput with new choices
    updateSelectInput(session, "sequence_select", choices = display_choices)
})



# Handle the button click to plot the selected sequence
output$sequenceAbundanceBarChart <- renderPlot({
    req(input$plot_sequence_abundance)  # Ensure the button is clicked

    seqtab.nochim <- reactiveInputData()$seqtab.nochim
    selected_sequence <- NULL

    # Get the sequence either from selection or text input
    if (input$sequence_input != "") {
        selected_sequence <- input$sequence_input
    } else {
        selected_sequence <- input$sequence_select
    }

    # Check if the selected sequence exists in the dataset
    if (selected_sequence %in% colnames(seqtab.nochim)) {
        # Create a data frame for the selected sequence
        abundance_df <- data.frame(
            Sample = rownames(seqtab.nochim),
            Abundance = seqtab.nochim[, selected_sequence]
        )

        # Plot the abundance using ggplot2
        ggplot(abundance_df, aes(x = Sample, y = Abundance)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            theme_minimal() +
            labs(
                title = paste("Abundance of Sequence:", substr(selected_sequence, 1, 20), "..."),
                x = "Sample",
                y = "Abundance"
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
        showNotification("The pasted sequence is not found in the dataset.", type = "error")
    }
})


