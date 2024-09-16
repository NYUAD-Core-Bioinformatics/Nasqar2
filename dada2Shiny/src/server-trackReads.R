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
        
        # Function to create a FASTA file for each sample
        createFasta <- function(sequences, filename) {
          seqs <- names(getUniques(sequences))
          #seqs <- colnames(sequences)
          print(seqs)
          index_list <- 1:length(seqs)
          fastaContent <- paste0('sample',index_list, '\n', seqs)
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
            #print(unique_sequences_list[i])
            createFasta(unique_sequences_list[[i]], fasta_filename)
          }
          
          # Create the ZIP file containing all the FASTA files
          zip_filename <- file
          
          print(list.files(temp_dir, full.names = TRUE))
          zip::zipr(zip_filename, files = list.files(temp_dir, full.names = TRUE))
        }
        
      
        
        # Download handler for denoisedF category
        output$download_zip_denoisedF <- downloadHandler(
          filename = function() { "denoisedF.zip" },
          content = function(file) {
            createZip("denoisedF", dadaFs, file)
          }
        )
        
        # Download handler for denoisedR category
        output$download_zip_denoisedR <- downloadHandler(
          filename = function() { "denoisedR.zip" },
          content = function(file) {
            createZip("denoisedR", dadaRs, file)
          }
        )
        
        # Download handler for merged category
        output$download_zip_merged <- downloadHandler(
          filename = function() { "merged.zip" },
          content = function(file) {
            createZip("merged", mergers, file)
          }
        )
        
        # Download handler for non-chimeric category
        output$download_zip_nonchim <- downloadHandler(
          filename = function() { "nonchim.fasta" },
          content = function(file) {
            seqs <- colnames(seqtab.nochim)
            print(seqs)
            counts <- 1:length(colnames(seqtab.nochim))
            fastaContent <- paste0('seq',counts, '\n', seqs)
            writeLines(fastaContent, con = file)
            #createZip("nonchim", seqtab.nochim, file)
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
