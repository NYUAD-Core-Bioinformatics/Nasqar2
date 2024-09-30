observe({
    js$addStatusIcon("input_tab", "next")
    shinyjs::hide(selector = "a[data-value=\"qualityprofile_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"errorRatesTab\"]")
    shinyjs::hide(selector = "a[data-value=\"filter_and_trim_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"margePairedReadsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"trackReadsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"taxanomyTab\"]")
    shinyjs::hide(selector = "a[data-value=\"alphaDiversityTab\"]")
})





my_values <- reactiveValues()
my_values$mounted_dir <- FALSE
my_values$downloaded_files <- FALSE

# observeEvent(input$connect_remote_server, {
#     print(input$username)
#     print(input$hostname)
#     print(input$mountpoint)
#     print(input$id_rsa$datapath)
#     my_values$mounted_dir <- FALSE


#     system(paste(
#         "sh generate_ssh_config.sh ", input$username, " ",
#         input$hostname, " ", input$mountpoint, " ",
#         input$id_rsa$datapath
#     ))


#     # Get the list of files in the directory
#     # files <- list.files(base_dir, full.names = FALSE)
#     my_values$mounted_dir <- TRUE
# })

downloadFiles <- function(){
    output$status <- renderText("Downloading files...")
    
    folder_url <- input$folder_url

  
    
    # Specify the folder where files will be saved locally
    download_path <- tempfile() 
    dir.create(download_path)
    my_values$download_path <- download_path
    
    # Example function to get list of files and download them
    tryCatch({
      # Assuming the server provides an index.html with links to files
      response <- GET(folder_url)
      page_content <- content(response, "text")
      
      # Extract file URLs (using regex or an HTML parser like xml2)
      file_links <- regmatches(page_content, gregexpr("href=\"(.*?)\"", page_content))[[1]]
      print(file_links)
      file_links <- gsub("href=\"|\"", "", file_links) # Clean up URLs
      
      # Loop through and download each file
      for (fname in file_links) {
        print(fname)
        print(download_path)
        file_path <- file.path(download_path, basename(fname))
        print(file_path)
        file_url <- paste0(folder_url,'/', fname) 
        download.file(file_url, file_path, mode = "wb")
      }
      my_values$downloaded_files <- TRUE
    #   output$status <- renderText("Files downloaded successfully, started analysis...")
    }, error = function(e) {
    #   output$status <- renderText(paste("Error downloading files:", e))
      output$status <- renderText({
        js$addStatusIcon("input_tab", "done")
        js$addStatusIcon("input_tab", "next")
        print('input$folder_url')
        print(input$folder_url)
       if (is.null(input$folder_url) | nchar(input$folder_url)  < 3){
        return ('Please provide ur')
       } 

       return('Error downloading files from provided url')
        
        })
    })
  }

qc_done <- reactiveVal(FALSE)

divergen_done <- reactiveVal(FALSE)

output$divergen_available <- reactive({
    return(divergen_done())
})
outputOptions(output, "divergen_available", suspendWhenHidden = FALSE)


output$qc_result_available <- reactive({
    reactiveInputData()
    return(qc_done())
})
outputOptions(output, "qc_result_available", suspendWhenHidden = FALSE)


# observeEvent(input$initFastq, {
#     print("Load fastq files")

#     qc_done(FALSE)

#     shinyjs::hide(selector = "a[data-value=\"errorRatesTab\"]")
#     shinyjs::hide(selector = "a[data-value=\"margePairedReadsTab\"]")
#     shinyjs::hide(selector = "a[data-value=\"trackReadsTab\"]")

#     shinyjs::show(selector = "a[data-value=\"filter_and_trim_tab\"]")
#     shinyjs::show(selector = "a[data-value=\"qualityprofile_tab\"]")
#     shinyjs::show(selector = "a[data-value=\"input_tab\"]")
#     js$addStatusIcon("input_tab", "done")
#      js$addStatusIcon("filter_and_trim_tab", "next")
# })

# observeEvent(input$tx_db_input, {
#     if(input$bs_genome_input != 'empty'){
#         phast_cons <- list("BSgenome.Hsapiens.UCSC.hg19" = c("phastCons100way.UCSC.hg19"))

#         updateSelectInput(session, "phast_cons_input", choices = phast_cons[[input$bs_genome_input]])

#     }

# })

input_files_reactive <- eventReactive(input$initFastq, {
    # shiny::validate(
    #     need(identical(input$data_file_type, "example_bam_file") | (is.null(input$bam_files)),
    #         message = "Please select a file"
    #     )
    # )
    qc_done(FALSE)


    js$addStatusIcon("input_tab", "loading")
   


    if (identical(input$data_file_type, "upload_fastq_file") & (is.null(input$fastq_files) | length(input$fastq_files$name) < 1)){
        js$addStatusIcon("input_tab", "done")
        js$addStatusIcon("input_tab", "next")
        output$status <- renderText({
        
        
        validate(need(FALSE, 'Please upload files'))
        
        })
        req(input$fastq_files)
    
    }




    
  
    print('load')



   if(identical(input$data_file_type, "download_remote_server")){
    downloadFiles()
    req(my_values$downloaded_files)
   }  

        

        

     

     


    if (identical(input$data_file_type, "upload_fastq_file")) {
        # print(input$fastq_files)
        files <- c(input$fastq_files$name)
        print('uploaded_fastq_file')
        print(files)

        print('forward_pattern')
        print(input$forward_pattern)

        # Filter fastq files
        # Filter fastq files

        print('grepl(input$forward_pattern, files)')
        print(grepl(input$forward_pattern, files))
        fn_Fs <- files[grepl(input$forward_pattern, files)]

        print('fn_Fs')
        print(fn_Fs)

        if (input$seq_type == "paired") {
            fn_Rs <- files[grepl(input$reverse_pattern, files)]

            matching_files <- sapply(fn_Fs, function(fastq_file) {
                fastq_file <- gsub(paste0(input$forward_pattern, "$"), input$reverse_pattern, fastq_file)
                fastq_file %in% fn_Rs
            })
            fn_Fs <- names(matching_files[matching_files == TRUE])
            fn_Rs <- stringr::str_replace_all(fn_Fs, input$forward_pattern, input$reverse_pattern)

            shiny::validate(
                need(sum(matching_files) == length(fn_Fs) & sum(matching_files) == length(fn_Rs),
                    message = "Please upload both R1 and R2 fastq files having same base name(ex: <name>_R1_001.fastq and <name>_R2_001.fastq) "
                )
            )
        }

        base_dir <- tempfile()
        dir.create(base_dir)
        apply(input$fastq_files, 1, function(row) {
            # Access row elements by index or name
            # print(str(row))
            datapath <- row["datapath"]
            name <- row["name"]
            file.remove(file.path(base_dir, name))
            file.symlink(datapath, file.path(base_dir, name))
            # file.copy(datapath, file.path(base_dir, name))
            # print(name)
        })
    } else if (identical(input$data_file_type, "example_fastq_file")) {
        base_dir <- "www/exampleData/MiSeq_SOP/"
        # Get the list of files in the directory
        files <- list.files(base_dir, full.names = FALSE)

        print('base_dir...')
        print(base_dir)
        print(files)
        print(input$forward_pattern)
        print('grepl(input$forward_pattern, files)')
        grepl(input$forward_pattern, files)

        # Filter fastq files
        fn_Fs <- files[grepl(input$forward_pattern, files)]

        print(fn_Fs)

        if (input$seq_type == "paired") {
            fn_Rs <- files[grepl(input$reverse_pattern, files)]
            matching_files <- sapply(fn_Fs, function(fastq_file) {
                fastq_file <- gsub(paste0(input$forward_pattern, "$"), input$reverse_pattern, fastq_file)
                fastq_file %in% fn_Rs
            })
            fn_Fs <- names(matching_files[matching_files == TRUE])
            fn_Rs <- stringr::str_replace_all(fn_Fs, input$forward_pattern, input$reverse_pattern)
        }
    } else {
        base_dir <- my_values$download_path
        print(base_dir)
        # Get the list of files in the directory
        files <- list.files(base_dir, full.names = FALSE)
        print(files)

        print('grepl(input$forward_pattern, files) ..')
        print(grepl(input$forward_pattern, files))
        # Filter fastq files
        fn_Fs <- files[grepl(input$forward_pattern, files)]
        print('fn_Fs ....')
        print(fn_Fs)

        if (input$seq_type == "paired") {
            fn_Rs <- files[grepl(input$reverse_pattern, files)]
            matching_files <- sapply(fn_Fs, function(fastq_file) {
                fastq_file <- gsub(paste0(input$forward_pattern, "$"), input$reverse_pattern, fastq_file)
                fastq_file %in% fn_Rs
            })
            fn_Fs <- names(matching_files[matching_files == TRUE])
            fn_Rs <- stringr::str_replace_all(fn_Fs, input$forward_pattern, input$reverse_pattern)
            shiny::validate(
                need(sum(matching_files) == length(fn_Fs) & sum(matching_files) == length(fn_Rs),
                    message = "Please upload both R1 and R2 fastq files having same base name(ex: <name>_R1_001.fastq and <name>_R2_001.fastq) "
                )
            )
        }
    }


    sample_names <- sapply(fn_Fs, function(fastq_file) {
        fastq_file <- gsub(paste0(input$forward_pattern, "$"), "", fastq_file)
    })

    sample_names <- unname(sample_names)
    print('dir(base_dir)')
    print(base_dir)
    print(dir(base_dir))

    print('fn_Fs')
    print(fn_Fs)

      print('fn_Rs')
    print(fn_Rs)

    if (input$seq_type == "paired") {
        samples_df <- data.frame(FASTQ_Fs = fn_Fs, FASTQ_Rs = fn_Rs, row.names = sample_names)
    } else {
        samples_df <- data.frame(FASTQ_Fs = fn_Fs, row.names = sample_names)
    }
    my_values$base_dir <- base_dir
    my_values$samples_df <- samples_df

    if(nrow(samples_df) > 0) {

    updateSelectInput(session, "sel_sample_qualityprofile_tab", choices = sample_names, selected = NULL)
    # library(input$bs_genome_input, character.only = T)
    # bamfile <- my_values$bamfile
    print("sel_sample_for_npositioning")

    js$addStatusIcon("input_tab", "done")
    shinyjs::show(selector = "a[data-value=\"qualityprofile_tab\"]")
    js$addStatusIcon("qualityprofile_tab", "done")
    shinyjs::show(selector = "a[data-value=\"filter_and_trim_tab\"]")
    js$addStatusIcon("filter_and_trim_tab", "next")
    shinyjs::show(selector = "a[data-value=\"input_tab\"]")
    
    print(samples_df)
    } else {
        output$status <- renderText({
        
        
        validate(need(FALSE, 'Incorrect fastq file pattern or missing files'))
        
        })
    js$addStatusIcon("input_tab", "done")
    js$addStatusIcon("input_tab", "next")
    
    shinyjs::show(selector = "a[data-value=\"input_tab\"]")
    
    }

    samples_df
})

  output$result1 <- renderText({
        # Ensure the input is between 1 and 10
        shiny::validate(
        need(identical(input$data_file_type, "example_fastq_file") | identical(input$data_file_type, "download_remote_server") | (identical(input$data_file_type, "upload_fastq_file") & !is.null(input$fastq_files) & length(input$fastq_files$name) > 1),
            message = "Please upload both R1 andd R2 fastq files "
        )
    )

    print('load')

    shiny::validate(
        need(identical(input$data_file_type, "example_fastq_file") | identical(input$data_file_type, "upload_fastq_file") | identical(input$data_file_type, "download_remote_server") & my_values$downloaded_files,
            message = "Please connect to the server "
        )
    )
        paste("You entered:", input$num)
    })


output$fastqfiles_uploaded <- reactive({
    return(!is.null(input_files_reactive()))
})
outputOptions(output, "fastqfiles_uploaded", suspendWhenHidden = FALSE)


output$fastq_samples_table <- DT::renderDataTable(
    {
        input_files_reactive()
    },
    options = list(scrollX = TRUE, pageLength = 10)
)
