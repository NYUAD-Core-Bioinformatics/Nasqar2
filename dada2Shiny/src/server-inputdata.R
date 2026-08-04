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
my_values$work_dir <- NULL
my_values$local_work_dir <- NULL
my_values$cache_status <- NULL

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
    fn_Rs <- character()
    # shiny::validate(
    #     need(identical(input$data_file_type, "example_bam_file") | (is.null(input$bam_files)),
    #         message = "Please select a file"
    #     )
    # )
    qc_done(FALSE)
    divergen_done(FALSE)
    
    # Clear all reactive values from downstream processing
    if (exists("reactiveTaxonomyData")) {
        reactiveTaxonomyData$taxa <- NULL
    }
    if (exists("selectedTaxonomyRows")) {
        selectedTaxonomyRows(NULL)
    }
    if (exists("alphaDiversityResults")) {
        alphaDiversityResults$ps <- NULL
        alphaDiversityResults$ps.prop <- NULL
        alphaDiversityResults$ord.nmds.bray <- NULL
        alphaDiversityResults$ps.top20 <- NULL
    }


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
    } else if (identical(input$data_file_type, "hpc_scratch_directory")) {
        shiny::validate(need(nzchar(trimws(input$hpc_fastq_directory)), "Enter an HPC FASTQ directory."))
        base_dir <- tryCatch(
            resolve_hpc_path(input$hpc_fastq_directory),
            error = function(e) {
                shiny::validate(need(FALSE, e$message))
            }
        )
        files <- list.files(base_dir, full.names = FALSE)
        fn_Fs <- files[grepl(input$forward_pattern, files)]
        if (input$seq_type == "paired") {
            fn_Rs <- files[grepl(input$reverse_pattern, files)]
            matching_files <- sapply(fn_Fs, function(fastq_file) {
                expected <- gsub(
                    paste0(input$forward_pattern, "$"),
                    input$reverse_pattern,
                    fastq_file
                )
                expected %in% fn_Rs
            })
            fn_Fs <- names(matching_files[matching_files])
            fn_Rs <- stringr::str_replace_all(
                fn_Fs,
                input$forward_pattern,
                input$reverse_pattern
            )
            shiny::validate(need(
                length(fn_Fs) > 0L && length(fn_Fs) == length(fn_Rs),
                "No matching paired FASTQ files were found with these patterns."
            ))
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
    my_values$work_dir <- project_directory(
        if (identical(input$data_file_type, "hpc_scratch_directory")) {
            input$hpc_project_name
        } else {
            paste0("session-", session$token)
        }
    )
    my_values$local_work_dir <- local_working_directory(
        if (identical(input$data_file_type, "hpc_scratch_directory")) {
            input$hpc_project_name
        } else {
            paste0("session-", session$token)
        }
    )
    my_values$cache_status <- NULL

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
        need(identical(input$data_file_type, "example_fastq_file") | identical(input$data_file_type, "hpc_scratch_directory") | (identical(input$data_file_type, "upload_fastq_file") & !is.null(input$fastq_files) & length(input$fastq_files$name) > 1),
            message = "Please upload both R1 andd R2 fastq files "
        )
    )

    print('load')

    shiny::validate(
        need(identical(input$data_file_type, "example_fastq_file") | identical(input$data_file_type, "upload_fastq_file") | identical(input$data_file_type, "hpc_scratch_directory"),
            message = "Please select a FASTQ data source."
        )
    )
        paste("You entered:", input$num)
    })

output$hpc_dada2_path <- renderText({
    req(my_values$work_dir)
    paste("DADA2 project output:", my_values$work_dir)
})

output$dada_cache_status <- renderText({
    req(my_values$cache_status)
    my_values$cache_status
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
