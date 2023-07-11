observe({
   
    shinyjs::hide(selector = "a[data-value=\"fragmentsize_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"nucleosomepositioning_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"heatmap_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"librarycomplexity_tab\"]")
    
})


observe({
    # shinyjs::hide(selector = "a[data-value=\"tab2\"]")
    #ddsReactive()
})

# observeEvent(input$bam_files,{
#     print(input$bam_files$name)

#     if(length(input$bam_files$name) == 1){
#             files <- c(input$bam_files$name)
#     }

#     #  print(input$bam_files$name[[which('GL1.bai' == input$bam_files$name)]])
#     # print(input$bam_files$datapath[[which('GL1.bai' == input$bam_files$name)]])


#     bam_files <- files[grepl(".bam$", files)]
#     bai_files <- files[grepl(".bai$", files)]

#     print(bam_files)
#     print(bai_files)

#     # Check if there are matching BAM and BMI files
#     matching_files <- sapply(bam_files, function(bam_file) {
#         bmi_file <- gsub(".bam$", ".bai", bam_file)
#         bmi_file %in% bai_files
#     })

#     sample_names <- sapply(bam_files, function(bam_file) {
#         bam_file <- gsub(".bam$", "", bam_file)
#     })
#     sample_names <-  unname(sample_names)
#     print('sample_names')
#     print(sample_names)

#     samples_df <- data.frame(BamFile = bam_files, IndexFile = bai_files, row.names = sample_names )
#     print(samples_df)
    
    
# } )

my_values <- reactiveValues()

observeEvent(input$bs_genome_input, {
    if(input$bs_genome_input != ''){
        print('input$bs_genome_input')
        print(input$bs_genome_input)
        library(input$bs_genome_input, character.only = T)
        tx_db <- list("BSgenome.Hsapiens.UCSC.hg19" = c("TxDb.Hsapiens.UCSC.hg19.knownGene"))
         shinyjs::enable("initQC")
        # updateSelectInput(session, "tx_db_input", choices = tx_db[[input$bs_genome_input]])
    }

})

# observeEvent(input$tx_db_input, {
#     if(input$bs_genome_input != 'empty'){
#         phast_cons <- list("BSgenome.Hsapiens.UCSC.hg19" = c("phastCons100way.UCSC.hg19"))

#         updateSelectInput(session, "phast_cons_input", choices = phast_cons[[input$bs_genome_input]])
       
#     }

# })

input_files_reactive <- reactive({

    shiny::validate(
        need(identical(input$data_file_type, "example_bam_file") | (!is.null(input$bam_files)),
            message = "Please select a file"
        )
    )

    shiny::validate(
        need(identical(input$data_file_type, "example_bam_file") | (identical(input$data_file_type, "upload_bam_file") &  !is.null(input$bam_files) & length(input$bam_files$name) > 1),
            message = "Please upload both .bam and .bai files "
        )
    )


    if(identical(input$data_file_type, "upload_bam_file")){
        files <- c(input$bam_files$name)
        bam_files <- files[grepl(".bam$", files)]
        bai_files <- files[grepl(".bai$", files)]
        # Check if there are matching BAM and BMI files
        matching_files <- sapply(bam_files, function(bam_file) {
            bmi_file <- gsub(".bam$", ".bai", bam_file)
            bmi_file %in% bai_files
        })

        shiny::validate(
            need(sum(matching_files) == length(bam_files) & sum(matching_files)  == length(bai_files),
                message = "Please upload both .bam and .bai files having same base name(ex: s1.bam and s1.bai) "
            )
        )
        base_dir <- tempfile()
        dir.create(base_dir)
        apply(input$bam_files, 1, function(row) {
            # Access row elements by index or name
            # print(str(row))
            datapath <- row['datapath']
            name <- row['name']
            file.remove(file.path(base_dir, name))
            file.symlink(datapath, file.path(base_dir, name))
            # file.copy(datapath, file.path(base_dir, name))
            # print(name)
        })
    } else {
        base_dir <- 'www/exampleData/bamfiles/'
        # Get the list of files in the directory
        files <- list.files(base_dir, full.names = FALSE)

        # Filter BAM and BMI files
        bam_files <- files[grepl(".bam$", files)]
        bai_files <- files[grepl(".bai$", files)]
    }

            
    sample_names <- sapply(bam_files, function(bam_file) {
        bam_file <- gsub(".bam$", "", bam_file)
    })

    sample_names <-  unname(sample_names)

    print(dir(base_dir))
    
    bs_genome_db <- c( "Human(BSgenome.Hsapiens.UCSC.hg19)" = "BSgenome.Hsapiens.UCSC.hg19", "Mouse(BSgenome.Mmusculus.UCSC.mm10)"="BSgenome.Mmusculus.UCSC.mm10", "ZebraFish(BSgenome.Drerio.UCSC.danRer11)"="BSgenome.Drerio.UCSC.danRer11", "Worm(BSgenome.Celegans.UCSC.ce6)"="BSgenome.Celegans.UCSC.ce6")
    updateSelectInput(session, "bs_genome_input", choices = bs_genome_db)
    updateSelectInput(session, "sample_librarycomplexity", choices=sample_names,select=NULL)
    updateSelectInput(session, "sample_fragmentsize", choices=sample_names,select=NULL);
    updateSelectInput(session, "sel_sample_for_npositioning", choices=sample_names, select=NULL);
        

    samples_df <- data.frame(BamFile = bam_files, IndexFile = bai_files, row.names = sample_names )
    my_values$base_dir <-  base_dir
    my_values$samples_df <- samples_df
    tags_integer_types <- c(
        "AM", "AS", "CM", "CP", "FI", "H0", "H1", "H2",
        "HI", "IH", "MQ", "NH", "NM", "OP", "PQ", "SM",
        "TC", "UQ"
    )

    tags_char_types <- c(
        "BC", "BQ", "BZ", "CB", "CC", "CO", "CQ", "CR",
        "CS", "CT", "CY", "E2", "FS", "LB", "MC", "MD",
        "MI", "OA", "OC", "OQ", "OX", "PG", "PT", "PU",
        "Q2", "QT", "QX", "R2", "RG", "RX", "SA", "TS",
        "U2"
    )
   
    updateSelectizeInput(session, "sel_tag_integer_type", choices = tags_integer_types, selected= tags_integer_types)
    updateSelectizeInput(session, "sel_tag_char_type", choices = tags_char_types, selected= tags_char_types)
    

    # library(input$bs_genome_input, character.only = T)
    # bamfile <- my_values$bamfile
    print('sel_sample_for_npositioning')
    bamfile <- file.path(my_values$base_dir,my_values$samples_df[1, 'BamFile'])
        # Create a BamFile object

       print(bamfile)
 

    header <- scanBamHeader(bamfile)
    print('header')

    chromosomes <- names(header[[bamfile]]$targets)

    default_chromosome = NULL
    if ('chr1' %in% chromosomes){
        default_chromosome = 'chr1'
    }
    updateSelectInput(session, "sel_chromosome_heatmap_tab", choices = chromosomes, selected=default_chromosome)






    shinyjs::show(id="qc_parameters")
    shinyjs::disable("initQC")
    samples_df
})


output$bamfiles_uploaded <- reactive({
  return(!is.null(input_files_reactive()))
})
outputOptions(output, 'bamfiles_uploaded', suspendWhenHidden=FALSE)


output$bam_samples_table <-  renderTable({
            input_files_reactive()
}, bordered = TRUE, spacing = "xs", rownames = T)

observeEvent(input$initQC, {
    js$addStatusIcon("input_tab", "loading")
   
    
    shinyjs::show(selector = "a[data-value=\"fragmentsize_tab\"]")
    shinyjs::show(selector = "a[data-value=\"nucleosomepositioning_tab\"]")
    shinyjs::show(selector = "a[data-value=\"heatmap_tab\"]")
    shinyjs::show(selector = "a[data-value=\"librarycomplexity_tab\"]")

   
    # js$collapse("uploadbox")

    # output$plot <- renderPlot({
    #     js$addStatusIcon("tab1", "loading")
    #     withProgress(
    #         message = "Calculation in progress",
    #         detail = "This may take a while...",
    #         value = 0,
    #         {
    #             for (i in 1:15) {
    #                 incProgress(1 / 15)
    #                 Sys.sleep(0.25)
    #             }
    #         }
    #     )
    #     plot(cars)
    #     js$addStatusIcon("tab1", "done")
    #     shinyjs::show(selector = "a[data-value=\"tab2\"]")
    # })
})