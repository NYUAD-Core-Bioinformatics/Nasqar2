#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#





server <- function(input, output, session) {
  # Initialize the number of uploaders
  num_uploaders <- reactiveVal(1)
  
  observeEvent(input$addUploader, {
    cat('addUploader',num_uploaders())
    num_uploaders(num_uploaders() + 1)
    print(num_uploaders())
  })
  
  observeEvent(input$removeUploader, {
    if (num_uploaders() > 0) {
      num_uploaders(num_uploaders() - 1)
    }
  })
  
  input_files_reactive <- eventReactive(input$upload, {
    
    n <- input$sample_count
    print(n)
    
    l <- NULL
    
    l <- lapply(1:n, function(i) {
      input_sample_name <- paste0("input_sample_name_", i)
      input_sample_type <- paste0("input_sample_type_", i)
      print(input_sample_name)
      #req(input[[input_sample_name]])
      sample_name<-input[[input_sample_name]]
      sample_type <- input[[input_sample_type]]
      file_input <- paste0("sample_", i)
      
      input_files <- input[[file_input]]
      indexes <- 1: length(input[[file_input]]$name)
      
      base_dir <- tempfile()
      sample_dir <- file.path(base_dir, sample_name)
      dir.create(sample_dir,recursive=TRUE)
      print(sample_dir)
      print(input_files)
      print(indexes)
      
      l <- lapply(indexes, function(i) {
   
        datapath <- input_files$datapath[[i]]
        name <-  input_files$name[[i]]
       
        
        newpath<-file.path(sample_dir, name)
        file.remove(file.path(sample_dir, name))
        file.symlink(datapath, file.path(sample_dir, name))
        # file.copy(datapath, file.path(base_dir, name))
        c(name, newpath)
      })
      
      i <- 1
      barcode_idx <- 0
      features_idx <- 0
      matrix_idx  <- 0
      for(item in l){
        if(item[1] == 'barcodes.tsv.gz'){
          barcode_idx<-i
        }
        if(item[1] == 'features.tsv.gz'){
          features_idx<-i
        }
        if(item[1] == 'matrix.mtx.gz'){
          matrix_idx<-i
        }
        i <- i + 1
      }
      list(sample=sample_name, SampleType = sample_type,  barcode = l[[barcode_idx]][2],features = l[[features_idx]][2],matrix_idx=l[[matrix_idx]][2])
    
    })
    
    print(l)
    
    
    
    samples_df <- data.frame(do.call(rbind, l))
    
    print(samples_df)
    
    return (samples_df)
   
    
  })
  
  output$uploaders <- renderUI({
    
    n <- input$sample_count
   
    uploaders <- lapply(1:n, function(i) {
      cat('uploaders',num_uploaders(), i)
      wellPanel(
        textInput(paste0("input_sample_name_", i), label = paste("Sample Name", i), value="" ),
        selectInput(paste0("input_sample_type_", i), 'Sample Type', choices = c('wild', 'ko')),
        fileInput( paste0("sample_", i), paste("Upload barcode, feature, matrix"), multiple = T, accept = ".gz")
        
        )
    })
    do.call(column, c(12,uploaders))
  })
  
  #output$all_samples_uploaded <- reactive({
  #  return(!is.null(input_files_reactive()))
  #})
  #outputOptions(output, 'all_samples_uploaded', suspendWhenHidden=FALSE)
  
  
  
  
  output$samples_table <-  DT::renderDataTable({
    input_files_reactive()
  },options = list(scrollX = TRUE, pageLength = 15))
  
}


