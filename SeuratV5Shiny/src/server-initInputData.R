
# What a usable counts input looks like; shown to the user when a file cannot be read.
counts_matrix_expected <- c(
  "A comma- or tab-delimited text table (.csv, .tsv, .txt)",
  "A header row holding the cell barcodes / sample names",
  "First column: gene names (unique, one gene per row)",
  "Every other column: numeric counts, one column per cell"
)
tenx_expected <- c(
  "A 10x Genomics (Cell Ranger) matrix directory with exactly three files",
  "barcodes.tsv or barcodes.tsv.gz: one cell barcode per line",
  "features.tsv(.gz) (Cell Ranger >= 3) or genes.tsv(.gz) (Cell Ranger < 3): gene ids and names",
  "matrix.mtx or matrix.mtx.gz: the sparse counts in Matrix Market format"
)

# Separator of a delimited counts file, detected from its header line (for the script log).
counts_separator <- function(path) {
  header <- nasqar_file_head(path, n = 1L, width = .Machine$integer.max)
  if (!length(header)) return(",")
  fields <- vapply(c("\t", ",", ";"), function(sep) length(strsplit(header, sep, fixed = TRUE)[[1]]), integer(1))
  c("\t", ",", ";")[which.max(fields)]
}

# Read a genes x cells counts table: checked with nasqar_read_table(), then the
# first column becomes the row names, exactly as read.csv(row.names = 1) produced.
read_counts_matrix <- function(path, ...) {
  seqdata <- nasqar_read_table(
    path, counts_matrix_expected,
    check = function(df) nasqar_check_count_table(df, id_columns = 1, min_samples = 2),
    label = "counts matrix", row.names = NULL, ...
  )
  rownames(seqdata) <- as.character(seqdata[[1]])
  seqdata[, -1, drop = FALSE]
}

# Read a 10x directory; a failure explains which files were found instead.
read_10x_directory <- function(dir) {
  tryCatch(Read10X(data.dir = dir), error = function(e) {
    found <- list.files(dir)
    nasqar_input_problem(
      sprintf("The 10x directory could not be read (%s). It contains: %s.",
              conditionMessage(e),
              if (length(found)) paste(head(found, 10), collapse = ", ") else "no files"),
      tenx_expected, path = NULL, label = "10x directory"
    )
  })
}

observe({

  shinyjs::hide(selector = "a[data-value=\"qcFilterTab\"]")
  shinyjs::hide(selector = "a[data-value=\"vlnplot\"]")
  shinyjs::hide(selector = "a[data-value=\"filterNormSelectTab\"]")
  
  shinyjs::hide(selector = "a[data-value=\"vizPcaPlot\"]")
  shinyjs::hide(selector = "a[data-value=\"pcaPlot\"]")
  shinyjs::hide(selector = "a[data-value=\"heatmapPlot\"]")
  shinyjs::hide(selector = "a[data-value=\"runPcaTab\"]")
  shinyjs::hide(selector = "a[data-value=\"jackStrawPlot\"]")
  shinyjs::hide(selector = "a[data-value=\"clusterCells\"]")
  shinyjs::hide(selector = "a[data-value=\"nonLinReductTab\"]")
  shinyjs::hide(selector = "a[data-value=\"finishTab\"]")
  shinyjs::hide(selector = "a[data-value=\"findMarkersTab\"]")
  shinyjs::hide(selector = "a[data-value=\"vizMarkersTab\"]")

  inputDataReactive()


})


inputDataReactive <- reactive({

  print("inputting data")

  query <- parseQueryString(session$clientData$url_search)

  # Check if example selected, or if not then ask to upload a file.
  shiny:: validate(
    need( identical(input$data_file_type,"examplecounts")|identical(input$data_file_type,"hpcScratch")|(!is.null(input$datafile))|(!is.null(query[['countsdata']])),
         message = "Please select a file")
  )

  removeNotification("errorNotify")
  
  if (!is.null(query[['countsdata']]) ) {
    inFile = decryptUrlParam(query[['countsdata']])

    shinyjs::show(selector = "a[data-value=\"datainput\"]")
    shinyjs::disable("data_file_type")
    shinyjs::disable("datafile")
    #js$collapse("uploadbox")

  }
  else
  {
    inFile <- input$datafile
    # if (is.null(inFile))
    #   return(NULL)
    #
    # inFile = inFile$datapath
  }

  #inFile <- input$datafile

  validate(need(
    tryCatch({
      scriptCommands = c()

      if (identical(input$data_file_type, "hpcScratch")) {
        req(input$load_hpc_data > 0)
        validate(need(nzchar(trimws(input$hpc_data_path)), "Enter a path in the private space."))
        hpc_path <- nasqar_private_path(input$hpc_data_path, directory = NA, what = "counts file or 10x directory")
        js$addStatusIcon("datainput", "loading")
        if (dir.exists(hpc_path)) {
          seqdata <- read_10x_directory(hpc_path)
          scriptCommands$readCounts <- sprintf(
            "counts.data <- Read10X(data.dir = %s)",
            dQuote(hpc_path)
          )
        } else {
          seqdata <- read_counts_matrix(hpc_path, check.names = FALSE)
          separator <- counts_separator(hpc_path)
          scriptCommands$readCounts <- sprintf(
            "counts.data <- read.csv(%s, header=TRUE, sep=%s, row.names=1)",
            dQuote(hpc_path),
            dQuote(separator)
          )
        }
        isolate(myValues$scriptCommands <- scriptCommands)
        js$addStatusIcon("datainput", "done")
        js$collapse("uploadbox")
        return(list(data = seqdata))
      }
      
      if (!is.null(inFile) && !is.null(query[['countsdata']])) {
        #js$addStatusIcon("datainput","loading")
        seqdata <- read_counts_matrix(inFile)
        print('uploaded seqdata')
        isolate({
          myValues$scriptCommands$readCounts = paste0('counts.data <- read.csv("/path/to/csv/counts/file.csv", header=TRUE, sep=",", row.names = 1)')
        })
        
        return(list('data'=seqdata))
      }
      else if(!is.null(inFile) & input$data_file_type=="upload10x")
      {
        js$addStatusIcon()
        shiny:: validate(
          need(dim(inFile)[1] == 3,
               message = "need 3 files, 1 .mtx and 2 .tsv")
        )
        
        
        filesdir = dirname(inFile[1,4])
        
        #inFile = inFile$datapath
        
        file.rename(inFile$datapath[1],paste0(filesdir,'/',inFile$name[1]))
        file.rename(inFile$datapath[2],paste0(filesdir,'/',inFile$name[2]))
        file.rename(inFile$datapath[3],paste0(filesdir,'/',inFile$name[3]))
        
        withProgress(message = "Reading 10X data, please wait ...",{
          
          # cellranger < 3.0
          if(any(list.files(filesdir) == "genes.tsv.gz"))
          {
            file.rename(file.path(filesdir,"genes.tsv.gz"), file.path(filesdir,"features.tsv.gz"))
          }
          
          shiny::setProgress(value = 0.8)
          
          pbmc.data <- read_10x_directory(filesdir)
          scriptCommands$readCounts = paste0('counts.data <- Read10X(data.dir = "/path/to/10x/dir/")')
          isolate({
            myValues$scriptCommands = scriptCommands
          })
          
        })
        
        js$collapse("uploadbox")
        return(list('data'=pbmc.data))
      }
      else if(!is.null(inFile) & input$data_file_type=="uploadNonUmi")
      {
        js$addStatusIcon("datainput","loading")
        
        inFile = inFile$datapath
        
        seqdata <- read_counts_matrix(inFile[1])

        separator <- counts_separator(inFile[1])
        scriptCommands$readCounts = sprintf(
          'counts.data <- read.csv("/path/to/csv/counts/file.csv", header=TRUE, sep="%s", row.names = 1)',
          if (identical(separator, "\t")) "\\t" else separator
        )

        print('uploaded seqdata')

        isolate({
          myValues$scriptCommands = scriptCommands
        })
        
        js$addStatusIcon("datainput","done")
        js$collapse("uploadbox")
        return(list('data'=seqdata))
      }
      else{
        if(input$data_file_type=="examplecounts")
        {
          js$addStatusIcon("datainput","loading")
          pbmc.data <- Read10X(data.dir = "www/hg19/")
          
          isolate({
            myValues$scriptCommands$readCounts = paste0('counts.data <- Read10X(data.dir = "/path/to/10x/dir/hg19/")')
          })
          
          js$addStatusIcon("datainput","done")
          js$collapse("uploadbox")
          return(list('data'=pbmc.data))
        }
        return(NULL)
      }
      
    },
    error = function(e)
    {
      # An input problem already explained itself (modal); let the validation
      # error reach the outputs instead of turning it into a generic "Error".
      if (inherits(e, "shiny.silent.error")) {
        js$addStatusIcon("datainput","fail")
        stop(e)
      }
      myValues$status = paste("Upload File Error: ",e$message)
      
      showNotification(id="errorNotify", myValues$status, type = "error", duration = NULL)
      
      
      js$addStatusIcon("datainput","fail") 
    }
    ),
    "Error"
  )) 
  
  
})

decryptUrlParam = function (cipher)
{
  keyHex <- readr::read_file("private.txt")

  key = hex2bin(keyHex)
  cipher = hex2bin(cipher)

  orig <- simple_decrypt(cipher, key)

  unserialize(orig)
}

output$example_counts_file <- nasqar_download_handler(filename="examplecounts_short.csv",
                                              content=function(file){
                                                file.copy("www/examplecounts_short.csv",file)
                                              })


output$countdataDT <- renderDataTable({
  tmp <- inputDataReactive()

  if(!is.null(tmp))
  {
    if(ncol(tmp$data) > 20)
     return(as.matrix(tmp$data[,1:20]))

    return(as.matrix(tmp$data))

  }

},
options = list(scrollX = TRUE))

output$inputInfo <- renderText({

  tmp <- inputDataReactive()

  if(!is.null(tmp))
  {
    outStr = paste0(
      paste("dense size: ", object.size(x = as.matrix(x = tmp$data))),
      '\n',
      paste("sparse size: ", object.size(x = tmp$data)))

  }


})


# check if a file has been uploaded and create output variable to report this
output$fileUploaded <- reactive({

  return(!is.null(inputDataReactive()))

})
outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)


observe({
  initSeuratObjReactive()
})

initSeuratObjReactive <-
  eventReactive(input$upload_data,
                ignoreNULL = TRUE,
                {
                  withProgress(message = "Initializing Seurat Object, please wait",{


                    updateCollapse(session,id =  "input_collapse_panel", open="analysis_panel",
                                   style = list("analysis_panel" = "success",
                                                "data_panel"="primary"))

                    rawData = inputDataReactive()$data
                    js$addStatusIcon("datainput","loading")
                    

                    pbmc <- CreateSeuratObject(counts = rawData, min.cells = input$mincells, min.features = input$mingenes,
                                               project = input$projectname, names.delim = "\\-", names.field = 2)
                    
                    myValues$scriptCommands$createSeurat = paste0('pbmc <- CreateSeuratObject(counts = counts.data, min.cells = ',input$mincells,', min.features = ',input$mingenes,',
                                               project = "',input$projectname,'")')
                    
                    if(all(is.na(Idents(pbmc)))) {
                      idents <- rep.int("1", times = length(Idents(pbmc)))
                      pbmc[['orig.ident']] <- idents
                      Idents(pbmc) <- "1"
                    }
                    
                    shiny::setProgress(value = 0.8, detail = "Done.")
                    js$addStatusIcon("datainput","done")
                    shinyjs::show(selector = "a[data-value=\"qcFilterTab\"]")
                    js$addStatusIcon("qcFilterTab","next")
                    
                    shinyjs::runjs("window.scrollTo(0, 0)")

                    return(list('pbmc'=pbmc))
                  })})


vectorToStr <- function(v)
{
  paste0('c(', paste0("'",v,"'", collapse = ","), ')')
}
