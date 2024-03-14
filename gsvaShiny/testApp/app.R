library(shiny)
library(shinyFiles)

ui <- fluidPage(
  titlePanel("Upload Files to a Specific Folder"),
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("dir", "Select a Folder", title = "Please select a folder:", 
                     buttonLabel = "Select Folder"),
      textInput("subfolder_name", "Enter a Subfolder Name", ""),
      fileInput("files", "Upload Files", multiple = TRUE)
    ),
    mainPanel(
      verbatimTextOutput("file_info")
    )
  )
)

server <- function(input, output, session) {
  volumes <- shinyDirChoose(input, "dir", roots = c(wd = getwd()))
  
  observe({
    shinyFileChoose(input, "files", roots = volumes$dir)
  })
  
  observe({
    req(input$subfolder_name)
    if (!is.null(input$subfolder_name) && input$subfolder_name != "") {
      subfolder_path <- file.path(volumes$dir, input$subfolder_name)
      if (!dir.exists(subfolder_path)) {
        dir.create(subfolder_path)
      }
    }
  })
  
  output$file_info <- renderPrint({
    if (length(input$files$datapath) > 0) {
      file.info(input$files$datapath)
    } else {
      "No files uploaded yet."
    }
  })
}

shinyApp(ui, server)
