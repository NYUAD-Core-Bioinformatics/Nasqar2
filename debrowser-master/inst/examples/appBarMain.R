library(debrowser)
library(plotly)
source("../../R/barmain.R")

options(warn =-1)

header <- dashboardHeader(
  title = "DEBrowser Bar Plots"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DEAnlysis",
      menuItem("BarMain", tabName = "BarMain"),
      textInput("genename", "Gene/Region Name", value = "Foxa3" ),
      plotSizeMarginsUI("barmain", h=200, t = 30)
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName="BarMain", 
        fluidRow(
        column(12,
            getBarMainPlotUI("barmain")))
    )
  ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    load(system.file("extdata", "demo", "demodata.Rda",
                   package = "debrowser"))
    
    cond_names <- c("Control", "Treat")
    observe({
        if (!is.null(input$genename))
            callModule(debrowserbarmainplot, "barmain", demodata, 
                metadatatable$sample,  
                metadatatable$treatment, cond_names, input$genename)
    })
}

shinyApp(ui, server)
