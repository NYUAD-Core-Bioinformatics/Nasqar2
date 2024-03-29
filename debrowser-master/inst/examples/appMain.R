library(plotly)
library(debrowser)
library(DESeq2)
source("../../R/mainScatter.R")
source("../../R/deprogs.R")

header <- dashboardHeader(
    title = "DEBrowser Main Plots"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DEAnalysis",
    menuItem("Main", tabName = "Main"),
    mainPlotControlsUI("main"),
    plotSizeMarginsUI("main")))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="Main", getMainPlotUI("main"),
                column(4,
                       verbatimTextOutput("main_hover"),
                       verbatimTextOutput("main_selected")
                )
        )
    ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    #Example usage with demodata
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    dat <-c()
    dat$columns <- c( "control_rep1", "control_rep2", "control_rep3",
                      "exper_rep1", "exper_rep2", "exper_rep3")
    dat$conds <- factor( c("Control", "Control", "Control",
                       "Treat", "Treat", "Treat") )
    
    cond_names <- c("Control", "Treat")
    
    
    dat$data <- data.frame(demodata[, dat$columns])
    
    # You can also use your dataset by reading your data from a file like below;
    # The data in this commented out exampele is not supplied but these lines 
    # can give you an idea about how to read the data from a file;
    #
    # data  <- read.table("~/Downloads/shKRAS.tsv", header=T, row.names=1, sep="\t")
    # dat$columns <- c("CNT.2", "CNT.3", "CNT.4", 
    #               "shKRAS_T1", "shKRAS_T2", "shKRAS_T3")
    # dat$conds <- factor( c("Control", "Control", "Control",
    #                        "shKRAS", "shKRAS", "shKRAS") )
    # dat$data <- data.frame(data[, dat$columns])
    #
    xdata <- generateTestData(dat)
    selected <- callModule(debrowsermainplot, "main", xdata, cond_names)
    
    output$main_hover <- renderPrint({
        selected$shgClicked()
    })
    output$main_selected <- renderPrint({
        selected$selGenes()
    })
}
shinyApp(ui, server)
