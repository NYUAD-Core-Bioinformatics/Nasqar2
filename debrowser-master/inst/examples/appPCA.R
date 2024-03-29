library(debrowser)

header <- shinydashboard::dashboardHeader(
    title = "DEBrowser PCA Plots"
)
sidebar <- shinydashboard::dashboardSidebar(  
    shinydashboard::sidebarMenu(id="DataAssessment",
        shinydashboard::menuItem("PCA", tabName = "PCA"),
        shinydashboard::menuItem("PCA Options",
    pcaPlotControlsUI("pca")),
    plotSizeMarginsUI("pca", w=600, h=400, t=0, b=0, l=0, r=0)
    ))

body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
        shinydashboard::tabItem(tabName="PCA", getPCAPlotUI("pca"),
                column(4,
                       verbatimTextOutput("pca_hover"),
                       verbatimTextOutput("pca_selected")
                )
        )
    ))

ui <- shinydashboard::dashboardPage(header, sidebar, body, skin = "blue")


server <- function(input, output, session) {
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    selected <- callModule(debrowserpcaplot, "pca", demodata, metadatatable)
}

shinyApp(ui, server)
