options(shiny.maxRequestSize = 30 * 1024^4)

server <- function(input, output, session) {
    observe({
        # Ping the server every 10 seconds to keep the connection alive
        invalidateLater(10000, session)
        input$keepAlive
    })
    source("server-inputdata.R", local = TRUE)
    source("server-qualityprofile.R", local = TRUE)
    source("server-filter_and_trim.R", local = TRUE)
    source("server-errorRates.R", local = TRUE)
    source("server-mergePairedReads.R", local = TRUE)
    source("server-trackReads.R", local = TRUE)
    source("server-taxonomy.R", local = TRUE)
    source("server-alphaDiversity.R", local = TRUE)
}
