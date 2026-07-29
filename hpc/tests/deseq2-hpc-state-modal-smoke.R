app_dir <- Sys.getenv(
    "NASQAR_DESEQ2_APP_DIR",
    unset = "/srv/shiny-server/deseq2shiny/src"
)
setwd(app_dir)

source("global.R", local = globalenv())
source("ui.R", local = globalenv())
source("server.R", local = globalenv())

shiny::testServer(server, {
    session$setInputs(showStateModal = 1)
    session$flushReact()
})

cat("DESeq2 HPC state modal opened without a server error.\n")
