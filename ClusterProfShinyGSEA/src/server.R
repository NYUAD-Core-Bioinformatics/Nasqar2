# Max upload size
options(shiny.maxRequestSize = 60 * 1024^2)

# Define server
server <- function(input, output, session) {
    # Private-space projects: projects/ClusterProfShinyGSEA/<name> (shared convention)
    source("nasqar-project.R", local = TRUE)
    project_app <- "ClusterProfShinyGSEA"
    private_space <- nasqar_project_available(project_app)
    # A restore names the project before the name box has round-tripped.
    project_name_override <- reactiveVal(NULL)
    current_project_name <- function() {
        name <- project_name_override()
        if (is.null(name)) name <- input$nasqar_project_name
        if (private_space && !is.null(name) && nzchar(trimws(name))) trimws(name) else paste0("session-", session$token)
    }
    restored_input <- reactiveVal(NULL)
    restored_file_type <- reactiveVal(NULL)

    # Private data root: input files read in place (private instance only)
    data_root <- Sys.getenv("NASQAR_DATA_ROOT", unset = "")
    data_root <- if (nzchar(data_root)) normalizePath(data_root, winslash = "/", mustWork = FALSE) else NULL
    if (!is.null(data_root)) {
        browse_roots <- nasqar_browse_roots()
        shinyFiles::shinyFileChoose(input, "browse_private_file", roots = browse_roots, session = session,
                                    filetypes = c("csv", "txt", "tsv", "gz"))
        observeEvent(input$browse_private_file, {
            selected <- tryCatch(shinyFiles::parseFilePaths(browse_roots, input$browse_private_file), error = function(e) NULL)
            if (!is.null(selected) && nrow(selected)) updateTextInput(session, "private_file_path", value = as.character(selected$datapath[1]))
        }, ignoreInit = TRUE)
    } else {
        removeUI(selector = "#browse_private_file")
        shinyjs::runjs("$('input[name=data_file_type][value=privatefile]').closest('.radio').remove()")
    }

    source("exchange-helpers.R", local = TRUE)
    source("plot-helpers.R",     local = TRUE)
    source("server-inputData.R", local = TRUE)
    #
    source("server-gseGo.R", local = TRUE)
    #
    source("server-plots.R", local = TRUE)
    source("server-project.R", local = TRUE)


    GotoTab <- function(name) {
        # updateTabItems(session, "tabs", name)

        shinyjs::show(selector = paste0("a[data-value=\"", name, "\"]"))

        shinyjs::runjs("window.scrollTo(0, 0)")
    }
}
