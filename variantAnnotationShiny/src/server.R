options(shiny.maxRequestSize = 30 * 1024^4)

server <- function(input, output, session) {
    hpc_workers <- available_workers(limit = 8L)

    data_root <- Sys.getenv("NASQAR_DATA_ROOT", unset = "")
    data_root <- if (nzchar(data_root)) {
        normalizePath(data_root, winslash = "/", mustWork = FALSE)
    } else {
        NULL
    }

    # Server-side browser jailed to the private data root; it only fills the path box.
    if (!is.null(data_root)) {
        browse_roots <- nasqar_browse_roots()
        shinyFiles::shinyFileChoose(input, "browse_vcf_path", roots = browse_roots, session = session, filetypes = c("vcf", "gz", "bgz"))
        observeEvent(input$browse_vcf_path, {
            selected <- tryCatch(shinyFiles::parseFilePaths(browse_roots, input$browse_vcf_path), error = function(e) NULL)
            if (!is.null(selected) && nrow(selected)) {
                updateTextInput(session, "hpc_vcf_path", value = as.character(selected$datapath[1]))
            }
        }, ignoreInit = TRUE)
    } else {
        removeUI(selector = "#browse_vcf_path")
        shinyjs::runjs("$('input[name=data_file_type][value=hpc_vcf_file]').closest('.radio').remove()")
    }

    resolve_hpc_file <- function(path) {
        if (is.null(data_root)) {
            stop("This instance has no private space configured.", call. = FALSE)
        }
        if (!nzchar(trimws(path))) {
            stop("Enter the path of a VCF file in the private space.", call. = FALSE)
        }
        resolved <- tryCatch(
            normalizePath(trimws(path), winslash = "/", mustWork = TRUE),
            error = function(error) NULL
        )
        if (is.null(resolved) || dir.exists(resolved)) {
            stop("The VCF path does not exist, is not readable, or is a directory.", call. = FALSE)
        }
        inside <- nasqar_path_inside_roots(resolved)
        if (!inside) {
            stop("The VCF must be in the private space.", call. = FALSE)
        }
        if (!grepl("\\.(vcf|vcf\\.gz|vcf\\.bgz)$", resolved, ignore.case = TRUE)) {
            stop("The file must end in .vcf, .vcf.gz or .vcf.bgz.", call. = FALSE)
        }
        resolved
    }

    # Private-space projects: projects/VariantAnnotation/<name> (shared convention)
    source("nasqar-project.R", local = TRUE)
    project_app <- "VariantAnnotation"
    project_legacy_env <- "NASQAR_VARIANT_PROJECT_DIR"
    private_space <- nasqar_project_available(project_app, project_legacy_env)
    project_directory <- function(name) {
        path <- nasqar_project_dir(project_app, name, project_legacy_env, fallback = "variant-project")
        if (!dir.exists(path) || file.access(path, 2) != 0) {
            stop("The Variant Annotation project directory is not writable.", call. = FALSE)
        }
        path
    }

    observe({
        invalidateLater(10000, session)
        input$keepAlive
    })

    source("plot-downloads.R", local = TRUE)
    source("server-inputdata.R", local = TRUE)
    source("server-summary.R", local = TRUE)
    source("server-filter.R", local = TRUE)
    source("server-annotate.R", local = TRUE)
    source("server-plots.R", local = TRUE)
    source("server-downloads.R", local = TRUE)
    source("server-handoff.R", local = TRUE)
    source("server-project.R", local = TRUE)
}
