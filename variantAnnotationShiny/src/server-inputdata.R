observe({
    js$addStatusIcon("input_tab", "next")
    shinyjs::hide(selector = "a[data-value=\"summary_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"filter_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"annotate_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"plots_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"downloads_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"handoff_tab\"]")
})

my_values <- reactiveValues(
    overview = NULL,
    vcf_path = NULL,
    vcf_index = NULL,
    vcf_name = NULL,
    project_dir = NULL,
    genome = NULL,
    summary = NULL,
    filtered_path = NULL,
    filtered_index = NULL,
    filtered_records = NULL,
    filtered_sample_counts = NULL,
    filter_stats = NULL,
    filter_spec = NULL,
    filter_notes = NULL,
    annotation = NULL,
    cache_status = NULL,
    input_status = NULL
)

vcf_ready <- reactiveVal(FALSE)
summary_ready <- reactiveVal(FALSE)
filter_ready <- reactiveVal(FALSE)
annotation_ready <- reactiveVal(FALSE)

output$vcf_ready <- reactive(vcf_ready())
outputOptions(output, "vcf_ready", suspendWhenHidden = FALSE)
output$summary_ready <- reactive(summary_ready())
outputOptions(output, "summary_ready", suspendWhenHidden = FALSE)
output$filter_ready <- reactive(filter_ready())
outputOptions(output, "filter_ready", suspendWhenHidden = FALSE)
output$annotation_ready <- reactive(annotation_ready())
outputOptions(output, "annotation_ready", suspendWhenHidden = FALSE)

available_genomes <- reactiveVal(installed_genomes())
genome_install <- reactiveValues(process = NULL, log_file = NULL, resource = NULL, status = NULL)

observe({
    choices <- genome_choices(available_genomes())
    if (!length(choices)) {
        updateSelectInput(session, "genome_input",
            choices = c("No reference genome packages are installed" = ""))
        shinyjs::disable("loadVcf")
        return()
    }
    selected <- if (identical(input$data_file_type, "example_vcf") && "hg19" %in% choices) {
        "hg19"
    } else if (!is.null(input$genome_input) && input$genome_input %in% choices) {
        input$genome_input
    } else {
        choices[[1]]
    }
    updateSelectInput(session, "genome_input", choices = choices, selected = selected)
})

selected_resource <- reactive({
    key <- input$genome_input
    if (is.null(key) || !nzchar(key) || is.null(available_genomes()[[key]])) return(NULL)
    available_genomes()[[key]]
})

# ---------------------------------------------------------------------------
# Install additional reference genomes into the writable R library
# ---------------------------------------------------------------------------

# Installing genomes only makes sense where a persistent library exists.
if (is.null(genome_install_library())) removeUI(selector = "#genome_install_panel")

genome_install_choices <- reactive({
    catalog <- genome_resources()
    genome_choices(catalog[setdiff(names(catalog), names(available_genomes()))])
})

observe({
    choices <- genome_install_choices()
    if (!length(choices)) {
        updateSelectInput(session, "genome_install_choice",
            choices = c("All catalogued genomes are installed" = ""))
        shinyjs::disable("genome_install")
        return()
    }
    updateSelectInput(session, "genome_install_choice", choices = choices)
    if (is.null(genome_install$process)) shinyjs::enable("genome_install")
})

output$genome_install_library <- renderText({
    lib <- genome_install_library()
    if (is.null(lib)) {
        "No writable R library is available, so genomes cannot be installed from this session."
    } else {
        paste("Packages are installed into", lib)
    }
})

observeEvent(input$genome_install, {
    key <- input$genome_install_choice
    resource <- if (!is.null(key) && nzchar(key)) genome_resources()[[key]] else NULL
    if (is.null(resource) || !is.null(genome_install$process)) return()
    lib <- genome_install_library()
    if (is.null(lib)) {
        showNotification("No writable R library is available for installation.", type = "error")
        return()
    }
    packages <- missing_genome_packages(resource)
    if (!length(packages)) {
        available_genomes(installed_genomes())
        return()
    }
    log_file <- tempfile(pattern = "genome-install-", fileext = ".log")
    genome_install$log_file <- log_file
    genome_install$resource <- resource
    genome_install$status <- sprintf("Installing %s: %s", resource$label, paste(packages, collapse = ", "))
    genome_install$process <- tryCatch(
        start_genome_install(packages, lib, log_file),
        error = function(error) {
            genome_install$status <- paste("Could not start the installation:", conditionMessage(error))
            NULL
        }
    )
    if (!is.null(genome_install$process)) shinyjs::disable("genome_install")
})

observe({
    process <- genome_install$process
    if (is.null(process)) return()
    invalidateLater(2000, session)
    if (process$is_alive()) return()
    resource <- genome_install$resource
    lib <- genome_install_library()
    if (!is.null(lib)) .libPaths(unique(c(lib, .libPaths())))
    still_missing <- missing_genome_packages(resource)
    genome_install$process <- NULL
    if (length(still_missing)) {
        genome_install$status <- sprintf("Installation of %s failed; still missing: %s. See the log below. If this server has no internet access, use the command under \"No internet on this server?\".",
                                         resource$label, paste(still_missing, collapse = ", "))
        showNotification(genome_install$status, type = "error", duration = NULL)
    } else {
        genome_install$status <- paste(resource$label, "is installed and now listed under Reference genome.")
        showNotification(genome_install$status, type = "message")
        available_genomes(installed_genomes())
    }
    shinyjs::enable("genome_install")
})

output$genome_install_status <- renderText({
    status <- genome_install$status
    if (is.null(status)) "" else status
})

output$genome_install_log <- renderText({
    if (!is.null(genome_install$process)) invalidateLater(2000, session)
    log_file <- genome_install$log_file
    if (is.null(log_file) || !file.exists(log_file)) return("")
    lines <- readLines(log_file, warn = FALSE)
    paste(utils::tail(lines, 12), collapse = "\n")
})

output$genome_install_offline <- renderText({
    key <- input$genome_install_choice
    resource <- if (!is.null(key) && nzchar(key)) genome_resources()[[key]] else NULL
    lib <- genome_install_library()
    if (is.null(resource) || is.null(lib)) return("")
    offline_install_instructions(resource, lib)
})

output$rsid_note <- renderUI({
    resource <- selected_resource()
    if (is.null(resource) || !snplocs_available(resource)) return(NULL)
    helpText("dbSNP data are available for this genome: rsIDs will be included when you run Annotate.")
})

reset_downstream <- function(from = c("filter", "annotate")) {
    from <- match.arg(from)
    if (identical(from, "filter")) {
        filter_ready(FALSE)
        my_values$filtered_path <- NULL
        my_values$filtered_index <- NULL
        my_values$filtered_records <- NULL
        my_values$filtered_sample_counts <- NULL
        my_values$filter_stats <- NULL
        shinyjs::hide(selector = "a[data-value=\"annotate_tab\"]")
    }
    annotation_ready(FALSE)
    my_values$annotation <- NULL
    my_values$cache_status <- NULL
    shinyjs::hide(selector = "a[data-value=\"plots_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"downloads_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"handoff_tab\"]")
}

# Project name: the shared input in a private instance, a throw-away session
# name otherwise (nothing persists on a public instance).
current_project_name <- function() {
    name <- input$nasqar_project_name
    if (private_space && !is.null(name) && nzchar(trimws(name))) trimws(name) else paste0("session-", session$token)
}

resolve_input_source <- function() {
    type <- input$data_file_type
    if (identical(type, "upload_vcf")) {
        files <- input$vcf_files
        if (is.null(files) || !nrow(files)) stop("Upload a VCF file first.", call. = FALSE)
        staging <- tempfile(pattern = "vcf-upload-")
        dir.create(staging)
        for (i in seq_len(nrow(files))) {
            file.symlink(files$datapath[i], file.path(staging, files$name[i]))
        }
        vcf_names <- files$name[grepl("\\.(vcf|vcf\\.gz|vcf\\.bgz)$", files$name, ignore.case = TRUE)]
        if (length(vcf_names) != 1L) {
            stop("Upload exactly one VCF (.vcf, .vcf.gz or .vcf.bgz), optionally with its .tbi index.", call. = FALSE)
        }
        index_name <- paste0(vcf_names, ".tbi")
        list(
            path = file.path(staging, vcf_names),
            index = if (index_name %in% files$name) file.path(staging, index_name) else NULL,
            project = current_project_name(),
            label = vcf_names,
            type = "upload_vcf",
            copy_indexed = TRUE
        )
    } else if (identical(type, "example_vcf")) {
        path <- system.file("extdata", "chr22.vcf.gz", package = "VariantAnnotation")
        if (!nzchar(path)) stop("The example VCF is not available in this installation.", call. = FALSE)
        list(path = path, index = NULL, project = current_project_name(), label = "chr22.vcf.gz",
             type = "example_vcf", copy_indexed = FALSE)
    } else {
        path <- nasqar_private_path(input$hpc_vcf_path, what = "VCF file")
        list(path = path, index = NULL, project = current_project_name(), label = basename(path),
             type = "hpc_vcf_file", copy_indexed = FALSE)
    }
}

vcf_expected <- c(
    "A VCF file (VCFv4.x): '##fileformat=VCFv4...' meta lines, a '#CHROM POS ID REF ALT QUAL FILTER INFO ...' column line, then one tab-separated variant record per line",
    "Plain text (.vcf), gzip-compressed (.vcf.gz) or bgzip-compressed (.vcf.bgz)",
    "Optionally its tabix index (.tbi) uploaded alongside a bgzip-compressed file",
    "Records sorted by chromosome and position (needed for tabix indexing)"
)

# A VCF that cannot be compressed, indexed or parsed: explain and show its first
# lines (readLines decompresses .gz/.bgz on the fly, so the preview works for both).
vcf_input_problem <- function(path, label, error) {
    nasqar_input_problem(
        sprintf("%s could not be read as a VCF: %s", label, conditionMessage(error)),
        vcf_expected, path = path, label = "VCF file"
    )
}

observeEvent(input$loadVcf, {
    js$addStatusIcon("input_tab", "loading")
    vcf_ready(FALSE)
    summary_ready(FALSE)
    my_values$summary <- NULL
    my_values$input_status <- NULL
    reset_downstream("filter")
    shinyjs::hide(selector = "a[data-value=\"summary_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"filter_tab\"]")

    resource <- selected_resource()
    outcome <- tryCatch({
        if (is.null(resource)) stop("Select a reference genome.", call. = FALSE)
        withProgress(message = "Loading VCF", value = 0.05, {
            source_info <- resolve_input_source()
            project_dir <- project_directory(source_info$project)
            setProgress(0.15, detail = "Checking compression and index")
            prepared <- tryCatch(
                prepare_vcf_input(
                    source_info$path, file.path(project_dir, "input"), source_info$index,
                    copy_indexed = isTRUE(source_info$copy_indexed)
                ),
                error = function(error) vcf_input_problem(source_info$path, source_info$label, error)
            )
            if (prepared$converted) {
                showNotification("The VCF was bgzip-compressed and tabix-indexed once into the project directory.",
                                 type = "message", duration = 8)
            }
            setProgress(0.6, detail = "Reading header and index")
            overview <- tryCatch(
                vcf_overview(prepared$path, prepared$index),
                error = function(error) vcf_input_problem(source_info$path, source_info$label, error)
            )
            setProgress(0.75, detail = paste("Loading", resource$label, "annotation packages"))
            genome <- load_genome_resource(resource, with_snplocs = TRUE)
            setProgress(1)
            list(prepared = prepared, overview = overview, genome = genome,
                 project_dir = project_dir, label = source_info$label, source = source_info)
        })
    }, error = function(error) error)

    if (inherits(outcome, "error")) {
        js$addStatusIcon("input_tab", "fail")
        my_values$input_status <- conditionMessage(outcome)
        # An input problem has already explained itself in a modal.
        if (!inherits(outcome, "shiny.silent.error")) {
            showNotification(conditionMessage(outcome), type = "error", duration = 12)
        }
        return()
    }

    my_values$vcf_path <- outcome$prepared$path
    my_values$vcf_index <- outcome$prepared$index
    my_values$vcf_name <- outcome$label
    my_values$overview <- outcome$overview
    my_values$genome <- outcome$genome
    my_values$project_dir <- outcome$project_dir
    vcf_ready(TRUE)

    if (private_space) {
        source_path <- if (identical(outcome$source$type, "hpc_vcf_file")) outcome$source$path else outcome$prepared$path
        nasqar_project_state_write(
            outcome$project_dir, project_app, basename(outcome$project_dir), "loaded",
            input_label = outcome$label,
            source_type = outcome$source$type,
            source_path = source_path,
            source_signature = nasqar_file_signature(source_path),
            vcf_path = outcome$prepared$path,
            vcf_index = outcome$prepared$index,
            vcf_name = outcome$label,
            genome = resource$key
        )
        project_refresh(basename(outcome$project_dir))
    }

    updateSelectizeInput(session, "chromosomes",
        choices = names(outcome$overview$contig_counts), selected = character(0), server = TRUE)

    js$addStatusIcon("input_tab", "done")
    shinyjs::show(selector = "a[data-value=\"summary_tab\"]")
    shinyjs::show(selector = "a[data-value=\"filter_tab\"]")
    js$addStatusIcon("summary_tab", "done")
    js$addStatusIcon("filter_tab", "next")

    if (outcome$overview$total_records <= yield_size_limit()) {
        run_record_summary()
    }
})

output$input_status <- renderText(my_values$input_status)

output$project_path_text <- renderText({
    req(my_values$project_dir)
    paste("Project directory:", my_values$project_dir)
})

output$overview_table <- DT::renderDataTable({
    req(my_values$overview)
    overview <- my_values$overview
    genome <- my_values$genome
    txdb_levels <- GenomeInfoDb::seqlevels(genome$txdb)
    contigs <- names(overview$contig_counts)
    style <- tryCatch(GenomeInfoDb::seqlevelsStyle(txdb_levels)[1], error = function(e) NA)
    mapped <- tryCatch({
        copy <- contigs
        if (!is.na(style) && !identical(overview$contig_style, style)) {
            copy <- GenomeInfoDb::mapSeqlevels(contigs, style)
            copy <- ifelse(is.na(copy), contigs, copy)
        }
        copy
    }, error = function(e) contigs)
    known <- sum(mapped %in% txdb_levels)
    rows <- data.frame(
        Item = c("File", "Records (from index)", "Samples", "Contigs with records",
                 "Contig naming style", "Contigs known to the gene model",
                 "INFO fields", "FORMAT fields", "Reference genome", "Annotation chunk size"),
        Value = c(
            my_values$vcf_name,
            format(overview$total_records, big.mark = ","),
            length(overview$samples),
            length(contigs),
            overview$contig_style,
            paste(known, "of", length(contigs)),
            paste(overview$info_fields, collapse = ", "),
            paste(overview$format_fields, collapse = ", "),
            genome$label,
            paste(format_count(annotation_chunk_size()), "records")
        ),
        stringsAsFactors = FALSE
    )
    DT::datatable(rows, rownames = FALSE, options = list(dom = "t", pageLength = 20))
})
