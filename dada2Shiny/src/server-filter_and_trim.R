activate_dada_results <- function(seq_type) {
    shinyjs::show(selector = "a[data-value=\"errorRatesTab\"]")
    if (identical(seq_type, "paired")) {
        shinyjs::show(selector = "a[data-value=\"margePairedReadsTab\"]")
        js$addStatusIcon("margePairedReadsTab", "done")
    }
    shinyjs::show(selector = "a[data-value=\"taxanomyTab\"]")
    shinyjs::show(selector = "a[data-value=\"trackReadsTab\"]")
    shinyjs::show(selector = "a[data-value=\"filter_and_trim_tab\"]")
    js$addStatusIcon("filter_and_trim_tab", "done")
    js$addStatusIcon("errorRatesTab", "done")
    js$addStatusIcon("trackReadsTab", "done")
    js$addStatusIcon("taxanomyTab", "next")
    qc_done(TRUE)
}

reactiveInputData <- eventReactive(input$runDADA2, {
    shinyjs::disable("runDADA2")
    on.exit(shinyjs::enable("runDADA2"), add = TRUE)

    # Forked DADA2 workers must receive ordinary values, not Shiny reactives.
    seq_type <- isolate(input$seq_type)
    trunc_fwd <- as.integer(isolate(input$truncLen_fwd))
    trunc_rev <- as.integer(isolate(input$truncLen_rev))
    max_ee_fwd <- as.numeric(isolate(input$maxEE_fwd))
    max_ee_rev <- as.numeric(isolate(input$maxEE_rev))
    selected_sample <- isolate(input$sel_sample_qualityprofile_tab)
    req(selected_sample)

    qc_done(FALSE)
    divergen_done(FALSE)
    if (exists("reactiveTaxonomyData")) reactiveTaxonomyData$taxa <- NULL
    if (exists("selectedTaxonomyRows")) selectedTaxonomyRows(NULL)
    if (exists("alphaDiversityResults")) {
        alphaDiversityResults$ps <- NULL
        alphaDiversityResults$ps.prop <- NULL
        alphaDiversityResults$ord.nmds.bray <- NULL
        alphaDiversityResults$ps.top20 <- NULL
    }

    shinyjs::hide(selector = "a[data-value=\"errorRatesTab\"]")
    shinyjs::hide(selector = "a[data-value=\"margePairedReadsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"trackReadsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"taxanomyTab\"]")
    shinyjs::hide(selector = "a[data-value=\"alphaDiversityTab\"]")
    js$addStatusIcon("filter_and_trim_tab", "loading")

    input_path <- my_values$base_dir
    project_path <- my_values$work_dir
    local_work_path <- my_values$local_work_dir
    req(input_path, project_path, local_work_path, my_values$samples_df)

    sample_names <- row.names(my_values$samples_df)
    fn_fs <- file.path(input_path, my_values$samples_df[, "FASTQ_Fs"])
    fn_rs <- character()
    if (identical(seq_type, "paired")) {
        fn_rs <- file.path(input_path, my_values$samples_df[, "FASTQ_Rs"])
    }

    input_paths <- c(fn_fs, fn_rs)
    input_info <- file.info(input_paths)
    validate(need(
        all(file.exists(input_paths)) && all(!input_info$isdir),
        "One or more FASTQ files are unavailable."
    ))

    parameters <- list(
        seq_type = seq_type,
        trunc_fwd = trunc_fwd,
        trunc_rev = if (identical(seq_type, "paired")) trunc_rev else NULL,
        max_ee_fwd = max_ee_fwd,
        max_ee_rev = if (identical(seq_type, "paired")) max_ee_rev else NULL,
        dada2_version = as.character(utils::packageVersion("dada2"))
    )
    cache_key <- digest::digest(
        list(
            paths = normalizePath(input_paths, mustWork = TRUE),
            sizes = input_info$size,
            mtimes = as.numeric(input_info$mtime),
            parameters = parameters
        ),
        algo = "xxhash64"
    )
    cache_dir <- file.path(project_path, "cache")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

    if (file.exists(cache_file)) {
        cached_result <- tryCatch(readRDS(cache_file), error = function(e) NULL)
        if (!is.null(cached_result)) {
            my_values$cache_status <- paste("Loaded cached DADA2 result:", cache_key)
            activate_dada_results(seq_type)
            return(cached_result)
        }
    }

    my_values$cache_status <- paste("Computing DADA2 result:", cache_key)
    filtered_path <- file.path(local_work_path, cache_key, "filtered")
    dir.create(filtered_path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    filt_fs <- file.path(filtered_path, paste0(sample_names, "_F_filt.fastq.gz"))
    names(filt_fs) <- sample_names
    filt_rs <- character()
    if (identical(seq_type, "paired")) {
        filt_rs <- file.path(filtered_path, paste0(sample_names, "_R_filt.fastq.gz"))
        names(filt_rs) <- sample_names
    }

    withProgress(message = "Running DADA2, please wait", {
        shiny::setProgress(value = 0.1, detail = "Filtering and trimming")
        if (identical(seq_type, "paired")) {
            filtered_counts <- filterAndTrim(
                fn_fs,
                filt_fs,
                fn_rs,
                filt_rs,
                truncLen = c(trunc_fwd, trunc_rev),
                maxN = 0,
                maxEE = c(max_ee_fwd, max_ee_rev),
                truncQ = 2,
                rm.phix = TRUE,
                compress = TRUE,
                multithread = hpc_workers
            )
        } else {
            filtered_counts <- filterAndTrim(
                fn_fs,
                filt_fs,
                truncLen = trunc_fwd,
                maxN = 0,
                maxEE = max_ee_fwd,
                truncQ = 2,
                rm.phix = TRUE,
                compress = TRUE,
                multithread = hpc_workers
            )
        }
        rownames(filtered_counts) <- sample_names

        shiny::setProgress(value = 0.25, detail = "Learning forward-read errors")
        err_f <- learnErrors(filt_fs, multithread = hpc_workers)
        err_r <- NULL
        if (identical(seq_type, "paired")) {
            shiny::setProgress(value = 0.4, detail = "Learning reverse-read errors")
            err_r <- learnErrors(filt_rs, multithread = hpc_workers)
        }

        shiny::setProgress(value = 0.55, detail = "Denoising reads")
        dada_fs <- dada(filt_fs, err = err_f, multithread = hpc_workers)
        dada_rs <- NULL
        mergers <- NULL
        if (identical(seq_type, "paired")) {
            dada_rs <- dada(filt_rs, err = err_r, multithread = hpc_workers)
            shiny::setProgress(value = 0.7, detail = "Merging paired reads")
            mergers <- mergePairs(
                dada_fs,
                filt_fs,
                dada_rs,
                filt_rs,
                verbose = TRUE
            )
            updateSelectInput(
                session,
                "selSample4margePairedReadsTab",
                choices = names(mergers)
            )
        }

        shiny::setProgress(value = 0.82, detail = "Removing chimeras")
        sequence_table <- if (identical(seq_type, "paired")) {
            makeSequenceTable(mergers)
        } else {
            makeSequenceTable(dada_fs)
        }
        sequence_lengths <- table(nchar(getSequences(sequence_table)))
        sequence_table_nochim <- removeBimeraDenovo(
            sequence_table,
            method = "consensus",
            multithread = hpc_workers,
            verbose = TRUE
        )

        get_n <- function(x) sum(getUniques(x))
        denoised_f <- if (inherits(dada_fs, "dada")) {
            get_n(dada_fs)
        } else {
            vapply(dada_fs, get_n, numeric(1))
        }
        if (identical(seq_type, "paired")) {
            denoised_r <- if (inherits(dada_rs, "dada")) {
                get_n(dada_rs)
            } else {
                vapply(dada_rs, get_n, numeric(1))
            }
            merged <- if (is.data.frame(mergers)) {
                get_n(mergers)
            } else {
                vapply(mergers, get_n, numeric(1))
            }
            track <- cbind(
                filtered_counts,
                denoised_f,
                denoised_r,
                merged,
                rowSums(sequence_table_nochim)
            )
            colnames(track) <- c(
                "input",
                "filtered",
                "denoisedF",
                "denoisedR",
                "merged",
                "nonchim"
            )
        } else {
            track <- cbind(
                filtered_counts,
                denoised_f,
                rowSums(sequence_table_nochim)
            )
            colnames(track) <- c("input", "filtered", "denoisedF", "nonchim")
        }
        rownames(track) <- sample_names
        shiny::setProgress(value = 1, detail = "Done")
    })

    result <- list(
        sample.names = sample_names,
        dadaFs = dada_fs,
        out = filtered_counts,
        errF = err_f,
        seqtabTable = sequence_lengths,
        track = track,
        seqtab.nochim = sequence_table_nochim
    )
    if (identical(seq_type, "paired")) {
        result$dadaRs <- dada_rs
        result$errR <- err_r
        result$mergers <- mergers
    }

    saveRDS(result, file.path(project_path, "dada2-results.rds"))
    write.csv(track, file.path(project_path, "read-tracking.csv"))
    write.csv(
        sequence_table_nochim,
        file.path(project_path, "sequence-table-nochim.csv")
    )
    cache_tmp <- paste0(cache_file, ".tmp-", Sys.getpid())
    saveRDS(result, cache_tmp)
    if (!file.rename(cache_tmp, cache_file)) unlink(cache_tmp)
    my_values$cache_status <- paste("Saved DADA2 cache:", cache_key)
    activate_dada_results(seq_type)
    result
})

output$dada2object_ready <- reactive({
    !is.null(reactiveInputData())
})
outputOptions(output, "dada2object_ready", suspendWhenHidden = FALSE)

fragments_summary_table <- reactive({
    data <- reactiveInputData()
    req(data)
    output_table <- data.frame(
        Sample = data$sample.names,
        data$out,
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
    colnames(output_table) <- c(
        "Sample",
        "Fragments before quality trimming",
        "Fragments after quality trimming"
    )
    output_table
})

output$filterAndTrim_output_table <- DT::renderDataTable(
    fragments_summary_table(),
    rownames = FALSE,
    options = list(scrollX = TRUE, pageLength = 10)
)

output$download_fragments_summary <- downloadHandler(
    filename = function() {
        paste0("dada2-fragments-summary-", Sys.Date(), ".csv")
    },
    contentType = "text/csv",
    content = function(file) {
        utils::write.csv(
            fragments_summary_table(),
            file,
            row.names = FALSE,
            na = ""
        )
    }
)
