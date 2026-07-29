tx_db_list <- list(
    "BSgenome.Hsapiens.UCSC.hg19" = "TxDb.Hsapiens.UCSC.hg19.knownGene",
    "BSgenome.Mmusculus.UCSC.mm10" = "TxDb.Mmusculus.UCSC.mm10.knownGene",
    "BSgenome.Drerio.UCSC.danRer11" = "TxDb.Drerio.UCSC.danRer11.refGene",
    "BSgenome.Celegans.UCSC.ce11" = "TxDb.Celegans.UCSC.ce11.refGene"
)

atac_cache_root <- Sys.getenv(
    "NASQAR_ATAC_CACHE_DIR",
    unset = file.path(tempdir(check = TRUE), "ATACseqQC-cache")
)
dir.create(atac_cache_root, recursive = TRUE, showWarnings = FALSE)

core_done <- reactiveVal(FALSE)
nucleosome_done <- reactiveVal(FALSE)
footprint_done <- reactiveVal(FALSE)

output$task_done <- reactive(core_done())
output$nucleosome_task_done <- reactive(nucleosome_done())
output$footprint_task_done <- reactive(footprint_done())
outputOptions(output, "task_done", suspendWhenHidden = FALSE)
outputOptions(output, "nucleosome_task_done", suspendWhenHidden = FALSE)
outputOptions(output, "footprint_task_done", suspendWhenHidden = FALSE)

observeEvent(
    list(
        input$sel_sample_for_npositioning,
        input$sel_chromosome,
        input$bs_genome_input
    ),
    {
        core_done(FALSE)
        nucleosome_done(FALSE)
        footprint_done(FALSE)
    },
    ignoreInit = TRUE
)

observeEvent(input$motif_value, {
    footprint_done(FALSE)
}, ignoreInit = TRUE)

available_workers <- function(limit) {
    requested <- suppressWarnings(as.integer(
        Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1")
    ))
    if (is.na(requested) || requested < 1L) {
        requested <- 1L
    }
    max(1L, min(as.integer(limit), requested))
}

parallel_map <- function(values, callback, workers) {
    if (workers > 1L && .Platform$OS.type == "unix") {
        results <- parallel::mclapply(
            values,
            callback,
            mc.cores = workers,
            mc.preschedule = TRUE,
            mc.set.seed = FALSE
        )
        failed <- vapply(results, inherits, logical(1), what = "try-error")
        if (any(failed)) {
            stop(as.character(results[[which(failed)[1]]]))
        }
        return(results)
    }
    lapply(values, callback)
}

with_null_device <- function(expression) {
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    force(expression)
}

write_cache <- function(value, path) {
    temporary <- tempfile(pattern = "cache-", tmpdir = dirname(path))
    saveRDS(value, temporary, compress = FALSE)
    if (!file.rename(temporary, path)) {
        unlink(temporary)
        stop("Unable to save the ATACseqQC cache.")
    }
    value
}

cache_key_for <- function(stage, ...) {
    digest::digest(
        list(stage = stage, version = 2L, ...),
        algo = "xxhash64"
    )
}

inputDataReactive <- eventReactive(input$run_qc, {
    req(
        input$sel_sample_for_npositioning,
        input$sel_chromosome,
        input$bs_genome_input
    )

    shinyjs::disable("run_qc")
    on.exit(shinyjs::enable("run_qc"), add = TRUE)
    core_done(FALSE)
    nucleosome_done(FALSE)
    footprint_done(FALSE)
    js$addStatusIcon("nucleosomepositioning_tab", "loading")

    withProgress(
        message = "Calculating core ATAC-seq QC scores",
        value = 0,
        {
            bamfile <- file.path(
                my_values$base_dir,
                my_values$samples_df[
                    input$sel_sample_for_npositioning,
                    "BamFile"
                ]
            )
            bam_info <- file.info(bamfile)
            validate(need(!is.na(bam_info$size), "The selected BAM file is unavailable."))

            key <- cache_key_for(
                "core",
                normalizePath(bamfile, winslash = "/", mustWork = TRUE),
                unname(bam_info$size),
                as.numeric(bam_info$mtime),
                input$bs_genome_input,
                input$sel_chromosome
            )
            cache_dir <- file.path(atac_cache_root, key)
            cache_file <- file.path(cache_dir, "core.rds")
            shifted_bam <- file.path(cache_dir, "shifted.bam")
            shifted_bai <- paste0(shifted_bam, ".bai")
            dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

            tx_db_package <- tx_db_list[[input$bs_genome_input]]
            validate(need(
                !is.null(tx_db_package),
                "No transcript annotation is configured for this genome."
            ))
            check_and_load_bioc_package(input$bs_genome_input)
            check_and_load_bioc_package(tx_db_package)

            if (
                file.exists(cache_file) &&
                file.exists(shifted_bam) &&
                file.exists(shifted_bai)
            ) {
                incProgress(1, detail = "Loaded cached result")
                cached <- readRDS(cache_file)
                core_done(TRUE)
                js$addStatusIcon("nucleosomepositioning_tab", "done")
                return(cached)
            }

            incProgress(0.1, detail = "Reading selected chromosome")
            possible_tags <- combn(LETTERS, 2)
            possible_tags <- c(
                paste0(possible_tags[1, ], possible_tags[2, ]),
                paste0(possible_tags[2, ], possible_tags[1, ])
            )
            bam_top_100 <- scanBam(
                BamFile(bamfile, yieldSize = 100),
                param = ScanBamParam(tag = possible_tags)
            )[[1]]$tag
            tags <- names(bam_top_100)[lengths(bam_top_100) > 0]

            tx_db <- get(tx_db_package)
            seqinformation <- seqinfo(tx_db)
            chromosome <- input$sel_chromosome
            validate(need(
                chromosome %in% seqlevels(seqinformation),
                "The chromosome is absent from the selected transcript database."
            ))
            chromosome_range <- as(seqinformation[chromosome], "GRanges")
            alignments <- readBamFile(
                bamfile,
                tag = tags,
                which = chromosome_range,
                asMates = TRUE,
                bigFile = TRUE
            )

            incProgress(0.2, detail = "Shifting alignments")
            unlink(c(shifted_bam, shifted_bai))
            shifted_alignments <- shiftGAlignmentsList(
                alignments,
                outbam = shifted_bam
            )

            transcripts_for_chromosome <- transcripts(tx_db)
            transcripts_for_chromosome <- transcripts_for_chromosome[
                as.character(seqnames(transcripts_for_chromosome)) == chromosome
            ]
            validate(need(
                length(transcripts_for_chromosome) > 0,
                "No transcripts were found for the selected chromosome."
            ))

            incProgress(0.25, detail = "Calculating PT, NFR, and TSSE scores")
            score_names <- c("pt", "nfr", "tsse")
            score_workers <- available_workers(length(score_names))
            score_results <- parallel_map(
                score_names,
                function(score_name) {
                    with_null_device(
                        switch(
                            score_name,
                            pt = PTscore(
                                shifted_alignments,
                                transcripts_for_chromosome
                            ),
                            nfr = NFRscore(
                                shifted_alignments,
                                transcripts_for_chromosome
                            ),
                            tsse = TSSEscore(
                                shifted_alignments,
                                transcripts_for_chromosome
                            )
                        )
                    )
                },
                workers = score_workers
            )
            names(score_results) <- score_names

            result <- list(
                key = key,
                cache_dir = cache_dir,
                bamfile = bamfile,
                shifted_bam = shifted_bam,
                chromosome_range = chromosome_range,
                chromosome = chromosome,
                genome_package = input$bs_genome_input,
                transcript_package = tx_db_package,
                alignments = shifted_alignments,
                transcripts = transcripts_for_chromosome,
                pt = score_results$pt,
                nfr = score_results$nfr,
                tsse = score_results$tsse,
                score_workers = score_workers
            )
            incProgress(0.2, detail = "Saving reusable result")
            write_cache(result, cache_file)
            core_done(TRUE)
            js$addStatusIcon("nucleosomepositioning_tab", "done")
            result
        }
    )
}, ignoreInit = TRUE)

nucleosomeDataReactive <- eventReactive(input$run_nucleosome_plots, {
    core <- inputDataReactive()
    req(core)

    shinyjs::disable("run_nucleosome_plots")
    on.exit(shinyjs::enable("run_nucleosome_plots"), add = TRUE)
    nucleosome_done(FALSE)

    withProgress(
        message = "Calculating nucleosome distributions",
        value = 0,
        {
            cache_file <- file.path(core$cache_dir, "nucleosome.rds")
            required_bams <- file.path(
                core$cache_dir,
                c(
                    "NucleosomeFree.bam",
                    "mononucleosome.bam",
                    "dinucleosome.bam",
                    "trinucleosome.bam"
                )
            )
            if (
                file.exists(cache_file) &&
                all(file.exists(required_bams))
            ) {
                incProgress(1, detail = "Loaded cached result")
                cached <- readRDS(cache_file)
                nucleosome_done(TRUE)
                return(cached)
            }

            genome_name <- strsplit(core$genome_package, ".", fixed = TRUE)[[1]][2]
            genome <- get(genome_name)
            incProgress(0.25, detail = "Splitting alignments by fragment size")
            split_alignments <- splitGAlignmentsByCut(
                core$alignments,
                txs = core$transcripts,
                genome = genome,
                outPath = core$cache_dir
            )

            tss <- unique(promoters(core$transcripts, upstream = 0, downstream = 1))
            library_size <- estLibSize(required_bams)
            n_tile <- 101L
            upstream <- 1010L
            downstream <- 1010L

            incProgress(0.35, detail = "Calculating TSS-aligned signals")
            tss_signals <- enrichedFragments(
                gal = split_alignments[c(
                    "NucleosomeFree",
                    "mononucleosome",
                    "dinucleosome",
                    "trinucleosome"
                )],
                TSS = tss,
                librarySize = library_size,
                seqlev = core$chromosome,
                TSS.filter = 0.5,
                n.tile = n_tile,
                upstream = upstream,
                downstream = downstream
            )
            log2_signals <- lapply(tss_signals, function(signal) log2(signal + 1))
            centered_tss <- reCenterPeaks(tss, width = upstream + downstream)

            incProgress(0.25, detail = "Normalizing distributions")
            distribution <- with_null_device(
                featureAlignedDistribution(
                    tss_signals,
                    centered_tss,
                    zeroAt = 0.5,
                    n.tile = n_tile,
                    type = "l",
                    ylab = "Averaged coverage"
                )
            )
            range_01 <- function(values) {
                value_range <- range(values, na.rm = TRUE)
                if (!all(is.finite(value_range)) || diff(value_range) == 0) {
                    return(rep(0, length(values)))
                }
                (values - value_range[1]) / diff(value_range)
            }
            distribution <- apply(distribution, 2, range_01)

            result <- list(
                split_alignments = split_alignments,
                bamfiles = required_bams,
                tss = tss,
                tss_signals = tss_signals,
                log2_signals = log2_signals,
                centered_tss = centered_tss,
                distribution = distribution,
                n_tile = n_tile,
                upstream = upstream,
                downstream = downstream
            )
            incProgress(0.15, detail = "Saving reusable result")
            write_cache(result, cache_file)
            nucleosome_done(TRUE)
            result
        }
    )
}, ignoreInit = TRUE)

footprintDataReactive <- eventReactive(input$run_footprint_plots, {
    core <- inputDataReactive()
    req(core, input$motif_value)

    shinyjs::disable("run_footprint_plots")
    on.exit(shinyjs::enable("run_footprint_plots"), add = TRUE)
    footprint_done(FALSE)

    withProgress(
        message = "Calculating motif footprinting",
        value = 0,
        {
            motif_key <- cache_key_for(
                "footprint",
                core$key,
                trimws(input$motif_value)
            )
            footprint_dir <- file.path(core$cache_dir, motif_key)
            dir.create(footprint_dir, recursive = TRUE, showWarnings = FALSE)
            cache_file <- file.path(footprint_dir, "footprint.rds")
            footprint_png <- file.path(footprint_dir, "footprint.png")
            vplot_png <- file.path(footprint_dir, "vplot.png")
            if (
                file.exists(cache_file) &&
                file.exists(footprint_png) &&
                file.exists(vplot_png)
            ) {
                incProgress(1, detail = "Loaded cached result")
                cached <- readRDS(cache_file)
                footprint_done(TRUE)
                return(cached)
            }

            motif <- as.list(query(MotifDb, trimws(input$motif_value)))
            validate(need(length(motif) > 0, "No matching motif was found."))
            genome_name <- strsplit(core$genome_package, ".", fixed = TRUE)[[1]][2]
            genome <- get(genome_name)

            incProgress(0.2, detail = "Calculating footprint profile")
            grDevices::png(footprint_png, width = 1200, height = 800, res = 120)
            footprint_signals <- tryCatch(
                factorFootprints(
                    core$shifted_bam,
                    pfm = motif[[1]],
                    genome = genome,
                    min.score = "90%",
                    seqlev = core$chromosome,
                    upstream = 100,
                    downstream = 100
                ),
                finally = grDevices::dev.off()
            )

            incProgress(0.45, detail = "Calculating fragment V-plot")
            grDevices::png(vplot_png, width = 1200, height = 800, res = 120)
            vplot <- tryCatch(
                vPlot(
                    core$shifted_bam,
                    bindingSites = footprint_signals$bindingSites,
                    genome = genome,
                    seqlev = core$chromosome,
                    upstream = 200,
                    downstream = 200,
                    ylim = c(30, 250),
                    bandwidth = c(2, 1)
                ),
                finally = grDevices::dev.off()
            )

            result <- list(
                signals = footprint_signals,
                vplot = vplot,
                footprint_png = footprint_png,
                vplot_png = vplot_png
            )
            incProgress(0.25, detail = "Saving reusable result")
            write_cache(result, cache_file)
            footprint_done(TRUE)
            result
        }
    )
}, ignoreInit = TRUE)

output$empty_txt_output <- renderText({
    inputDataReactive()
    ""
})

output$plot_pt_score <- renderPlot({
    pt <- inputDataReactive()$pt
    plot(
        pt$log2meanCoverage,
        pt$PT_score,
        xlab = "log2 mean coverage",
        ylab = "Promoter vs Transcript"
    )
})

output$plot_nfr_score <- renderPlot({
    nfr <- inputDataReactive()$nfr
    plot(
        nfr$log2meanCoverage,
        nfr$NFR_score,
        xlab = "log2 mean coverage",
        ylab = "Nucleosome Free Regions score",
        main = "NFR score for 200 bp flanking TSSs",
        xlim = c(-10, 0),
        ylim = c(-5, 5)
    )
})

output$plot_tssre_score <- renderPlot({
    tsse <- inputDataReactive()$tsse
    plot(
        100 * (-9:10 - 0.5),
        tsse$values,
        type = "b",
        xlab = "distance to TSS",
        ylab = "aggregate TSS score"
    )
})

output$plotCumulativePercentage <- renderPlot({
    core <- inputDataReactive()
    nucleosome <- nucleosomeDataReactive()
    cumulativePercentage(
        nucleosome$bamfiles[1:2],
        core$chromosome_range
    )
})

output$plot_tss_featureAlignedHeatmap <- renderPlot({
    nucleosome <- nucleosomeDataReactive()
    featureAlignedHeatmap(
        nucleosome$log2_signals,
        nucleosome$centered_tss,
        zeroAt = 0.5,
        n.tile = nucleosome$n_tile
    )
})

output$plot_signals <- renderPlot({
    distribution <- nucleosomeDataReactive()$distribution
    matplot(
        distribution,
        type = "l",
        xaxt = "n",
        xlab = "Position (bp)",
        ylab = "Fraction of signal"
    )
    axis(
        1,
        at = seq(0, 100, by = 10) + 1,
        labels = c("-1K", seq(-800, 800, by = 200), "1K"),
        las = 2
    )
    abline(v = seq(0, 100, by = 10) + 1, lty = 2, col = "gray")
})

output$plot_Footprints <- renderImage({
    result <- footprintDataReactive()
    list(
        src = result$footprint_png,
        contentType = "image/png",
        alt = "DNA-binding factor footprint"
    )
}, deleteFile = FALSE)

output$plot_binding_sites_featureAlignedHeatmap <- renderPlot({
    signals <- footprintDataReactive()$signals
    featureAlignedHeatmap(
        signals$signal,
        feature.gr = reCenterPeaks(
            signals$bindingSites,
            width = 200 + width(signals$bindingSites[1])
        ),
        annoMcols = "score",
        sortBy = "score",
        n.tile = ncol(signals$signal[[1]])
    )
})

output$plot_vp <- renderImage({
    result <- footprintDataReactive()
    list(
        src = result$vplot_png,
        contentType = "image/png",
        alt = "ATAC-seq fragment V-plot"
    )
}, deleteFile = FALSE)

output$plot_distanceDyad <- renderPlot({
    distanceDyad(footprintDataReactive()$vplot, pch = 20, cex = 0.5)
})

output$bamfilesTable <- renderDataTable(
    {
        files <- list.files(
            inputDataReactive()$cache_dir,
            pattern = "\\.bam(\\.bai)?$",
            full.names = TRUE
        )
        DT::datatable(
            data.frame(Filename = basename(files)),
            options = list(scrollX = TRUE)
        )
    },
    options = list(scrollX = TRUE)
)

output$download_bamfiles_btn <- downloadHandler(
    filename = function() {
        paste("bamfiles_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
        temporary_directory <- tempfile(pattern = "bam-download-")
        dir.create(temporary_directory)
        files <- list.files(
            inputDataReactive()$cache_dir,
            pattern = "\\.bam(\\.bai)?$",
            full.names = TRUE
        )
        selected <- input$bamfilesTable_rows_selected
        if (length(selected) > 0) {
            files <- files[selected]
        }
        file.copy(files, temporary_directory)
        zip::zip(
            zipfile = file,
            files = dir(temporary_directory),
            root = temporary_directory
        )
    },
    contentType = "application/zip"
)
