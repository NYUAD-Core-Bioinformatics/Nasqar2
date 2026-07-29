options(warn = 1)
library(shiny)

app_dir <- Sys.getenv(
    "NASQAR_ATAC_APP_DIR",
    unset = "/srv/shiny-server/ATACseqQCShniy/src"
)
setwd(app_dir)

source("ui.R", local = globalenv())
source("server.R", local = globalenv())

elapsed <- function(expression) {
    timing <- system.time(value <- force(expression))
    list(value = value, seconds = unname(timing[["elapsed"]]))
}

shiny::testServer(server, {
    session$setInputs(data_file_type = "example_bam_file")
    samples <- input_files_reactive()
    stopifnot(nrow(samples) > 0)

    sample_name <- rownames(samples)[1]
    session$setInputs(
        bs_genome_input = "BSgenome.Hsapiens.UCSC.hg19",
        sel_sample_for_npositioning = sample_name,
        sel_chromosome = "chr1",
        motif_value = "CTCF"
    )

    stopifnot(
        !core_done(),
        !nucleosome_done(),
        !footprint_done()
    )

    core_first_trigger <- elapsed(session$setInputs(run_qc = 1))
    core_first <- inputDataReactive()
    stopifnot(
        core_done(),
        !nucleosome_done(),
        !footprint_done(),
        identical(core_first$score_workers, 3L),
        file.exists(core_first$shifted_bam),
        file.exists(paste0(core_first$shifted_bam, ".bai")),
        !is.null(core_first$pt),
        !is.null(core_first$nfr),
        !is.null(core_first$tsse)
    )

    core_cached_trigger <- elapsed(session$setInputs(run_qc = 2))
    core_cached <- inputDataReactive()
    stopifnot(
        identical(core_first$key, core_cached$key),
        core_cached_trigger$seconds < core_first_trigger$seconds
    )

    cat(sprintf(
        "CORE cold=%.3fs cached=%.3fs workers=%d cache=%s\n",
        core_first_trigger$seconds,
        core_cached_trigger$seconds,
        core_first$score_workers,
        core_first$cache_dir
    ))

    if (identical(Sys.getenv("NASQAR_ATAC_TEST_ADVANCED"), "1")) {
        nucleosome_trigger <- elapsed(
            session$setInputs(run_nucleosome_plots = 1)
        )
        nucleosome <- nucleosomeDataReactive()
        stopifnot(
            nucleosome_done(),
            all(file.exists(nucleosome$bamfiles)),
            !is.null(nucleosome$distribution)
        )
        cat(sprintf(
            "NUCLEOSOME elapsed=%.3fs\n",
            nucleosome_trigger$seconds
        ))

        footprint_trigger <- elapsed(
            session$setInputs(run_footprint_plots = 1)
        )
        footprint <- footprintDataReactive()
        stopifnot(
            footprint_done(),
            file.exists(footprint$footprint_png),
            file.exists(footprint$vplot_png)
        )
        cat(sprintf(
            "FOOTPRINT elapsed=%.3fs\n",
            footprint_trigger$seconds
        ))
    }
})
