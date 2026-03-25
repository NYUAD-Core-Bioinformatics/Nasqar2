# ── Plot download helper ──────────────────────────────────────────────────────
# Registers PNG / PDF / SVG downloadHandlers for a static ggplot output.
#
# output   — the Shiny output list (passed from server scope)
# plot_id  — the outputId of the corresponding plotOutput (e.g. "barPlot")
# plot_fn  — a zero-argument reactive / function that returns the plot object
# width, height — export dimensions in inches (default 10 × 7)
#
# Naming convention: download buttons are "dl_{plot_id}_{fmt}"
# e.g. dl_barPlot_png, dl_barPlot_pdf, dl_barPlot_svg

make_downloader <- function(output, plot_id, plot_fn, width = 10, height = 7) {
    for (fmt in c("png", "pdf", "svg")) {
        local({
            f <- fmt
            output[[paste0("dl_", plot_id, "_", f)]] <- downloadHandler(
                filename = function() paste0(plot_id, "_", Sys.Date(), ".", f),
                content  = function(file) {
                    switch(f,
                        png = grDevices::png(file,
                                width  = round(width  * 150),
                                height = round(height * 150),
                                res    = 150),
                        pdf = grDevices::pdf(file, width = width, height = height),
                        svg = grDevices::svg(file, width = width, height = height)
                    )
                    tryCatch(print(plot_fn()), error = function(e) NULL)
                    grDevices::dev.off()
                }
            )
        })
    }
}

# ── KEGG KO ID converter ──────────────────────────────────────────────────────
# Converts a character vector of ENTREZ IDs → KEGG Orthology IDs (K#####).
#
# Two-level cache for the two expensive REST calls per organism:
#
#   Level 1 — in-memory (.kegg_mem):   fast lookup, cleared on app restart
#   Level 2 — BiocFileCache (disk):    persistent across restarts; same
#                                      infrastructure used by clusterProfiler
#
# With both levels, REST endpoints are called AT MOST ONCE ever per organism:
#   rest.kegg.jp/conv/ncbi-geneid/{org}  — full ENTREZ ↔ KEGG gene-ID table
#   rest.kegg.jp/link/ko/{org}           — full organism → KO table
#
# To manually clear the disk cache:
#   BiocFileCache::cleanbfc(BiocFileCache::BiocFileCache())
#
# Returns a named character vector (names = ENTREZ IDs, values = KO IDs).
# Genes with no KO assignment get NA — callers must filter before building URLs.

.kegg_mem <- new.env(parent = emptyenv())   # Level 1: in-memory (per session)

# Level 2: plain RDS files in a user cache directory.
# Uses only base R — no BiocFileCache, no dplyr, no SQL.
.kegg_cache_dir <- tools::R_user_dir("nasqar_kegg", which = "cache")

.cache_path <- function(key) {
    file.path(.kegg_cache_dir, paste0(key, ".rds"))
}

.bfc_get_kegg <- function(key) {
    p <- .cache_path(key)
    if (!file.exists(p)) return(NULL)
    readRDS(p)
}

.bfc_set_kegg <- function(key, value) {
    dir.create(.kegg_cache_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(value, .cache_path(key))
    invisible(value)
}

# Returns TRUE if KO data for 'org' is already cached (memory or disk).
kegg_is_cached <- function(org) {
    if (exists(paste0("ko_", org), envir = .kegg_mem, inherits = FALSE))
        return(TRUE)
    file.exists(.cache_path(paste0("kegg_ko_", org)))
}

# ── Main converter ────────────────────────────────────────────────────────────
build_ko_map <- function(entrez_ids, org) {
    result <- setNames(rep(NA_character_, length(entrez_ids)),
                       as.character(entrez_ids))
    tryCatch({
        # ── Step 1: Full ENTREZ ↔ KEGG gene-ID table ────────────────────────
        # REST: rest.kegg.jp/conv/ncbi-geneid/{org}
        conv_mem <- paste0("conv_", org)
        conv_bfc <- paste0("kegg_conv_ncbi_", org)
        if (!exists(conv_mem, envir = .kegg_mem, inherits = FALSE)) {
            tbl <- .bfc_get_kegg(conv_bfc)
            if (is.null(tbl)) {
                raw <- KEGGREST::keggConv("ncbi-geneid", org)
                tbl <- data.frame(
                    kegg   = names(raw),
                    entrez = sub("^ncbi-geneid:", "", as.character(raw)),
                    stringsAsFactors = FALSE
                )
                .bfc_set_kegg(conv_bfc, tbl)
            }
            assign(conv_mem, tbl, envir = .kegg_mem)
        }
        conv_tbl <- get(conv_mem, envir = .kegg_mem)

        # ── Step 2: Full organism → KO table ────────────────────────────────
        # REST: rest.kegg.jp/link/ko/{org}
        ko_mem <- paste0("ko_", org)
        ko_bfc <- paste0("kegg_ko_", org)
        if (!exists(ko_mem, envir = .kegg_mem, inherits = FALSE)) {
            ko_clean <- .bfc_get_kegg(ko_bfc)
            if (is.null(ko_clean)) {
                all_ko   <- KEGGREST::keggLink("ko", org)
                ko_clean <- sub("^ko:", "", all_ko)   # "ko:K08513" → "K08513"
                .bfc_set_kegg(ko_bfc, ko_clean)
            }
            assign(ko_mem, ko_clean, envir = .kegg_mem)
        }
        ko_clean <- get(ko_mem, envir = .kegg_mem)

        # ── Look up: ENTREZ → KEGG gene ID → KO ID ──────────────────────────
        entrez_ch  <- as.character(entrez_ids)
        idx        <- match(entrez_ch, conv_tbl$entrez)
        valid      <- !is.na(idx)
        kegg_ids   <- conv_tbl$kegg[idx[valid]]   # e.g. "dme:Dmel_CG12796"
        ko_matched <- ko_clean[kegg_ids]           # NA where no KO mapping
        names(ko_matched) <- entrez_ch[valid]

        filled <- ko_matched[entrez_ch]
        result[!is.na(filled)] <- filled[!is.na(filled)]
        result
    }, error = function(e) result)   # return all-NA on any error
}

# UI helper: returns a small download-button strip to place below a plotOutput.
# Use: plot_dl_buttons("barPlot")
plot_dl_buttons <- function(plot_id) {
    tags$div(
        style = "margin-top:6px;",
        downloadButton(paste0("dl_", plot_id, "_png"), "PNG",
                       class = "btn btn-xs btn-default"),
        tags$span("\u00a0"),
        downloadButton(paste0("dl_", plot_id, "_pdf"), "PDF",
                       class = "btn btn-xs btn-default"),
        tags$span("\u00a0"),
        downloadButton(paste0("dl_", plot_id, "_svg"), "SVG",
                       class = "btn btn-xs btn-default")
    )
}
