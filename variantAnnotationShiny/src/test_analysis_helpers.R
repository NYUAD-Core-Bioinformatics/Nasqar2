# Run from variantAnnotationShiny/src: Rscript test_analysis_helpers.R
suppressPackageStartupMessages({
    library(VariantAnnotation)
    library(GenomicRanges)
})
source("analysis-helpers.R")

exchange_root <- tempfile("exchange-")
dir.create(exchange_root)
Sys.setenv(NASQAR_EXCHANGE_DIR = exchange_root)

make_vcf <- function(chrom, pos, ref, alt, qual, filt, dp = NULL) {
    ranges <- GRanges(chrom, IRanges(pos, width = nchar(ref)))
    alt_list <- IRanges::CharacterList(alt)
    fixed <- S4Vectors::DataFrame(
        REF = Biostrings::DNAStringSet(ref),
        ALT = alt_list,
        QUAL = qual,
        FILTER = filt
    )
    info <- if (is.null(dp)) {
        S4Vectors::DataFrame(row.names = seq_along(pos))
    } else {
        S4Vectors::DataFrame(DP = dp)
    }
    VCF(rowRanges = ranges, fixed = fixed, info = info, collapsed = TRUE)
}

# --- classify_variant_type ----------------------------------------------------
types <- classify_variant_type(
    c("A", "AT", "AT", "A", "A", "G"),
    IRanges::CharacterList(list("G", "A", "GC", c("G", "T"), "<DEL>", "*"))
)
stopifnot(identical(as.character(types), c("SNV", "Indel", "MNV", "Multiallelic", "Other", "Other")))

# --- build_filter_predicate ---------------------------------------------------
vcf <- make_vcf(
    chrom = rep("22", 5), pos = c(100, 200, 300, 400, 500),
    ref = c("A", "C", "GT", "T", "A"),
    alt = list("G", "T", "G", c("A", "C"), "<INS>"),
    qual = c(50, 10, NA, 80, 90),
    filt = c("PASS", "LowQual", ".", "PASS", "PASS"),
    dp = c(30, 5, 40, NA, 60)
)
pass_only <- build_filter_predicate(list(pass_only = TRUE))
stopifnot(identical(pass_only(vcf), c(TRUE, FALSE, TRUE, TRUE, TRUE)))
qual_20 <- build_filter_predicate(list(pass_only = FALSE, min_qual = 20))
stopifnot(identical(qual_20(vcf), c(TRUE, FALSE, FALSE, TRUE, TRUE)))
dp_10 <- build_filter_predicate(list(pass_only = FALSE, min_dp = 10))
stopifnot(identical(dp_10(vcf), c(TRUE, FALSE, TRUE, FALSE, TRUE)))
snv_only <- build_filter_predicate(list(pass_only = FALSE, types = "SNV"))
stopifnot(identical(snv_only(vcf), c(TRUE, TRUE, FALSE, FALSE, FALSE)))
no_dp <- make_vcf("22", 100, "A", list("G"), 50, "PASS")
dp_clause_ignored <- build_filter_predicate(list(pass_only = FALSE, min_dp = 10))
stopifnot(identical(dp_clause_ignored(no_dp), TRUE))
stopifnot(identical(build_filter_predicate(list())(vcf[0]), logical(0)))

# --- consequence ranking and collapse ---------------------------------------
per_transcript <- data.frame(
    variant_key = c("chr1:10_A>G", "chr1:10_A>G", "chr1:10_A>G", "chr2:5_C>T"),
    transcript_id = c("tx1", "tx2", "tx3", "tx9"),
    gene_symbol = c("GENEA", "GENEA", "GENEB", NA),
    gene_id = c("1", "1", "2", NA),
    location = c("coding", "intron", "coding", "intergenic"),
    consequence = c("synonymous", NA, "frameshift", NA),
    stringsAsFactors = FALSE
)
per_transcript$effect <- effect_for(per_transcript$location, per_transcript$consequence)
per_transcript$effect_class <- effect_class(per_transcript$effect)
stopifnot(identical(per_transcript$effect, c("synonymous", "intron", "frameshift", "intergenic")))
collapsed <- collapse_most_severe(per_transcript)
stopifnot(nrow(collapsed) == 2L)
first <- collapsed[collapsed$variant_key == "chr1:10_A>G", ]
stopifnot(identical(first$effect, "frameshift"), first$n_transcripts == 3L,
          identical(first$all_genes, "GENEA;GENEB"))
genes <- data.frame(gene_id = c("1", "2", "3"), gene_symbol = c("GENEA", "GENEB", "GENEC"),
                    coding = c(TRUE, TRUE, FALSE), stringsAsFactors = FALSE)
stopifnot(identical(annotated_gene_list(genes, "SYMBOL", coding_only = TRUE), c("GENEA", "GENEB")))
stopifnot(identical(annotated_gene_list(genes, "ENTREZID"), c("1", "2", "3")))
stopifnot(identical(annotated_gene_list(NULL), character(0)))
stopifnot(identical(consequence_severity_levels()[1:3], c("frameshift", "nonsense", "nonsynonymous")))

# --- harmonise_seqlevels -------------------------------------------------------
target <- GenomeInfoDb::Seqinfo(
    seqnames = c("chr1", "chr2", "chrM"),
    seqlengths = c(1000L, 1000L, 16569L),
    genome = "hg19"
)
gr <- GRanges(c("1", "2", "MT", "GL000009.2"), IRanges(c(10, 20, 30, 40), width = 1))
mcols(gr) <- S4Vectors::DataFrame(REF = c("A", "C", "G", "T"), ALT = c("G", "T", "A", "C"),
                                  QUAL = 1:4, FILTER = "PASS", DP = 5:8)
harmonised <- harmonise_seqlevels(gr, target)
stopifnot(identical(as.character(seqnames(harmonised$ranges)), c("chr1", "chr2", "chrM")))
stopifnot(identical(harmonised$dropped_contigs, "GL000009.2"), harmonised$dropped_records == 1L)
stopifnot(identical(harmonised$ranges$variant_key[1], "chr1:10_A>G"))
stopifnot(identical(GenomeInfoDb::genome(harmonised$ranges)[["chr1"]], "hg19"))
already_ucsc <- GRanges("chr2", IRanges(5, width = 1))
mcols(already_ucsc) <- S4Vectors::DataFrame(REF = "A", ALT = "T", QUAL = 1, FILTER = "PASS", DP = 1)
stopifnot(identical(as.character(seqnames(harmonise_seqlevels(already_ucsc, target)$ranges)), "chr2"))

# --- titv_ratio and substitution_counts --------------------------------------
ratio <- titv_ratio(c("A", "C", "A", "G", "AT"), c("G", "T", "C", "T", "A"))
stopifnot(ratio$transitions == 2L, ratio$transversions == 2L, ratio$ratio == 1)
stopifnot(is.na(titv_ratio(c("A", "C"), c("G", "T"))$ratio))
subs <- substitution_counts(c("A", "C"), c("G", "A"))
stopifnot(nrow(subs) == 12L, sum(subs$Freq) == 2L)

# --- regions, plans and cap message ------------------------------------------
regions <- parse_regions("chr17:7,565,097-7590856\n22:100-200")
stopifnot(length(regions) == 2L, start(regions)[1] == 7565097)
stopifnot(inherits(tryCatch(parse_regions("chr17-100"), error = function(e) e), "error"))
stopifnot(identical(parse_gene_symbols("TP53, BRCA1\nEGFR"), c("TP53", "BRCA1", "EGFR")))

overview <- list(
    contig_counts = c("1" = 600000L, "2" = 400000L, "22" = 50000L),
    contig_lengths = c("1" = 249250621, "2" = 243199373),
    total_records = 1050000,
    has_dp = TRUE, samples = character(0), format_fields = character(0)
)
plan_all <- scan_plan(overview, yield_size = 100000L)
stopifnot(identical(plan_all$mode, "stream"), plan_all$estimate == 1050000)
plan_small <- scan_plan(list(contig_counts = c("22" = 5000L), contig_lengths = numeric(0),
                             total_records = 5000), yield_size = 100000L)
stopifnot(identical(plan_small$mode, "single"))
plan_chr <- scan_plan(overview, chromosomes = "22")
stopifnot(identical(plan_chr$mode, "regions"), plan_chr$estimate == 50000,
          length(plan_chr$regions) >= 1L)
plan_chr_ucsc <- scan_plan(overview, regions = GRanges("chr22", IRanges(1, 10)))
stopifnot(identical(as.character(seqnames(plan_chr_ucsc$regions)), "22"))
tiles <- tile_regions(GRanges("1", IRanges(1, 25e6)), width = 10e6)
stopifnot(length(tiles) == 3L, end(tiles)[3] == 25e6)
plan_unsorted <- scan_plan(overview, regions = GRanges(c("2", "1", "1"), IRanges(c(100, 500, 400), c(200, 600, 550))))
stopifnot(identical(as.character(seqnames(plan_unsorted$regions)), c("1", "2")),
          start(plan_unsorted$regions)[1] == 400, end(plan_unsorted$regions)[1] == 600)
stopifnot(identical(format_count(600000), "600,000"))
groups <- balanced_index_groups(1:10, 4L)
stopifnot(length(groups) == 4L, identical(unlist(groups), 1:10))
stopifnot(length(balanced_index_groups(1:3, 8L)) == 3L, length(balanced_index_groups(integer(0), 4L)) == 0L)

# --- exchange gene list --------------------------------------------------------
path <- write_gene_list_csv(c("TP53", "BRCA1", NA, "TP53"))
stopifnot(startsWith(normalizePath(path), normalizePath(exchange_root)))
stopifnot(grepl("^[0-9a-f-]{36}\\.csv$", basename(path)))
written <- read.csv(path, stringsAsFactors = FALSE)
stopifnot(identical(names(written), "x"), identical(written$x, c("TP53", "BRCA1")))
token <- encryptUrlParam(path)
stopifnot(grepl("^[0-9a-f]+$", token))
ora_key <- file.path("..", "..", "ClusterProfShinyORA", "src", "private.txt")
if (file.exists(ora_key)) {
    key <- sodium::hex2bin(readLines(ora_key, warn = FALSE)[1])
    decrypted <- unserialize(sodium::simple_decrypt(sodium::hex2bin(token), key))
    stopifnot(identical(decrypted, path))
}

# --- genome gating --------------------------------------------------------------
fake_installed <- function(pkg) pkg %in% c(
    "BSgenome.Hsapiens.UCSC.hg19", "TxDb.Hsapiens.UCSC.hg19.knownGene", "org.Hs.eg.db"
)
genomes <- installed_genomes(is_installed = fake_installed)
stopifnot(identical(names(genomes), "hg19"))
stopifnot(identical(unname(genome_choices(genomes)), "hg19"))
stopifnot(!snplocs_available(genomes$hg19, is_installed = fake_installed))

# --- bgzip / tabix preparation with the packaged example ----------------------
example <- system.file("extdata", "chr22.vcf.gz", package = "VariantAnnotation")
work <- tempfile("work-")
prepared <- prepare_vcf_input(example, work)
stopifnot(file.exists(prepared$path), file.exists(prepared$index))
overview_example <- vcf_overview(prepared$path, prepared$index)
stopifnot(overview_example$total_records > 0, "22" %in% names(overview_example$contig_counts))
plain <- tempfile(fileext = ".vcf")
writeLines(readLines(gzfile(example)), plain)
prepared_plain <- prepare_vcf_input(plain, work)
stopifnot(prepared_plain$converted, file.exists(prepared_plain$index))
stopifnot(vcf_overview(prepared_plain$path, prepared_plain$index)$total_records ==
          overview_example$total_records)
plain_gz <- tempfile(fileext = ".vcf.gz")
con <- gzfile(plain_gz, "w"); writeLines(readLines(plain), con); close(con)
stopifnot(!is_bgzip_file(plain_gz), is_gzip_file(plain_gz))
prepared_gz <- prepare_vcf_input(plain_gz, work)
stopifnot(prepared_gz$converted, is_bgzip_file(prepared_gz$path))

# --- streaming filter writing to an indexed VCF ---------------------------------
filter_to_file <- function(tag, plan, predicate, yield_size = 2000L) {
    writer <- new_filtered_vcf_writer(file.path(work, tag, "filtered"))
    counts <- stream_vcf_filtered(prepared$path, prepared$index, "hg19", overview_example,
                                  plan, predicate, writer, yield_size = yield_size)
    written <- writer$finish()
    list(counts = counts, written = written)
}
plan_stream <- scan_plan(overview_example, yield_size = 2000L)
stopifnot(identical(plan_stream$mode, "stream"))
keep_all <- build_filter_predicate(list(pass_only = FALSE))
streamed <- filter_to_file("stream", plan_stream, keep_all)
stopifnot(streamed$counts$kept == overview_example$total_records,
          streamed$written$records == streamed$counts$kept,
          file.exists(streamed$written$path), tabix_readable(streamed$written$path, streamed$written$index))
stopifnot(vcf_overview(streamed$written$path, streamed$written$index)$total_records == streamed$counts$kept)
single <- filter_to_file("single", scan_plan(overview_example, yield_size = 1e6), keep_all, yield_size = 1e6)
stopifnot(single$written$records == streamed$written$records)
stopifnot(!is.null(streamed$written$sample_counts), nrow(streamed$written$sample_counts) == length(overview_example$samples))
stopifnot(identical(streamed$written$sample_counts$variants, single$written$sample_counts$variants))
region_plan <- scan_plan(overview_example, regions = GRanges("chr22", IRanges(50300000, 50400000)))
regional <- filter_to_file("regional", region_plan, keep_all)
stopifnot(regional$counts$kept > 0, regional$counts$kept < streamed$counts$kept)
regional_vcf <- readVcf(regional$written$path, "hg19")
stopifnot(nrow(regional_vcf) == regional$counts$kept, all(start(rowRanges(regional_vcf)) >= 50300000))
snv_plan <- build_filter_predicate(list(pass_only = FALSE, types = "SNV"))
snvs <- filter_to_file("snv", plan_stream, snv_plan)
stopifnot(snvs$counts$kept < streamed$counts$kept, snvs$counts$kept > 0)
empty <- new_filtered_vcf_writer(file.path(work, "empty", "filtered"))
stopifnot(is.null(empty$finish()$path))
summary <- summarise_vcf_file(prepared$path, prepared$index, "hg19", overview_example, yield_size = 2000L)
stopifnot(summary$records == overview_example$total_records, sum(summary$type) == summary$records)

# --- expansion and variant ranges ----------------------------------------------
expanded <- expand_variants(regional_vcf)
stopifnot(nrow(expanded$vcf) >= nrow(regional_vcf))
ranges <- variant_ranges(expanded)
stopifnot(length(ranges) == nrow(expanded$vcf), all(nchar(ranges$ALT) >= 1))

# --- chunked annotation equals single-chunk annotation ----------------------------
if (all(vapply(c("TxDb.Hsapiens.UCSC.hg19.knownGene", "BSgenome.Hsapiens.UCSC.hg19", "org.Hs.eg.db"),
               requireNamespace, logical(1), quietly = TRUE))) {
    genome_hg19 <- load_genome_resource(genome_resources()[["hg19"]], with_snplocs = FALSE)
    options <- list(promoter_upstream = 2000L, promoter_downstream = 200L, add_rsid = FALSE)
    annotate_to <- function(tag, chunk_size) {
        stream_annotation(regional$written$path, regional$written$index, genome_hg19, options,
                          workers = 1L, output_dir = file.path(work, tag, "ann"),
                          total_records = regional$written$records, has_dp = overview_example$has_dp,
                          chunk_size = chunk_size)
    }
    whole <- annotate_to("whole", 1e6)
    pieces <- annotate_to("pieces", 25L)
    stopifnot(whole$report$chunks == 1L, pieces$report$chunks > 1L)
    read_sorted <- function(path) { df <- read.csv(path, stringsAsFactors = FALSE); df <- df[order(df$variant_key, df$transcript_id), ]; rownames(df) <- NULL; df }
    stopifnot(isTRUE(all.equal(read_sorted(whole$files$per_transcript), read_sorted(pieces$files$per_transcript))))
    stopifnot(identical(whole$rows, pieces$rows), identical(whole$stats$effect_counts, pieces$stats$effect_counts),
              identical(whole$stats$substitution_counts, pieces$stats$substitution_counts),
              identical(sort(whole$stats$qual), sort(pieces$stats$qual)),
              identical(whole$genes[order(whole$genes$gene_id), ], pieces$genes[order(pieces$genes$gene_id), ]))
    stopifnot(nrow(whole$previews$per_variant) == min(whole$rows$per_variant, table_preview_rows()))
} else {
    cat("hg19 annotation packages not installed; chunked annotation check skipped.\n")
}

cat("All Variant Annotation helper tests passed.\n")
