# Pure helpers shared by the Variant Annotation Shiny application and its
# automated tests. Nothing in this file touches Shiny reactives.

# ---------------------------------------------------------------------------
# NASQAR2 exchange directory and encrypted handoff parameters
# ---------------------------------------------------------------------------

nasqar_exchange_dir <- function(create = FALSE) {
    configured <- Sys.getenv("NASQAR_EXCHANGE_DIR", unset = "")
    path <- if (nzchar(configured)) {
        configured
    } else {
        file.path(dirname(tempdir(check = TRUE)), "nasqar_exchange")
    }
    if (create) {
        dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    }
    normalizePath(path, mustWork = create)
}

new_exchange_file <- function(extension = ".csv") {
    file.path(
        nasqar_exchange_dir(create = TRUE),
        paste0(uuid::UUIDgenerate(), extension)
    )
}

encryptUrlParam <- function(paramStr) {
    public_key_hex <- Sys.getenv(
        "NASQAR_PUBLIC_KEY_HEX",
        unset = "42b3781d6907cd426b9c05cac7155cce15bb9385a602716f619529485dab6c28"
    )
    public_key <- sodium::hex2bin(public_key_hex)
    message <- serialize(paramStr, NULL)
    sodium::bin2hex(sodium::simple_encrypt(message, public_key))
}

write_gene_list_csv <- function(genes, path = new_exchange_file(".csv")) {
    genes <- unique(genes[!is.na(genes) & nzchar(genes)])
    utils::write.csv(data.frame(x = genes, stringsAsFactors = FALSE),
                     file = path, row.names = FALSE)
    path
}

# ---------------------------------------------------------------------------
# Workers, caching and limits
# ---------------------------------------------------------------------------

available_workers <- function(limit = 8L) {
    requested <- suppressWarnings(as.integer(
        Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1")
    ))
    if (is.na(requested) || requested < 1L) {
        requested <- 1L
    }
    max(1L, min(as.integer(limit), requested))
}

parallel_map <- function(values, callback, workers) {
    if (workers > 1L && .Platform$OS.type == "unix" && length(values) > 1L) {
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

write_cache <- function(value, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE, mode = "0700")
    temporary <- tempfile(pattern = "cache-", tmpdir = dirname(path))
    saveRDS(value, temporary, compress = FALSE)
    if (!file.rename(temporary, path)) {
        unlink(temporary)
        stop("Unable to save the Variant Annotation cache.")
    }
    value
}

cache_key_for <- function(stage, ...) {
    digest::digest(
        list(stage = stage, version = 1L, ...),
        algo = "xxhash64"
    )
}

file_signature <- function(path) {
    info <- file.info(path)
    list(
        path = normalizePath(path, mustWork = TRUE),
        size = unname(info$size),
        mtime = as.numeric(info$mtime)
    )
}

env_integer <- function(name, default) {
    value <- suppressWarnings(as.integer(Sys.getenv(name, unset = "")))
    if (length(value) != 1L || is.na(value) || value < 1L) default else value
}

# Records read per chunk while summarising and filtering.
yield_size_limit <- function() env_integer("NASQAR_VARIANT_YIELD_SIZE", 100000L)

# Filtered records annotated per chunk; memory use scales with this, not with
# the size of the filtered set.
annotation_chunk_size <- function() env_integer("NASQAR_VARIANT_ANNOTATION_CHUNK", 50000L)

# Rows of each annotation table kept in memory for the interactive preview.
table_preview_rows <- function() env_integer("NASQAR_VARIANT_TABLE_PREVIEW", 10000L)

format_count <- function(value) format(value, big.mark = ",", scientific = FALSE)

# ---------------------------------------------------------------------------
# Reference genome resources
# ---------------------------------------------------------------------------

genome_resource <- function(key, label, bsgenome, txdb, orgdb, snplocs = NULL,
                            gene_keytype = "ENTREZID", symbol_column = "SYMBOL") {
    list(key = key, label = label, bsgenome = bsgenome, txdb = txdb, orgdb = orgdb,
         snplocs = snplocs, gene_keytype = gene_keytype, symbol_column = symbol_column)
}

# Catalog of supported reference genomes. Human ships in the image; the others
# can be installed from Bioconductor into the writable R library at run time.
# gene_keytype is the org.*.db keytype matching the TxDb gene_id column.
genome_resources <- function() {
    resources <- list(
        genome_resource("hg38", "Human (hg38 / GRCh38)",
            "BSgenome.Hsapiens.UCSC.hg38", "TxDb.Hsapiens.UCSC.hg38.knownGene", "org.Hs.eg.db",
            snplocs = "SNPlocs.Hsapiens.dbSNP144.GRCh38"),
        genome_resource("hg19", "Human (hg19 / GRCh37)",
            "BSgenome.Hsapiens.UCSC.hg19", "TxDb.Hsapiens.UCSC.hg19.knownGene", "org.Hs.eg.db",
            snplocs = "SNPlocs.Hsapiens.dbSNP144.GRCh37"),
        genome_resource("mm39", "Mouse (mm39 / GRCm39)",
            "BSgenome.Mmusculus.UCSC.mm39", "TxDb.Mmusculus.UCSC.mm39.knownGene", "org.Mm.eg.db"),
        genome_resource("mm10", "Mouse (mm10 / GRCm38)",
            "BSgenome.Mmusculus.UCSC.mm10", "TxDb.Mmusculus.UCSC.mm10.knownGene", "org.Mm.eg.db"),
        genome_resource("rn7", "Rat (rn7 / mRatBN7.2)",
            "BSgenome.Rnorvegicus.UCSC.rn7", "TxDb.Rnorvegicus.UCSC.rn7.refGene", "org.Rn.eg.db"),
        genome_resource("rn6", "Rat (rn6 / Rnor_6.0)",
            "BSgenome.Rnorvegicus.UCSC.rn6", "TxDb.Rnorvegicus.UCSC.rn6.refGene", "org.Rn.eg.db"),
        genome_resource("danRer11", "Zebrafish (danRer11 / GRCz11)",
            "BSgenome.Drerio.UCSC.danRer11", "TxDb.Drerio.UCSC.danRer11.refGene", "org.Dr.eg.db"),
        genome_resource("dm6", "Fruit fly (dm6 / BDGP6)",
            "BSgenome.Dmelanogaster.UCSC.dm6", "TxDb.Dmelanogaster.UCSC.dm6.ensGene", "org.Dm.eg.db",
            gene_keytype = "FLYBASE"),
        genome_resource("ce11", "Nematode (ce11 / WBcel235)",
            "BSgenome.Celegans.UCSC.ce11", "TxDb.Celegans.UCSC.ce11.refGene", "org.Ce.eg.db"),
        genome_resource("sacCer3", "Yeast (sacCer3 / R64)",
            "BSgenome.Scerevisiae.UCSC.sacCer3", "TxDb.Scerevisiae.UCSC.sacCer3.sgdGene", "org.Sc.sgd.db",
            gene_keytype = "ORF", symbol_column = "GENENAME"),
        genome_resource("galGal6", "Chicken (galGal6 / GRCg6a)",
            "BSgenome.Ggallus.UCSC.galGal6", "TxDb.Ggallus.UCSC.galGal6.refGene", "org.Gg.eg.db"),
        genome_resource("susScr11", "Pig (susScr11 / Sscrofa11.1)",
            "BSgenome.Sscrofa.UCSC.susScr11", "TxDb.Sscrofa.UCSC.susScr11.refGene", "org.Ss.eg.db"),
        genome_resource("bosTau9", "Cow (bosTau9 / ARS-UCD1.2)",
            "BSgenome.Btaurus.UCSC.bosTau9", "TxDb.Btaurus.UCSC.bosTau9.refGene", "org.Bt.eg.db"),
        genome_resource("canFam3", "Dog (canFam3 / CanFam3.1)",
            "BSgenome.Cfamiliaris.UCSC.canFam3", "TxDb.Cfamiliaris.UCSC.canFam3.refGene", "org.Cf.eg.db"),
        genome_resource("rheMac10", "Rhesus macaque (rheMac10 / Mmul_10)",
            "BSgenome.Mmulatta.UCSC.rheMac10", "TxDb.Mmulatta.UCSC.rheMac10.refGene", "org.Mmu.eg.db"),
        genome_resource("panTro6", "Chimpanzee (panTro6 / Pan_tro_3.0)",
            "BSgenome.Ptroglodytes.UCSC.panTro6", "TxDb.Ptroglodytes.UCSC.panTro6.refGene", "org.Pt.eg.db")
    )
    stats::setNames(resources, vapply(resources, function(resource) resource$key, character(1)))
}

default_is_installed <- function(package) {
    requireNamespace(package, quietly = TRUE)
}

installed_genomes <- function(resources = genome_resources(),
                              is_installed = default_is_installed) {
    keep <- vapply(resources, function(resource) {
        is_installed(resource$bsgenome) &&
            is_installed(resource$txdb) &&
            is_installed(resource$orgdb)
    }, logical(1))
    resources[keep]
}

genome_choices <- function(genomes) {
    if (!length(genomes)) return(character(0))
    stats::setNames(
        names(genomes),
        vapply(genomes, function(genome) genome$label, character(1))
    )
}

snplocs_available <- function(resource, is_installed = default_is_installed) {
    !is.null(resource$snplocs) && is_installed(resource$snplocs)
}

missing_genome_packages <- function(resource, is_installed = default_is_installed) {
    packages <- c(resource$bsgenome, resource$txdb, resource$orgdb)
    packages[!vapply(packages, is_installed, logical(1))]
}

# The persistent user library: an R_LIBS_USER directory that already exists
# and is writable. A private instance mounts it from the host (Docker) or the
# launcher creates it in the job runtime (Slurm). R always defines R_LIBS_USER,
# so the directory must not be created here: on a public instance it does not
# exist, nothing persists, and on-the-fly installs are not offered at all.
genome_install_library <- function() {
    user_libs <- strsplit(Sys.getenv("R_LIBS_USER", unset = ""), .Platform$path.sep, fixed = TRUE)[[1]]
    candidates <- user_libs[nzchar(user_libs)]
    for (lib in candidates) {
        if (dir.exists(lib) && file.access(lib, 2) == 0) {
            return(normalizePath(lib, winslash = "/", mustWork = FALSE))
        }
    }
    NULL
}

# Seconds allowed per package download (override with NASQAR_GENOME_DOWNLOAD_TIMEOUT).
genome_download_timeout <- function() {
    value <- suppressWarnings(as.integer(Sys.getenv("NASQAR_GENOME_DOWNLOAD_TIMEOUT", "7200")))
    if (is.na(value) || value < 60L) 7200L else value
}

# Install genome packages in a background R process so the session stays
# responsive; progress is appended to log_file.
start_genome_install <- function(packages, lib, log_file) {
    callr::r_bg(
        function(packages, lib, libpaths, timeout) {
            .libPaths(unique(c(lib, libpaths)))
            # BSgenome tarballs reach 1 GB; R's default 60 s download timeout aborts them.
            options(timeout = max(timeout, getOption("timeout", 60)))
            BiocManager::install(packages, lib = lib, update = FALSE, ask = FALSE)
            invisible(NULL)
        },
        args = list(packages = packages, lib = lib, libpaths = .libPaths(),
                    timeout = genome_download_timeout()),
        stdout = log_file, stderr = "2>&1", supervise = TRUE
    )
}

# Command to install a genome from a machine with internet access when this
# server has none (for example a cluster compute node). With the host-side paths the
# job script exports, this is an exact singularity command for a login node.
offline_install_instructions <- function(resource, lib) {
    packages <- c(resource$bsgenome, resource$txdb, resource$orgdb)
    r_call <- sprintf(
        "options(timeout = %d); BiocManager::install(c(%s), lib = \"%s\", update = FALSE, ask = FALSE)",
        genome_download_timeout(), paste(sprintf("\"%s\"", packages), collapse = ", "), lib
    )
    host_lib <- Sys.getenv("NASQAR_R_LIBRARY_HOST", unset = "")
    sif <- Sys.getenv("NASQAR_SIF_HOST", unset = "")
    if (nzchar(host_lib) && nzchar(sif)) {
        # The Slurm install keeps its helper scripts in <NASQAR2_HOME>/bin,
        # next to images/<sif>; the helper applies the same bind and library.
        home <- dirname(dirname(sif))
        paste(
            "# Compute nodes usually have no internet access. Run this once on a login",
            "# node. It installs into the private R library this job mounts, so the",
            "# genome is available here and in every later job.",
            sprintf("%s/bin/nasqar2-install-genome %s", home, resource$key),
            sep = "\n"
        )
    } else {
        paste(
            "# If this server has no internet access, run the same installation from a",
            "# shell inside the NASQAR2 container on a machine that does, for example:",
            "#   docker exec <container> /opt/conda/envs/merged_env/bin/Rscript -e '...'",
            sprintf("Rscript -e '%s'", r_call),
            sep = "\n"
        )
    }
}

load_genome_resource <- function(resource, with_snplocs = FALSE) {
    fetch <- function(package) getExportedValue(package, package)
    loaded <- list(
        key = resource$key,
        label = resource$label,
        bsgenome = fetch(resource$bsgenome),
        txdb = fetch(resource$txdb),
        orgdb = fetch(resource$orgdb),
        gene_keytype = resource$gene_keytype,
        symbol_column = resource$symbol_column,
        snplocs = NULL,
        packages = c(resource$bsgenome, resource$txdb, resource$orgdb)
    )
    if (isTRUE(with_snplocs) && snplocs_available(resource)) {
        loaded$snplocs <- fetch(resource$snplocs)
        loaded$packages <- c(loaded$packages, resource$snplocs)
    }
    loaded
}

package_versions <- function(packages) {
    vapply(packages, function(package) {
        as.character(utils::packageVersion(package))
    }, character(1))
}

# ---------------------------------------------------------------------------
# VCF preparation: bgzip + tabix, header and index summaries
# ---------------------------------------------------------------------------

is_gzip_file <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    header <- readBin(con, "raw", n = 2L)
    length(header) == 2L && header[1] == as.raw(0x1f) && header[2] == as.raw(0x8b)
}

is_bgzip_file <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    header <- readBin(con, "raw", n = 16L)
    length(header) >= 14L &&
        header[1] == as.raw(0x1f) && header[2] == as.raw(0x8b) &&
        header[13] == as.raw(0x42) && header[14] == as.raw(0x43)
}

vcf_base_name <- function(path) {
    base <- basename(path)
    sub("\\.(vcf\\.gz|vcf\\.bgz|vcf|gz|bgz)$", "", base, ignore.case = TRUE)
}

tabix_readable <- function(path, index) {
    tryCatch({
        Rsamtools::headerTabix(Rsamtools::TabixFile(path, index))
        TRUE
    }, error = function(error) FALSE)
}

# Returns list(path, index, converted). Never writes next to the source file.
# copy_indexed: also copy an already bgzip-compressed and indexed file into
# work_dir (needed when the source is a temporary upload the project must keep).
prepare_vcf_input <- function(source_path, work_dir, index_path = NULL, copy_indexed = FALSE) {
    source_path <- normalizePath(source_path, mustWork = TRUE)
    if (dir.exists(source_path)) {
        stop("The VCF path must be a file, not a directory.", call. = FALSE)
    }
    candidate_index <- if (!is.null(index_path) && nzchar(index_path)) {
        index_path
    } else {
        paste0(source_path, ".tbi")
    }
    if (is_bgzip_file(source_path) && file.exists(candidate_index) &&
        tabix_readable(source_path, candidate_index)) {
        if (!isTRUE(copy_indexed)) {
            return(list(path = source_path, index = candidate_index, converted = FALSE))
        }
        dir.create(work_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
        destination <- file.path(work_dir, paste0(vcf_base_name(source_path), ".vcf.bgz"))
        if (!file.copy(source_path, destination, overwrite = TRUE) ||
            !file.copy(candidate_index, paste0(destination, ".tbi"), overwrite = TRUE)) {
            stop("Could not copy the VCF into the project directory.", call. = FALSE)
        }
        return(list(path = destination, index = paste0(destination, ".tbi"), converted = FALSE))
    }

    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    destination <- file.path(work_dir, paste0(vcf_base_name(source_path), ".vcf.bgz"))
    if (is_bgzip_file(source_path)) {
        if (!file.copy(source_path, destination, overwrite = TRUE)) {
            stop("Could not copy the VCF into the working directory.", call. = FALSE)
        }
    } else if (is_gzip_file(source_path)) {
        plain <- tempfile(pattern = "vcf-", tmpdir = work_dir, fileext = ".vcf")
        on.exit(unlink(plain), add = TRUE)
        input <- gzfile(source_path, "rb")
        output <- file(plain, "wb")
        repeat {
            chunk <- readBin(input, "raw", n = 8L * 1024L^2)
            if (!length(chunk)) break
            writeBin(chunk, output)
        }
        close(input)
        close(output)
        Rsamtools::bgzip(plain, dest = destination, overwrite = TRUE)
    } else {
        Rsamtools::bgzip(source_path, dest = destination, overwrite = TRUE)
    }
    index <- tryCatch(
        Rsamtools::indexTabix(destination, format = "vcf"),
        error = function(error) {
            stop(paste(
                "Could not index the VCF. The file must be a valid,",
                "coordinate-sorted VCF.", conditionMessage(error)
            ), call. = FALSE)
        }
    )
    list(path = destination, index = index, converted = TRUE)
}

contig_lengths_from_header <- function(header) {
    contigs <- tryCatch(VariantAnnotation::meta(header)$contig, error = function(e) NULL)
    if (is.null(contigs) || !nrow(contigs) || !"length" %in% colnames(contigs)) {
        return(integer(0))
    }
    lengths <- suppressWarnings(as.numeric(contigs$length))
    stats::setNames(lengths, rownames(contigs))
}

contig_record_counts <- function(path, index) {
    tab <- Rsamtools::TabixFile(path, index)
    contigs <- Rsamtools::headerTabix(tab)$seqnames
    if (!length(contigs)) return(stats::setNames(integer(0), character(0)))
    ranges <- GenomicRanges::GRanges(contigs, IRanges::IRanges(1, tabix_max_position()))
    counts <- unlist(Rsamtools::countTabix(tab, param = ranges), use.names = FALSE)
    stats::setNames(as.integer(counts), contigs)
}

vcf_overview <- function(path, index) {
    header <- VariantAnnotation::scanVcfHeader(path)
    counts <- contig_record_counts(path, index)
    counts <- counts[counts > 0]
    style <- tryCatch(
        GenomeInfoDb::seqlevelsStyle(names(counts))[1],
        error = function(error) "unknown"
    )
    info <- VariantAnnotation::info(header)
    list(
        header = header,
        samples = VariantAnnotation::samples(header),
        contig_counts = counts,
        contig_lengths = contig_lengths_from_header(header),
        total_records = sum(as.numeric(counts)),
        contig_style = style,
        info_fields = rownames(info),
        format_fields = rownames(VariantAnnotation::geno(header)),
        has_dp = "DP" %in% rownames(info)
    )
}

# ---------------------------------------------------------------------------
# Variant classification and filtering
# ---------------------------------------------------------------------------

variant_type_levels <- function() c("SNV", "Indel", "MNV", "Multiallelic", "Other")

first_alt_allele <- function(alt) {
    n_alt <- lengths(alt)
    flat <- as.character(unlist(alt, use.names = FALSE))
    first <- rep(NA_character_, length(n_alt))
    has_alt <- n_alt > 0L
    first[has_alt] <- flat[(cumsum(n_alt) - n_alt + 1L)[has_alt]]
    first
}

classify_variant_type <- function(ref, alt) {
    ref <- toupper(as.character(ref))
    n_alt <- lengths(alt)
    alt1 <- toupper(first_alt_allele(alt))
    nucleotide <- grepl("^[ACGTN]+$", alt1) & grepl("^[ACGTN]+$", ref)
    type <- rep("Other", length(ref))
    ref_len <- nchar(ref)
    alt_len <- nchar(alt1)
    type[nucleotide & ref_len == 1L & alt_len == 1L] <- "SNV"
    type[nucleotide & ref_len != alt_len] <- "Indel"
    type[nucleotide & ref_len == alt_len & ref_len > 1L] <- "MNV"
    type[n_alt > 1L] <- "Multiallelic"
    factor(type, levels = variant_type_levels())
}

default_filter_spec <- function() {
    list(
        pass_only = TRUE,
        min_qual = NA_real_,
        min_dp = NA_real_,
        types = variant_type_levels()
    )
}

# Returns function(vcf) -> logical(nrow(vcf)).
build_filter_predicate <- function(spec) {
    spec <- utils::modifyList(default_filter_spec(), spec)
    function(vcf) {
        n <- nrow(vcf)
        keep <- rep(TRUE, n)
        if (!n) return(keep)
        if (isTRUE(spec$pass_only)) {
            filt <- as.character(VariantAnnotation::filt(vcf))
            keep <- keep & (is.na(filt) | filt %in% c("PASS", "."))
        }
        if (!is.null(spec$min_qual) && !is.na(spec$min_qual)) {
            qual <- VariantAnnotation::qual(vcf)
            keep <- keep & !is.na(qual) & qual >= spec$min_qual
        }
        if (!is.null(spec$min_dp) && !is.na(spec$min_dp) &&
            "DP" %in% names(VariantAnnotation::info(vcf))) {
            dp <- VariantAnnotation::info(vcf)$DP
            if (is.list(dp) || methods::is(dp, "List")) {
                dp <- vapply(as.list(dp), function(value) {
                    if (length(value)) as.numeric(value[1]) else NA_real_
                }, numeric(1))
            }
            dp <- suppressWarnings(as.numeric(dp))
            keep <- keep & !is.na(dp) & dp >= spec$min_dp
        }
        types <- spec$types
        if (!is.null(types) && length(types) &&
            !all(variant_type_levels() %in% types)) {
            type <- classify_variant_type(
                VariantAnnotation::ref(vcf),
                VariantAnnotation::alt(vcf)
            )
            keep <- keep & as.character(type) %in% types
        }
        keep
    }
}

# ---------------------------------------------------------------------------
# Regions and reading plans
# ---------------------------------------------------------------------------

tabix_max_position <- function() 536870911L

parse_regions <- function(text) {
    lines <- trimws(unlist(strsplit(text %||% "", "[\r\n;]+")))
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return(GenomicRanges::GRanges())
    pattern <- "^([^:[:space:]]+):([0-9]+)-([0-9]+)$"
    cleaned <- gsub("(?<=[0-9]),(?=[0-9])", "", lines, perl = TRUE)
    cleaned <- gsub("[[:space:]]+", "", cleaned)
    valid <- grepl(pattern, cleaned)
    if (any(!valid)) {
        stop(paste(
            "Regions must be written as chr:start-end. Invalid:",
            paste(lines[!valid], collapse = ", ")
        ), call. = FALSE)
    }
    chrom <- sub(pattern, "\\1", cleaned)
    start <- as.numeric(sub(pattern, "\\2", cleaned))
    end <- as.numeric(sub(pattern, "\\3", cleaned))
    if (any(end < start)) {
        stop("Region end must not be smaller than region start.", call. = FALSE)
    }
    GenomicRanges::GRanges(chrom, IRanges::IRanges(start = pmax(start, 1), end = end))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

parse_gene_symbols <- function(text) {
    symbols <- trimws(unlist(strsplit(text %||% "", "[\r\n,;[:space:]]+")))
    unique(symbols[nzchar(symbols)])
}

gene_regions <- function(symbols, txdb, orgdb, flank = 0L,
                         gene_keytype = "ENTREZID", symbol_column = "SYMBOL") {
    symbols <- unique(symbols)
    if (!length(symbols)) {
        return(list(regions = GenomicRanges::GRanges(), missing = character(0)))
    }
    mapped <- suppressMessages(AnnotationDbi::mapIds(
        orgdb, keys = symbols, column = gene_keytype,
        keytype = symbol_column, multiVals = "list"
    ))
    entrez <- unique(unlist(mapped, use.names = FALSE))
    entrez <- entrez[!is.na(entrez)]
    all_genes <- suppressMessages(GenomicFeatures::genes(txdb))
    regions <- all_genes[all_genes$gene_id %in% entrez]
    if (length(regions) && flank > 0L) {
        regions <- GenomicRanges::trim(regions + as.integer(flank))
    }
    found <- vapply(mapped, function(ids) {
        any(!is.na(ids) & ids %in% all_genes$gene_id)
    }, logical(1))
    list(regions = regions, missing = symbols[!found])
}

# Match user-supplied region names to the VCF's contig naming style.
match_region_style <- function(regions, contig_names) {
    if (!length(regions) || !length(contig_names)) return(regions)
    present <- as.character(GenomeInfoDb::seqnames(regions)) %in% contig_names
    if (all(present)) return(regions)
    target <- tryCatch(GenomeInfoDb::seqlevelsStyle(contig_names)[1], error = function(e) NA)
    if (is.na(target)) return(regions)
    converted <- tryCatch({
        copy <- regions
        GenomeInfoDb::seqlevelsStyle(copy) <- target
        copy
    }, error = function(error) regions)
    converted
}

chromosome_regions <- function(chromosomes, contig_lengths) {
    if (!length(chromosomes)) return(GenomicRanges::GRanges())
    ends <- vapply(chromosomes, function(chromosome) {
        len <- contig_lengths[chromosome]
        if (length(len) == 1L && !is.na(len) && len > 0) {
            min(as.numeric(len), tabix_max_position())
        } else {
            as.numeric(tabix_max_position())
        }
    }, numeric(1))
    GenomicRanges::GRanges(chromosomes, IRanges::IRanges(start = 1, end = ends))
}

tile_regions <- function(regions, width = 10e6) {
    if (!length(regions)) return(regions)
    regions <- GenomicRanges::reduce(GenomicRanges::sort(GenomeInfoDb::sortSeqlevels(regions)))
    pieces <- lapply(seq_along(regions), function(i) {
        region <- regions[i]
        if (GenomicRanges::width(region) <= width) return(region)
        starts <- seq(GenomicRanges::start(region), GenomicRanges::end(region), by = width)
        ends <- pmin(starts + width - 1, GenomicRanges::end(region))
        GenomicRanges::GRanges(
            rep(as.character(GenomeInfoDb::seqnames(region)), length(starts)),
            IRanges::IRanges(start = starts, end = ends)
        )
    })
    do.call(c, pieces)
}

# Decide how to read: list(mode, regions, estimate).
scan_plan <- function(overview, chromosomes = character(0), regions = NULL,
                      yield_size = yield_size_limit()) {
    counts <- overview$contig_counts
    has_regions <- !is.null(regions) && length(regions) > 0L
    has_chromosomes <- length(chromosomes) > 0L &&
        !all(names(counts) %in% chromosomes)
    if (has_regions) {
        regions <- match_region_style(regions, names(counts))
        if (has_chromosomes) {
            regions <- regions[as.character(GenomeInfoDb::seqnames(regions)) %in% chromosomes]
        }
        seqs <- unique(as.character(GenomeInfoDb::seqnames(regions)))
        estimate <- sum(as.numeric(counts[intersect(seqs, names(counts))]))
        # Sorted, non-overlapping tiles keep the streamed output coordinate-sorted
        # and free of duplicate records, which tabix indexing requires.
        regions <- GenomicRanges::reduce(GenomicRanges::sort(regions))
        return(list(mode = "regions", regions = tile_regions(regions),
                    estimate = estimate))
    }
    if (has_chromosomes) {
        chromosomes <- intersect(chromosomes, names(counts))
        estimate <- sum(as.numeric(counts[chromosomes]))
        regions <- chromosome_regions(chromosomes, overview$contig_lengths)
        return(list(mode = "regions", regions = tile_regions(regions),
                    estimate = estimate))
    }
    total <- overview$total_records
    list(
        mode = if (total <= yield_size) "single" else "stream",
        regions = NULL,
        estimate = total
    )
}

# ---------------------------------------------------------------------------
# Streaming reader with predicate and cap
# ---------------------------------------------------------------------------

scan_param_for <- function(overview, n_samples_with_genotypes = 50L,
                           regions = NULL, include_genotypes = TRUE) {
    info_fields <- if (overview$has_dp) "DP" else character(0)
    geno_fields <- if (include_genotypes &&
        length(overview$samples) > 0L &&
        length(overview$samples) <= n_samples_with_genotypes &&
        "GT" %in% overview$format_fields) "GT" else NA
    if (!is.null(regions) && length(regions)) {
        VariantAnnotation::ScanVcfParam(
            fixed = c("ALT", "QUAL", "FILTER"), info = info_fields,
            geno = geno_fields, which = regions
        )
    } else {
        VariantAnnotation::ScanVcfParam(
            fixed = c("ALT", "QUAL", "FILTER"), info = info_fields,
            geno = geno_fields
        )
    }
}

# Writer that streams kept records into an uncompressed VCF and, on finish,
# bgzips and tabix-indexes it. Per-sample non-reference counts are accumulated
# on the way when genotypes were read.
new_filtered_vcf_writer <- function(destination_prefix) {
    plain <- paste0(destination_prefix, ".vcf")
    dir.create(dirname(plain), recursive = TRUE, showWarnings = FALSE, mode = "0700")
    unlink(c(plain, paste0(destination_prefix, ".vcf.bgz"), paste0(destination_prefix, ".vcf.bgz.tbi")))
    con <- NULL
    records <- 0
    sample_counts <- NULL

    add <- function(chunk) {
        if (!nrow(chunk)) return(invisible(NULL))
        staging <- tempfile(pattern = "chunk-", fileext = ".vcf")
        on.exit(unlink(staging), add = TRUE)
        VariantAnnotation::writeVcf(chunk, staging)
        lines <- readLines(staging, warn = FALSE)
        header <- startsWith(lines, "#")
        if (is.null(con)) {
            con <<- file(plain, open = "w")
            writeLines(lines[header], con)
        }
        writeLines(lines[!header], con)
        records <<- records + nrow(chunk)
        counts <- per_sample_variant_counts(chunk)
        if (!is.null(counts)) {
            if (is.null(sample_counts)) {
                sample_counts <<- counts
            } else {
                added <- counts$variants[match(sample_counts$sample, counts$sample)]
                added[is.na(added)] <- 0L
                sample_counts$variants <<- sample_counts$variants + added
            }
        }
        invisible(NULL)
    }

    finish <- function() {
        if (is.null(con)) {
            return(list(path = NULL, index = NULL, records = 0, sample_counts = NULL))
        }
        close(con)
        con <<- NULL
        compressed <- paste0(destination_prefix, ".vcf.bgz")
        Rsamtools::bgzip(plain, dest = compressed, overwrite = TRUE)
        unlink(plain)
        index <- Rsamtools::indexTabix(compressed, format = "vcf")
        list(path = compressed, index = index, records = records, sample_counts = sample_counts)
    }

    discard <- function() {
        if (!is.null(con)) {
            close(con)
            con <<- NULL
        }
        unlink(plain)
        invisible(NULL)
    }

    list(add = add, finish = finish, discard = discard)
}

# Scan the VCF according to the plan, apply the predicate chunk by chunk and
# hand kept records to the writer. Nothing is retained in memory.
stream_vcf_filtered <- function(path, index, genome, overview, plan, predicate, writer,
                                yield_size = yield_size_limit(), progress = NULL) {
    kept <- 0
    scanned <- 0
    take <- function(chunk, tile = NULL) {
        if (!nrow(chunk)) return(invisible(NULL))
        if (!is.null(tile)) {
            starts <- GenomicRanges::start(SummarizedExperiment::rowRanges(chunk))
            chunk <- chunk[starts >= GenomicRanges::start(tile) &
                starts <= GenomicRanges::end(tile)]
        }
        scanned <<- scanned + nrow(chunk)
        chunk <- chunk[predicate(chunk)]
        kept <<- kept + nrow(chunk)
        writer$add(chunk)
        invisible(NULL)
    }

    if (identical(plan$mode, "regions")) {
        tiles <- plan$regions
        for (i in seq_along(tiles)) {
            tile <- tiles[i]
            param <- scan_param_for(overview, regions = tile)
            chunk <- VariantAnnotation::readVcf(
                Rsamtools::TabixFile(path, index), genome = genome, param = param
            )
            take(chunk, tile)
            if (!is.null(progress)) progress(i / length(tiles), scanned, kept)
        }
    } else {
        param <- scan_param_for(overview)
        if (identical(plan$mode, "single")) {
            take(VariantAnnotation::readVcf(
                Rsamtools::TabixFile(path, index), genome = genome, param = param
            ))
            if (!is.null(progress)) progress(1, scanned, kept)
        } else {
            tab <- Rsamtools::TabixFile(path, index, yieldSize = yield_size)
            open(tab)
            on.exit(close(tab), add = TRUE)
            total <- max(overview$total_records, 1)
            repeat {
                chunk <- VariantAnnotation::readVcf(tab, genome = genome, param = param)
                if (!nrow(chunk)) break
                take(chunk)
                if (!is.null(progress)) progress(min(scanned / total, 1), scanned, kept)
            }
        }
    }
    list(scanned = scanned, kept = kept)
}

# ---------------------------------------------------------------------------
# Summary statistics accumulated over chunks
# ---------------------------------------------------------------------------

qual_breaks <- function() c(0, 10, 20, 30, 50, 100, 200, 500, 1000, Inf)
dp_breaks <- function() c(0, 5, 10, 20, 30, 50, 100, 200, 500, Inf)

bin_counts <- function(values, breaks) {
    values <- suppressWarnings(as.numeric(values))
    values <- values[!is.na(values)]
    table(cut(values, breaks = breaks, right = FALSE, include.lowest = TRUE))
}

new_summary_accumulator <- function() {
    list(
        records = 0,
        filter = integer(0),
        type = stats::setNames(rep(0L, length(variant_type_levels())), variant_type_levels()),
        qual = stats::setNames(rep(0L, length(qual_breaks()) - 1L),
                               levels(cut(0, qual_breaks(), right = FALSE, include.lowest = TRUE))),
        dp = stats::setNames(rep(0L, length(dp_breaks()) - 1L),
                             levels(cut(0, dp_breaks(), right = FALSE, include.lowest = TRUE))),
        qual_missing = 0L,
        dp_missing = 0L
    )
}

add_named_counts <- function(current, addition) {
    for (name in names(addition)) {
        current[name] <- (if (is.na(current[name])) 0 else current[name]) + addition[[name]]
    }
    current
}

accumulate_summary <- function(acc, vcf) {
    n <- nrow(vcf)
    if (!n) return(acc)
    acc$records <- acc$records + n
    filt <- as.character(VariantAnnotation::filt(vcf))
    filt[is.na(filt)] <- "."
    acc$filter <- add_named_counts(acc$filter, table(filt))
    type <- classify_variant_type(VariantAnnotation::ref(vcf), VariantAnnotation::alt(vcf))
    acc$type <- add_named_counts(acc$type, table(type))
    qual <- VariantAnnotation::qual(vcf)
    acc$qual_missing <- acc$qual_missing + sum(is.na(qual))
    acc$qual <- add_named_counts(acc$qual, bin_counts(qual, qual_breaks()))
    if ("DP" %in% names(VariantAnnotation::info(vcf))) {
        dp <- VariantAnnotation::info(vcf)$DP
        if (is.list(dp) || methods::is(dp, "List")) {
            dp <- vapply(as.list(dp), function(value) {
                if (length(value)) as.numeric(value[1]) else NA_real_
            }, numeric(1))
        }
        acc$dp_missing <- acc$dp_missing + sum(is.na(dp))
        acc$dp <- add_named_counts(acc$dp, bin_counts(dp, dp_breaks()))
    }
    acc
}

summarise_vcf_file <- function(path, index, genome, overview,
                               yield_size = yield_size_limit(), progress = NULL) {
    acc <- new_summary_accumulator()
    param <- scan_param_for(overview, include_genotypes = FALSE)
    tab <- Rsamtools::TabixFile(path, index, yieldSize = yield_size)
    open(tab)
    on.exit(close(tab), add = TRUE)
    total <- max(overview$total_records, 1)
    repeat {
        chunk <- VariantAnnotation::readVcf(tab, genome = genome, param = param)
        if (!nrow(chunk)) break
        acc <- accumulate_summary(acc, chunk)
        if (!is.null(progress)) progress(min(acc$records / total, 1))
    }
    acc
}

# ---------------------------------------------------------------------------
# Annotation
# ---------------------------------------------------------------------------

consequence_severity_levels <- function() {
    c(
        "frameshift", "nonsense", "nonsynonymous", "spliceSite", "coding",
        "not translated", "synonymous", "promoter", "fiveUTR", "threeUTR",
        "intron", "intergenic"
    )
}

effect_for <- function(location, consequence) {
    location <- as.character(location)
    consequence <- as.character(consequence)
    effect <- location
    coding <- !is.na(location) & location == "coding" & !is.na(consequence)
    effect[coding] <- consequence[coding]
    effect
}

effect_class <- function(effect) {
    effect <- as.character(effect)
    class <- rep("Non-coding", length(effect))
    class[effect %in% c("frameshift", "nonsense", "nonsynonymous")] <- "Protein-altering"
    class[effect %in% c("synonymous", "coding", "not translated")] <- "Coding (other)"
    class[effect %in% "spliceSite"] <- "Splice site"
    class[effect %in% "intergenic"] <- "Intergenic"
    class
}

# Split multi-allelic records into one row per ALT and drop alleles that are
# not plain nucleotide sequences (symbolic, breakend, spanning deletion).
expand_variants <- function(vcf) {
    expanded <- VariantAnnotation::expand(vcf, row.names = FALSE)
    ref <- toupper(as.character(VariantAnnotation::ref(expanded)))
    alt <- toupper(as.character(VariantAnnotation::alt(expanded)))
    valid <- !is.na(alt) & grepl("^[ACGT]+$", alt) & grepl("^[ACGT]+$", ref)
    list(
        vcf = expanded[valid],
        ref = ref[valid],
        alt = alt[valid],
        dropped_alleles = sum(!valid)
    )
}

info_dp_vector <- function(vcf) {
    if (!"DP" %in% names(VariantAnnotation::info(vcf))) {
        return(rep(NA_real_, nrow(vcf)))
    }
    dp <- VariantAnnotation::info(vcf)$DP
    if (is.list(dp) || methods::is(dp, "List")) {
        dp <- vapply(as.list(dp), function(value) {
            if (length(value)) as.numeric(value[1]) else NA_real_
        }, numeric(1))
    }
    suppressWarnings(as.numeric(dp))
}

# Build a plain GRanges carrying REF/ALT/QUAL/FILTER/DP for annotation.
variant_ranges <- function(expanded) {
    vcf <- expanded$vcf
    ranges <- SummarizedExperiment::rowRanges(vcf)
    gr <- GenomicRanges::GRanges(
        seqnames = as.character(GenomeInfoDb::seqnames(ranges)),
        ranges = IRanges::ranges(ranges),
        strand = "*"
    )
    S4Vectors::mcols(gr) <- S4Vectors::DataFrame(
        REF = expanded$ref,
        ALT = expanded$alt,
        QUAL = as.numeric(VariantAnnotation::qual(vcf)),
        FILTER = as.character(VariantAnnotation::filt(vcf)),
        DP = info_dp_vector(vcf)
    )
    gr
}

# Rename contigs to the TxDb style, drop contigs unknown to the TxDb and adopt
# the TxDb seqinfo. Returns list(ranges, dropped_contigs, dropped_records).
harmonise_seqlevels <- function(gr, target_seqinfo, target_style = NULL) {
    if (is.null(target_style)) {
        target_style <- tryCatch(
            GenomeInfoDb::seqlevelsStyle(GenomeInfoDb::seqlevels(target_seqinfo))[1],
            error = function(error) NA_character_
        )
    }
    current_style <- tryCatch(
        GenomeInfoDb::seqlevelsStyle(GenomeInfoDb::seqlevels(gr))[1],
        error = function(error) NA_character_
    )
    if (!is.na(target_style) && !is.na(current_style) &&
        !identical(current_style, target_style)) {
        gr <- tryCatch({
            copy <- gr
            GenomeInfoDb::seqlevelsStyle(copy) <- target_style
            copy
        }, error = function(error) gr)
    }
    present <- as.character(GenomeInfoDb::seqnames(gr))
    keep_levels <- intersect(unique(present), GenomeInfoDb::seqlevels(target_seqinfo))
    dropped_contigs <- setdiff(unique(present), keep_levels)
    keep <- present %in% keep_levels
    rebuilt <- GenomicRanges::GRanges(
        seqnames = factor(present[keep], levels = keep_levels),
        ranges = IRanges::ranges(gr)[keep],
        strand = "*",
        seqinfo = target_seqinfo[keep_levels]
    )
    S4Vectors::mcols(rebuilt) <- S4Vectors::mcols(gr)[keep, , drop = FALSE]
    rebuilt$variant_key <- paste0(
        as.character(GenomeInfoDb::seqnames(rebuilt)), ":",
        GenomicRanges::start(rebuilt), "_", rebuilt$REF, ">", rebuilt$ALT
    )
    list(
        ranges = rebuilt,
        dropped_contigs = dropped_contigs,
        dropped_records = sum(!keep)
    )
}

# Contiguous, equally sized index groups: one per worker. locateVariants and
# predictCoding accept mixed chromosomes, so splitting by position keeps every
# worker busy even when a chunk holds a single chromosome.
balanced_index_groups <- function(indices, workers) {
    n <- length(indices)
    if (!n) return(list())
    groups <- max(1L, min(as.integer(workers), n))
    if (groups == 1L) return(list(indices))
    unname(split(indices, ceiling(seq_len(n) * groups / n)))
}

locate_all_variants <- function(gr, txdb, promoter_upstream = 2000L,
                                promoter_downstream = 200L, workers = 1L) {
    region <- VariantAnnotation::AllVariants(
        promoter = VariantAnnotation::PromoterVariants(
            upstream = as.integer(promoter_upstream),
            downstream = as.integer(promoter_downstream)
        )
    )
    index_groups <- balanced_index_groups(seq_along(gr), workers)
    pieces <- parallel_map(index_groups, function(indices) {
        located <- suppressWarnings(VariantAnnotation::locateVariants(
            gr[indices], txdb, region, ignore.strand = TRUE
        ))
        located$QUERYID <- indices[located$QUERYID]
        located
    }, workers)
    located <- do.call(c, unname(pieces))
    located
}

predict_coding_variants <- function(gr, txdb, bsgenome, coding_query_ids,
                                    workers = 1L) {
    coding_query_ids <- sort(unique(coding_query_ids))
    if (!length(coding_query_ids)) return(NULL)
    index_groups <- balanced_index_groups(coding_query_ids, workers)
    pieces <- parallel_map(index_groups, function(indices) {
        query <- gr[indices]
        predicted <- suppressWarnings(VariantAnnotation::predictCoding(
            query = query, subject = txdb, seqSource = bsgenome,
            varAllele = Biostrings::DNAStringSet(query$ALT), ignore.strand = TRUE
        ))
        predicted$QUERYID <- indices[predicted$QUERYID]
        predicted
    }, workers)
    do.call(c, unname(pieces))
}

flatten_integer_list <- function(values) {
    if (is.null(values)) return(character(0))
    vapply(as.list(values), function(value) {
        if (!length(value) || all(is.na(value))) NA_character_ else paste(value, collapse = "-")
    }, character(1))
}

map_gene_symbols <- function(gene_ids, orgdb,
                             gene_keytype = "ENTREZID", symbol_column = "SYMBOL") {
    gene_ids <- unique(gene_ids[!is.na(gene_ids) & nzchar(gene_ids)])
    if (!length(gene_ids)) return(character(0))
    symbols <- suppressMessages(AnnotationDbi::mapIds(
        orgdb, keys = gene_ids, column = symbol_column,
        keytype = gene_keytype, multiVals = "first"
    ))
    symbols <- unlist(symbols)
    symbols[is.na(symbols)] <- NA_character_
    stats::setNames(as.character(symbols), gene_ids)
}

annotate_rsids <- function(gr, snplocs) {
    rsid <- rep(NA_character_, length(gr))
    if (is.null(snplocs) || !length(gr)) return(rsid)
    is_snv <- nchar(gr$REF) == 1L & nchar(gr$ALT) == 1L
    if (!any(is_snv)) return(rsid)
    query <- GenomicRanges::GRanges(
        as.character(GenomeInfoDb::seqnames(gr))[is_snv],
        IRanges::ranges(gr)[is_snv]
    )
    query <- tryCatch({
        copy <- query
        GenomeInfoDb::seqlevelsStyle(copy) <- "NCBI"
        copy
    }, error = function(error) query)
    query <- GenomeInfoDb::keepSeqlevels(
        query,
        intersect(GenomeInfoDb::seqlevels(query), GenomeInfoDb::seqlevels(snplocs)),
        pruning.mode = "coarse"
    )
    if (!length(query)) return(rsid)
    snps <- tryCatch(
        BSgenome::snpsByOverlaps(snplocs, query),
        error = function(error) NULL
    )
    if (is.null(snps) || !length(snps)) return(rsid)
    query_key <- paste(as.character(GenomeInfoDb::seqnames(query)), GenomicRanges::start(query))
    snp_key <- paste(as.character(GenomeInfoDb::seqnames(snps)), GenomicRanges::pos(snps))
    snv_alt <- gr$ALT[is_snv]
    ambig <- as.character(snps$alleles_as_ambig)
    allowed <- Biostrings::IUPAC_CODE_MAP[ambig]
    hits <- match(snp_key, query_key)
    ok <- !is.na(hits)
    hits <- hits[ok]
    ids <- as.character(snps$RefSNP_id)[ok]
    allowed <- allowed[ok]
    compatible <- mapply(function(alt, code) grepl(alt, code, fixed = TRUE),
                         snv_alt[hits], allowed)
    hits <- hits[compatible]
    ids <- ids[compatible]
    if (!length(hits)) return(rsid)
    collapsed <- tapply(ids, hits, function(values) paste(unique(values), collapse = ";"))
    snv_positions <- which(is_snv)
    rsid[snv_positions[as.integer(names(collapsed))]] <- as.character(collapsed)
    rsid
}

build_annotation_tables <- function(gr, located, predicted, symbols, rsid = NULL) {
    query <- located$QUERYID
    table <- data.frame(
        variant_key = gr$variant_key[query],
        chromosome = as.character(GenomeInfoDb::seqnames(gr))[query],
        position = GenomicRanges::start(gr)[query],
        REF = gr$REF[query],
        ALT = gr$ALT[query],
        QUAL = gr$QUAL[query],
        FILTER = gr$FILTER[query],
        DP = gr$DP[query],
        location = as.character(located$LOCATION),
        transcript_id = as.character(located$TXID),
        gene_id = as.character(located$GENEID),
        stringsAsFactors = FALSE
    )
    if (!is.null(rsid)) table$rsID <- rsid[query]
    # length-matched so an empty chunk (no located variants) still yields a
    # zero-row table with the full set of columns
    blank <- rep(NA_character_, nrow(table))
    table$consequence <- blank
    table$ref_codon <- blank
    table$var_codon <- blank
    table$ref_aa <- blank
    table$var_aa <- blank
    table$protein_position <- blank
    if (!is.null(predicted) && length(predicted)) {
        key_table <- paste(table$variant_key, table$transcript_id)
        key_pred <- paste(gr$variant_key[predicted$QUERYID], as.character(predicted$TXID))
        hit <- match(key_table, key_pred)
        has <- !is.na(hit)
        table$consequence[has] <- as.character(predicted$CONSEQUENCE)[hit[has]]
        table$ref_codon[has] <- as.character(predicted$REFCODON)[hit[has]]
        table$var_codon[has] <- as.character(predicted$VARCODON)[hit[has]]
        table$ref_aa[has] <- as.character(predicted$REFAA)[hit[has]]
        table$var_aa[has] <- as.character(predicted$VARAA)[hit[has]]
        table$protein_position[has] <- flatten_integer_list(predicted$PROTEINLOC)[hit[has]]
    }
    table$effect <- effect_for(table$location, table$consequence)
    table$effect_class <- effect_class(table$effect)
    table$gene_symbol <- as.character(unname(symbols[table$gene_id]))
    table$gene_symbol[is.na(table$gene_id)] <- NA_character_
    table$amino_acid_change <- ifelse(
        !is.na(table$ref_aa) & !is.na(table$protein_position),
        paste0(table$ref_aa, table$protein_position, table$var_aa),
        NA_character_
    )
    ordering <- c(
        "variant_key", "chromosome", "position", "REF", "ALT",
        if (!is.null(rsid)) "rsID", "QUAL", "FILTER", "DP", "gene_symbol", "gene_id",
        "transcript_id", "location", "consequence", "effect", "effect_class",
        "amino_acid_change", "ref_codon", "var_codon", "protein_position"
    )
    table[, ordering]
}

collapse_most_severe <- function(per_transcript) {
    if (!nrow(per_transcript)) return(per_transcript)
    rank <- match(per_transcript$effect, consequence_severity_levels())
    rank[is.na(rank)] <- length(consequence_severity_levels()) + 1L
    ordering <- order(per_transcript$variant_key, rank, per_transcript$transcript_id)
    ordered <- per_transcript[ordering, , drop = FALSE]
    first <- !duplicated(ordered$variant_key)
    collapsed <- ordered[first, , drop = FALSE]
    counts <- table(ordered$variant_key)
    collapsed$n_transcripts <- as.integer(counts[collapsed$variant_key])
    genes <- tapply(ordered$gene_symbol, ordered$variant_key, function(values) {
        values <- sort(unique(values[!is.na(values)]))
        if (length(values)) paste(values, collapse = ";") else NA_character_
    })
    collapsed$all_genes <- as.character(genes[collapsed$variant_key])
    rownames(collapsed) <- NULL
    collapsed
}

coding_effect_classes <- function() c("Protein-altering", "Splice site", "Coding (other)")

# genes: data frame with gene_id, gene_symbol and coding (any coding effect).
annotated_gene_list <- function(genes, id_type = c("SYMBOL", "ENTREZID"),
                                coding_only = FALSE) {
    id_type <- match.arg(id_type)
    rows <- genes
    if (is.null(rows) || !nrow(rows)) return(character(0))
    if (isTRUE(coding_only)) rows <- rows[rows$coding, , drop = FALSE]
    ids <- if (identical(id_type, "SYMBOL")) rows$gene_symbol else rows$gene_id
    ids <- unique(ids[!is.na(ids) & nzchar(ids)])
    sort(ids)
}

# ---------------------------------------------------------------------------
# Plot summaries
# ---------------------------------------------------------------------------

titv_ratio <- function(ref, alt) {
    ref <- toupper(as.character(ref))
    alt <- toupper(as.character(alt))
    snv <- !is.na(ref) & !is.na(alt) & nchar(ref) == 1L & nchar(alt) == 1L &
        ref %in% c("A", "C", "G", "T") & alt %in% c("A", "C", "G", "T") & ref != alt
    pair <- paste0(ref[snv], alt[snv])
    transitions <- sum(pair %in% c("AG", "GA", "CT", "TC"))
    transversions <- sum(snv) - transitions
    list(
        transitions = transitions,
        transversions = transversions,
        ratio = if (transversions > 0L) transitions / transversions else NA_real_
    )
}

substitution_counts <- function(ref, alt) {
    ref <- toupper(as.character(ref))
    alt <- toupper(as.character(alt))
    snv <- nchar(ref) == 1L & nchar(alt) == 1L & ref %in% c("A", "C", "G", "T") &
        alt %in% c("A", "C", "G", "T") & ref != alt
    pair <- paste0(ref[snv], ">", alt[snv])
    pair <- factor(pair, levels = c(
        "A>C", "A>G", "A>T", "C>A", "C>G", "C>T",
        "G>A", "G>C", "G>T", "T>A", "T>C", "T>G"
    ))
    counts <- as.data.frame(table(substitution = pair), stringsAsFactors = FALSE)
    counts$class <- ifelse(counts$substitution %in% c("A>G", "G>A", "C>T", "T>C"),
                           "Transition", "Transversion")
    counts
}

per_sample_variant_counts <- function(vcf) {
    genotypes <- tryCatch(VariantAnnotation::geno(vcf)$GT, error = function(error) NULL)
    if (is.null(genotypes) || !length(genotypes)) return(NULL)
    genotypes <- as.matrix(genotypes)
    non_ref <- !is.na(genotypes) & !genotypes %in% c("0/0", "0|0", "./.", ".|.", ".", "0")
    data.frame(
        sample = colnames(genotypes),
        variants = colSums(non_ref),
        stringsAsFactors = FALSE
    )
}

# ---------------------------------------------------------------------------
# Streamed annotation: one chunk of the filtered VCF at a time
# ---------------------------------------------------------------------------

# Annotate one chunk. Returns the two tables plus the counts the report needs.
annotate_vcf_chunk <- function(vcf, genome, options, workers) {
    expanded <- expand_variants(vcf)
    base <- list(
        input_records = nrow(vcf), alleles = 0L, coding = 0L,
        dropped_alleles = expanded$dropped_alleles,
        dropped_contigs = character(0), dropped_records = 0L,
        per_transcript = NULL, per_variant = NULL, seqlevels = character(0), rsid = FALSE
    )
    if (!nrow(expanded$vcf)) return(base)
    ranges <- variant_ranges(expanded)
    harmonised <- harmonise_seqlevels(ranges, GenomeInfoDb::seqinfo(genome$txdb))
    base$dropped_contigs <- harmonised$dropped_contigs
    base$dropped_records <- harmonised$dropped_records
    gr <- harmonised$ranges
    if (!length(gr)) return(base)

    located <- locate_all_variants(gr, genome$txdb, options$promoter_upstream,
                                   options$promoter_downstream, workers)
    coding_ids <- unique(located$QUERYID[as.character(located$LOCATION) == "coding"])
    predicted <- predict_coding_variants(gr, genome$txdb, genome$bsgenome, coding_ids, workers)
    symbols <- map_gene_symbols(as.character(located$GENEID), genome$orgdb,
                                gene_keytype = genome$gene_keytype, symbol_column = genome$symbol_column)
    rsid <- NULL
    if (isTRUE(options$add_rsid) && !is.null(genome$snplocs)) {
        rsid <- annotate_rsids(gr, genome$snplocs)
    }
    per_transcript <- build_annotation_tables(gr, located, predicted, symbols, rsid)
    per_variant <- collapse_most_severe(per_transcript)
    base$alleles <- length(gr)
    base$coding <- length(coding_ids)
    base$per_transcript <- per_transcript
    base$per_variant <- per_variant
    base$seqlevels <- unique(as.character(GenomeInfoDb::seqnames(gr)))
    base$rsid <- !is.null(rsid)
    base
}

new_annotation_accumulator <- function() {
    effects <- consequence_severity_levels()
    locations <- c("coding", "spliceSite", "promoter", "fiveUTR", "threeUTR", "intron", "intergenic")
    substitutions <- c("A>C", "A>G", "A>T", "C>A", "C>G", "C>T", "G>A", "G>C", "G>T", "T>A", "T>C", "T>G")
    list(
        chunks = 0L, input_records = 0, alleles = 0, coding = 0,
        dropped_alleles = 0, dropped_records = 0, dropped_contigs = character(0), rsid = FALSE,
        per_variant_rows = 0, per_transcript_rows = 0,
        type_counts = stats::setNames(rep(0, 3), c("SNV", "Indel", "MNV")),
        effect_counts = stats::setNames(rep(0, length(effects)), effects),
        location_counts = stats::setNames(rep(0, length(locations)), locations),
        substitution_counts = stats::setNames(rep(0, length(substitutions)), substitutions),
        chromosome_counts = numeric(0),
        qual = numeric(0), dp = numeric(0),
        genes = data.frame(gene_id = character(0), gene_symbol = character(0),
                           coding = logical(0), stringsAsFactors = FALSE),
        seqlevels = character(0),
        per_variant_preview = NULL, per_transcript_preview = NULL
    )
}

append_csv <- function(frame, path) {
    exists <- file.exists(path)
    utils::write.table(frame, path, sep = ",", dec = ".", qmethod = "double",
                       row.names = FALSE, col.names = !exists, append = exists)
}

take_preview <- function(current, frame, limit) {
    have <- if (is.null(current)) 0L else nrow(current)
    want <- limit - have
    if (want <= 0L || !nrow(frame)) return(current)
    piece <- utils::head(frame, want)
    if (is.null(current)) piece else rbind(current, piece)
}

accumulate_annotation <- function(acc, chunk, per_variant_file, per_transcript_file,
                                  preview_rows = table_preview_rows()) {
    acc$chunks <- acc$chunks + 1L
    acc$input_records <- acc$input_records + chunk$input_records
    acc$alleles <- acc$alleles + chunk$alleles
    acc$coding <- acc$coding + chunk$coding
    acc$dropped_alleles <- acc$dropped_alleles + chunk$dropped_alleles
    acc$dropped_records <- acc$dropped_records + chunk$dropped_records
    acc$dropped_contigs <- union(acc$dropped_contigs, chunk$dropped_contigs)
    acc$rsid <- acc$rsid || isTRUE(chunk$rsid)
    pv <- chunk$per_variant
    pt <- chunk$per_transcript
    if (is.null(pv) || !nrow(pv)) return(acc)

    append_csv(pv, per_variant_file)
    append_csv(pt, per_transcript_file)
    acc$per_variant_rows <- acc$per_variant_rows + nrow(pv)
    acc$per_transcript_rows <- acc$per_transcript_rows + nrow(pt)
    acc$per_variant_preview <- take_preview(acc$per_variant_preview, pv, preview_rows)
    acc$per_transcript_preview <- take_preview(acc$per_transcript_preview, pt, preview_rows)

    type <- ifelse(nchar(pv$REF) == 1L & nchar(pv$ALT) == 1L, "SNV",
            ifelse(nchar(pv$REF) == nchar(pv$ALT), "MNV", "Indel"))
    acc$type_counts <- add_named_counts(acc$type_counts, table(type))
    acc$effect_counts <- add_named_counts(acc$effect_counts,
        table(factor(pv$effect, levels = names(acc$effect_counts))))
    acc$location_counts <- add_named_counts(acc$location_counts,
        table(factor(pt$location, levels = names(acc$location_counts))))
    subs <- substitution_counts(pv$REF, pv$ALT)
    acc$substitution_counts <- add_named_counts(acc$substitution_counts,
        stats::setNames(subs$Freq, as.character(subs$substitution)))
    acc$chromosome_counts <- add_named_counts(acc$chromosome_counts, table(pv$chromosome))
    acc$qual <- c(acc$qual, pv$QUAL[!is.na(pv$QUAL)])
    acc$dp <- c(acc$dp, pv$DP[!is.na(pv$DP)])
    acc$seqlevels <- union(acc$seqlevels, chunk$seqlevels)

    with_gene <- pt[!is.na(pt$gene_id) & nzchar(pt$gene_id), c("gene_id", "gene_symbol", "effect_class"), drop = FALSE]
    if (nrow(with_gene)) {
        with_gene$coding <- with_gene$effect_class %in% coding_effect_classes()
        with_gene$effect_class <- NULL
        combined <- rbind(acc$genes, with_gene)
        combined <- combined[order(combined$gene_id, !combined$coding), , drop = FALSE]
        acc$genes <- combined[!duplicated(combined$gene_id), , drop = FALSE]
        rownames(acc$genes) <- NULL
    }
    acc
}

# Stream the filtered VCF through the annotation pipeline. Tables go to CSV
# files in output_dir; the returned list holds only summaries and previews.
stream_annotation <- function(path, index, genome, options, workers, output_dir,
                              total_records, has_dp,
                              chunk_size = annotation_chunk_size(), progress = NULL) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    per_variant_file <- file.path(output_dir, "per_variant.csv")
    per_transcript_file <- file.path(output_dir, "per_transcript.csv")
    unlink(c(per_variant_file, per_transcript_file))
    param <- VariantAnnotation::ScanVcfParam(
        fixed = c("ALT", "QUAL", "FILTER"),
        info = if (isTRUE(has_dp)) "DP" else character(0),
        geno = NA
    )
    tab <- Rsamtools::TabixFile(path, index, yieldSize = chunk_size)
    open(tab)
    on.exit(close(tab), add = TRUE)
    acc <- new_annotation_accumulator()
    total <- max(total_records, 1)
    repeat {
        chunk <- VariantAnnotation::readVcf(tab, genome = genome$key, param = param)
        if (!nrow(chunk)) break
        result <- annotate_vcf_chunk(chunk, genome, options, workers)
        acc <- accumulate_annotation(acc, result, per_variant_file, per_transcript_file)
        if (!is.null(progress)) progress(min(acc$input_records / total, 1), acc)
    }
    if (!acc$alleles) {
        stop("None of the filtered variants could be annotated against the selected gene model.", call. = FALSE)
    }
    txdb_levels <- GenomeInfoDb::seqlevels(genome$txdb)
    seqlevels <- c(intersect(txdb_levels, acc$seqlevels), setdiff(acc$seqlevels, txdb_levels))
    transitions <- sum(acc$substitution_counts[c("A>G", "G>A", "C>T", "T>C")])
    transversions <- sum(acc$substitution_counts) - transitions
    list(
        files = list(per_variant = per_variant_file, per_transcript = per_transcript_file),
        previews = list(per_variant = acc$per_variant_preview, per_transcript = acc$per_transcript_preview),
        rows = list(per_variant = acc$per_variant_rows, per_transcript = acc$per_transcript_rows),
        stats = list(
            type_counts = acc$type_counts,
            effect_counts = acc$effect_counts,
            location_counts = acc$location_counts,
            substitution_counts = acc$substitution_counts,
            chromosome_counts = acc$chromosome_counts,
            titv = list(transitions = transitions, transversions = transversions,
                        ratio = if (transversions > 0) transitions / transversions else NA_real_),
            qual = acc$qual,
            dp = acc$dp
        ),
        genes = acc$genes,
        seqlevels = seqlevels,
        report = list(
            input_records = acc$input_records,
            alleles_annotated = acc$alleles,
            dropped_alleles = acc$dropped_alleles,
            dropped_contigs = acc$dropped_contigs,
            dropped_records = acc$dropped_records,
            coding_variants = acc$coding,
            genes = sum(!is.na(acc$genes$gene_symbol)),
            chunks = acc$chunks,
            chunk_size = chunk_size,
            rsid = acc$rsid,
            genome = genome$label,
            packages = package_versions(genome$packages),
            variantannotation = as.character(utils::packageVersion("VariantAnnotation"))
        )
    )
}
