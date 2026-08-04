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

validate_exchange_path <- function(path, extension = ".csv") {
    if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
        stop("Invalid NASQAR2 exchange path.", call. = FALSE)
    }

    root <- nasqar_exchange_dir(create = TRUE)
    resolved <- normalizePath(path, mustWork = TRUE)
    prefix <- paste0(root, .Platform$file.sep)
    uuid_file <- paste0(
        "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-",
        "[0-9a-fA-F]{4}-[0-9a-fA-F]{12}",
        gsub(".", "[.]", extension, fixed = TRUE),
        "$"
    )
    if (!startsWith(resolved, prefix) ||
        !grepl(uuid_file, basename(resolved))) {
        stop("Exchange file is outside the NASQAR2 exchange directory.",
             call. = FALSE)
    }

    info <- file.info(resolved)
    if (is.na(info$isdir) || info$isdir || info$size > 600 * 1024^2) {
        stop("Exchange file is invalid or too large.", call. = FALSE)
    }
    resolved
}

normalize_enrichment_columns <- function(data) {
    if (ncol(data) > 0L && (is.na(names(data)[1L]) || !nzchar(names(data)[1L]))) {
        names(data)[1L] <- "X"
        names(data) <- make.unique(names(data), sep = ".")
    }
    data
}

installed_organism_choices <- function() {
    choices <- c(
        "Human" = "org.Hs.eg.db",
        "Mouse" = "org.Mm.eg.db",
        "Rat" = "org.Rn.eg.db",
        "Yeast" = "org.Sc.sgd.db",
        "Fly" = "org.Dm.eg.db",
        "Arabidopsis" = "org.At.tair.db",
        "Zebrafish" = "org.Dr.eg.db",
        "Bovine" = "org.Bt.eg.db",
        "Worm" = "org.Ce.eg.db",
        "Chicken" = "org.Gg.eg.db",
        "Canine" = "org.Cf.eg.db",
        "Pig" = "org.Ss.eg.db",
        "Rhesus" = "org.Mmu.eg.db",
        "E. coli K12" = "org.EcK12.eg.db",
        "Xenopus" = "org.Xl.eg.db",
        "Chimpanzee" = "org.Pt.eg.db",
        "Anopheles" = "org.Ag.eg.db",
        "Malaria parasite" = "org.Pf.plasmo.db",
        "E. coli Sakai" = "org.EcSakai.eg.db"
    )
    choices[vapply(
        unname(choices),
        requireNamespace,
        quietly = TRUE,
        FUN.VALUE = logical(1)
    )]
}
