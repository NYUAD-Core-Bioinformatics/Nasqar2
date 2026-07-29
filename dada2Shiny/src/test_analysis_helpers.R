source("analysis-helpers.R")

metadata <- data.frame(
    condition = c("control", "treated"),
    row.names = c("sample_b", "sample_a")
)
aligned <- align_sample_metadata(metadata, c("sample_a", "sample_b"))
stopifnot(
    identical(rownames(aligned), c("sample_a", "sample_b")),
    identical(as.character(aligned$condition), c("treated", "control"))
)

failed <- FALSE
tryCatch(
    align_sample_metadata(metadata, c("sample_a", "missing")),
    error = function(error) failed <<- TRUE
)
stopifnot(failed)

abundance <- c(asv_a = 3, asv_b = 10, asv_c = 5)
stopifnot(identical(
    top_taxa_names(abundance, 2L),
    c("asv_b", "asv_c")
))
stopifnot(dir.exists(nasqar_exchange_dir(create = TRUE)))

cat("DADA2 analysis helper tests passed\n")
