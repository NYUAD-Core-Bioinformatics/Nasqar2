source("exchange-helpers.R")

path <- file.path(
    nasqar_exchange_dir(create = TRUE),
    "550e8400-e29b-41d4-a716-446655440000.csv"
)
write.csv(data.frame(gene = "g1"), path, row.names = FALSE)
stopifnot(identical(validate_exchange_path(path), normalizePath(path)))

failed <- FALSE
tryCatch(
    validate_exchange_path("/etc/passwd"),
    error = function(error) failed <<- TRUE
)
stopifnot(failed)
stopifnot(is.character(installed_organism_choices()))

legacy <- data.frame(gene = c("gene1", "gene2"), value = 1:2,
                     check.names = FALSE)
names(legacy)[1L] <- ""
normalized <- normalize_enrichment_columns(legacy)
stopifnot(identical(names(normalized), c("X", "value")))
stopifnot(identical(normalized$X, c("gene1", "gene2")))

cat("ORA exchange helper tests passed\n")
