source("app.R")

first <- data.frame(
    gene.ids = c("gene_a", "gene_b"),
    sample_1 = c(10, 20),
    check.names = FALSE
)
second <- data.frame(
    gene.ids = c("gene_b", "gene_c"),
    sample_2 = c(30, 40),
    check.names = FALSE
)

merged <- merge_count_frames(list(first, second))
stopifnot(
    identical(merged$gene.ids, c("gene_a", "gene_b", "gene_c")),
    identical(merged$sample_1, c(10, 20, 0)),
    identical(merged$sample_2, c(0, 30, 40))
)

expect_error <- function(expression) {
    raised <- FALSE
    tryCatch(
        force(expression),
        error = function(error) raised <<- TRUE
    )
    stopifnot(raised)
}

expect_error(merge_count_frames(list(
    data.frame(gene.ids = c("gene_a", "gene_a"), sample_1 = c(1, 2))
)))
expect_error(merge_count_frames(list(
    first,
    data.frame(gene.ids = "gene_c", sample_1 = 3)
)))

cat("Gene Count Merger regression tests passed\n")
