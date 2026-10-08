# Private data root (NASQAR_DATA_ROOT): input files read in place in a private
# instance. Pure helpers, sourced globally so every server module can use them.

data_root <- Sys.getenv("NASQAR_DATA_ROOT", unset = "")
data_root <- if (nzchar(data_root) && dir.exists(data_root)) normalizePath(data_root, winslash = "/", mustWork = FALSE) else NULL

resolve_hpc_file <- function(path) {
    if (is.null(data_root)) stop("This instance has no private space configured.")
    if (is.null(path) || !nzchar(trimws(path))) stop("Enter the path of a counts file in the private space.")
    resolved <- normalizePath(trimws(path), winslash = "/", mustWork = TRUE)
    inside <- nasqar_path_inside_roots(resolved)
    if (!inside) stop("The file must be in the private space.")
    if (dir.exists(resolved)) stop("Select a file, not a directory.")
    resolved
}
