# Private data root (NASQAR_DATA_ROOT): input files read in place in a private
# instance. Pure helpers, sourced globally so every server module can use them.

data_root <- Sys.getenv("NASQAR_DATA_ROOT", unset = "")
data_root <- if (nzchar(data_root) && dir.exists(data_root)) normalizePath(data_root, winslash = "/", mustWork = FALSE) else NULL

