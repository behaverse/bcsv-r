#' SHA-256 of a file
#'
#' Streams the file through `digest::digest()` so large CSVs don't get loaded
#' into memory. Returns lowercase hex.
#'
#' @param path Filesystem path.
#' @return Length-1 character vector (64 hex characters).
#' @keywords internal
sha256_of_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}
