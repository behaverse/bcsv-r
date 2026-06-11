#' Access to the vendored bcsv JSON Schema
#'
#' The schema file is shipped inside the package at `inst/extdata/schema.json`
#' and is pinned to a specific calver release. To bump, replace the file in
#' `inst/extdata/` and update [SCHEMA_VERSION] below.
#'
#' @name bcsv-schema
NULL


#' @rdname bcsv-schema
#' @export
SCHEMA_VERSION <- "26.0610"

#' @rdname bcsv-schema
#' @export
SCHEMA_URL <- paste0(
  "https://behaverse.org/schemas/bcsv/v", SCHEMA_VERSION, "/schema.json"
)

#' @rdname bcsv-schema
#' @export
CONTEXT_URL <- "https://behaverse.org/schemas/bcsv/context.jsonld"


#' Return the path to the vendored schema.json on disk.
#'
#' @return Length-1 character vector pointing at the installed schema.json.
#' @keywords internal
schema_path <- function() {
  system.file("extdata", "schema.json", package = "bcsv", mustWork = TRUE)
}

#' Return the vendored bcsv JSON Schema as an R list.
#'
#' @return Parsed schema as a list (`jsonlite::fromJSON(simplifyVector=FALSE)`).
#' @keywords internal
load_schema <- function() {
  jsonlite::fromJSON(schema_path(), simplifyVector = FALSE)
}
