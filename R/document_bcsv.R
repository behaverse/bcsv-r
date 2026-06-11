#' Generate a bcsv metadata draft from a typed tibble
#'
#' Infers bcsv datatypes from each column of `data`, then layers in the
#' overrides supplied via `column_descriptions`. Locked signature mirrors
#' the Python `bcsv.document_bcsv` (see the [API spec
#' §3](https://github.com/behaverse/bcsv-r/blob/main/docs/04-api-spec.md)).
#'
#' @param data A data frame / tibble whose columns have already been typed
#'   (factors via `factor()` / `ordered()`, dates via `as.Date()`, etc.).
#' @param description Required description of what the table is about. The
#'   bcsv schema (since v26.0608) requires every table to carry one.
#' @param data_url Optional URL/path that populates the metadata's `url`
#'   field. When `NULL` (default), the field is omitted; [write_bcsv()]
#'   populates it from the `file` basename.
#' @param column_descriptions Named list of per-column overrides. Allowed
#'   inner keys: `label`, `description`, `unit`, `minimum`, `maximum`,
#'   `min_length`, `max_length`, `na_strings`, `required`, `format`.
#'   Unknown keys raise `bcsv_unknown_key_error`.
#' @param pretty_name Optional human-readable dataset title.
#' @param creator A length-1 character vector (simple form), a single list
#'   with `name`/`email`/`orcid`/`affiliation` keys (structured form), or
#'   a list of such structured creators (multi-author form).
#' @param date_created A [Date] for the metadata's `date_created` field.
#'   Defaults to today.
#' @param primary_key A column name or character vector of column names
#'   declaring the primary key. Stored under `table_schema$primary_key`.
#'
#' @return A list of metadata properties suitable for [write_bcsv()].
#' @export
document_bcsv <- function(data,
                         description,
                         data_url = NULL,
                         column_descriptions = NULL,
                         pretty_name = NULL,
                         creator = NULL,
                         date_created = NULL,
                         primary_key = NULL) {
  if (missing(description) || !is.character(description) ||
      length(description) != 1L || !nzchar(trimws(description))) {
    stop(paste0(
      "document_bcsv() requires a non-empty `description`\u2014the bcsv ",
      "schema requires every table to describe what it is about."
    ), call. = FALSE)
  }
  .validate_description_keys(column_descriptions)

  columns_meta <- lapply(names(data), function(col_name) {
    inferred <- .infer_column(data[[col_name]])
    overrides <- column_descriptions[[col_name]]
    if (is.null(overrides)) overrides <- list()
    c(list(name = col_name), inferred, overrides)
  })

  metadata <- list(
    `@context` = CONTEXT_URL,
    description = description,
    table_schema = list(columns = columns_meta)
  )

  if (!is.null(data_url))     metadata$url <- data_url
  if (!is.null(pretty_name))  metadata$pretty_name <- pretty_name
  if (!is.null(creator))      metadata$creator <- creator
  metadata$date_created <- format(
    if (is.null(date_created)) Sys.Date() else date_created,
    "%Y-%m-%d"
  )
  if (!is.null(primary_key)) {
    metadata$table_schema$primary_key <- primary_key
  }

  class(metadata) <- c("bcsv_metadata", "list")
  metadata
}

#' @export
print.bcsv_metadata <- function(x, ...) {
  title <- x$pretty_name %||% "(unnamed)"
  cat(sprintf("<bcsv metadata>  %s\n", title))
  print(tabularize(x))
  invisible(x)
}


# --- helpers ---------------------------------------------------------------

.ALLOWED_DESCRIPTION_KEYS <- c(
  "label", "description", "unit",
  "minimum", "maximum",
  "min_length", "max_length",
  "na_strings", "required", "format"
)

.validate_description_keys <- function(column_descriptions) {
  if (is.null(column_descriptions) || length(column_descriptions) == 0L) {
    return(invisible(NULL))
  }
  for (col_name in names(column_descriptions)) {
    keys <- names(column_descriptions[[col_name]])
    unknown <- setdiff(keys, .ALLOWED_DESCRIPTION_KEYS)
    if (length(unknown) > 0L) {
      bcsv_unknown_key_error(sprintf(
        "column_descriptions[[%s]] has unknown key(s) [%s]. Allowed keys: [%s].",
        shQuote(col_name),
        paste(shQuote(sort(unknown)), collapse = ", "),
        paste(shQuote(sort(.ALLOWED_DESCRIPTION_KEYS)), collapse = ", ")
      ))
    }
  }
}

# Inference order matters: factors are tested before is.integer (factor inherits
# integer at the storage layer), and hms is tested before is.numeric (hms is a
# numeric-valued S3 class).
.infer_column <- function(x) {
  if (is.factor(x)) {
    return(list(
      datatype = if (is.ordered(x)) "ordered" else "categorical",
      levels = as.list(levels(x))
    ))
  }
  if (inherits(x, "Date"))    return(list(datatype = "date"))
  if (inherits(x, "POSIXct")) return(list(datatype = "datetime"))
  if (inherits(x, "hms"))     return(list(datatype = "time"))
  if (is.logical(x))          return(list(datatype = "boolean"))
  if (is.integer(x))          return(list(datatype = "integer"))
  if (is.numeric(x))          return(list(datatype = "number"))
  list(datatype = "string")
}
