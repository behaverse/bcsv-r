#' Null-coalesce: return `x` if non-NULL, else `y`.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


#' v0 CSV dialect keys honored by the readers.
#' @keywords internal
#' @noRd
.SUPPORTED_DIALECT_KEYS <- c("delimiter", "encoding")


#' Parse a metadata `dialect` block into delimiter / encoding / unsupported keys.
#'
#' Callers decide whether unsupported keys should emit a Python-style warning
#' (read_bcsv) or be collected into a ValidationResult (validate_bcsv).
#'
#' @keywords internal
#' @noRd
.parse_dialect <- function(metadata) {
  dialect <- metadata$dialect %||% list()
  list(
    delimiter = dialect$delimiter %||% ",",
    encoding = dialect$encoding %||% "UTF-8",
    unsupported = setdiff(names(dialect), .SUPPORTED_DIALECT_KEYS)
  )
}


#' Resolve a default metadata path from a CSV path per spec B6.
#'
#' Strict rule: requires `.csv` (case-insensitive) suffix. Otherwise the user
#' must pass `metadata_file` / `file` explicitly. Same behaviour in
#' read_bcsv, write_bcsv, and validate_bcsv.
#'
#' @keywords internal
#' @noRd
.default_metadata_path <- function(data_path) {
  if (grepl("\\.csv$", data_path, ignore.case = TRUE)) {
    sub("\\.csv$", ".json", data_path, ignore.case = TRUE)
  } else {
    bcsv_schema_error(sprintf(
      "data_file %s does not end in '.csv'; pass metadata_file explicitly.",
      shQuote(data_path)
    ))
  }
}


#' Render a bcsv validation result or metadata object as a tidy table
#'
#' @param x A validation result (from [validate_bcsv()]) or a metadata object
#'   (from [document_bcsv()] or a parsed sidecar).
#' @return A [tibble][tibble::tibble]: for a result, one row per issue
#'   (`severity`, `code`, `location`, `message`); for metadata, one row per
#'   column with every declared property (`levels` shown as quoted, comma-joined
#'   text).
#' @export
tabularize <- function(x) {
  if (!is.list(x)) {
    stop("tabularize() expects a bcsv validation result or metadata object (a list).",
         call. = FALSE)
  }
  if (!is.null(x$table_schema)) return(.tabularize_metadata(x))
  if (!is.null(x$valid) && !is.null(x$errors)) return(.tabularize_result(x))
  stop("tabularize(): unrecognized object; expected a validation result or metadata.",
       call. = FALSE)
}

#' @rdname tabularize
#' @export
tabularise <- tabularize

.tabularize_result <- function(result) {
  make_rows <- function(issues, severity) {
    if (length(issues) == 0L) return(NULL)
    do.call(rbind, lapply(issues, function(issue) {
      data.frame(
        severity = severity,
        code     = issue$code %||% NA_character_,
        location = issue$location %||% NA_character_,
        message  = issue$message %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }))
  }
  rows <- rbind(make_rows(result$errors, "error"),
                make_rows(result$warnings, "warning"))
  if (is.null(rows)) {
    rows <- data.frame(severity = character(), code = character(),
                       location = character(), message = character(),
                       stringsAsFactors = FALSE)
  }
  tibble::as_tibble(rows)
}

.COLUMN_PROP_ORDER <- c(
  "name", "datatype", "label", "description", "unit", "levels",
  "minimum", "maximum", "min_length", "max_length", "required", "format"
)

.tabularize_metadata <- function(metadata) {
  columns <- metadata$table_schema$columns
  if (is.null(columns) || length(columns) == 0L) {
    return(tibble::tibble(name = character(), datatype = character()))
  }
  has_key <- function(k) any(vapply(columns, function(col) !is.null(col[[k]]), logical(1)))
  keys <- Filter(has_key, .COLUMN_PROP_ORDER)
  all_keys <- unique(unlist(lapply(columns, names)))
  keys <- c(keys, sort(setdiff(all_keys, keys)))
  fmt_cell <- function(col, k) {
    v <- col[[k]]
    if (is.null(v)) return(NA_character_)
    if (k == "levels") return(paste(sprintf('"%s"', unlist(v)), collapse = ", "))
    paste(as.character(unlist(v)), collapse = ", ")
  }
  rows <- lapply(columns, function(col) {
    cells <- vapply(keys, function(k) fmt_cell(col, k), character(1))
    as.data.frame(as.list(stats::setNames(cells, keys)),
                  stringsAsFactors = FALSE, check.names = FALSE)
  })
  tibble::as_tibble(do.call(rbind, rows))
}
