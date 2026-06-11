#' Write a data frame + bcsv metadata as an atomic pair
#'
#' Writes the CSV and the metadata JSON via temp-then-rename atomicity so a
#' crash mid-write never leaves a stale/mismatched pair on disk. Locked
#' signature mirrors the Python `bcsv.write_bcsv` (see the [API spec
#' §2](https://github.com/behaverse/bcsv-r/blob/main/docs/04-api-spec.md)).
#'
#' @param data A data frame / tibble to write.
#' @param file Path for the CSV output. Must end in `.csv` (case-insensitive)
#'   for the default `metadata_file` resolution to succeed.
#' @param metadata_obj The bcsv metadata list to associate with `file`. If
#'   `metadata_obj$url` is unset/empty, it is populated from `basename(file)`.
#' @param metadata_file Path for the metadata JSON. If `NULL` (default), uses
#'   `file` with `.csv` replaced by `.json`.
#' @param overwrite Refuse to clobber existing files unless `TRUE`. Default
#'   `FALSE`.
#' @param validate Run schema + constraint validation against `metadata_obj`
#'   (and `data`) before committing to disk. Default `TRUE`. On failure
#'   raises `bcsv_schema_error` or `bcsv_validation_error`; neither file is
#'   touched.
#'
#' @return A list with elements `data` (CSV path written), `metadata` (JSON
#'   path written), and `file_hash` (lowercase-hex SHA-256 of the CSV).
#' @export
write_bcsv <- function(data,
                       file,
                       metadata_obj,
                       metadata_file = NULL,
                       overwrite = FALSE,
                       validate = TRUE) {
  file_path <- as.character(file)
  meta_path <- if (!is.null(metadata_file)) {
    as.character(metadata_file)
  } else {
    .default_metadata_path(file_path)
  }

  if (!isTRUE(overwrite)) {
    for (p in c(file_path, meta_path)) {
      if (file.exists(p)) {
        stop(sprintf(
          "Refusing to overwrite existing file: %s. Pass overwrite = TRUE to force.",
          p
        ), call. = FALSE)
      }
    }
  }

  # Auto-populate url so downstream validation sees a complete document.
  if (is.null(metadata_obj$url) || !nzchar(as.character(metadata_obj$url))) {
    metadata_obj$url <- basename(file_path)
  }

  if (isTRUE(validate)) {
    .enforce_schema(metadata_obj)
    .validate_constraints_for_write(data, metadata_obj)
  }

  csv_dir <- dirname(file_path); if (csv_dir == "") csv_dir <- "."
  json_dir <- dirname(meta_path); if (json_dir == "") json_dir <- "."

  # Track temp files so on.exit can clean up on failure / mid-rename.
  state <- new.env(parent = emptyenv())
  state$csv_tmp <- character(0)
  state$json_tmp <- character(0)
  on.exit({
    for (p in c(state$csv_tmp, state$json_tmp)) {
      if (length(p) && file.exists(p)) try(unlink(p), silent = TRUE)
    }
  })

  # Stage 1: write CSV to temp.
  state$csv_tmp <- tempfile(
    pattern = paste0(basename(file_path), ".tmp."),
    tmpdir = csv_dir,
    fileext = ""
  )
  readr::write_csv(data, state$csv_tmp, na = "")

  # Stage 2: hash + write JSON to temp.
  file_hash <- sha256_of_file(state$csv_tmp)
  metadata_obj$file_hash <- file_hash

  state$json_tmp <- tempfile(
    pattern = paste0(basename(meta_path), ".tmp."),
    tmpdir = json_dir,
    fileext = ""
  )
  json_raw <- jsonlite::toJSON(
    metadata_obj,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  writeLines(json_raw, state$json_tmp, useBytes = TRUE)

  # Stage 3: atomic renames. CSV first; if the second rename fails the CSV is
  # present and metadata is absent—a clean "missing metadata" state that the
  # next read will surface explicitly, rather than a stale-mismatch pair.
  if (!file.rename(state$csv_tmp, file_path)) {
    stop(sprintf("Atomic rename of CSV temp failed: %s -> %s",
                 state$csv_tmp, file_path), call. = FALSE)
  }
  state$csv_tmp <- character(0)

  if (!file.rename(state$json_tmp, meta_path)) {
    stop(sprintf("Atomic rename of metadata temp failed: %s -> %s",
                 state$json_tmp, meta_path), call. = FALSE)
  }
  state$json_tmp <- character(0)

  list(data = file_path, metadata = meta_path, file_hash = file_hash)
}


# --- pre-write constraint checks ------------------------------------------

.validate_constraints_for_write <- function(data, metadata_obj) {
  columns <- metadata_obj$table_schema$columns %||% list()
  for (col_meta in columns) {
    col_name <- col_meta$name
    if (!col_name %in% names(data)) {
      bcsv_validation_error(sprintf(
        "COLUMN_MISSING_IN_DATA: column %s declared in metadata but not in data.",
        shQuote(col_name)
      ))
    }
    .check_column_for_write(data[[col_name]], col_meta)
  }
  .check_primary_key_for_write(data, metadata_obj)
}

.check_column_for_write <- function(series, col_meta) {
  name <- col_meta$name
  datatype <- col_meta$datatype %||% "string"

  if (isTRUE(col_meta$required) && any(is.na(series))) {
    bcsv_validation_error(sprintf(
      "REQUIRED_VIOLATION: column %s has NA value(s) but is marked required.",
      shQuote(name)
    ))
  }

  if (datatype %in% c("integer", "number", "date", "datetime")) {
    non_na <- series[!is.na(series)]
    if (!is.null(col_meta$minimum) && length(non_na) > 0L && any(non_na < col_meta$minimum)) {
      bcsv_validation_error(sprintf(
        "RANGE_VIOLATION: column %s has value(s) below minimum %s.",
        shQuote(name), format(col_meta$minimum)
      ))
    }
    if (!is.null(col_meta$maximum) && length(non_na) > 0L && any(non_na > col_meta$maximum)) {
      bcsv_validation_error(sprintf(
        "RANGE_VIOLATION: column %s has value(s) above maximum %s.",
        shQuote(name), format(col_meta$maximum)
      ))
    }
  }

  if (datatype == "string") {
    lengths <- nchar(as.character(series[!is.na(series)]))
    if (!is.null(col_meta$min_length) && length(lengths) > 0L && any(lengths < col_meta$min_length)) {
      bcsv_validation_error(sprintf(
        "LENGTH_VIOLATION: column %s has value(s) shorter than min_length %d.",
        shQuote(name), col_meta$min_length
      ))
    }
    if (!is.null(col_meta$max_length) && length(lengths) > 0L && any(lengths > col_meta$max_length)) {
      bcsv_validation_error(sprintf(
        "LENGTH_VIOLATION: column %s has value(s) longer than max_length %d.",
        shQuote(name), col_meta$max_length
      ))
    }
  }

  if (datatype %in% c("categorical", "ordered")) {
    declared_levels <- as.character(unlist(col_meta$levels %||% list()))
    if (is.factor(series)) {
      if (!identical(levels(series), declared_levels)) {
        bcsv_validation_error(sprintf(
          "LEVEL_NOT_DECLARED: column %s has categories [%s] but metadata declares [%s].",
          shQuote(name),
          paste(levels(series), collapse = ", "),
          paste(declared_levels, collapse = ", ")
        ))
      }
    } else {
      vals <- as.character(series[!is.na(series)])
      unknown <- setdiff(vals, declared_levels)
      if (length(unknown) > 0L) {
        bcsv_validation_error(sprintf(
          "LEVEL_NOT_DECLARED: column %s has values not in levels: [%s].",
          shQuote(name),
          paste(shQuote(sort(unique(unknown))), collapse = ", ")
        ))
      }
    }
  }
}

.check_primary_key_for_write <- function(data, metadata_obj) {
  pk <- metadata_obj$table_schema$primary_key
  if (is.null(pk)) return(invisible(NULL))
  cols <- as.character(unlist(pk))
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) {
    bcsv_validation_error(sprintf(
      "COLUMN_MISSING_IN_DATA: primary_key references columns not in data: [%s].",
      paste(shQuote(missing_cols), collapse = ", ")
    ))
  }
  key_df <- as.data.frame(data)[, cols, drop = FALSE]
  if (any(duplicated(key_df))) {
    n <- sum(duplicated(key_df) | duplicated(key_df, fromLast = TRUE))
    bcsv_validation_error(sprintf(
      "PRIMARY_KEY_VIOLATION: %d row(s) violate uniqueness of primary_key [%s].",
      n, paste(cols, collapse = ", ")
    ))
  }
}
