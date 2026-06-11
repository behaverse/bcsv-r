#' Read a CSV with paired bcsv metadata
#'
#' Reads a CSV file and applies data types defined in the paired bcsv metadata
#' JSON file. Locked signature mirrors the Python `bcsv.read_bcsv` (see the
#' [API spec §1](https://github.com/behaverse/bcsv-r/blob/main/docs/04-api-spec.md)).
#'
#' @param data_file Path to CSV file.
#' @param metadata_file Path to metadata JSON file. If `NULL` (default), uses
#'   `data_file` with `.csv` (case-insensitive) replaced by `.json`. `data_file`
#'   must end in `.csv` for the default to apply; otherwise pass an explicit
#'   `metadata_file`.
#' @param check_schema Validate metadata against `bcsv/schema.json` first.
#'   Failures raise `bcsv_schema_error`. Default `TRUE`.
#' @param check_constraints Evaluate data-level constraints (`minimum`/`maximum`,
#'   `levels`, `min_length`/`max_length`, `required`, type coercion). Severity
#'   is governed by `on_violation`. Default `TRUE`.
#' @param check_hash Compare SHA-256 of the CSV against `file_hash` in metadata.
#'   Mismatch raises `bcsv_validation_error`; absent hash emits a `HASH_ABSENT`
#'   warning. Default `TRUE`.
#' @param on_violation `"warn"` (default) keeps the row (with NA for coercion
#'   failures or level violations) and emits a warning. `"raise"` raises
#'   immediately. Does not affect schema or hash checks.
#' @param columns If a character vector, only the named columns are loaded.
#'   The returned tibble preserves the order given.
#' @param verbose Print per-column type-application progress.
#'
#' @return A [tibble][tibble::tibble] with bcsv-typed columns.
#' @export
read_bcsv <- function(data_file,
                     metadata_file = NULL,
                     check_schema = TRUE,
                     check_constraints = TRUE,
                     check_hash = TRUE,
                     on_violation = c("warn", "raise"),
                     columns = NULL,
                     verbose = FALSE) {
  on_violation <- match.arg(on_violation)
  if (!is.null(columns) && length(columns) == 0L) {
    stop("columns of length 0 is not valid; pass NULL to load all columns")
  }

  data_path <- as.character(data_file)
  meta_path <- if (!is.null(metadata_file)) {
    as.character(metadata_file)
  } else {
    .default_metadata_path(data_path)
  }

  metadata <- .load_metadata(meta_path)

  if (check_schema) .enforce_schema(metadata)

  if (check_hash) .enforce_hash(data_path, metadata)

  dialect <- .resolve_dialect(metadata)

  df <- .read_raw(data_path,
                  delimiter = dialect$delimiter,
                  encoding = dialect$encoding,
                  columns = columns)

  column_metas <- .columns_by_name(metadata)

  for (col_name in names(df)) {
    col_meta <- column_metas[[col_name]]
    if (is.null(col_meta)) next
    df[[col_name]] <- .apply_column(
      df[[col_name]], col_meta,
      on_violation = on_violation,
      check_constraints = check_constraints,
      verbose = verbose
    )
  }

  if (check_constraints) .check_primary_key(df, metadata, on_violation)

  if (!is.null(columns)) {
    df <- df[, columns, drop = FALSE]
  }

  tibble::as_tibble(df)
}


# --- helpers ---------------------------------------------------------------

.load_metadata <- function(meta_path) {
  if (!file.exists(meta_path)) {
    stop(sprintf("Metadata file not found: %s", meta_path), call. = FALSE)
  }
  tryCatch(
    jsonlite::fromJSON(meta_path, simplifyVector = FALSE),
    error = function(e) {
      bcsv_schema_error(sprintf(
        "Metadata file is not valid JSON: %s (%s)", meta_path, conditionMessage(e)
      ))
    }
  )
}

.enforce_schema <- function(metadata) {
  raw <- jsonlite::toJSON(metadata, auto_unbox = TRUE, null = "null")
  result <- jsonvalidate::json_validate(
    json = raw, schema = schema_path(),
    engine = "ajv", verbose = TRUE, greedy = TRUE
  )
  if (isTRUE(result)) return(invisible(NULL))
  err_df <- attr(result, "errors")
  if (is.null(err_df) || nrow(err_df) == 0L) {
    bcsv_schema_error("SCHEMA_VIOLATION: metadata fails JSON Schema validation")
  }
  pointer <- err_df$instancePath[1]
  if (is.na(pointer) || identical(pointer, "")) pointer <- "/"
  bcsv_schema_error(sprintf(
    "SCHEMA_VIOLATION at %s: %s", pointer, err_df$message[1]
  ))
}

.enforce_hash <- function(data_path, metadata) {
  declared <- metadata$file_hash
  if (is.null(declared) || !nzchar(as.character(declared))) {
    warning("HASH_ABSENT: metadata has no `file_hash`; integrity not verified.",
            call. = FALSE)
    return(invisible(NULL))
  }
  actual <- sha256_of_file(data_path)
  if (!identical(actual, as.character(declared))) {
    bcsv_validation_error(sprintf(
      "HASH_MISMATCH: expected %s, got %s. Data file may have been modified since metadata was created.",
      declared, actual
    ))
  }
}

.resolve_dialect <- function(metadata) {
  d <- .parse_dialect(metadata)
  if (length(d$unsupported) > 0L) {
    warning(sprintf(
      "DIALECT_UNSUPPORTED: metadata declares dialect keys not honored in v0: %s. Honored: %s.",
      paste(sort(d$unsupported), collapse = ", "),
      paste(sort(.SUPPORTED_DIALECT_KEYS), collapse = ", ")
    ), call. = FALSE)
  }
  list(delimiter = d$delimiter, encoding = d$encoding)
}

.read_raw <- function(data_path, delimiter, encoding, columns) {
  df <- readr::read_delim(
    data_path,
    delim = delimiter,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(0),
    locale = readr::locale(encoding = encoding),
    show_col_types = FALSE,
    progress = FALSE
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!is.null(columns)) {
    missing_cols <- setdiff(columns, names(df))
    if (length(missing_cols) > 0L) {
      stop(sprintf("columns not found in CSV: %s",
                   paste(shQuote(missing_cols), collapse = ", ")),
           call. = FALSE)
    }
    df <- df[, columns, drop = FALSE]
  }
  df
}

.columns_by_name <- function(metadata) {
  cols <- metadata$table_schema$columns %||% list()
  out <- list()
  for (c in cols) {
    if (!is.null(c$name)) out[[c$name]] <- c
  }
  out
}

.apply_column <- function(raw, col_meta, on_violation, check_constraints, verbose) {
  col_name <- col_meta$name
  datatype <- col_meta$datatype %||% "string"

  tokens <- na_tokens(col_meta)
  pre_na_mask <- raw %in% tokens & !is.na(raw)
  raw[pre_na_mask] <- NA_character_

  if (isTRUE(verbose)) {
    message(sprintf("  %s: applying datatype=%s", col_name, shQuote(datatype)))
  }

  coerced <- coerce_column(raw, datatype, col_meta, pre_na_mask)

  if (isTRUE(check_constraints)) {
    .check_constraints(
      col_name = col_name,
      coerced = coerced$value,
      failures = coerced$failures,
      col_meta = col_meta,
      datatype = datatype,
      on_violation = on_violation
    )
  }

  coerced$value
}

.check_constraints <- function(col_name, coerced, failures, col_meta, datatype, on_violation) {
  if (any(failures, na.rm = TRUE)) {
    n <- sum(failures, na.rm = TRUE)
    if (datatype %in% c("categorical", "ordered")) {
      .emit(sprintf(
        "LEVEL_NOT_DECLARED: %s: %d value(s) not in declared levels; set to NA.",
        col_name, n
      ), on_violation)
    } else {
      .emit(sprintf(
        "COERCION_FAILED: %s: %d value(s) could not be coerced to %s; set to NA.",
        col_name, n, datatype
      ), on_violation)
    }
  }

  if (isTRUE(col_meta$required) && any(is.na(coerced))) {
    n <- sum(is.na(coerced))
    .emit(sprintf(
      "REQUIRED_VIOLATION: %s: %d NA value(s) in a column marked `required: true`.",
      col_name, n
    ), on_violation)
  }

  if (datatype %in% c("integer", "number", "date", "datetime")) {
    non_na <- coerced[!is.na(coerced)]
    if (!is.null(col_meta$minimum) && length(non_na) > 0L) {
      n <- sum(non_na < col_meta$minimum, na.rm = TRUE)
      if (n > 0L) {
        .emit(sprintf("RANGE_VIOLATION: %s: %d value(s) below minimum %s.",
                      col_name, n, format(col_meta$minimum)), on_violation)
      }
    }
    if (!is.null(col_meta$maximum) && length(non_na) > 0L) {
      n <- sum(non_na > col_meta$maximum, na.rm = TRUE)
      if (n > 0L) {
        .emit(sprintf("RANGE_VIOLATION: %s: %d value(s) above maximum %s.",
                      col_name, n, format(col_meta$maximum)), on_violation)
      }
    }
  }

  if (datatype == "string") {
    lengths <- nchar(coerced[!is.na(coerced)])
    if (!is.null(col_meta$min_length) && length(lengths) > 0L) {
      n <- sum(lengths < col_meta$min_length)
      if (n > 0L) {
        .emit(sprintf("LENGTH_VIOLATION: %s: %d value(s) below min_length %d.",
                      col_name, n, col_meta$min_length), on_violation)
      }
    }
    if (!is.null(col_meta$max_length) && length(lengths) > 0L) {
      n <- sum(lengths > col_meta$max_length)
      if (n > 0L) {
        .emit(sprintf("LENGTH_VIOLATION: %s: %d value(s) above max_length %d.",
                      col_name, n, col_meta$max_length), on_violation)
      }
    }
  }
}

.check_primary_key <- function(df, metadata, on_violation) {
  pk <- metadata$table_schema$primary_key
  if (is.null(pk)) return(invisible(NULL))
  cols <- as.character(unlist(pk))
  if (!all(cols %in% names(df))) return(invisible(NULL))
  key_df <- df[, cols, drop = FALSE]
  dup <- duplicated(key_df) | duplicated(key_df, fromLast = TRUE)
  if (any(dup)) {
    .emit(sprintf(
      "PRIMARY_KEY_VIOLATION: %d row(s) violate uniqueness of primary key [%s].",
      sum(dup), paste(cols, collapse = ", ")
    ), on_violation)
  }
}

.emit <- function(message, on_violation) {
  if (identical(on_violation, "raise")) {
    bcsv_validation_error(message)
  }
  warning(message, call. = FALSE)
}

