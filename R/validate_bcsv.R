#' Validate a CSV + bcsv-metadata pair
#'
#' Verifies that a CSV file and its paired bcsv metadata are well-formed and
#' consistent. Collects issues into a `ValidationResult`; the function itself
#' does **not** raise (callers inspect `result$valid` and `result$errors`).
#' Locked signature mirrors the Python `bcsv.validate_bcsv` (see the
#' [API spec §4](https://github.com/behaverse/bcsv-r/blob/main/docs/04-api-spec.md)).
#'
#' Pipeline (§4.2):
#'
#'   1. Files exist (always).
#'   2. `check_schema`—metadata conforms to `bcsv/schema.json`.
#'   3. Column names and order match between CSV header and metadata.
#'   4. `check_hash`—SHA-256 of CSV matches `file_hash`.
#'   5. `check_constraints`—coerce columns, evaluate ranges / lengths / levels
#'      / required; check `primary_key` uniqueness.
#'
#' When `on_violation = "raise"`, all collected warnings are promoted to errors
#' before returning (so `valid` flips appropriately).
#'
#' @inheritParams read_bcsv
#'
#' @return A list with elements `valid`, `errors`, `warnings`,
#'   `data_columns`, `meta_columns`. Each `errors`/`warnings` entry is a list
#'   with `code`, `message`, and `location`.
#' @export
validate_bcsv <- function(data_file,
                         metadata_file = NULL,
                         check_hash = TRUE,
                         check_schema = TRUE,
                         check_constraints = TRUE,
                         on_violation = c("warn", "raise")) {
  on_violation <- match.arg(on_violation)

  errors <- list()
  warnings_list <- list()
  data_columns <- character()
  meta_columns <- character()

  data_path <- as.character(data_file)
  meta_path <- tryCatch(
    if (!is.null(metadata_file)) as.character(metadata_file) else .default_metadata_path(data_path),
    bcsv_schema_error = function(e) {
      errors[[length(errors) + 1L]] <<- .issue("FILE_NOT_FOUND", conditionMessage(e), NULL)
      NULL
    }
  )
  if (is.null(meta_path)) {
    return(.build_result(errors, warnings_list, data_columns, meta_columns, on_violation))
  }

  # Stage 1: file existence (always).
  for (p in c(data_path, meta_path)) {
    if (!file.exists(p)) {
      errors[[length(errors) + 1L]] <- .issue(
        "FILE_NOT_FOUND", sprintf("File not found: %s", p), p
      )
    }
  }
  if (length(errors) > 0L) {
    return(.build_result(errors, warnings_list, data_columns, meta_columns, on_violation))
  }

  # Parse metadata JSON.
  metadata <- tryCatch(
    jsonlite::fromJSON(meta_path, simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(metadata, "error")) {
    errors[[length(errors) + 1L]] <- .issue(
      "METADATA_INVALID_JSON",
      sprintf("%s: %s", meta_path, conditionMessage(metadata)),
      meta_path
    )
    return(.build_result(errors, warnings_list, data_columns, meta_columns, on_violation))
  }

  meta_columns <- vapply(
    metadata$table_schema$columns %||% list(),
    function(c) c$name %||% "",
    character(1)
  )

  # Stage 2: JSON-Schema validation.
  if (isTRUE(check_schema)) {
    errors <- c(errors, .run_schema_validation(metadata))
  }

  # Resolve dialect for stages 3 + 5.
  dialect <- .parse_dialect(metadata)
  if (length(dialect$unsupported) > 0L) {
    warnings_list[[length(warnings_list) + 1L]] <- .issue(
      "DIALECT_UNSUPPORTED",
      sprintf("Dialect keys not honored in v0: %s. Honored: %s.",
              paste(sort(dialect$unsupported), collapse = ", "),
              paste(sort(.SUPPORTED_DIALECT_KEYS), collapse = ", ")),
      "/dialect"
    )
  }

  # Stage 3: read CSV header + column matching.
  header <- tryCatch(
    readr::read_delim(
      data_path,
      delim = dialect$delimiter,
      n_max = 0L,
      col_types = readr::cols(.default = readr::col_character()),
      na = character(0),
      locale = readr::locale(encoding = dialect$encoding),
      show_col_types = FALSE,
      progress = FALSE
    ),
    error = function(e) e
  )
  if (inherits(header, "error")) {
    errors[[length(errors) + 1L]] <- .issue(
      "METADATA_INVALID_JSON",
      sprintf("Could not read CSV header at %s: %s", data_path, conditionMessage(header)),
      data_path
    )
    return(.build_result(errors, warnings_list, data_columns, meta_columns, on_violation))
  }
  data_columns <- colnames(header)

  col_check <- .check_columns_collect(data_columns, meta_columns)
  errors <- c(errors, col_check$errors)
  warnings_list <- c(warnings_list, col_check$warnings)

  # Stage 4: hash check.
  if (isTRUE(check_hash)) {
    hash_check <- .run_hash_check_collect(data_path, metadata)
    errors <- c(errors, hash_check$errors)
    warnings_list <- c(warnings_list, hash_check$warnings)
  }

  # Stage 5: data-level constraint check.
  # Skip when column membership is broken—diagnostics would be misleading.
  if (isTRUE(check_constraints)) {
    has_col_problem <- any(vapply(
      errors,
      function(e) e$code %in% c("COLUMN_MISSING_IN_DATA", "COLUMN_MISSING_IN_METADATA"),
      logical(1)
    ))
    if (!has_col_problem) {
      constr <- .run_constraint_check(
        data_path, metadata, dialect$delimiter, dialect$encoding
      )
      errors <- c(errors, constr$errors)
      warnings_list <- c(warnings_list, constr$warnings)
    }
  }

  .build_result(errors, warnings_list, data_columns, meta_columns, on_violation)
}


# --- stage helpers --------------------------------------------------------

.issue <- function(code, message, location = NULL) {
  list(code = code, message = message, location = location)
}

.run_schema_validation <- function(metadata) {
  raw <- jsonlite::toJSON(metadata, auto_unbox = TRUE, null = "null")
  result <- jsonvalidate::json_validate(
    json = raw, schema = schema_path(),
    engine = "ajv", verbose = TRUE, greedy = TRUE
  )
  if (isTRUE(result)) return(list())
  err_df <- attr(result, "errors")
  if (is.null(err_df) || nrow(err_df) == 0L) {
    return(list(.issue("SCHEMA_VIOLATION", "Metadata fails JSON Schema validation", NULL)))
  }
  issues <- lapply(seq_len(nrow(err_df)), function(i) {
    pointer <- err_df$instancePath[i]
    if (is.na(pointer) || identical(pointer, "")) pointer <- "/"
    .issue("SCHEMA_VIOLATION", as.character(err_df$message[i]), pointer)
  })
  # Dedupe by (code, location). ajv with `greedy=TRUE` can report a single
  # underlying violation multiple times when several allOf branches fail; the
  # parity contract is about distinct (code, location) pairs, not raw counts.
  seen <- character(0)
  out <- list()
  for (iss in issues) {
    key <- paste0(iss$code, "::", if (is.null(iss$location)) "" else iss$location)
    if (!(key %in% seen)) {
      seen <- c(seen, key)
      out[[length(out) + 1L]] <- iss
    }
  }
  out
}

.check_columns_collect <- function(data_columns, meta_columns) {
  errors <- list()
  warnings_list <- list()

  for (col in sort(setdiff(meta_columns, data_columns))) {
    errors[[length(errors) + 1L]] <- .issue(
      "COLUMN_MISSING_IN_DATA",
      sprintf("Column declared in metadata not found in CSV: %s", shQuote(col)),
      col
    )
  }
  for (col in sort(setdiff(data_columns, meta_columns))) {
    errors[[length(errors) + 1L]] <- .issue(
      "COLUMN_MISSING_IN_METADATA",
      sprintf("Column found in CSV but not declared in metadata: %s", shQuote(col)),
      col
    )
  }

  if (setequal(data_columns, meta_columns) && !identical(data_columns, meta_columns)) {
    warnings_list[[length(warnings_list) + 1L]] <- .issue(
      "COLUMN_ORDER_DIFFERS",
      sprintf("Data columns are in different order than metadata declares: data=[%s], metadata=[%s]",
              paste(data_columns, collapse = ", "),
              paste(meta_columns, collapse = ", ")),
      NULL
    )
  }

  list(errors = errors, warnings = warnings_list)
}

.run_hash_check_collect <- function(data_path, metadata) {
  errors <- list()
  warnings_list <- list()
  declared <- metadata$file_hash
  if (is.null(declared) || !nzchar(as.character(declared))) {
    warnings_list[[length(warnings_list) + 1L]] <- .issue(
      "HASH_ABSENT",
      "Metadata has no `file_hash`; integrity not verified.",
      "/file_hash"
    )
    return(list(errors = errors, warnings = warnings_list))
  }
  actual <- sha256_of_file(data_path)
  if (!identical(actual, as.character(declared))) {
    errors[[length(errors) + 1L]] <- .issue(
      "HASH_MISMATCH",
      sprintf("Expected %s, got %s. Data file may have been modified since metadata was created.",
              declared, actual),
      data_path
    )
  }
  list(errors = errors, warnings = warnings_list)
}

.run_constraint_check <- function(data_path, metadata, delimiter, encoding) {
  errors <- list()
  warnings_list <- list()

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

  column_metas <- metadata$table_schema$columns %||% list()
  coerced_cols <- list()

  for (col_meta in column_metas) {
    col_name <- col_meta$name
    if (is.null(col_name) || !(col_name %in% names(df))) next

    datatype <- col_meta$datatype %||% "string"
    has_levels <- !is.null(col_meta$levels) && length(col_meta$levels) > 0L

    # LEVELS_REQUIRED / LEVELS_FORBIDDEN: independent of schema check, so they
    # surface even when caller passed check_schema = FALSE.
    if (datatype %in% c("categorical", "ordered") && !has_levels) {
      errors[[length(errors) + 1L]] <- .issue(
        "LEVELS_REQUIRED",
        sprintf("Column %s datatype is %s but `levels` is missing.",
                shQuote(col_name), shQuote(datatype)),
        col_name
      )
      next
    }
    if (!(datatype %in% c("categorical", "ordered")) && has_levels) {
      errors[[length(errors) + 1L]] <- .issue(
        "LEVELS_FORBIDDEN",
        sprintf("Column %s has `levels` but datatype is %s.",
                shQuote(col_name), shQuote(datatype)),
        col_name
      )
      next
    }

    raw <- df[[col_name]]
    tokens <- na_tokens(col_meta)
    pre_na_mask <- raw %in% tokens & !is.na(raw)
    raw[pre_na_mask] <- NA_character_

    coerce_result <- tryCatch(
      coerce_column(raw, datatype, col_meta, pre_na_mask),
      error = function(e) e
    )
    if (inherits(coerce_result, "error")) {
      errors[[length(errors) + 1L]] <- .issue(
        "COERCION_FAILED",
        sprintf("Column %s: %s", shQuote(col_name), conditionMessage(coerce_result)),
        col_name
      )
      next
    }

    coerced <- coerce_result$value
    failures <- coerce_result$failures
    coerced_cols[[col_name]] <- coerced

    if (any(failures, na.rm = TRUE)) {
      n <- sum(failures, na.rm = TRUE)
      if (datatype %in% c("categorical", "ordered")) {
        warnings_list[[length(warnings_list) + 1L]] <- .issue(
          "LEVEL_NOT_DECLARED",
          sprintf("Column %s: %d value(s) not in declared levels.", shQuote(col_name), n),
          col_name
        )
      } else {
        warnings_list[[length(warnings_list) + 1L]] <- .issue(
          "COERCION_FAILED",
          sprintf("Column %s: %d value(s) could not be coerced to %s.",
                  shQuote(col_name), n, datatype),
          col_name
        )
      }
    }

    if (isTRUE(col_meta$required) && any(is.na(coerced))) {
      n <- sum(is.na(coerced))
      warnings_list[[length(warnings_list) + 1L]] <- .issue(
        "REQUIRED_VIOLATION",
        sprintf("Column %s: %d NA value(s) in a required column.", shQuote(col_name), n),
        col_name
      )
    }

    if (datatype %in% c("integer", "number", "date", "datetime")) {
      non_na <- coerced[!is.na(coerced)]
      if (!is.null(col_meta$minimum) && length(non_na) > 0L) {
        below <- sum(non_na < col_meta$minimum, na.rm = TRUE)
        if (below > 0L) {
          warnings_list[[length(warnings_list) + 1L]] <- .issue(
            "RANGE_VIOLATION",
            sprintf("Column %s: %d value(s) below minimum %s.",
                    shQuote(col_name), below, format(col_meta$minimum)),
            col_name
          )
        }
      }
      if (!is.null(col_meta$maximum) && length(non_na) > 0L) {
        above <- sum(non_na > col_meta$maximum, na.rm = TRUE)
        if (above > 0L) {
          warnings_list[[length(warnings_list) + 1L]] <- .issue(
            "RANGE_VIOLATION",
            sprintf("Column %s: %d value(s) above maximum %s.",
                    shQuote(col_name), above, format(col_meta$maximum)),
            col_name
          )
        }
      }
    }

    if (datatype == "string") {
      lengths <- nchar(coerced[!is.na(coerced)])
      if (!is.null(col_meta$min_length) && length(lengths) > 0L) {
        short <- sum(lengths < col_meta$min_length)
        if (short > 0L) {
          warnings_list[[length(warnings_list) + 1L]] <- .issue(
            "LENGTH_VIOLATION",
            sprintf("Column %s: %d value(s) shorter than min_length %d.",
                    shQuote(col_name), short, col_meta$min_length),
            col_name
          )
        }
      }
      if (!is.null(col_meta$max_length) && length(lengths) > 0L) {
        long_ <- sum(lengths > col_meta$max_length)
        if (long_ > 0L) {
          warnings_list[[length(warnings_list) + 1L]] <- .issue(
            "LENGTH_VIOLATION",
            sprintf("Column %s: %d value(s) longer than max_length %d.",
                    shQuote(col_name), long_, col_meta$max_length),
            col_name
          )
        }
      }
    }
  }

  # Primary-key uniqueness.
  pk <- metadata$table_schema$primary_key
  if (!is.null(pk)) {
    cols <- as.character(unlist(pk))
    if (all(cols %in% names(coerced_cols))) {
      pk_df <- as.data.frame(coerced_cols[cols], stringsAsFactors = FALSE)
      dup <- duplicated(pk_df) | duplicated(pk_df, fromLast = TRUE)
      if (any(dup)) {
        warnings_list[[length(warnings_list) + 1L]] <- .issue(
          "PRIMARY_KEY_VIOLATION",
          sprintf("%d row(s) violate uniqueness of primary_key [%s].",
                  sum(dup), paste(cols, collapse = ", ")),
          NULL
        )
      }
    }
  }

  list(errors = errors, warnings = warnings_list)
}

.build_result <- function(errors, warnings_list, data_columns, meta_columns, on_violation) {
  if (identical(on_violation, "raise")) {
    errors <- c(errors, warnings_list)
    warnings_list <- list()
  }
  out <- list(
    valid = length(errors) == 0L,
    errors = errors,
    warnings = warnings_list,
    data_columns = data_columns,
    meta_columns = meta_columns
  )
  class(out) <- c("bcsv_validation_result", "list")
  out
}

#' @export
print.bcsv_validation_result <- function(x, ...) {
  cat(sprintf("<bcsv validation result>  valid: %s\n", isTRUE(x$valid)))
  issues <- tabularize(x)
  if (nrow(issues) == 0L) cat("no issues\n") else print(issues)
  invisible(x)
}
