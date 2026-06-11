#' Shared NA-handling and type-coercion primitives
#'
#' Both `read_bcsv()` (during read) and `validate_bcsv()` (during the constraint
#' stage) use these helpers to perform the *exact same* coercion logic. Keeping
#' them in one place is the load-bearing way to guarantee the two functions
#' agree on what "coerces successfully" means.
#'
#' @name bcsv-coercion
#' @keywords internal
NULL

.TRUE_TOKENS <- c("true", "t", "yes", "y", "1")
.FALSE_TOKENS <- c("false", "f", "no", "n", "0")


#' Union of CSVW `null` (default `""`) and bcsv `na_strings` for a column.
#'
#' @param col_meta One column entry from `table_schema$columns`.
#' @return Character vector of NA tokens.
#' @keywords internal
na_tokens <- function(col_meta) {
  null_val <- col_meta$null %||% ""
  na_strings <- col_meta$na_strings %||% character(0)
  unique(c(as.character(unlist(null_val)), as.character(unlist(na_strings))))
}


#' Coerce a raw character vector to its declared bcsv datatype.
#'
#' @param raw Character vector (the raw column values, with NA tokens already
#'   replaced by `NA_character_`).
#' @param datatype One of `string`/`integer`/`number`/`boolean`/`date`/
#'   `datetime`/`time`/`categorical`/`ordered`.
#' @param col_meta The full column metadata list (used for `levels`).
#' @param pre_na_mask Logical vector—`TRUE` where the raw value was already
#'   NA before coercion (an NA token, not a coercion failure).
#' @return A list with `value` (coerced vector) and `failures` (logical vector
#'   marking positions where the raw value was non-NA but coercion yielded
#'   NA—i.e. COERCION_FAILED / LEVEL_NOT_DECLARED candidates).
#' @keywords internal
coerce_column <- function(raw, datatype, col_meta, pre_na_mask) {
  switch(
    datatype,
    string  = list(value = raw, failures = rep(FALSE, length(raw))),
    integer = .coerce_simple(raw, pre_na_mask, function(x) suppressWarnings(as.integer(x))),
    number  = .coerce_simple(raw, pre_na_mask, function(x) suppressWarnings(as.numeric(x))),
    boolean = .coerce_simple(raw, pre_na_mask, .parse_boolean),
    date    = .coerce_simple(raw, pre_na_mask, .parse_date),
    datetime = .coerce_simple(raw, pre_na_mask, .parse_datetime),
    time    = .coerce_simple(raw, pre_na_mask, .parse_time),
    categorical = .coerce_factor(raw, col_meta, ordered = FALSE, pre_na_mask),
    ordered     = .coerce_factor(raw, col_meta, ordered = TRUE, pre_na_mask),
    bcsv_schema_error(sprintf(
      "Unsupported datatype %s for column %s",
      shQuote(datatype), shQuote(col_meta$name %||% "<unknown>")
    ))
  )
}


# --- per-type coercers ----------------------------------------------------

.coerce_simple <- function(raw, pre_na_mask, parser) {
  val <- parser(raw)
  list(value = val, failures = is.na(val) & !pre_na_mask)
}

.parse_boolean <- function(x) {
  lower <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(x))
  out[lower %in% .TRUE_TOKENS] <- TRUE
  out[lower %in% .FALSE_TOKENS] <- FALSE
  as.logical(out)
}

.parse_date <- function(x) {
  # CSVW `format` is Java-style (yyyy-MM-dd); Python wants strftime (%Y-%m-%d).
  # v0 lets readr infer ISO 8601 from the value; CSVW→strftime translation
  # is deferred.
  suppressWarnings(readr::parse_date(x))
}

.parse_datetime <- function(x) {
  # §1.4.1: R has no truly-naive datetime; default to UTC. readr::parse_datetime
  # defaults to UTC for inputs without timezone information—exactly what we want.
  suppressWarnings(readr::parse_datetime(x))
}

.parse_time <- function(x) {
  out <- suppressWarnings(readr::parse_time(x, format = "%H:%M:%S"))
  # Retry HH:MM (without seconds) for values that didn't parse.
  retry <- is.na(out) & !is.na(x)
  if (any(retry)) {
    out2 <- suppressWarnings(readr::parse_time(x[retry], format = "%H:%M"))
    out[retry] <- out2
  }
  out
}

.coerce_factor <- function(raw, col_meta, ordered, pre_na_mask) {
  levels <- col_meta$levels
  if (is.null(levels) || length(levels) == 0L) {
    bcsv_validation_error(sprintf(
      "LEVELS_REQUIRED: column %s has datatype %s but no `levels`.",
      shQuote(col_meta$name %||% "<unknown>"),
      if (ordered) "ordered" else "categorical"
    ))
  }
  # Compare via stringified levels—handles string + numeric (Likert) levels
  # uniformly via the raw CSV string form.
  levels_chr <- as.character(unlist(levels))
  raw_chr <- as.character(raw)

  in_levels_or_na <- is.na(raw_chr) | raw_chr %in% levels_chr
  failures <- !in_levels_or_na & !pre_na_mask
  safe_values <- raw_chr
  safe_values[!in_levels_or_na] <- NA_character_

  val <- factor(safe_values, levels = levels_chr, ordered = ordered)
  list(value = val, failures = failures)
}


