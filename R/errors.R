#' bcsv condition hierarchy
#'
#' All bcsv-raised conditions inherit from `bcsv_error`. Catch the family with
#' a single `tryCatch(..., bcsv_error = function(e) ...)`; catch specific
#' subclasses with `bcsv_schema_error`, `bcsv_validation_error`, or
#' `bcsv_unknown_key_error`. Matches the Python `BcsvError` hierarchy (spec §9).
#'
#' @param message Human-readable error message.
#' @return Never returns; signals a condition.
#' @name bcsv-conditions
NULL


#' @rdname bcsv-conditions
#' @export
bcsv_error <- function(message) {
  stop(
    structure(
      class = c("bcsv_error", "error", "condition"),
      list(message = message, call = sys.call(-1))
    )
  )
}

#' @rdname bcsv-conditions
#' @export
bcsv_schema_error <- function(message) {
  stop(
    structure(
      class = c("bcsv_schema_error", "bcsv_error", "error", "condition"),
      list(message = message, call = sys.call(-1))
    )
  )
}

#' @rdname bcsv-conditions
#' @export
bcsv_validation_error <- function(message) {
  stop(
    structure(
      class = c("bcsv_validation_error", "bcsv_error", "error", "condition"),
      list(message = message, call = sys.call(-1))
    )
  )
}

#' @rdname bcsv-conditions
#' @export
bcsv_unknown_key_error <- function(message) {
  stop(
    structure(
      class = c("bcsv_unknown_key_error", "bcsv_error", "error", "condition"),
      list(message = message, call = sys.call(-1))
    )
  )
}
