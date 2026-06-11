# Conformance suite—drive bcsv-r's validate_bcsv against the cross-language
# fixtures shipped in `behaverse-schemas/bcsv/conformance/`.
#
# Mirrors bcsv-py's tests/test_conformance.py. The two implementations MUST
# emit identical `code` strings on these fixtures—that's the cross-language
# parity contract.

CONFORMANCE_DIR <- file.path(
  "..", "..", "..", "..", "behaverse-schemas", "bcsv", "conformance"
)
PATH_BEARING_CODES <- c("FILE_NOT_FOUND", "METADATA_INVALID_JSON", "HASH_MISMATCH")


.normalise_issue <- function(issue) {
  code <- issue$code
  loc <- issue$location
  if (is.null(loc) || !(code %in% PATH_BEARING_CODES)) {
    list(code = code, location = loc)
  } else {
    list(code = code, location = basename(loc))
  }
}

.issue_set <- function(issues) {
  if (length(issues) == 0L) return(list())
  pairs <- lapply(issues, .normalise_issue)
  # Sort by (code, location-as-string) so set comparison is deterministic.
  keys <- vapply(pairs, function(p) paste0(p$code, "",
                                           if (is.null(p$location)) "" else p$location),
                 character(1))
  pairs[order(keys)]
}

.pairs_equal <- function(a, b) {
  if (length(a) != length(b)) return(FALSE)
  for (i in seq_along(a)) {
    if (!identical(a[[i]]$code, b[[i]]$code)) return(FALSE)
    if (!identical(a[[i]]$location, b[[i]]$location)) return(FALSE)
  }
  TRUE
}


.discover_fixtures <- function() {
  if (!dir.exists(CONFORMANCE_DIR)) return(list())
  out <- list()
  for (kind in c("positive", "negative")) {
    root <- file.path(CONFORMANCE_DIR, kind)
    if (!dir.exists(root)) next
    for (d in sort(list.dirs(root, recursive = FALSE))) {
      if (file.exists(file.path(d, "expected.json"))) {
        out[[length(out) + 1L]] <- list(
          name = paste0(kind, "/", basename(d)),
          path = d
        )
      }
    }
  }
  out
}


fixtures <- .discover_fixtures()

if (length(fixtures) == 0L) {
  test_that("conformance fixtures present", {
    skip("bcsv conformance fixtures not present in this checkout")
  })
} else {
  for (fx in fixtures) {
    local({
      name <- fx$name
      path <- fx$path
      test_that(sprintf("conformance: %s", name), {
        expected <- jsonlite::fromJSON(
          file.path(path, "expected.json"),
          simplifyVector = FALSE
        )
        opts <- expected$validate_with %||% list()

        data_path <- file.path(path, "data.csv")  # may not exist (FILE_NOT_FOUND)
        meta_path <- file.path(path, "metadata.json")

        args <- c(list(data_file = data_path, metadata_file = meta_path), opts)
        result <- do.call(validate_bcsv, args)

        expect_equal(
          result$valid, expected$valid,
          info = sprintf(
            "%s: valid mismatch—got %s, expected %s\n  errors: %s\n  warnings: %s",
            name, result$valid, expected$valid,
            jsonlite::toJSON(result$errors, auto_unbox = TRUE),
            jsonlite::toJSON(result$warnings, auto_unbox = TRUE)
          )
        )

        expect_true(
          .pairs_equal(.issue_set(result$errors), .issue_set(expected$errors)),
          info = sprintf(
            "%s: errors mismatch\n  got: %s\n  expected: %s",
            name,
            jsonlite::toJSON(.issue_set(result$errors), auto_unbox = TRUE),
            jsonlite::toJSON(.issue_set(expected$errors), auto_unbox = TRUE)
          )
        )

        expect_true(
          .pairs_equal(.issue_set(result$warnings), .issue_set(expected$warnings)),
          info = sprintf(
            "%s: warnings mismatch\n  got: %s\n  expected: %s",
            name,
            jsonlite::toJSON(.issue_set(result$warnings), auto_unbox = TRUE),
            jsonlite::toJSON(.issue_set(expected$warnings), auto_unbox = TRUE)
          )
        )
      })
    })
  }
}
