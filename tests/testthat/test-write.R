# Tests for write_bcsv—atomic write, overwrite policy, url auto-populate,
# file hash computation, validate=TRUE pre-write checks, and round-trip via
# read_bcsv.

# --- helpers ---------------------------------------------------------------

basic_metadata <- function(columns) {
  list(
    `@context` = CONTEXT_URL,
    description = "Test data.",
    table_schema = list(columns = columns)
  )
}


# --- side-effect order + return shape -------------------------------------

test_that("write produces both CSV and JSON files", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(id = 1:3, name = c("a", "b", "c"), stringsAsFactors = FALSE)
  meta <- basic_metadata(list(
    list(name = "id", datatype = "integer"),
    list(name = "name", datatype = "string")
  ))
  result <- write_bcsv(df, file.path(tmp, "out.csv"), meta)
  expect_true(file.exists(result$data))
  expect_true(file.exists(result$metadata))
  expect_match(result$data, "out\\.csv$")
  expect_match(result$metadata, "out\\.json$")
})

test_that("WriteResult file_hash matches sha256 of CSV", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1:2)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  result <- write_bcsv(df, file.path(tmp, "out.csv"), meta)
  expect_equal(result$file_hash, bcsv:::sha256_of_file(result$data))
})

test_that("metadata file on disk contains the computed hash", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1:2)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  result <- write_bcsv(df, file.path(tmp, "out.csv"), meta)
  on_disk <- jsonlite::fromJSON(result$metadata, simplifyVector = FALSE)
  expect_equal(on_disk$file_hash, result$file_hash)
})


# --- url auto-populate ----------------------------------------------------

test_that("url auto-populated from file basename when absent", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1L)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  expect_null(meta$url)
  write_bcsv(df, file.path(tmp, "auto.csv"), meta)
  on_disk <- jsonlite::fromJSON(file.path(tmp, "auto.json"), simplifyVector = FALSE)
  expect_equal(on_disk$url, "auto.csv")
})

test_that("explicit url is preserved", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1L)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  meta$url <- "https://example.org/dataset/x.csv"
  write_bcsv(df, file.path(tmp, "auto.csv"), meta)
  on_disk <- jsonlite::fromJSON(file.path(tmp, "auto.json"), simplifyVector = FALSE)
  expect_equal(on_disk$url, "https://example.org/dataset/x.csv")
})


# --- overwrite policy -----------------------------------------------------

test_that("refuses to clobber existing CSV", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  writeLines("preexisting", file.path(tmp, "out.csv"))
  df <- data.frame(x = 1L)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  expect_error(write_bcsv(df, file.path(tmp, "out.csv"), meta),
               "Refusing to overwrite")
  expect_equal(readLines(file.path(tmp, "out.csv")), "preexisting")
})

test_that("refuses to clobber existing JSON", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  writeLines("{}", file.path(tmp, "out.json"))
  df <- data.frame(x = 1L)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  expect_error(write_bcsv(df, file.path(tmp, "out.csv"), meta),
               "Refusing to overwrite")
})

test_that("overwrite = TRUE succeeds", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  writeLines("preexisting", file.path(tmp, "out.csv"))
  writeLines("{}", file.path(tmp, "out.json"))
  df <- data.frame(x = 1:2)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  write_bcsv(df, file.path(tmp, "out.csv"), meta, overwrite = TRUE)
  expect_false(any(grepl("preexisting", readLines(file.path(tmp, "out.csv")))))
})


# --- atomicity ------------------------------------------------------------

test_that("no temp files left after successful write", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1L)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  write_bcsv(df, file.path(tmp, "out.csv"), meta)
  leftovers <- list.files(tmp, pattern = "\\.tmp\\.")
  expect_length(leftovers, 0)
})

test_that("invalid metadata leaves disk untouched (zero side effects)", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1L)
  # Missing levels for categorical → schema violation.
  bad <- basic_metadata(list(list(name = "x", datatype = "categorical")))
  expect_error(write_bcsv(df, file.path(tmp, "out.csv"), bad),
               class = "bcsv_schema_error")
  expect_false(file.exists(file.path(tmp, "out.csv")))
  expect_false(file.exists(file.path(tmp, "out.json")))
  expect_length(list.files(tmp), 0)
})


# --- validate = TRUE pre-write checks -------------------------------------

test_that("validate=TRUE catches schema violation", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1L)
  bad <- basic_metadata(list(list(name = "x", datatype = "categorical")))
  expect_error(write_bcsv(df, file.path(tmp, "out.csv"), bad),
               class = "bcsv_schema_error")
})

test_that("validate=TRUE catches range violation", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(age = c(25L, 200L))
  meta <- basic_metadata(list(
    list(name = "age", datatype = "integer", minimum = 0, maximum = 120)
  ))
  expect_error(write_bcsv(df, file.path(tmp, "out.csv"), meta),
               class = "bcsv_validation_error")
})

test_that("validate=TRUE catches required violation", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(id = c(1L, NA_integer_, 3L))
  meta <- basic_metadata(list(
    list(name = "id", datatype = "integer", required = TRUE)
  ))
  expect_error(write_bcsv(df, file.path(tmp, "out.csv"), meta),
               class = "bcsv_validation_error")
})

test_that("validate=TRUE catches primary_key violation", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(id = c(1L, 1L, 2L))
  meta <- basic_metadata(list(list(name = "id", datatype = "integer")))
  meta$table_schema$primary_key <- "id"
  expect_error(write_bcsv(df, file.path(tmp, "out.csv"), meta),
               class = "bcsv_validation_error")
})

test_that("validate=TRUE catches missing column", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(a = 1L)
  meta <- basic_metadata(list(
    list(name = "a", datatype = "integer"),
    list(name = "missing", datatype = "string")
  ))
  expect_error(write_bcsv(df, file.path(tmp, "out.csv"), meta),
               class = "bcsv_validation_error")
})

test_that("validate=FALSE skips constraint check", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(age = c(25L, 200L))
  meta <- basic_metadata(list(
    list(name = "age", datatype = "integer", minimum = 0, maximum = 120)
  ))
  # Should succeed despite the range violation.
  write_bcsv(df, file.path(tmp, "out.csv"), meta, validate = FALSE)
  expect_true(file.exists(file.path(tmp, "out.csv")))
})


# --- metadata_file path resolution ---------------------------------------

test_that("explicit metadata_file path is honored", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1L)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  custom_meta <- file.path(tmp, "custom.json")
  result <- write_bcsv(df, file.path(tmp, "data.csv"), meta,
                       metadata_file = custom_meta)
  expect_equal(result$metadata, custom_meta)
  expect_true(file.exists(custom_meta))
})

test_that("non-.csv extension requires explicit metadata_file", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1L)
  meta <- basic_metadata(list(list(name = "x", datatype = "integer")))
  expect_error(write_bcsv(df, file.path(tmp, "data.tsv"), meta),
               class = "bcsv_schema_error")
})


# --- round-trip: write then read -----------------------------------------

test_that("round-trip preserves types via read_bcsv", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(
    id    = 1:3,
    score = c(85.5, 92.0, 78.5),
    grade = ordered(c("B", "A", "C"), levels = c("A", "B", "C", "D"))
  )
  meta <- document_bcsv(df, "Test dataset.", data_url = "rt.csv",
                        pretty_name = "Test", primary_key = "id")
  result <- write_bcsv(df, file.path(tmp, "rt.csv"), meta)
  df2 <- read_bcsv(result$data)  # default check_hash = TRUE
  expect_true(is.integer(df2$id))
  expect_type(df2$score, "double")
  expect_true(is.ordered(df2$grade))
  expect_equal(levels(df2$grade), c("A", "B", "C", "D"))
  expect_equal(df2$id, 1:3)
})

test_that("round-trip hash verifies on read", {
  tmp <- tempfile("bcsv-w-"); dir.create(tmp)
  df <- data.frame(x = 1:2)
  meta <- document_bcsv(df, "Test dataset.", data_url = "rt.csv")
  write_bcsv(df, file.path(tmp, "rt.csv"), meta)
  # If the hash had been wrong, this read would raise HASH_MISMATCH.
  df2 <- read_bcsv(file.path(tmp, "rt.csv"))
  expect_equal(df2$x, 1:2)
})
