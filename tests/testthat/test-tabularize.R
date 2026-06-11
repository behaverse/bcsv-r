test_that("tabularise is an alias of tabularize", {
  expect_identical(tabularise, tabularize)
})

test_that("clean result -> 0-row frame with the four columns", {
  res <- list(valid = TRUE, errors = list(), warnings = list(),
              data_columns = character(), meta_columns = character())
  out <- tabularize(res)
  expect_equal(names(out), c("severity", "code", "location", "message"))
  expect_equal(nrow(out), 0L)
})

test_that("result lists errors then warnings", {
  res <- list(
    valid = FALSE,
    errors = list(list(code = "HASH_MISMATCH", location = "d.csv", message = "m1")),
    warnings = list(list(code = "RANGE_VIOLATION", location = "score", message = "m2")),
    data_columns = character(), meta_columns = character()
  )
  out <- tabularize(res)
  expect_equal(out$severity, c("error", "warning"))
  expect_equal(out$code, c("HASH_MISMATCH", "RANGE_VIOLATION"))
})

test_that("metadata -> one row per column with quoted levels", {
  meta <- list(`@context` = "x", url = "u.csv", table_schema = list(columns = list(
    list(name = "id", datatype = "integer"),
    list(name = "rating", datatype = "ordered", levels = list("very bad", "bad", "good"))
  )))
  out <- tabularize(meta)
  expect_equal(out$name, c("id", "rating"))
  expect_equal(out$levels[out$name == "rating"], '"very bad", "bad", "good"')
})

test_that("unrecognized object errors", {
  expect_error(tabularize(list(foo = 1)), "unrecognized")
  expect_error(tabularize(42), "expects a bcsv")
})

test_that("validate_bcsv result is classed and prints", {
  tmp <- tempfile(fileext = ".csv")
  json <- sub("\\.csv$", ".json", tmp)
  readr::write_csv(data.frame(x = 1L), tmp)
  jsonlite::write_json(
    list(`@context` = CONTEXT_URL, url = basename(tmp),
         description = "Test data.",
         table_schema = list(columns = list(list(name = "x", datatype = "integer")))),
    json, auto_unbox = TRUE, null = "null")
  res <- validate_bcsv(tmp, json, check_hash = FALSE)
  expect_s3_class(res, "bcsv_validation_result")
  expect_true(res$valid)
  expect_output(print(res), "valid: TRUE")
})

test_that("document_bcsv metadata is classed and prints", {
  meta <- document_bcsv(data.frame(id = 1L, score = 0.5), "Test dataset.")
  expect_s3_class(meta, "bcsv_metadata")
  expect_equal(meta$table_schema$columns[[1]]$name, "id")
  expect_output(print(meta), "bcsv metadata")
})

test_that("metadata with no columns -> 0-row frame with name + datatype", {
  meta <- list(`@context` = "x", url = "u.csv", table_schema = list(columns = list()))
  out <- tabularize(meta)
  expect_equal(names(out), c("name", "datatype"))
  expect_equal(nrow(out), 0L)
})
