# Smoke tests—verify the package loads, exposes the four functions, and that
# the vendored schema is reachable.

test_that("public API is exported", {
  for (fn in c("read_bcsv", "write_bcsv", "document_bcsv", "validate_bcsv")) {
    expect_true(exists(fn, mode = "function", inherits = TRUE), info = fn)
  }
})

test_that("condition hierarchy is exposed and well-formed", {
  expect_error(bcsv_validation_error("x"), class = "bcsv_validation_error")
  expect_error(bcsv_validation_error("x"), class = "bcsv_error")
  expect_error(bcsv_schema_error("x"), class = "bcsv_schema_error")
  expect_error(bcsv_schema_error("x"), class = "bcsv_error")
  expect_error(bcsv_unknown_key_error("x"), class = "bcsv_unknown_key_error")
  expect_error(bcsv_unknown_key_error("x"), class = "bcsv_error")
})

test_that("vendored schema is loadable and version-matches", {
  s <- bcsv:::load_schema()
  expect_equal(s$version, SCHEMA_VERSION)
})

test_that("schema-only validate_bcsv accepts canonical student_data example", {
  examples <- file.path(
    "..", "..", "..", "..", "behaverse-schemas", "bcsv", "examples"
  )
  if (!dir.exists(examples)) {
    skip("canonical schema examples not present in this checkout")
  }
  result <- validate_bcsv(
    file.path(examples, "student_data.csv"),
    file.path(examples, "student_data.json"),
    check_hash = FALSE,
    check_constraints = FALSE
  )
  expect_true(result$valid)
  expect_length(result$errors, 0)
})

test_that("schema-only validate_bcsv rejects malformed metadata", {
  csv <- tempfile(fileext = ".csv")
  meta <- tempfile(fileext = ".json")
  writeLines(c("a,b", "1,2"), csv)
  writeLines('{"url": "x.csv", "table_schema": {"columns": [{"name": "a", "datatype": "categorical"}]}}', meta)

  result <- validate_bcsv(csv, meta, check_hash = FALSE, check_constraints = FALSE)
  expect_false(result$valid)
  codes <- vapply(result$errors, function(e) e$code, character(1))
  expect_true("SCHEMA_VIOLATION" %in% codes)
})

