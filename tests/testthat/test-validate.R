# Tests for validate_bcsv—all five stages, on_violation promotion,
# and the ValidationResult shape.

# --- helpers --------------------------------------------------------------

write_validation_pair <- function(csv_text, metadata, with_hash = FALSE) {
  tmp <- tempfile("bcsv-v-"); dir.create(tmp)
  csv <- file.path(tmp, "data.csv")
  json <- file.path(tmp, "data.json")
  writeLines(csv_text, csv, sep = "")
  if (isTRUE(with_hash)) {
    metadata$file_hash <- bcsv:::sha256_of_file(csv)
  }
  writeLines(jsonlite::toJSON(metadata, auto_unbox = TRUE, pretty = TRUE), json)
  list(csv = csv, json = json)
}

vbase <- function(columns) {
  list(`@context` = CONTEXT_URL, url = "data.csv",
       description = "Test data.",
       table_schema = list(columns = columns))
}

codes <- function(issues) vapply(issues, function(i) i$code, character(1))


# --- ValidationResult shape ----------------------------------------------

test_that("clean pair yields valid=TRUE with no errors/warnings", {
  d <- write_validation_pair(
    "x\n1\n2\n",
    vbase(list(list(name = "x", datatype = "integer"))),
    with_hash = TRUE
  )
  result <- validate_bcsv(d$csv, d$json)
  expect_true(result$valid)
  expect_length(result$errors, 0)
  expect_length(result$warnings, 0)
  expect_equal(result$data_columns, "x")
  expect_equal(result$meta_columns, "x")
})


# --- stage 1: file existence ---------------------------------------------

test_that("data file not found", {
  tmp <- tempfile("bcsv-v-"); dir.create(tmp)
  json <- file.path(tmp, "data.json")
  writeLines(jsonlite::toJSON(vbase(list(list(name = "x", datatype = "integer"))),
                              auto_unbox = TRUE), json)
  result <- validate_bcsv(file.path(tmp, "missing.csv"), json)
  expect_false(result$valid)
  expect_true("FILE_NOT_FOUND" %in% codes(result$errors))
})

test_that("metadata file not found", {
  tmp <- tempfile("bcsv-v-"); dir.create(tmp)
  writeLines("x\n1\n", file.path(tmp, "data.csv"))
  result <- validate_bcsv(file.path(tmp, "data.csv"))
  expect_false(result$valid)
  expect_true("FILE_NOT_FOUND" %in% codes(result$errors))
})


# --- METADATA_INVALID_JSON -----------------------------------------------

test_that("invalid JSON metadata yields METADATA_INVALID_JSON", {
  tmp <- tempfile("bcsv-v-"); dir.create(tmp)
  writeLines("x\n1\n", file.path(tmp, "data.csv"))
  writeLines("not json {{{", file.path(tmp, "data.json"))
  result <- validate_bcsv(file.path(tmp, "data.csv"))
  expect_false(result$valid)
  expect_true("METADATA_INVALID_JSON" %in% codes(result$errors))
})


# --- stage 2: schema validation ------------------------------------------

test_that("schema violation surfaced with JSON pointer location", {
  d <- write_validation_pair(
    "x\nA\n",
    vbase(list(list(name = "x", datatype = "categorical"))) # missing levels
  )
  result <- validate_bcsv(d$csv, d$json, check_hash = FALSE, check_constraints = FALSE)
  expect_false(result$valid)
  schema_errs <- Filter(function(e) e$code == "SCHEMA_VIOLATION", result$errors)
  expect_gt(length(schema_errs), 0)
  expect_match(schema_errs[[1]]$location, "/table_schema/columns/0")
})

test_that("check_schema=FALSE skips schema violations", {
  d <- write_validation_pair(
    "x\nA\n",
    vbase(list(list(name = "x", datatype = "categorical")))
  )
  result <- validate_bcsv(d$csv, d$json,
                          check_schema = FALSE,
                          check_hash = FALSE,
                          check_constraints = FALSE)
  expect_false("SCHEMA_VIOLATION" %in% codes(result$errors))
})


# --- stage 3: column matching --------------------------------------------

test_that("column missing in data", {
  d <- write_validation_pair(
    "a\n1\n",
    vbase(list(
      list(name = "a", datatype = "integer"),
      list(name = "b", datatype = "string")
    ))
  )
  result <- validate_bcsv(d$csv, d$json, check_hash = FALSE, check_constraints = FALSE)
  expect_true("COLUMN_MISSING_IN_DATA" %in% codes(result$errors))
})

test_that("column missing in metadata", {
  d <- write_validation_pair(
    "a,b\n1,x\n",
    vbase(list(list(name = "a", datatype = "integer")))
  )
  result <- validate_bcsv(d$csv, d$json, check_hash = FALSE, check_constraints = FALSE)
  expect_true("COLUMN_MISSING_IN_METADATA" %in% codes(result$errors))
})

test_that("column order differs is a warning, not an error", {
  d <- write_validation_pair(
    "b,a\nx,1\n",
    vbase(list(
      list(name = "a", datatype = "integer"),
      list(name = "b", datatype = "string")
    ))
  )
  result <- validate_bcsv(d$csv, d$json, check_hash = FALSE, check_constraints = FALSE)
  expect_true("COLUMN_ORDER_DIFFERS" %in% codes(result$warnings))
  expect_false("COLUMN_ORDER_DIFFERS" %in% codes(result$errors))
  expect_true(result$valid)  # order-only differences don't invalidate
})


# --- stage 4: hash check -------------------------------------------------

test_that("hash match passes silently", {
  d <- write_validation_pair("x\n1\n", vbase(list(list(name = "x", datatype = "integer"))),
                             with_hash = TRUE)
  result <- validate_bcsv(d$csv, d$json)
  expect_false("HASH_MISMATCH" %in% codes(result$errors))
  expect_false("HASH_ABSENT" %in% codes(result$warnings))
})

test_that("hash mismatch yields HASH_MISMATCH error", {
  d <- write_validation_pair("x\n1\n", vbase(list(list(name = "x", datatype = "integer"))),
                             with_hash = TRUE)
  writeLines("x\n999\n", d$csv, sep = "")
  result <- validate_bcsv(d$csv, d$json)
  expect_false(result$valid)
  expect_true("HASH_MISMATCH" %in% codes(result$errors))
})

test_that("hash absent is a warning, not an error", {
  d <- write_validation_pair("x\n1\n", vbase(list(list(name = "x", datatype = "integer"))))
  result <- validate_bcsv(d$csv, d$json)
  expect_true("HASH_ABSENT" %in% codes(result$warnings))
  expect_false("HASH_ABSENT" %in% codes(result$errors))
  expect_true(result$valid)
})

test_that("check_hash=FALSE skips all hash codes", {
  d <- write_validation_pair("x\n1\n", vbase(list(list(name = "x", datatype = "integer"))))
  result <- validate_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_false("HASH_MISMATCH" %in% codes(result$errors))
  expect_false("HASH_ABSENT" %in% codes(result$warnings))
})


# --- stage 5: constraints ------------------------------------------------

test_that("range violation is a warning", {
  d <- write_validation_pair(
    "age\n25\n200\n",
    vbase(list(list(name = "age", datatype = "integer",
                    minimum = 0, maximum = 120))),
    with_hash = TRUE
  )
  result <- validate_bcsv(d$csv, d$json)
  expect_true("RANGE_VIOLATION" %in% codes(result$warnings))
  expect_true(result$valid)
})

test_that("required violation is a warning", {
  d <- write_validation_pair(
    "id,_\n1,a\nNA,a\n",
    vbase(list(
      list(name = "id", datatype = "integer", required = TRUE,
           na_strings = list("NA")),
      list(name = "_", datatype = "string")
    )),
    with_hash = TRUE
  )
  result <- validate_bcsv(d$csv, d$json)
  expect_true("REQUIRED_VIOLATION" %in% codes(result$warnings))
})

test_that("length violation is a warning (both directions)", {
  d <- write_validation_pair(
    "code\nAB\nABCDEFGHIJ\n",
    vbase(list(list(name = "code", datatype = "string",
                    min_length = 3, max_length = 5))),
    with_hash = TRUE
  )
  result <- validate_bcsv(d$csv, d$json)
  length_violations <- sum(codes(result$warnings) == "LENGTH_VIOLATION")
  expect_equal(length_violations, 2)
})

test_that("coercion failed is a warning", {
  d <- write_validation_pair(
    "n\n1\nabc\n",
    vbase(list(list(name = "n", datatype = "integer"))),
    with_hash = TRUE
  )
  result <- validate_bcsv(d$csv, d$json)
  expect_true("COERCION_FAILED" %in% codes(result$warnings))
})

test_that("level not declared is a warning", {
  d <- write_validation_pair(
    "g\nA\nC\n",
    vbase(list(list(name = "g", datatype = "categorical", levels = list("A", "B")))),
    with_hash = TRUE
  )
  result <- validate_bcsv(d$csv, d$json)
  expect_true("LEVEL_NOT_DECLARED" %in% codes(result$warnings))
})

test_that("levels required surfaces even when check_schema=FALSE", {
  d <- write_validation_pair(
    "g\nA\n",
    vbase(list(list(name = "g", datatype = "categorical"))),
    with_hash = TRUE
  )
  result <- validate_bcsv(d$csv, d$json, check_schema = FALSE)
  expect_true("LEVELS_REQUIRED" %in% codes(result$errors))
})

test_that("levels forbidden surfaces even when check_schema=FALSE", {
  d <- write_validation_pair(
    "x\n1\n",
    vbase(list(list(name = "x", datatype = "integer", levels = list("a", "b")))),
    with_hash = TRUE
  )
  result <- validate_bcsv(d$csv, d$json, check_schema = FALSE)
  expect_true("LEVELS_FORBIDDEN" %in% codes(result$errors))
})

test_that("primary_key uniqueness violation is a warning", {
  meta <- vbase(list(list(name = "id", datatype = "integer")))
  meta$table_schema$primary_key <- "id"
  d <- write_validation_pair("id\n1\n1\n2\n", meta, with_hash = TRUE)
  result <- validate_bcsv(d$csv, d$json)
  expect_true("PRIMARY_KEY_VIOLATION" %in% codes(result$warnings))
})

test_that("composite primary_key", {
  meta <- vbase(list(
    list(name = "a", datatype = "integer"),
    list(name = "b", datatype = "string")
  ))
  meta$table_schema$primary_key <- list("a", "b")
  d <- write_validation_pair("a,b\n1,x\n1,y\n1,x\n", meta, with_hash = TRUE)
  result <- validate_bcsv(d$csv, d$json)
  expect_true("PRIMARY_KEY_VIOLATION" %in% codes(result$warnings))
})

test_that("check_constraints=FALSE skips all constraint codes", {
  d <- write_validation_pair(
    "age\n25\n200\n",
    vbase(list(list(name = "age", datatype = "integer",
                    minimum = 0, maximum = 120))),
    with_hash = TRUE
  )
  result <- validate_bcsv(d$csv, d$json, check_constraints = FALSE)
  expect_false("RANGE_VIOLATION" %in% codes(result$warnings))
})


# --- on_violation = "raise" promotes warnings to errors ------------------

test_that("on_violation='raise' promotes warnings to errors", {
  d <- write_validation_pair(
    "age\n25\n200\n",
    vbase(list(list(name = "age", datatype = "integer",
                    minimum = 0, maximum = 120))),
    with_hash = TRUE
  )
  r_warn <- validate_bcsv(d$csv, d$json, on_violation = "warn")
  expect_true(r_warn$valid)
  expect_true("RANGE_VIOLATION" %in% codes(r_warn$warnings))

  r_raise <- validate_bcsv(d$csv, d$json, on_violation = "raise")
  expect_false(r_raise$valid)
  expect_true("RANGE_VIOLATION" %in% codes(r_raise$errors))
  expect_length(r_raise$warnings, 0)
})

test_that("on_violation='raise' promotes HASH_ABSENT", {
  d <- write_validation_pair("x\n1\n", vbase(list(list(name = "x", datatype = "integer"))))
  result <- validate_bcsv(d$csv, d$json, on_violation = "raise")
  expect_false(result$valid)
  expect_true("HASH_ABSENT" %in% codes(result$errors))
})

test_that("validate_bcsv never raises (spec §4.1)", {
  d <- write_validation_pair(
    "x\n1\nabc\n",
    vbase(list(list(name = "x", datatype = "integer")))
  )
  # Should NOT throw—even with on_violation='raise', returns a result list.
  result <- validate_bcsv(d$csv, d$json,
                          on_violation = "raise", check_hash = FALSE)
  expect_false(result$valid)
})


# --- dialect --------------------------------------------------------------

test_that("unsupported dialect key surfaces as warning", {
  meta <- vbase(list(list(name = "x", datatype = "integer")))
  meta$dialect <- list(delimiter = ",", quoteChar = '"')
  d <- write_validation_pair("x\n1\n", meta, with_hash = TRUE)
  result <- validate_bcsv(d$csv, d$json)
  expect_true("DIALECT_UNSUPPORTED" %in% codes(result$warnings))
})


# --- canonical schema-repo examples --------------------------------------

test_that("canonical student_data validates cleanly", {
  examples <- file.path("..", "..", "..", "..",
                        "behaverse-schemas", "bcsv", "examples")
  skip_if_not(dir.exists(examples), "canonical schema examples not in checkout")
  result <- validate_bcsv(file.path(examples, "student_data.csv"),
                          file.path(examples, "student_data.json"))
  expect_length(result$errors, 0)
})

test_that("canonical measurements validates cleanly (HASH_ABSENT warning ok)", {
  examples <- file.path("..", "..", "..", "..",
                        "behaverse-schemas", "bcsv", "examples")
  skip_if_not(dir.exists(examples), "canonical schema examples not in checkout")
  result <- validate_bcsv(file.path(examples, "measurements.csv"),
                          file.path(examples, "measurements.json"))
  expect_length(result$errors, 0)
})
