# Tests for read_bcsv covering the §1 type-application table, the four
# check_* toggles, on_violation policy, and columns subset loading.

# Helpers -----------------------------------------------------------------

write_pair <- function(tmp_dir, csv_text, metadata, with_hash = FALSE) {
  csv_path <- file.path(tmp_dir, "data.csv")
  json_path <- file.path(tmp_dir, "data.json")
  writeLines(csv_text, csv_path, sep = "")
  if (isTRUE(with_hash)) {
    metadata$file_hash <- bcsv:::sha256_of_file(csv_path)
  }
  writeLines(jsonlite::toJSON(metadata, auto_unbox = TRUE, pretty = TRUE), json_path)
  list(csv = csv_path, json = json_path)
}

base_metadata <- function(columns) {
  list(
    `@context` = CONTEXT_URL,
    description = "Test data.",
    url = "data.csv",
    table_schema = list(columns = columns)
  )
}


# Type-application table per §1.4 -----------------------------------------

test_that("string column stays character", {
  d <- write_pair(tempdir(), "name\nalice\nbob\ncharlie\n",
                  base_metadata(list(list(name = "name", datatype = "string"))))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_type(df$name, "character")
  expect_equal(df$name, c("alice", "bob", "charlie"))
})

test_that("integer column uses integer", {
  d <- write_pair(tempdir(), "n,x\n1,a\n2,a\nNA,a\n",
                  base_metadata(list(
                    list(name = "n", datatype = "integer", na_strings = list("NA")),
                    list(name = "x", datatype = "string")
                  )))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_type(df$n, "integer")
  expect_equal(df$n[1], 1L)
  expect_equal(df$n[2], 2L)
  expect_true(is.na(df$n[3]))
})

test_that("number column uses double", {
  d <- write_pair(tempdir(), "x,_\n1.5,a\n2.0,a\nNA,a\n",
                  base_metadata(list(
                    list(name = "x", datatype = "number", na_strings = list("NA")),
                    list(name = "_", datatype = "string")
                  )))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_type(df$x, "double")
  expect_equal(df$x[1], 1.5)
  expect_true(is.na(df$x[3]))
})

test_that("boolean column accepts true/false/1/0/yes/no", {
  d <- write_pair(tempdir(), "flag,_\ntrue,a\nfalse,a\n1,a\n0,a\nyes,a\nno,a\nNA,a\n",
                  base_metadata(list(
                    list(name = "flag", datatype = "boolean", na_strings = list("NA")),
                    list(name = "_", datatype = "string")
                  )))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_type(df$flag, "logical")
  expect_equal(df$flag[1:6], c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_true(is.na(df$flag[7]))
})

test_that("categorical column becomes unordered factor with declared levels", {
  d <- write_pair(tempdir(), "group\nA\nB\nA\n",
                  base_metadata(list(
                    list(name = "group", datatype = "categorical",
                         levels = list("A", "B", "C"))
                  )))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_s3_class(df$group, "factor")
  expect_false(is.ordered(df$group))
  expect_equal(levels(df$group), c("A", "B", "C"))
})

test_that("ordered column preserves level order", {
  d <- write_pair(tempdir(), "grade\nmild\nsevere\nmoderate\n",
                  base_metadata(list(
                    list(name = "grade", datatype = "ordered",
                         levels = list("mild", "moderate", "severe"))
                  )))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_true(is.ordered(df$grade))
  expect_equal(levels(df$grade), c("mild", "moderate", "severe"))
  expect_true(df$grade[1] < df$grade[2])  # mild < severe
})

test_that("date column becomes Date", {
  d <- write_pair(tempdir(), "d\n2026-01-15\n2026-02-20\n",
                  base_metadata(list(list(name = "d", datatype = "date"))))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_s3_class(df$d, "Date")
  expect_equal(df$d[1], as.Date("2026-01-15"))
})

test_that("datetime column becomes POSIXct (UTC default)", {
  d <- write_pair(tempdir(), "ts\n2026-01-15T13:30:00\n2026-01-15T14:00:00\n",
                  base_metadata(list(list(name = "ts", datatype = "datetime"))))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_s3_class(df$ts, "POSIXct")
})

test_that("time column becomes hms", {
  d <- write_pair(tempdir(), "t\n13:30:00\n14:00:00\n",
                  base_metadata(list(list(name = "t", datatype = "time"))))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_s3_class(df$t, "hms")
})


# NA handling --------------------------------------------------------------

test_that("na_strings tokens are recognised", {
  d <- write_pair(tempdir(), "c\n1.5\nBDL\n2.0\nNA\n",
                  base_metadata(list(
                    list(name = "c", datatype = "number",
                         na_strings = list("BDL", "NA"))
                  )))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE)
  expect_true(is.na(df$c[2]))
  expect_true(is.na(df$c[4]))
  expect_equal(df$c[1], 1.5)
})


# check_hash + HASH_ABSENT -------------------------------------------------

test_that("hash match passes", {
  d <- write_pair(tempdir(), "x\n1\n2\n",
                  base_metadata(list(list(name = "x", datatype = "integer"))),
                  with_hash = TRUE)
  df <- read_bcsv(d$csv, d$json, check_hash = TRUE)
  expect_equal(df$x, c(1L, 2L))
})

test_that("hash mismatch raises bcsv_validation_error", {
  d <- write_pair(tempdir(), "x\n1\n2\n",
                  base_metadata(list(list(name = "x", datatype = "integer"))),
                  with_hash = TRUE)
  writeLines("x\n1\n999\n", d$csv, sep = "")
  expect_error(read_bcsv(d$csv, d$json, check_hash = TRUE),
               class = "bcsv_validation_error")
})

test_that("absent hash emits HASH_ABSENT warning", {
  d <- write_pair(tempdir(), "x\n1\n",
                  base_metadata(list(list(name = "x", datatype = "integer"))))
  expect_warning(read_bcsv(d$csv, d$json, check_hash = TRUE),
                 "HASH_ABSENT")
})


# check_schema ------------------------------------------------------------

test_that("schema violation raises bcsv_schema_error", {
  d <- write_pair(tempdir(), "x\nA\n",
                  base_metadata(list(list(name = "x", datatype = "categorical"))))
  expect_error(read_bcsv(d$csv, d$json, check_hash = FALSE),
               class = "bcsv_schema_error")
})


# on_violation: warn vs raise ----------------------------------------------

test_that("range violation warns by default, raises with on_violation=raise", {
  d <- write_pair(tempdir(), "age\n25\n150\n",
                  base_metadata(list(
                    list(name = "age", datatype = "integer", minimum = 0, maximum = 120)
                  )))
  expect_warning(df <- read_bcsv(d$csv, d$json, check_hash = FALSE), "RANGE_VIOLATION")
  expect_equal(df$age[2], 150L)
  expect_error(read_bcsv(d$csv, d$json, check_hash = FALSE, on_violation = "raise"),
               class = "bcsv_validation_error")
})

test_that("required violation warns", {
  d <- write_pair(tempdir(), "id,_\n1,a\nNA,a\n",
                  base_metadata(list(
                    list(name = "id", datatype = "integer", required = TRUE,
                         na_strings = list("NA")),
                    list(name = "_", datatype = "string")
                  )))
  expect_warning(read_bcsv(d$csv, d$json, check_hash = FALSE), "REQUIRED_VIOLATION")
})

test_that("coercion failure warns and sets NA", {
  d <- write_pair(tempdir(), "n\n1\nabc\n3\n",
                  base_metadata(list(list(name = "n", datatype = "integer"))))
  expect_warning(df <- read_bcsv(d$csv, d$json, check_hash = FALSE), "COERCION_FAILED")
  expect_equal(df$n[1], 1L)
  expect_true(is.na(df$n[2]))
  expect_equal(df$n[3], 3L)
})

test_that("level not declared warns and sets NA", {
  d <- write_pair(tempdir(), "g\nA\nC\nA\n",
                  base_metadata(list(
                    list(name = "g", datatype = "categorical", levels = list("A", "B"))
                  )))
  expect_warning(df <- read_bcsv(d$csv, d$json, check_hash = FALSE), "LEVEL_NOT_DECLARED")
  expect_true(is.na(df$g[2]))
})


# columns subset ----------------------------------------------------------

test_that("columns subset returns only requested, in given order", {
  d <- write_pair(tempdir(), "a,b,c\n1,x,3.0\n2,y,6.0\n",
                  base_metadata(list(
                    list(name = "a", datatype = "integer"),
                    list(name = "b", datatype = "string"),
                    list(name = "c", datatype = "number")
                  )))
  df <- read_bcsv(d$csv, d$json, check_hash = FALSE, columns = c("c", "a"))
  expect_equal(names(df), c("c", "a"))
  expect_equal(df$a, c(1L, 2L))
})

test_that("columns of length 0 is rejected", {
  d <- write_pair(tempdir(), "a\n1\n",
                  base_metadata(list(list(name = "a", datatype = "integer"))))
  expect_error(read_bcsv(d$csv, d$json, check_hash = FALSE, columns = character(0)))
})


# primary_key uniqueness --------------------------------------------------

test_that("duplicate primary key warns", {
  meta <- base_metadata(list(list(name = "id", datatype = "integer")))
  meta$table_schema$primary_key <- "id"
  d <- write_pair(tempdir(), "id\n1\n1\n2\n", meta)
  expect_warning(read_bcsv(d$csv, d$json, check_hash = FALSE), "PRIMARY_KEY_VIOLATION")
})


# Canonical schema-repo examples ------------------------------------------

test_that("canonical student_data reads cleanly with ordered grades", {
  examples <- file.path("..", "..", "..", "..",
                        "behaverse-schemas", "bcsv", "examples")
  skip_if_not(dir.exists(examples), "canonical schema examples not in checkout")
  df <- read_bcsv(file.path(examples, "student_data.csv"),
                  file.path(examples, "student_data.json"))
  expect_gt(nrow(df), 0L)
  ordered_cols <- vapply(df, is.ordered, logical(1))
  expect_true(any(ordered_cols))
})

test_that("canonical measurements reads cleanly", {
  examples <- file.path("..", "..", "..", "..",
                        "behaverse-schemas", "bcsv", "examples")
  skip_if_not(dir.exists(examples), "canonical schema examples not in checkout")
  df <- suppressWarnings(  # HASH_ABSENT expected (canonical measurements has no hash)
    read_bcsv(file.path(examples, "measurements.csv"),
              file.path(examples, "measurements.json"))
  )
  expect_gt(nrow(df), 0L)
})
