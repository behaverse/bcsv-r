# Tests for document_bcsv—type inference, column_descriptions layering,
# unknown-key rejection, top-level metadata composition, and round-trip
# with read_bcsv.

# --- type inference -------------------------------------------------------

test_that("string inference (character column)", {
  meta <- document_bcsv(data.frame(label = c("a", "b"), stringsAsFactors = FALSE), "Test dataset.")
  expect_equal(meta$table_schema$columns[[1]]$datatype, "string")
})

test_that("integer inference (integer column)", {
  meta <- document_bcsv(data.frame(n = 1:3), "Test dataset.")
  expect_equal(meta$table_schema$columns[[1]]$datatype, "integer")
})

test_that("number inference (double column)", {
  meta <- document_bcsv(data.frame(x = c(1.5, 2.5, 3.5)), "Test dataset.")
  expect_equal(meta$table_schema$columns[[1]]$datatype, "number")
})

test_that("boolean inference (logical column)", {
  meta <- document_bcsv(data.frame(flag = c(TRUE, FALSE, TRUE)), "Test dataset.")
  expect_equal(meta$table_schema$columns[[1]]$datatype, "boolean")
})

test_that("categorical inference (unordered factor)", {
  df <- data.frame(group = factor(c("A", "B", "A"), levels = c("A", "B", "C")))
  meta <- document_bcsv(df, "Test dataset.")
  col <- meta$table_schema$columns[[1]]
  expect_equal(col$datatype, "categorical")
  expect_equal(unlist(col$levels), c("A", "B", "C"))
})

test_that("ordered inference (ordered factor preserves level order)", {
  df <- data.frame(severity = ordered(
    c("mild", "severe"),
    levels = c("mild", "moderate", "severe", "critical")
  ))
  meta <- document_bcsv(df, "Test dataset.")
  col <- meta$table_schema$columns[[1]]
  expect_equal(col$datatype, "ordered")
  expect_equal(unlist(col$levels), c("mild", "moderate", "severe", "critical"))
})

test_that("date inference (Date column)", {
  df <- data.frame(d = as.Date(c("2026-01-15", "2026-02-20")))
  meta <- document_bcsv(df, "Test dataset.")
  expect_equal(meta$table_schema$columns[[1]]$datatype, "date")
})

test_that("datetime inference (POSIXct column)", {
  df <- data.frame(ts = as.POSIXct(c("2026-01-15 13:30:00", "2026-01-15 14:00:00"),
                                    tz = "UTC"))
  meta <- document_bcsv(df, "Test dataset.")
  expect_equal(meta$table_schema$columns[[1]]$datatype, "datetime")
})

test_that("time inference (hms column)", {
  skip_if_not_installed("hms")
  df <- data.frame(t = hms::as_hms(c("13:30:00", "14:00:00")))
  meta <- document_bcsv(df, "Test dataset.")
  expect_equal(meta$table_schema$columns[[1]]$datatype, "time")
})


# --- column_descriptions overrides + unknown-key rejection ---------------

test_that("column_descriptions layered over inferred datatype", {
  meta <- document_bcsv(
    data.frame(age = 25:30),
    "Test dataset.",
    column_descriptions = list(
      age = list(label = "Age", unit = "years", minimum = 18, maximum = 100)
    )
  )
  col <- meta$table_schema$columns[[1]]
  expect_equal(col$datatype, "integer")  # inferred
  expect_equal(col$label, "Age")
  expect_equal(col$unit, "years")
  expect_equal(col$minimum, 18)
})

test_that("unknown column_descriptions key raises bcsv_unknown_key_error", {
  expect_error(
    document_bcsv(
      data.frame(x = 1:3),
      "Test dataset.",
      column_descriptions = list(x = list(unti = "kg"))
    ),
    class = "bcsv_unknown_key_error"
  )
})

test_that("unknown_key error message lists allowed keys to help discovery", {
  err <- tryCatch(
    document_bcsv(
      data.frame(x = 1:3),
      "Test dataset.",
      column_descriptions = list(x = list(bogus = TRUE))
    ),
    bcsv_unknown_key_error = function(e) e
  )
  for (key in c("label", "description", "unit", "minimum", "na_strings")) {
    expect_match(conditionMessage(err), key, fixed = TRUE)
  }
})

test_that("bcsv_unknown_key_error inherits from bcsv_error", {
  err <- tryCatch(
    document_bcsv(data.frame(x = 1:3), "Test dataset.",
                  column_descriptions = list(x = list(bogus = TRUE))),
    bcsv_unknown_key_error = function(e) e
  )
  expect_s3_class(err, "bcsv_error")
})


# --- top-level metadata composition --------------------------------------

test_that("@context uses canonical URL", {
  meta <- document_bcsv(data.frame(x = 1), "Test dataset.")
  expect_equal(meta$`@context`, CONTEXT_URL)
})

test_that("date_created defaults to today (ISO format)", {
  meta <- document_bcsv(data.frame(x = 1), "Test dataset.")
  expect_equal(meta$date_created, format(Sys.Date(), "%Y-%m-%d"))
})

test_that("date_created can be set explicitly", {
  meta <- document_bcsv(data.frame(x = 1), "Test dataset.", date_created = as.Date("2026-01-15"))
  expect_equal(meta$date_created, "2026-01-15")
})

test_that("url is omitted when data_url is NULL", {
  meta <- document_bcsv(data.frame(x = 1), "Test dataset.")
  expect_null(meta$url)
})

test_that("url is set when data_url is given", {
  meta <- document_bcsv(data.frame(x = 1), "Test dataset.", data_url = "trial.csv")
  expect_equal(meta$url, "trial.csv")
})

test_that("creator: string passes through", {
  meta <- document_bcsv(data.frame(x = 1), "Test dataset.", creator = "Research Lab")
  expect_equal(meta$creator, "Research Lab")
})

test_that("creator: single structured list passes through", {
  meta <- document_bcsv(
    data.frame(x = 1),
    "Test dataset.",
    creator = list(name = "Alice", orcid = "0000-0001-2345-6789")
  )
  expect_equal(meta$creator$name, "Alice")
})

test_that("creator: list of structured creators passes through", {
  meta <- document_bcsv(
    data.frame(x = 1),
    "Test dataset.",
    creator = list(list(name = "Alice"), list(name = "Bob"))
  )
  expect_length(meta$creator, 2)
})

test_that("primary_key as string nests under table_schema (not top level)", {
  meta <- document_bcsv(data.frame(id = 1:2), "Test dataset.", primary_key = "id")
  expect_equal(meta$table_schema$primary_key, "id")
  expect_null(meta$primary_key)
})

test_that("primary_key as composite vector", {
  meta <- document_bcsv(
    data.frame(a = c(1, 1), b = c("x", "y")),
    "Test dataset.",
    primary_key = c("a", "b")
  )
  expect_equal(meta$table_schema$primary_key, c("a", "b"))
})

test_that("emitted JSON uses snake_case throughout (no camelCase forms)", {
  meta <- document_bcsv(
    data.frame(x = 1),
    "Test dataset.",
    pretty_name = "Title",
    primary_key = "x",
    data_url = "x.csv"
  )
  json <- jsonlite::toJSON(meta, auto_unbox = TRUE)
  for (camel in c("tableSchema", "primaryKey", "prettyName", "dateCreated")) {
    expect_false(grepl(camel, json, fixed = TRUE), info = camel)
  }
})


# --- emitted metadata passes schema validation ---------------------------

test_that("emitted metadata validates against the live JSON Schema", {
  df <- data.frame(
    id = 1:3,
    score = c(85.5, 92.0, 78.5),
    grade = ordered(c("B", "A", "C"), levels = c("A", "B", "C", "D"))
  )
  meta <- document_bcsv(
    df,
    data_url = "grades.csv",
    pretty_name = "Test Grades",
    description = "Single test",
    primary_key = "id",
    column_descriptions = list(
      score = list(minimum = 0, maximum = 100, unit = "points")
    )
  )
  raw <- jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null")
  result <- jsonvalidate::json_validate(
    json = raw,
    schema = bcsv:::schema_path(),
    engine = "ajv",
    verbose = TRUE
  )
  if (!isTRUE(result)) {
    cat("Schema errors:\n")
    print(attr(result, "errors"))
  }
  expect_true(isTRUE(result))
})


# --- round-trip: document → write → read ---------------------------------

test_that("documenting then reading back preserves types", {
  df <- data.frame(
    id = 1:3,
    label = c("x", "y", "z"),
    grade = ordered(c("B", "A", "C"), levels = c("A", "B", "C")),
    stringsAsFactors = FALSE
  )
  meta <- document_bcsv(df, "Test dataset.", data_url = "rt.csv")

  tmp <- tempdir()
  csv_path <- file.path(tmp, "rt.csv")
  json_path <- file.path(tmp, "rt.json")
  write.csv(df, csv_path, row.names = FALSE)
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), json_path)

  df2 <- read_bcsv(csv_path, json_path, check_hash = FALSE)

  expect_true(is.integer(df2$id))
  expect_type(df2$label, "character")
  expect_true(is.ordered(df2$grade))
  expect_equal(levels(df2$grade), c("A", "B", "C"))
})


# --- required description ----------------------------------------------------

test_that("missing description errors with a helpful message", {
  expect_error(document_bcsv(data.frame(x = 1)), "description")
})

test_that("empty description errors", {
  expect_error(document_bcsv(data.frame(x = 1), "   "), "description")
})

test_that("description is emitted right after @context", {
  meta <- document_bcsv(data.frame(x = 1), "A test table.")
  expect_identical(meta$description, "A test table.")
  expect_identical(names(meta)[1:2], c("@context", "description"))
})
