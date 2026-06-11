test_that("metadata <-> yaml round-trips", {
  meta <- list(`@context` = "https://x", url = "d.csv",
               table_schema = list(columns = list(
                 list(name = "id", datatype = "integer"),
                 list(name = "g", datatype = "ordered", levels = list("lo", "hi")))))
  p <- tempfile(fileext = ".yaml")
  bcsv:::.metadata_to_yaml(meta, p)
  expect_true(file.exists(p))
  back <- bcsv:::.metadata_from_yaml(p)
  expect_equal(back$url, "d.csv")
  expect_equal(back$table_schema$columns[[2]]$datatype, "ordered")
})

.write_survey_csv <- function(path) {
  readr::write_csv(data.frame(id = 1:2, score = c(88, 92)), path)
}

# Replace the draft's table-description placeholder with real text.
.describe <- function(d, name = "survey") {
  yaml_path <- file.path(d, paste0(name, ".yaml"))
  meta <- bcsv:::.metadata_from_yaml(yaml_path)
  meta$description <- "Survey responses."
  bcsv:::.metadata_to_yaml(meta, yaml_path)
}

test_that("draft then finalize round-trips and enforces single-doc", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  yaml_path <- draft_metadata(csv)
  expect_true(file.exists(yaml_path))
  .describe(d)
  res <- finalize_metadata(csv)
  expect_true(res$valid)
  expect_true(file.exists(file.path(d, "survey.json")))
  expect_false(file.exists(yaml_path))
})

test_that("draft refuses to clobber without overwrite", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  draft_metadata(csv)
  expect_error(draft_metadata(csv), "already exists")
  expect_silent(draft_metadata(csv, overwrite = TRUE))
})

test_that("finalize aborts on invalid yaml, leaving files intact", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  yaml_path <- draft_metadata(csv)
  meta <- bcsv:::.metadata_from_yaml(yaml_path)
  meta$table_schema$columns <- c(meta$table_schema$columns,
                                 list(list(name = "ghost", datatype = "string")))
  bcsv:::.metadata_to_yaml(meta, yaml_path)
  res <- suppressMessages(finalize_metadata(csv))
  expect_false(res$valid)
  expect_false(file.exists(file.path(d, "survey.json")))
  expect_true(file.exists(yaml_path))
})

test_that("edit converts json back to yaml and removes the json", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  draft_metadata(csv); .describe(d); finalize_metadata(csv)
  yaml_path <- edit_metadata(csv)
  expect_true(file.exists(yaml_path))
  expect_false(file.exists(file.path(d, "survey.json")))
})

test_that("export writes a copy and leaves the canonical untouched", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  draft_metadata(csv); .describe(d); finalize_metadata(csv)
  out <- export_metadata(csv, output_format = "yaml")
  expect_match(out, "survey_export\\.yaml$")
  expect_true(file.exists(out))
  expect_true(file.exists(file.path(d, "survey.json")))
})

test_that("finalize keep_source keeps both files", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  yaml_path <- draft_metadata(csv)
  .describe(d)
  finalize_metadata(csv, keep_source = TRUE)
  expect_true(file.exists(yaml_path))
  expect_true(file.exists(file.path(d, "survey.json")))
})

test_that("finalize errors when both yaml and json exist", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  draft_metadata(csv)
  .describe(d)
  finalize_metadata(csv, keep_source = TRUE)   # both survey.yaml and survey.json now exist
  expect_error(finalize_metadata(csv), "ambiguous")
})

test_that("print_metadata returns the metadata object", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  draft_metadata(csv); .describe(d); finalize_metadata(csv)
  meta <- print_metadata(csv)
  expect_equal(meta$table_schema$columns[[1]]$name, "id")
})

test_that("finalize writes a canonical CSV (LF, comma, no index)", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv")
  writeBin(charToRaw("id,score\r\n1,88\r\n2,92\r\n"), csv)
  jsonlite::write_json(list(`@context` = CONTEXT_URL, url = "survey.csv",
    description = "Survey responses.",
    table_schema = list(columns = list(
      list(name = "id", datatype = "integer"),
      list(name = "score", datatype = "integer")))),
    file.path(d, "survey.json"), auto_unbox = TRUE, null = "null")
  res <- finalize_metadata(csv)
  expect_true(res$valid)
  out <- rawToChar(readBin(csv, "raw", file.info(csv)$size))
  expect_identical(out, "id,score\n1,88\n2,92\n")
})

test_that("finalize normalizes a non-comma delimiter to comma", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv")
  writeLines(c("id;score", "1;88", "2;92"), csv)
  bcsv:::.metadata_to_yaml(list(`@context` = CONTEXT_URL, url = "survey.csv",
    description = "Survey responses.",
    dialect = list(delimiter = ";"),
    table_schema = list(columns = list(
      list(name = "id", datatype = "integer"),
      list(name = "score", datatype = "integer")))),
    file.path(d, "survey.yaml"))
  res <- finalize_metadata(csv)
  expect_true(res$valid)
  out <- rawToChar(readBin(csv, "raw", file.info(csv)$size))
  expect_identical(out, "id,score\n1,88\n2,92\n")
})

test_that("finalize on invalid metadata leaves the CSV untouched", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv")
  writeBin(charToRaw("id,score\n1,88\n2,92\n"), csv)
  original <- readBin(csv, "raw", file.info(csv)$size)
  yaml_path <- draft_metadata(csv)
  meta <- bcsv:::.metadata_from_yaml(yaml_path)
  meta$table_schema$columns <- c(meta$table_schema$columns,
                                 list(list(name = "ghost", datatype = "string")))
  bcsv:::.metadata_to_yaml(meta, yaml_path)
  res <- suppressMessages(finalize_metadata(csv))
  expect_false(res$valid)
  expect_identical(readBin(csv, "raw", file.info(csv)$size), original)
  expect_false(file.exists(paste0(csv, ".tmp")))
})

test_that("draft from a data frame writes csv + yaml", {
  d <- tempfile(); dir.create(d)
  df <- data.frame(id = 1:2, score = c(88, 92))
  stem <- file.path(d, "survey")
  yaml_path <- draft_metadata(df, stem)
  expect_identical(yaml_path, paste0(stem, ".yaml"))
  expect_true(file.exists(paste0(stem, ".csv")))
  .describe(d)
  res <- finalize_metadata(paste0(stem, ".csv"))
  expect_true(res$valid)
  expect_true(file.exists(paste0(stem, ".json")))
})

test_that("draft from a df refuses to clobber an existing csv", {
  d <- tempfile(); dir.create(d)
  df <- data.frame(id = 1:2, score = c(88, 92))
  csv <- file.path(d, "survey.csv")
  writeLines(c("id,score", "9,9"), csv)
  expect_error(draft_metadata(df, file.path(d, "survey")), "already exists")
  expect_silent(draft_metadata(df, file.path(d, "survey"), overwrite = TRUE))
})

# Whole-number columns draft as integer, NA or fractional ones as number—the
# shared cross-language inference rule for bare CSVs (mirrors pandas).
test_that("bare-CSV drafts infer integer like pandas", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv")
  writeLines(c("a,b,c", "1,1.5,2", "2,3.5,"), csv)
  cols <- bcsv:::.metadata_from_yaml(draft_metadata(csv))$table_schema$columns
  names(cols) <- vapply(cols, `[[`, "", "name")
  expect_identical(cols$a$datatype, "integer")   # whole, no NA
  expect_identical(cols$b$datatype, "number")    # fractional
  expect_identical(cols$c$datatype, "number")    # has NA → pandas float64
})

# --- draft boilerplate + placeholder stripping ------------------------------

test_that("draft contains the legend and description placeholders", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv")
  readr::write_csv(data.frame(id = 1:2, q12r = 3:4), csv)
  text <- paste(readLines(draft_metadata(csv)), collapse = "\n")
  expect_match(text, "^# Optional keys per column")
  for (key in c("label", "unit", "minimum", "levels", "na_strings", "required")) {
    expect_match(text, key)
  }
  expect_match(text, "description: Description of the survey dataset", fixed = TRUE)
  expect_match(text, "description: Description of id", fixed = TRUE)
  expect_match(text, "description: Description of q12r", fixed = TRUE)
})

test_that("draft respects caller-supplied descriptions", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  yaml_path <- draft_metadata(
    csv,
    description = "Survey responses.",
    column_descriptions = list(score = list(description = "Exam score"))
  )
  meta <- bcsv:::.metadata_from_yaml(yaml_path)
  expect_identical(meta$description, "Survey responses.")
  cols <- meta$table_schema$columns
  names(cols) <- vapply(cols, `[[`, "", "name")
  expect_identical(cols$score$description, "Exam score")
  expect_identical(cols$id$description, "Description of id")   # still placeholder
})

test_that("finalize strips unedited column placeholders", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  draft_metadata(csv)
  .describe(d)                       # real table description; columns untouched
  res <- finalize_metadata(csv)
  expect_true(res$valid)
  meta <- jsonlite::fromJSON(file.path(d, "survey.json"), simplifyVector = FALSE)
  for (col in meta$table_schema$columns) {
    expect_null(col$description)
  }
})

test_that("finalize fails while the table description is a placeholder", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  yaml_path <- draft_metadata(csv)
  res <- suppressMessages(finalize_metadata(csv))   # placeholder never edited
  expect_false(res$valid)
  codes <- vapply(res$errors, `[[`, "", "code")
  expect_true("SCHEMA_VIOLATION" %in% codes)
  expect_false(file.exists(file.path(d, "survey.json")))
  expect_true(file.exists(yaml_path))   # YAML kept for another editing round
})

test_that("edit_metadata yaml carries the legend", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv"); .write_survey_csv(csv)
  draft_metadata(csv); .describe(d); finalize_metadata(csv)
  text <- paste(readLines(edit_metadata(csv)), collapse = "\n")
  expect_match(text, "^# Optional keys per column")
})

.golden_draft <- paste0(
  "# Optional keys per column (uncomment/add as needed):\n",
  "#   label: short display name\n",
  "#   unit: e.g. ms, %, years\n",
  "#   minimum: / maximum: numeric bounds\n",
  "#   min_length: / max_length: string lengths\n",
  "#   levels: [...] required for categorical/ordered datatypes\n",
  "#   na_strings: [...] extra strings to read as NA\n",
  "#   required: true to forbid missing values\n",
  "'@context': https://behaverse.org/schemas/bcsv/context.jsonld\n",
  "description: Description of the survey dataset\n",
  "table_schema:\n",
  "  columns:\n",
  "  - name: id\n",
  "    datatype: integer\n",
  "    description: Description of id\n",
  "  - name: name\n",
  "    datatype: string\n",
  "    description: Description of name\n",
  "  - name: score\n",
  "    datatype: number\n",
  "    description: Description of score\n",
  "url: survey.csv\n",
  "date_created: '2026-06-11'\n"
)

test_that("draft golden text matches the cross-language contract", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv")
  writeBin(charToRaw("id,name,score\n1,alice,88.5\n2,bob,92.5\n"), csv)
  draft <- draft_metadata(csv, date_created = as.Date("2026-06-11"))
  text <- rawToChar(readBin(draft, "raw", file.info(draft)$size))
  expect_identical(text, .golden_draft)
})

test_that("finalize cleans up temp files when validation raises", {
  d <- tempfile(); dir.create(d)
  csv <- file.path(d, "survey.csv")
  writeLines(c("id,score", "1,88"), csv)
  bcsv:::.metadata_to_yaml(list(`@context` = CONTEXT_URL, url = "survey.csv",
    table_schema = list(columns = "oops")), file.path(d, "survey.yaml"))
  expect_error(finalize_metadata(csv))
  expect_false(file.exists(paste0(csv, ".tmp")))
  expect_false(file.exists(paste0(file.path(d, "survey.json"), ".tmp")))
  expect_identical(readLines(csv), c("id,score", "1,88"))   # original untouched
})
