# High-level metadata-authoring workflow. Five verbs wrap the core functions;
# a single metadata document (YAML while editing, JSON when finalized) exists
# at a time.

# Commented legend prepended to editable YAML drafts. One block at the top
# (not per column) so wide tables stay readable; YAML parsers ignore it.
.YAML_LEGEND <- paste0(
  "# Optional keys per column (uncomment/add as needed):\n",
  "#   label: short display name\n",
  "#   unit: e.g. ms, %, years\n",
  "#   minimum: / maximum: numeric bounds\n",
  "#   min_length: / max_length: string lengths\n",
  "#   levels: [...] required for categorical/ordered datatypes\n",
  "#   na_strings: [...] extra strings to read as NA\n",
  "#   required: true to forbid missing values\n"
)

# Draft placeholder for the (required) table description.
.table_placeholder <- function(stem) {
  sprintf("Description of the %s dataset", basename(stem))
}

# Draft placeholder for a column description.
.column_placeholder <- function(name) {
  sprintf("Description of %s", name)
}

# readr reads whole-number CSV columns as double; pandas reads them as int64.
# Mirror pandas so the same bare CSV drafts the same datatype in both
# languages: a double column with no NAs whose values are all whole (and in
# integer range) drafts as integer. A column with NAs stays double, matching
# pandas' fallback to float64 for missing data.
.align_integer_inference <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.double(col) && length(col) > 0L && !anyNA(col) &&
        all(col == trunc(col)) && all(abs(col) <= .Machine$integer.max)) {
      df[[nm]] <- as.integer(col)
    }
  }
  df
}

# Drop descriptions still matching their draft placeholders. Column
# descriptions are optional, so they simply vanish; a still-placeholder table
# description is dropped too, letting schema validation fail finalization with
# its own required-property error.
.strip_placeholders <- function(meta, stem) {
  if (identical(meta$description, .table_placeholder(stem))) meta$description <- NULL
  cols <- meta$table_schema$columns
  if (!is.null(cols)) {
    meta$table_schema$columns <- lapply(cols, function(col) {
      if (is.list(col) &&
          identical(col$description, .column_placeholder(col$name %||% ""))) {
        col$description <- NULL
      }
      col
    })
  }
  meta
}

.metadata_to_yaml <- function(metadata, file = NULL, legend = FALSE) {
  text <- yaml::as.yaml(unclass(metadata))
  if (isTRUE(legend)) text <- paste0(.YAML_LEGEND, text)
  # writeLines appends a newline; trim the serializer's own so the file ends
  # with exactly one (keeps draft YAML byte-identical with Python's).
  if (!is.null(file)) writeLines(sub("\n$", "", text), file, useBytes = TRUE)
  invisible(text)
}

.metadata_from_yaml <- function(file) {
  yaml::read_yaml(file)
}

.stem <- function(path) {
  sub("\\.(csv|yaml|yml|json)$", "", path, ignore.case = TRUE)
}

.read_sidecar <- function(path) {
  if (grepl("\\.(yaml|yml)$", path, ignore.case = TRUE)) {
    return(.metadata_from_yaml(path))
  }
  if (grepl("\\.json$", path, ignore.case = TRUE)) {
    return(jsonlite::fromJSON(path, simplifyVector = FALSE))
  }
  stem <- .stem(path)
  for (ext in c(".yaml", ".yml", ".json")) {
    cand <- paste0(stem, ext)
    if (file.exists(cand)) return(.read_sidecar(cand))
  }
  stop(sprintf("No metadata sidecar found for %s.", path), call. = FALSE)
}

.read_csv_raw <- function(path, delimiter = ",", encoding = "UTF-8") {
  df <- readr::read_delim(
    path, delim = delimiter,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(0),
    locale = readr::locale(encoding = encoding),
    show_col_types = FALSE, progress = FALSE
  )
  as.data.frame(df, stringsAsFactors = FALSE)
}

.write_csv_canonical <- function(df, path) {
  readr::write_csv(df, path, na = "", eol = "\n")
}

#' Draft an editable YAML metadata file for a dataset
#'
#' @param data A CSV path or a data frame.
#'   - CSV path: the YAML is written next to it; the CSV is read for inference but
#'     left untouched (`finalize_metadata` standardizes it later).
#'   - Data frame: `out` is an output **stem** (e.g. "survey"); both `<stem>.csv`
#'     (canonical) and `<stem>.yaml` are written. A CSV written from a typed data
#'     frame is not byte-identical across R/Python—use the CSV-path entry if you
#'     need that guarantee.
#' @param out Output stem (required when `data` is a data frame; ignored for a CSV path).
#' @param overwrite Overwrite an existing sidecar (or, for a df, an existing CSV). Default `FALSE`.
#' @param ... Passed to [document_bcsv()].
#' @return The YAML path (invisibly).
#' @export
draft_metadata <- function(data, out = NULL, overwrite = FALSE, ...) {
  document_args <- list(...)
  if (is.character(data)) {
    csv_path <- data
    df <- .align_integer_inference(readr::read_csv(csv_path, show_col_types = FALSE))
    stem <- .stem(csv_path)
    write_csv <- FALSE
    if (is.null(document_args$data_url)) document_args$data_url <- basename(csv_path)
  } else {
    df <- data
    if (is.null(out)) {
      stop('draft_metadata(): `out` (an output stem, e.g. "survey") is required when `data` is a data frame.',
           call. = FALSE)
    }
    stem <- .stem(out)
    write_csv <- TRUE
    if (is.null(document_args$data_url)) document_args$data_url <- paste0(basename(stem), ".csv")
  }
  yaml_path <- paste0(stem, ".yaml"); csv_target <- paste0(stem, ".csv")
  if (!isTRUE(overwrite)) {
    guard <- c(if (write_csv) csv_target, yaml_path, paste0(stem, ".json"))
    for (cand in guard) {
      if (file.exists(cand)) {
        stop(sprintf(paste0("A file already exists: %s. Pass overwrite = TRUE ",
                            "(or use edit_metadata() to resume editing existing metadata)."), cand),
             call. = FALSE)
      }
    }
  }
  if (is.null(document_args$description)) {
    document_args$description <- .table_placeholder(stem)
  }
  meta <- do.call(document_bcsv, c(list(df), document_args))
  meta$table_schema$columns <- lapply(meta$table_schema$columns, function(col) {
    if (is.null(col$description)) col$description <- .column_placeholder(col$name)
    col
  })
  if (write_csv) .write_csv_canonical(df, csv_target)
  .metadata_to_yaml(meta, yaml_path, legend = TRUE)
  invisible(yaml_path)
}

#' Standardize the CSV and write the canonical JSON sidecar
#'
#' Reads the CSV raw (value-preserving, honoring the declared dialect), rewrites it
#' canonically (comma, no row numbers, LF, UTF-8), hashes the standardized CSV, and
#' validates. On success it atomically replaces `<stem>.csv` and writes `<stem>.json`
#' (with a fresh `file_hash`), removing the YAML unless `keep_source`. On failure
#' nothing is changed. Standardizing a CSV file is byte-identical across R and Python.
#'
#' @param csv_path Path to the CSV.
#' @param keep_source Keep the YAML source after writing JSON. Default `FALSE`.
#' @return The [validate_bcsv()] result (invisibly).
#' @export
finalize_metadata <- function(csv_path, keep_source = FALSE) {
  stem <- .stem(csv_path)
  yaml_path <- paste0(stem, ".yaml"); json_path <- paste0(stem, ".json")
  if (file.exists(yaml_path) && file.exists(json_path)) {
    bcsv_validation_error(sprintf(
      "Both %s and %s exist; the metadata document is ambiguous. Keep only one.",
      yaml_path, json_path))
  }
  if (file.exists(yaml_path)) {
    source <- yaml_path; meta <- .metadata_from_yaml(yaml_path)
  } else if (file.exists(json_path)) {
    source <- json_path; meta <- .read_sidecar(json_path)
  } else {
    stop(sprintf("No metadata sidecar next to %s (looked for %s / %s).",
                 csv_path, yaml_path, json_path), call. = FALSE)
  }

  meta <- .strip_placeholders(meta, stem)
  delim <- meta$dialect$delimiter %||% ","
  enc <- meta$dialect$encoding %||% "UTF-8"
  raw <- .read_csv_raw(csv_path, delim, enc)
  meta$dialect <- NULL   # output is canonical comma/UTF-8—the bcsv defaults

  csv_tmp <- paste0(csv_path, ".tmp"); json_tmp <- paste0(json_path, ".tmp")
  # Never leave a stray temp behind—on the invalid return or on any error.
  on.exit(for (t in c(csv_tmp, json_tmp)) if (file.exists(t)) unlink(t), add = TRUE)

  .write_csv_canonical(raw, csv_tmp)
  meta$file_hash <- sha256_of_file(csv_tmp)
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE, null = "null"),
             json_tmp, useBytes = TRUE)

  res <- validate_bcsv(csv_tmp, json_tmp)
  if (!isTRUE(res$valid)) {
    message("Metadata is not valid\u2014nothing written:")
    print(tabularize(res))
    return(invisible(res))
  }
  file.rename(csv_tmp, csv_path); csv_tmp <- NULL
  file.rename(json_tmp, json_path); json_tmp <- NULL
  if (identical(source, yaml_path) && !isTRUE(keep_source)) unlink(yaml_path)
  invisible(res)
}

#' Convert the canonical JSON metadata back to an editable YAML
#'
#' @param csv_path Path to the CSV.
#' @param keep_source Keep the JSON after writing YAML. Default `FALSE`.
#' @return The YAML path (invisibly).
#' @export
edit_metadata <- function(csv_path, keep_source = FALSE) {
  stem <- .stem(csv_path)
  yaml_path <- paste0(stem, ".yaml"); json_path <- paste0(stem, ".json")
  if (!file.exists(json_path)) {
    stop(sprintf("No JSON metadata to edit next to %s (%s).", csv_path, json_path), call. = FALSE)
  }
  meta <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  .metadata_to_yaml(meta, yaml_path, legend = TRUE)
  if (!isTRUE(keep_source)) unlink(json_path)
  invisible(yaml_path)
}

#' Print a dataset's metadata as a table
#'
#' @param x A CSV/sidecar path or a metadata object.
#' @return The metadata object (invisibly).
#' @export
print_metadata <- function(x) {
  meta <- if (is.character(x)) .read_sidecar(x) else x
  print(tabularize(meta))
  invisible(meta)
}

#' Export a copy of the canonical metadata under a distinct name
#'
#' @param csv_path Path to the CSV.
#' @param output_format `"yaml"` or `"json"`.
#' @return The export path (invisibly). Never deletes the canonical sidecar.
#' @export
export_metadata <- function(csv_path, output_format = "yaml") {
  output_format <- match.arg(output_format, c("yaml", "json"))
  stem <- .stem(csv_path)
  meta <- .read_sidecar(csv_path)
  out <- paste0(stem, "_export.", output_format)
  if (output_format == "yaml") {
    .metadata_to_yaml(meta, out)
  } else {
    writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE, null = "null"),
               out, useBytes = TRUE)
  }
  invisible(out)
}
