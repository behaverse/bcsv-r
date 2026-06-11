# bcsv (R)

R package to read, write, document, and validate CSV files paired with [**bcsv**](https://behaverse.org/schemas/bcsv/) (Better CSV) metadata.

bcsv is a JSON-LD extension of W3C CSVW that adds first-class categorical/ordered factors, units, multiple NA codes, file-integrity hashes (SHA-256), and a few documentation properties. Pure-CSVW readers ignore the bcsv additions; bcsv-aware readers get typed factors and dates out of the box.

> **Status:** alpha. Public API is complete and locked against schema **v26.0610**, mirroring the Python `bcsv` package (see [behaverse/bcsv-py](https://github.com/behaverse/bcsv-py)); 349 testthat expectations passing. Not yet published to CRAN—install from source in the meantime.

## Install

Once published:

```r
# install.packages("remotes")
remotes::install_github("behaverse/bcsv-r")
```

From source (current state):

```r
# install.packages("devtools")
devtools::install_local("path/to/bcsv-r")
```

## Usage

```r
library(bcsv)

# Read a CSV with its paired metadata
df <- read_bcsv("trial_data.csv")  # metadata defaults to trial_data.json

# Generate metadata from an in-memory tibble
meta <- document_bcsv(df, pretty_name = "Stroop task data")

# Write CSV + metadata as an atomic pair (with SHA-256 hash)
write_bcsv(df, "trial_data.csv", meta)

# Validate an existing pair
result <- validate_bcsv("trial_data.csv")
stopifnot(result$valid)

# Inspect a result or metadata object as a tidy table
tabularize(result)
```

Or author metadata by hand via an editable YAML (one metadata document at a time):

```r
draft_metadata("trial_data.csv")      # writes trial_data.yaml (a prefilled draft to edit)
# ...edit trial_data.yaml: add descriptions, units, ordered levels...
finalize_metadata("trial_data.csv")   # validates -> writes the canonical trial_data.json, removes the YAML
```

The full public API surface (the four core functions plus `tabularize` and the
`*_metadata` authoring family) mirrors the Python package and is documented in the
[API spec](https://github.com/behaverse/bcsv-r/blob/main/docs/04-api-spec.md).

## License

MIT. See [LICENSE](LICENSE).

The bcsv schema itself is published separately under CC-BY-4.0 at <https://github.com/behaverse/schemas>.
