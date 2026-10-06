#!/usr/bin/env Rscript

# Regenerate the static Ohio ZIP-to-CTPD proof-of-concept website.
#
# Required inputs, produced by R/build_ctpd_crosswalk.R:
#   output/ohio_ctpd_spatial.gpkg
#   output/zcta_ctpd_crosswalk_many_to_many.csv
#
# Generated site:
#   docs/index.html
#   docs/data/zcta_ctpd_crosswalk_many_to_many.csv

suppressPackageStartupMessages({
  library(jsonlite)
  library(sf)
})

args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[[1]], mustWork = TRUE) else getwd()

gpkg_path <- file.path(project_dir, "output", "ohio_ctpd_spatial.gpkg")
crosswalk_path <- file.path(project_dir, "output", "zcta_ctpd_crosswalk_many_to_many.csv")
template_path <- file.path(project_dir, "web", "index-template.html")
docs_dir <- file.path(project_dir, "docs")
docs_data_dir <- file.path(docs_dir, "data")
index_path <- file.path(docs_dir, "index.html")

required_files <- c(gpkg_path, crosswalk_path, template_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required file(s): ", paste(missing_files, collapse = ", "))
}

dir.create(docs_data_dir, recursive = TRUE, showWarnings = FALSE)

compact_geojson <- function(x, precision = 5) {
  temp_geojson <- tempfile(fileext = ".geojson")
  on.exit(unlink(temp_geojson), add = TRUE)
  st_write(
    x,
    temp_geojson,
    delete_dsn = TRUE,
    quiet = TRUE,
    layer_options = paste0("COORDINATE_PRECISION=", precision)
  )
  object <- fromJSON(temp_geojson, simplifyVector = FALSE)
  object$name <- NULL
  object$crs <- NULL
  toJSON(object, auto_unbox = TRUE, digits = precision, na = "null")
}

ctpd <- st_read(gpkg_path, layer = "ctpd_boundaries", quiet = TRUE)
ctpd <- ctpd[order(ctpd$ctpd_name), ]
ctpd$i <- seq_len(nrow(ctpd)) - 1L
ctpd$n <- ctpd$ctpd_name
ctpd$r <- ctpd$ctpd_irn
ctpd <- ctpd[, c("i", "n", "r")]
ctpd <- st_transform(st_simplify(ctpd, dTolerance = 900, preserveTopology = TRUE), 4326)

zcta <- st_read(gpkg_path, layer = "zcta_primary", quiet = TRUE)
zcta$z <- zcta$zcta
zcta <- zcta[, "z"]
zcta <- st_transform(st_simplify(zcta, dTolerance = 1100, preserveTopology = TRUE), 4326)

crosswalk <- read.csv(
  crosswalk_path,
  colClasses = c(zcta = "character", ctpd_irn = "character")
)
ctpd_index <- setNames(ctpd$i, ctpd$r)
crosswalk$i <- unname(ctpd_index[crosswalk$ctpd_irn])
stopifnot(!anyNA(crosswalk$i))
crosswalk <- crosswalk[order(crosswalk$zcta, crosswalk$overlap_rank), ]

crosswalk_data <- lapply(seq_len(nrow(crosswalk)), function(row) {
  list(
    crosswalk$zcta[row],
    crosswalk$i[row],
    round(100 * crosswalk$pct_of_zcta_ohio[row], 1)
  )
})

template <- readChar(template_path, file.info(template_path)$size, useBytes = TRUE)
html <- sub("__CTPD_DATA__", compact_geojson(ctpd), template, fixed = TRUE)
html <- sub("__ZCTA_DATA__", compact_geojson(zcta), html, fixed = TRUE)
html <- sub(
  "__CROSSWALK_DATA__",
  toJSON(crosswalk_data, auto_unbox = TRUE, digits = 4),
  html,
  fixed = TRUE
)

if (grepl("__CTPD_DATA__|__ZCTA_DATA__|__CROSSWALK_DATA__", html)) {
  stop("One or more data placeholders were not replaced.")
}

writeLines(html, index_path, useBytes = TRUE)
file.copy(
  crosswalk_path,
  file.path(docs_data_dir, "zcta_ctpd_crosswalk_many_to_many.csv"),
  overwrite = TRUE
)
writeLines(character(), file.path(docs_dir, ".nojekyll"))

message("Static website written to: ", docs_dir)
message("Crosswalk records included: ", nrow(crosswalk))
message("Ohio ZCTAs included: ", length(unique(crosswalk$zcta)))
message("Geographic CTPDs included: ", nrow(ctpd))
