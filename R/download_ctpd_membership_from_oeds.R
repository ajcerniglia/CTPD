#!/usr/bin/env Rscript

# Rebuild Ohio's current CTPD-to-member-district table from public OEDS data.
#
# Source endpoints used by the public OEDS website:
#   POST https://oeds.education.ohio.gov/Api/searchOrg
#   GET  https://oeds.education.ohio.gov/Api/OrgRelationships/GetOrgRelationships/{OrgKey}/0
#
# IRNs are identifiers, not numbers. They are normalized to six-character text.

suppressPackageStartupMessages({
  library(dplyr)
  library(httr2)
  library(jsonlite)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[[1]], mustWork = FALSE) else getwd()
data_dir <- file.path(project_dir, "data_raw")
output_dir <- file.path(project_dir, "output")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

normalize_irn <- function(x) {
  x <- str_replace_all(as.character(x), "[^0-9]", "")
  if_else(is.na(x) | x == "", NA_character_, str_pad(x, width = 6, side = "left", pad = "0"))
}

as_text_or_na <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]]) || identical(x[[1]], "")) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

is_true <- function(x) {
  isTRUE(x) || identical(tolower(as.character(x)), "true") || identical(as.character(x), "1")
}

search_payload <- list(
  SearchField = "",
  AddressText = "",
  CityText = "",
  ZipText = "",
  FedTaxId = "",
  DeptAccntsRvwedFlg = FALSE,
  SelectedOrgApprovalStatusKey = 0,
  selectedOrgTypeKeyForDdl = 3,
  selectedStateForAddress = 0,
  selectedCountyForAddress = 0,
  Counties = list(),
  OrgTypes = list(),
  SchoolTypes = list(),
  OrgStatuses = list(1),
  Cities = list(),
  IncludeOrgsWithNoIrn = FALSE
)

message("Downloading the current open CTPD list from OEDS...")
search_resp <- request("https://oeds.education.ohio.gov/Api/searchOrg") |>
  req_method("POST") |>
  req_headers(
    Accept = "application/json, text/plain, */*",
    `Content-Type` = "application/json;charset=UTF-8",
    `User-Agent` = "Ohio-CTPD-public-data-rebuild/1.0"
  ) |>
  req_body_json(search_payload, auto_unbox = TRUE) |>
  req_retry(max_tries = 4, backoff = ~ min(2^.x, 10)) |>
  req_timeout(60) |>
  req_perform()

search_json <- resp_body_json(search_resp, simplifyVector = TRUE)
search_rows <- search_json$searchOrganizationResponse$SearchOrgRows
if (is.null(search_rows) || nrow(search_rows) == 0) {
  stop("OEDS returned no CTPDs; the public endpoint may have changed.")
}

ctpd_list <- as_tibble(search_rows) |>
  transmute(
    ctpd_org_key = as.integer(OrgKey),
    ctpd_irn = normalize_irn(Irn),
    ctpd_name = str_squish(Name),
    # The request itself is restricted to OEDS status key 1 (Open).
    ctpd_status = "Open"
  ) |>
  arrange(ctpd_irn)

if (nrow(ctpd_list) != 95L) {
  warning("Expected 95 open CTPDs from OEDS, but received ", nrow(ctpd_list), ".")
}

message("Downloading the current open traditional-district master list from OEDS...")
district_search_payload <- search_payload
district_search_payload$selectedOrgTypeKeyForDdl <- 1
county_list_resp <- request("https://oeds.education.ohio.gov/Api/searchOrg/ListCounties") |>
  req_method("POST") |>
  req_headers(
    Accept = "application/json, text/plain, */*",
    `Content-Type` = "application/json;charset=UTF-8",
    `User-Agent` = "Ohio-CTPD-public-data-rebuild/1.0"
  ) |>
  req_body_json(district_search_payload, auto_unbox = TRUE) |>
  req_retry(max_tries = 4, backoff = ~ min(2^.x, 10)) |>
  req_timeout(60) |>
  req_perform()

county_list_json <- resp_body_json(county_list_resp, simplifyVector = TRUE)
county_keys <- county_list_json$ListCountiesResponse$KeyValueList$Key
if (is.null(county_keys) || length(county_keys) == 0) {
  stop("OEDS returned no county filters for open traditional public districts.")
}

# The public OEDS organization search intentionally returns at most 100 rows.
# County batches keep every request below that cap and can be deduplicated by IRN.
district_search_requests <- map(county_keys, function(county_key) {
  county_payload <- district_search_payload
  county_payload$Counties <- list(county_key)
  request("https://oeds.education.ohio.gov/Api/searchOrg") |>
    req_method("POST") |>
    req_headers(
      Accept = "application/json, text/plain, */*",
      `Content-Type` = "application/json;charset=UTF-8",
      `User-Agent` = "Ohio-CTPD-public-data-rebuild/1.0"
    ) |>
    req_body_json(county_payload, auto_unbox = TRUE) |>
    req_retry(max_tries = 4, backoff = ~ min(2^.x, 10)) |>
    req_timeout(60)
})

district_search_responses <- req_perform_parallel(
  district_search_requests,
  on_error = "return",
  progress = interactive(),
  max_active = 6
)

failed_district_batches <- which(map_lgl(district_search_responses, inherits, what = "error"))
if (length(failed_district_batches) > 0) {
  stop(
    "Failed to download district batches for OEDS county key(s): ",
    paste(county_keys[failed_district_batches], collapse = ", ")
  )
}

district_search_json <- map(
  district_search_responses,
  resp_body_json,
  simplifyVector = TRUE
)

district_batch_totals <- map_int(
  district_search_json,
  ~ .x$searchOrganizationResponse$TotalOrgCount %||% 0L
)
if (any(district_batch_totals > 100L)) {
  stop("At least one county batch exceeds OEDS's 100-row public-search cap.")
}

district_search_rows <- map_dfr(
  district_search_json,
  ~ as_tibble(.x$searchOrganizationResponse$SearchOrgRows)
)
if (nrow(district_search_rows) == 0) {
  stop("OEDS returned no open traditional public districts; the public endpoint may have changed.")
}

current_districts <- as_tibble(district_search_rows) |>
  transmute(
    district_org_key = as.integer(OrgKey),
    district_irn = normalize_irn(Irn),
    district_name = str_squish(Name),
    district_status = "Open"
  ) |>
  distinct(district_irn, .keep_all = TRUE) |>
  arrange(district_irn)

if (nrow(current_districts) != 611L) {
  warning("Expected 611 open traditional public districts from OEDS, but received ", nrow(current_districts), ".")
}

message("Downloading relationship records for ", nrow(ctpd_list), " CTPDs...")
relationship_urls <- paste0(
  "https://oeds.education.ohio.gov/Api/OrgRelationships/GetOrgRelationships/",
  ctpd_list$ctpd_org_key,
  "/0"
)

relationship_requests <- map(relationship_urls, function(url) {
  request(url) |>
    req_headers(
      Accept = "application/json, text/plain, */*",
      `User-Agent` = "Ohio-CTPD-public-data-rebuild/1.0"
    ) |>
    req_retry(max_tries = 4, backoff = ~ min(2^.x, 10)) |>
    req_timeout(60)
})

relationship_responses <- req_perform_parallel(
  relationship_requests,
  on_error = "return",
  progress = interactive(),
  max_active = 4
)

failed <- which(map_lgl(relationship_responses, inherits, what = "error"))
if (length(failed) > 0) {
  stop(
    "Failed to download OEDS relationships for CTPD IRN(s): ",
    paste(ctpd_list$ctpd_irn[failed], collapse = ", ")
  )
}

parse_ctpd_relationships <- function(resp, ctpd_row, source_url) {
  body <- resp_body_json(resp, simplifyVector = FALSE)
  rel_root <- body$orgRelationships
  groups <- rel_root$ChildRelationshipList

  coordinating_irn <- normalize_irn(as_text_or_na(rel_root$CtpDistCoordOrgIrn))
  coordinating_name <- as_text_or_na(rel_root$CtpDistCoordOrgName)

  if (is.null(groups) || length(groups) == 0) {
    return(tibble())
  }

  ctpd_groups <- keep(groups, function(group) {
    identical(as_text_or_na(group$Key), "Career Technical Planning District")
  })

  if (length(ctpd_groups) == 0) {
    return(tibble())
  }

  records <- flatten(map(ctpd_groups, "Value"))
  if (length(records) == 0) {
    return(tibble())
  }

  map_dfr(records, function(record) {
    tibble(
      ctpd_irn = ctpd_row$ctpd_irn,
      ctpd_name = ctpd_row$ctpd_name,
      ctpd_org_key = ctpd_row$ctpd_org_key,
      coordinating_irn = coordinating_irn,
      coordinating_name = coordinating_name,
      member_district_irn = normalize_irn(as_text_or_na(record$ChildIrn)),
      member_district_name = as_text_or_na(record$ChildOrgName),
      member_org_type = as_text_or_na(record$ChildOrgTypeDesc),
      relationship_type = as_text_or_na(record$RelationshipType),
      relationship_begin_date = as_text_or_na(record$BeginDate),
      relationship_end_date = as_text_or_na(record$EndDate),
      relationship_active = is_true(record$ActiveFlg),
      source_url = source_url
    )
  })
}

membership_all <- map_dfr(seq_len(nrow(ctpd_list)), function(i) {
  parse_ctpd_relationships(
    relationship_responses[[i]],
    ctpd_list[i, ],
    relationship_urls[[i]]
  )
})

membership <- membership_all |>
  filter(
    relationship_active,
    relationship_type == "Career Technical Planning District",
    member_org_type == "Traditional Public District",
    !is.na(member_district_irn)
  ) |>
  inner_join(
    current_districts |>
      transmute(
        member_district_irn = district_irn,
        current_district_name = district_name,
        member_district_org_key = district_org_key
      ),
    by = "member_district_irn"
  ) |>
  mutate(member_district_name = current_district_name) |>
  select(-current_district_name) |>
  distinct(ctpd_irn, member_district_irn, .keep_all = TRUE) |>
  arrange(ctpd_irn, member_district_irn)

if (any(nchar(membership$ctpd_irn) != 6L) || any(nchar(membership$member_district_irn) != 6L)) {
  stop("One or more IRNs were not normalized to six characters.")
}

if (anyDuplicated(membership[c("ctpd_irn", "member_district_irn")])) {
  stop("Duplicate active CTPD/member-district pairs remain after extraction.")
}

extracted_at <- format(Sys.time(), tz = "America/New_York", usetz = TRUE)
membership <- membership |> mutate(extracted_at = extracted_at, .after = source_url)

compatibility <- membership |>
  transmute(
    ctpd_irn,
    ctpd_name,
    district_irn = member_district_irn,
    district_name = member_district_name
  )

ctpd_summary <- ctpd_list |>
  left_join(
    membership |>
      count(ctpd_irn, name = "active_traditional_districts"),
    by = "ctpd_irn"
  ) |>
  mutate(active_traditional_districts = coalesce(active_traditional_districts, 0L)) |>
  arrange(ctpd_irn)

write_csv(
  compatibility,
  file.path(data_dir, "oeds_ctpd_member_districts.csv"),
  na = ""
)
write_csv(
  membership,
  file.path(output_dir, "oeds_ctpd_member_districts_with_provenance.csv"),
  na = ""
)
write_csv(
  ctpd_summary,
  file.path(output_dir, "oeds_current_ctpd_list.csv"),
  na = ""
)
write_csv(
  current_districts,
  file.path(output_dir, "oeds_current_traditional_district_list.csv"),
  na = ""
)

cat("\nOEDS public-data rebuild complete\n")
cat("Open CTPDs:", nrow(ctpd_list), "\n")
cat("Open traditional public districts:", nrow(current_districts), "\n")
cat("CTPDs with active traditional-district members:", n_distinct(membership$ctpd_irn), "\n")
cat("Unique traditional districts:", n_distinct(membership$member_district_irn), "\n")
cat("Active CTPD/member-district relationships:", nrow(membership), "\n")
cat("Districts in more than one CTPD:", sum(count(membership, member_district_irn)$n > 1), "\n")
cat("Outputs written under:", project_dir, "\n")
