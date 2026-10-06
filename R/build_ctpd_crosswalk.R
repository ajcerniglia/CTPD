#!/usr/bin/env Rscript

# Build a statewide Ohio ZIP Code Tabulation Area (ZCTA)-to-CTPD crosswalk.
#
# Inputs:
#   data_raw/ohio_ctpd_updated.csv
#   data_raw/oeds_district_ctpd_2026-10-06.csv
#   data_raw/census/*.rds (downloaded automatically if absent)
#
# Outputs are written to output/. Areas are computed in EPSG:5070.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(igraph)
  library(readr)
  library(sf)
  library(stringdist)
  library(stringi)
  library(stringr)
  library(tidyr)
  library(tigris)
})

options(
  timeout = 600,
  tigris_use_cache = TRUE,
  scipen = 999
)

sf::sf_use_s2(FALSE)

args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[[1]], mustWork = TRUE) else getwd()
data_dir <- file.path(project_dir, "data_raw")
census_dir <- file.path(data_dir, "census")
output_dir <- file.path(project_dir, "output")
dir.create(census_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TIGRIS_CACHE_DIR = file.path(census_dir, "tigris_cache"))

relationship_path <- file.path(data_dir, "ohio_ctpd_updated.csv")
oeds_path <- file.path(data_dir, "oeds_district_ctpd_2026-10-06.csv")
school_path <- file.path(census_dir, "oh_unified_school_districts_2025.rds")
county_path <- file.path(census_dir, "oh_counties_2025.rds")
state_path <- file.path(census_dir, "oh_state_2020.rds")
zcta_path <- file.path(census_dir, "zcta_prefix4_2020_cb.rds")

stopifnot(file.exists(relationship_path), file.exists(oeds_path))

download_if_missing <- function(path, loader) {
  if (!file.exists(path)) {
    message("Downloading Census geography: ", basename(path))
    saveRDS(loader(), path)
  }
  readRDS(path)
}

school_raw <- download_if_missing(
  school_path,
  function() tigris::school_districts(
    state = "OH", type = "unified", year = 2025,
    cb = TRUE, progress_bar = FALSE
  )
)

county_raw <- download_if_missing(
  county_path,
  function() tigris::counties(
    state = "OH", year = 2025, cb = TRUE, progress_bar = FALSE
  )
)

state_raw <- download_if_missing(
  state_path,
  function() {
    states <- tigris::states(year = 2020, cb = TRUE, progress_bar = FALSE)
    states[states$STUSPS == "OH", ]
  }
)

# The Census Bureau does not publish an annual cartographic-boundary ZCTA file.
# 2020 is the latest cartographic ZCTA vintage. Prefix 4 contains Ohio and its
# neighboring ZIP areas; the script clips it to the Ohio boundary below.
zcta_raw <- download_if_missing(
  zcta_path,
  function() tigris::zctas(
    year = 2020, cb = TRUE, starts_with = "4", progress_bar = FALSE
  )
)

normalize_irn <- function(x) {
  out <- str_extract(as.character(x), "[0-9]+")
  out <- if_else(is.na(out), NA_character_, str_pad(out, width = 6, side = "left", pad = "0"))
  if (any(!is.na(out) & !str_detect(out, "^[0-9]{6}$"))) {
    stop("All non-missing IRNs must normalize to six digits.")
  }
  out
}

snake_names <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

name_key <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("\\bmt\\b", "mount") |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_replace(" (public )?school(s)? district$", "") |>
    str_replace(" schools$", "") |>
    str_replace(" sd$", "") |>
    str_squish()
}

ctpd_name_key <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("career technical|technical and career|career technology", "") |>
    str_replace_all("planning district|ctpd|jvsd|school district|schools|school|sd|area", "") |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}

relationships <- read_csv(
  relationship_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  transmute(
    source_ctpd_irn = normalize_irn(ctpd_irn),
    source_ctpd_name = str_squish(ctpd_name),
    child_irn = normalize_irn(district_irn)
  ) |>
  filter(!is.na(source_ctpd_irn), !is.na(child_irn)) |>
  distinct()

if (nrow(relationships) == 0) stop("The CTPD relationship input has no usable rows.")

oeds <- read_csv(
  oeds_path,
  skip = 1,
  col_types = cols(.default = col_character()),
  name_repair = "minimal",
  show_col_types = FALSE
) |>
  rename_with(snake_names) |>
  mutate(irn = normalize_irn(irn)) |>
  filter(!is.na(irn)) |>
  distinct(irn, organization_type, .keep_all = TRUE)

districts <- oeds |>
  filter(organization_type == "Traditional Public District", status == "Open") |>
  transmute(
    district_irn = irn,
    district_name = organization_name,
    designated_county,
    district_key = name_key(organization_name),
    county_key = name_key(designated_county)
  ) |>
  distinct(district_irn, .keep_all = TRUE)

official_ctpds <- oeds |>
  filter(organization_type == "Career Technical Planning District", status == "Open") |>
  transmute(
    ctpd_irn = irn,
    ctpd_name = organization_name,
    designated_county,
    official_name_key = ctpd_name_key(organization_name)
  ) |>
  distinct(ctpd_irn, .keep_all = TRUE)

# ----- Match OEDS district IRNs to Census school-district geometry ----------

school <- school_raw |>
  st_make_valid() |>
  st_transform(5070) |>
  filter(GEOID != "3999997") |>
  transmute(
    census_school_geoid = GEOID,
    census_school_name = NAME,
    census_name_key = name_key(NAME),
    geometry
  )

county <- county_raw |>
  st_make_valid() |>
  st_transform(5070) |>
  transmute(census_county_name = NAME, county_key = name_key(NAME), geometry)

# Use every county with material district overlap, rather than only the county
# containing the centroid. This correctly handles cross-county districts.
school_counties <- suppressWarnings(st_intersection(
  school |> select(census_school_geoid, census_school_name, census_name_key),
  county |> select(census_county_name, county_key)
)) |>
  mutate(overlap_sqm = as.numeric(st_area(geometry))) |>
  st_drop_geometry() |>
  filter(overlap_sqm > 10000) |>
  distinct(census_school_geoid, county_key, .keep_all = TRUE)

district_candidates <- districts |>
  inner_join(
    school_counties,
    by = c("district_key" = "census_name_key", "county_key")
  ) |>
  distinct(district_irn, census_school_geoid, .keep_all = TRUE)

manual_district_aliases <- tribble(
  ~district_irn, ~census_school_geoid, ~alias_note,
  "043752", "3904375", "Cincinnati Public Schools -> Cincinnati City School District",
  "044073", "3904407", "Grandview Heights Schools -> Grandview Heights City School District",
  "044479", "3904447", "New Lexington School District -> New Lexington City School District",
  "044875", "3904487", "Sylvania Schools -> Sylvania City School District",
  "050583", "3905058", "Waynedale Local -> Southeast Local School District (2025 Census name)"
)

district_match_exact <- district_candidates |>
  anti_join(manual_district_aliases, by = "district_irn") |>
  transmute(
    district_irn,
    census_school_geoid,
    geometry_match_method = "normalized_name_plus_overlapping_county",
    match_note = NA_character_
  )

district_match_manual <- manual_district_aliases |>
  transmute(
    district_irn,
    census_school_geoid,
    geometry_match_method = "manual_alias",
    match_note = alias_note
  )

district_match <- bind_rows(district_match_exact, district_match_manual) |>
  distinct(district_irn, .keep_all = TRUE) |>
  left_join(districts, by = "district_irn") |>
  left_join(
    school |> st_drop_geometry() |> select(census_school_geoid, census_school_name),
    by = "census_school_geoid"
  )

duplicate_match_irn <- district_match |> count(district_irn) |> filter(n != 1)
duplicate_match_geoid <- district_match |> count(census_school_geoid) |> filter(n != 1)
if (nrow(duplicate_match_irn) > 0 || nrow(duplicate_match_geoid) > 0) {
  stop("District-to-Census matching is not one-to-one; inspect the match logic.")
}

district_match_diagnostics <- full_join(
  districts |> select(district_irn, district_name, designated_county),
  district_match |>
    select(district_irn, census_school_geoid, census_school_name, geometry_match_method, match_note),
  by = "district_irn"
) |>
  mutate(match_status = if_else(is.na(census_school_geoid), "unmatched", "matched"))

unmatched_census <- anti_join(
  school |> st_drop_geometry() |> select(census_school_geoid, census_school_name),
  district_match,
  by = "census_school_geoid"
)

# ----- Map the current OEDS CTPD list to the supplied relationship graph -----

source_groups <- relationships |>
  distinct(source_ctpd_irn, source_ctpd_name) |>
  mutate(source_name_key = ctpd_name_key(source_ctpd_name))

# These are intentionally few and auditable. Self-member fallbacks are used
# only for comprehensive districts whose current OEDS name identifies a single
# traditional district. Western Lake is mapped to the supplied Lake Shore
# Compact hierarchy, its reviewed predecessor/successor compact relationship.
ctpd_overrides <- tribble(
  ~ctpd_irn, ~override_kind, ~override_irn, ~override_note,
  "200003", "source_seed", "062042", "Heartland is the renamed Ashland Co/West Holmes CTPD in the supplied relationships",
  "200079", "self_member", "064964", "College Corner comprehensive CTPD; current traditional district used as its sole member",
  "022546", "self_member", "043950", "Euclid comprehensive CTPD; current traditional district used as its sole member",
  "022548", "self_member", "045492", "Mentor comprehensive CTPD; current traditional district used as its sole member",
  "022550", "source_seed", "200053", "Western Lake County Compact is the reviewed successor to the supplied Lake Shore Compact hierarchy",
  "200600", "non_geographic", NA_character_, "Correctional-institution CTPD; no traditional school-district boundary",
  "200602", "non_geographic", NA_character_, "Correctional-institution CTPD; no traditional school-district boundary"
)

direct_map <- official_ctpds |>
  filter(ctpd_irn %in% relationships$source_ctpd_irn) |>
  transmute(
    ctpd_irn,
    source_seed_irn = ctpd_irn,
    self_member_irn = NA_character_,
    mapping_method = "official_irn_in_source_graph",
    mapping_note = NA_character_,
    name_distance = 0
  )

needs_name_match <- official_ctpds |>
  anti_join(direct_map, by = "ctpd_irn") |>
  anti_join(ctpd_overrides, by = "ctpd_irn")

name_candidates <- crossing(
  needs_name_match |> select(ctpd_irn, official_name_key),
  source_groups |> select(source_ctpd_irn, source_name_key)
) |>
  mutate(name_distance = stringdist(official_name_key, source_name_key, method = "jw", p = 0.1)) |>
  group_by(ctpd_irn) |>
  slice_min(name_distance, n = 1, with_ties = FALSE) |>
  ungroup()

name_map <- name_candidates |>
  filter(name_distance <= 0.18) |>
  transmute(
    ctpd_irn,
    source_seed_irn = source_ctpd_irn,
    self_member_irn = NA_character_,
    mapping_method = if_else(name_distance == 0, "normalized_name", "fuzzy_name_reviewed"),
    mapping_note = NA_character_,
    name_distance
  )

override_map <- ctpd_overrides |>
  transmute(
    ctpd_irn,
    source_seed_irn = if_else(override_kind == "source_seed", override_irn, NA_character_),
    self_member_irn = if_else(override_kind == "self_member", override_irn, NA_character_),
    mapping_method = override_kind,
    mapping_note = override_note,
    name_distance = NA_real_
  )

ctpd_validation <- official_ctpds |>
  left_join(
    bind_rows(direct_map, name_map, override_map) |> distinct(ctpd_irn, .keep_all = TRUE),
    by = "ctpd_irn"
  ) |>
  mutate(
    mapping_method = replace_na(mapping_method, "unresolved"),
    mapping_note = if_else(
      mapping_method == "unresolved" & is.na(mapping_note),
      "No source-graph match met the reviewed name-distance threshold",
      mapping_note
    )
  )

# Directed reachability resolves one-hop lead-district links, longer chains,
# and cycles. Any reachable node that is a current traditional district is a
# member; the seed itself is included when it is a traditional district IRN.
graph <- graph_from_data_frame(
  relationships |> select(source_ctpd_irn, child_irn),
  directed = TRUE
)

resolve_members <- function(source_seed_irn, self_member_irn) {
  if (!is.na(self_member_irn)) return(self_member_irn)
  if (is.na(source_seed_irn) || !(source_seed_irn %in% V(graph)$name)) return(character())
  reachable <- subcomponent(graph, source_seed_irn, mode = "out") |> names()
  intersect(reachable, districts$district_irn)
}

member_map <- ctpd_validation |>
  select(ctpd_irn, ctpd_name, mapping_method, source_seed_irn, self_member_irn) |>
  rowwise() |>
  mutate(member_district_irn = list(resolve_members(source_seed_irn, self_member_irn))) |>
  ungroup() |>
  unnest_longer(member_district_irn, values_to = "district_irn") |>
  left_join(districts |> select(district_irn, district_name, designated_county), by = "district_irn") |>
  distinct(ctpd_irn, district_irn, .keep_all = TRUE) |>
  arrange(ctpd_irn, district_irn)

member_counts <- member_map |>
  count(ctpd_irn, name = "resolved_member_count")

ctpd_validation <- ctpd_validation |>
  left_join(member_counts, by = "ctpd_irn") |>
  mutate(
    resolved_member_count = replace_na(resolved_member_count, 0L),
    geometry_status = case_when(
      mapping_method == "non_geographic" ~ "not_applicable",
      resolved_member_count == 0 ~ "unresolved",
      TRUE ~ "resolved"
    )
  ) |>
  arrange(ctpd_irn)

# Strongly connected components with more than one node are hierarchy cycles.
strong_components <- components(graph, mode = "strong")
hierarchy_cycles <- tibble(
  node_irn = names(strong_components$membership),
  component_id = unname(strong_components$membership)
) |>
  add_count(component_id, name = "component_size") |>
  filter(component_size > 1) |>
  left_join(
    source_groups |> transmute(node_irn = source_ctpd_irn, source_ctpd_name),
    by = "node_irn"
  ) |>
  arrange(component_id, node_irn)

used_source_seeds <- ctpd_validation$source_seed_irn |> na.omit() |> unique()
source_groups_not_current <- source_groups |>
  filter(!source_ctpd_irn %in% used_source_seeds) |>
  select(source_ctpd_irn, source_ctpd_name) |>
  arrange(source_ctpd_irn)

# ----- CTPD boundary construction -------------------------------------------

district_sf <- district_match |>
  inner_join(school |> select(census_school_geoid, geometry), by = "census_school_geoid") |>
  st_as_sf()

member_sf <- member_map |>
  left_join(
    district_sf |>
      select(district_irn, census_school_geoid, census_school_name, geometry_match_method, geometry),
    by = "district_irn"
  ) |>
  st_as_sf()

if (any(st_is_empty(member_sf)) || any(is.na(member_sf$census_school_geoid))) {
  stop("At least one resolved member district lacks Census geometry.")
}

ctpd_boundaries <- member_sf |>
  group_by(ctpd_irn, ctpd_name, mapping_method) |>
  summarise(
    member_count = n_distinct(district_irn),
    geometry = st_union(st_make_valid(geometry)),
    .groups = "drop"
  ) |>
  st_make_valid() |>
  mutate(area_sq_km = as.numeric(st_area(geometry)) / 1e6) |>
  arrange(ctpd_irn)

# ----- Ohio-clipped ZCTAs and many-to-many intersections -------------------

# Clip 2020 cartographic ZCTAs with the matching 2020 cartographic state
# boundary. Using the same vintage avoids false out-of-state border slivers.
ohio_boundary <- state_raw |>
  st_make_valid() |>
  st_transform(5070) |>
  summarise(geometry = st_union(geometry)) |>
  st_make_valid()

zcta_field <- intersect(c("ZCTA5CE20", "GEOID20", "ZCTA5CE10", "GEOID10"), names(zcta_raw))[[1]]
zcta <- zcta_raw |>
  st_make_valid() |>
  st_transform(5070) |>
  transmute(zcta = as.character(.data[[zcta_field]]), geometry)

zcta <- st_filter(zcta, ohio_boundary, .predicate = st_intersects)

zcta_ohio <- suppressWarnings(st_intersection(zcta, ohio_boundary)) |>
  group_by(zcta) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid() |>
  mutate(zcta_ohio_area_sq_km = as.numeric(st_area(geometry)) / 1e6) |>
  filter(zcta_ohio_area_sq_km > 0)

ctpd_for_intersection <- ctpd_boundaries |>
  select(ctpd_irn, ctpd_name, ctpd_area_sq_km = area_sq_km)

zcta_ctpd_sf <- suppressWarnings(st_intersection(
  zcta_ohio |> select(zcta, zcta_ohio_area_sq_km),
  ctpd_for_intersection
)) |>
  mutate(overlap_sq_km = as.numeric(st_area(geometry)) / 1e6) |>
  filter(overlap_sq_km > 0) |>
  mutate(
    pct_of_zcta_ohio = pmin(1, pmax(0, overlap_sq_km / zcta_ohio_area_sq_km)),
    pct_of_ctpd = pmin(1, pmax(0, overlap_sq_km / ctpd_area_sq_km))
  ) |>
  group_by(zcta) |>
  arrange(desc(overlap_sq_km), ctpd_irn, .by_group = TRUE) |>
  mutate(overlap_rank = row_number()) |>
  ungroup()

zcta_ctpd_crosswalk <- zcta_ctpd_sf |>
  st_drop_geometry() |>
  select(
    zcta, ctpd_irn, ctpd_name, overlap_rank,
    overlap_sq_km, zcta_ohio_area_sq_km, ctpd_area_sq_km,
    pct_of_zcta_ohio, pct_of_ctpd
  ) |>
  arrange(zcta, overlap_rank, ctpd_irn)

zcta_primary <- zcta_ohio |>
  st_drop_geometry() |>
  select(zcta, zcta_ohio_area_sq_km) |>
  left_join(
    zcta_ctpd_crosswalk |>
      filter(overlap_rank == 1) |>
      select(
        zcta, primary_ctpd_irn = ctpd_irn, primary_ctpd_name = ctpd_name,
        primary_overlap_sq_km = overlap_sq_km,
        primary_pct_of_zcta_ohio = pct_of_zcta_ohio
      ),
    by = "zcta"
  ) |>
  mutate(assignment_status = if_else(is.na(primary_ctpd_irn), "unassigned", "assigned")) |>
  arrange(zcta)

zcta_diagnostics <- zcta_ctpd_crosswalk |>
  group_by(zcta) |>
  summarise(
    ctpd_count = n_distinct(ctpd_irn),
    sum_pct_of_zcta_ohio = sum(pct_of_zcta_ohio),
    primary_pct = first(pct_of_zcta_ohio),
    second_pct = nth(pct_of_zcta_ohio, 2, default = 0),
    primary_margin = primary_pct - second_pct,
    .groups = "drop"
  ) |>
  right_join(zcta_primary |> select(zcta, assignment_status), by = "zcta") |>
  mutate(
    ctpd_count = replace_na(ctpd_count, 0L),
    sum_pct_of_zcta_ohio = replace_na(sum_pct_of_zcta_ohio, 0),
    primary_pct = replace_na(primary_pct, 0),
    second_pct = replace_na(second_pct, 0),
    primary_margin = replace_na(primary_margin, 0)
  ) |>
  arrange(zcta)

zcta_primary_sf <- zcta_ohio |>
  left_join(zcta_primary |> select(-zcta_ohio_area_sq_km), by = "zcta")

# ----- Diagnostics and validation map ---------------------------------------

district_membership_coverage <- districts |>
  left_join(member_map |> count(district_irn, name = "ctpd_count"), by = "district_irn") |>
  mutate(
    ctpd_count = replace_na(ctpd_count, 0L),
    coverage_class = case_when(
      ctpd_count == 0 ~ "Unassigned",
      ctpd_count == 1 ~ "One CTPD",
      TRUE ~ "Multiple CTPDs"
    )
  ) |>
  arrange(desc(ctpd_count), district_irn)

district_validation_sf <- district_sf |>
  select(district_irn, district_name, geometry) |>
  left_join(
    district_membership_coverage |> select(district_irn, ctpd_count, coverage_class),
    by = "district_irn"
  )

run_summary <- tibble(
  metric = c(
    "relationship_rows", "source_parent_irns", "open_oeds_ctpds",
    "resolved_geographic_ctpds", "unresolved_geographic_ctpds",
    "non_geographic_ctpds", "open_traditional_districts",
    "districts_matched_to_census", "districts_unassigned_to_ctpd",
    "districts_in_multiple_ctpds", "ohio_zctas",
    "many_to_many_crosswalk_rows", "zctas_without_ctpd",
    "zctas_below_99pct_total_coverage", "zctas_above_101pct_total_overlap",
    "hierarchy_cycle_nodes"
  ),
  value = c(
    nrow(relationships), n_distinct(relationships$source_ctpd_irn), nrow(official_ctpds),
    sum(ctpd_validation$geometry_status == "resolved"),
    sum(ctpd_validation$geometry_status == "unresolved"),
    sum(ctpd_validation$geometry_status == "not_applicable"),
    nrow(districts), nrow(district_match),
    sum(district_membership_coverage$ctpd_count == 0),
    sum(district_membership_coverage$ctpd_count > 1),
    nrow(zcta_ohio), nrow(zcta_ctpd_crosswalk),
    sum(zcta_primary$assignment_status == "unassigned"),
    sum(zcta_diagnostics$sum_pct_of_zcta_ohio < 0.99),
    sum(zcta_diagnostics$sum_pct_of_zcta_ohio > 1.01),
    nrow(hierarchy_cycles)
  )
)

checks <- tribble(
  ~check, ~passed, ~value, ~expectation,
  "All OEDS traditional districts match Census geometry", nrow(district_match) == nrow(districts), nrow(district_match), paste0("= ", nrow(districts)),
  "District-Census mapping is one-to-one", n_distinct(district_match$census_school_geoid) == nrow(district_match), n_distinct(district_match$census_school_geoid), paste0("= ", nrow(district_match)),
  "All resolved members have geometry", !any(is.na(member_sf$census_school_geoid)), sum(is.na(member_sf$census_school_geoid)), "= 0",
  "No unresolved geographic current CTPDs", sum(ctpd_validation$geometry_status == "unresolved") == 0, sum(ctpd_validation$geometry_status == "unresolved"), "= 0",
  "Every Ohio district is assigned to a current CTPD", sum(district_membership_coverage$ctpd_count == 0) == 0, sum(district_membership_coverage$ctpd_count == 0), "= 0",
  "Every Ohio-intersecting ZCTA receives a primary CTPD", sum(zcta_primary$assignment_status == "unassigned") == 0, sum(zcta_primary$assignment_status == "unassigned"), "= 0",
  "Every ZCTA has at least 90% resolved CTPD coverage", min(zcta_diagnostics$sum_pct_of_zcta_ohio) >= 0.90, min(zcta_diagnostics$sum_pct_of_zcta_ohio), ">= 0.90",
  "No duplicate ZCTA-CTPD pairs", nrow(zcta_ctpd_crosswalk) == nrow(distinct(zcta_ctpd_crosswalk, zcta, ctpd_irn)), nrow(zcta_ctpd_crosswalk) - nrow(distinct(zcta_ctpd_crosswalk, zcta, ctpd_irn)), "= 0"
) |>
  mutate(status = if_else(passed, "PASS", "REVIEW")) |>
  select(check, status, value, expectation)

validation_map_path <- file.path(output_dir, "validation_map.png")
validation_plot <- ggplot() +
  geom_sf(
    data = district_validation_sf,
    aes(fill = coverage_class),
    color = "white", linewidth = 0.08
  ) +
  geom_sf(
    data = ctpd_boundaries,
    fill = NA, color = "#164A7B", linewidth = 0.25, alpha = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "Unassigned" = "#D73027",
      "One CTPD" = "#E8E8E8",
      "Multiple CTPDs" = "#7B3294"
    ),
    drop = FALSE
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Ohio CTPD boundary validation",
    subtitle = paste0(
      "District fill shows resolved membership multiplicity; blue lines are dissolved CTPD boundaries. ",
      "Current OEDS snapshot: 2026-10-06."
    ),
    fill = "District membership",
    caption = "Sources: Ohio OEDS; supplied ohio_ctpd_updated.csv; U.S. Census Bureau 2025 school districts."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10, margin = margin(b = 8)),
    legend.position = "bottom",
    plot.caption = element_text(size = 8, color = "grey35")
  )

ggsave(validation_map_path, validation_plot, width = 10, height = 9, dpi = 300, bg = "white")

# ----- Write outputs ---------------------------------------------------------

write_csv(member_map, file.path(output_dir, "ctpd_member_districts.csv"), na = "")
write_csv(ctpd_validation, file.path(output_dir, "ctpd_validation.csv"), na = "")
write_csv(zcta_ctpd_crosswalk, file.path(output_dir, "zcta_ctpd_crosswalk_many_to_many.csv"), na = "")
write_csv(zcta_primary, file.path(output_dir, "zcta_ctpd_primary.csv"), na = "")
write_csv(zcta_diagnostics, file.path(output_dir, "zcta_assignment_diagnostics.csv"), na = "")
write_csv(district_match_diagnostics, file.path(output_dir, "district_geometry_match.csv"), na = "")
write_csv(unmatched_census, file.path(output_dir, "census_school_districts_unmatched.csv"), na = "")
write_csv(district_membership_coverage, file.path(output_dir, "district_membership_coverage.csv"), na = "")
write_csv(hierarchy_cycles, file.path(output_dir, "hierarchy_cycles.csv"), na = "")
write_csv(source_groups_not_current, file.path(output_dir, "source_groups_not_current.csv"), na = "")
write_csv(run_summary, file.path(output_dir, "run_summary.csv"), na = "")
write_csv(checks, file.path(output_dir, "validation_checks.csv"), na = "")

gpkg_path <- file.path(output_dir, "ohio_ctpd_spatial.gpkg")
if (file.exists(gpkg_path)) unlink(gpkg_path)
st_write(ctpd_boundaries, gpkg_path, layer = "ctpd_boundaries", quiet = TRUE)
st_write(member_sf, gpkg_path, layer = "ctpd_member_districts", append = TRUE, quiet = TRUE)
st_write(zcta_primary_sf, gpkg_path, layer = "zcta_primary", append = TRUE, quiet = TRUE)
st_write(zcta_ctpd_sf, gpkg_path, layer = "zcta_ctpd_intersections", append = TRUE, quiet = TRUE)
st_write(district_validation_sf, gpkg_path, layer = "district_validation", append = TRUE, quiet = TRUE)

summary_lookup <- setNames(run_summary$value, run_summary$metric)
review_checks <- checks |> filter(status == "REVIEW") |> pull(check)
review_text <- if (length(review_checks) == 0) {
  "- All automated validation checks passed."
} else {
  paste0("- REVIEW: ", review_checks, collapse = "\n")
}

report_lines <- c(
  "# Ohio ZIP-to-CTPD validation report",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Summary",
  "",
  paste0("- Current open OEDS CTPDs: ", summary_lookup[["open_oeds_ctpds"]]),
  paste0("- Resolved geographic CTPDs: ", summary_lookup[["resolved_geographic_ctpds"]]),
  paste0("- Unresolved geographic CTPDs: ", summary_lookup[["unresolved_geographic_ctpds"]]),
  paste0("- Non-geographic correctional CTPDs: ", summary_lookup[["non_geographic_ctpds"]]),
  paste0("- Traditional districts matched to Census: ", summary_lookup[["districts_matched_to_census"]]),
  paste0("- Ohio-intersecting ZCTAs: ", summary_lookup[["ohio_zctas"]]),
  paste0("- Many-to-many crosswalk rows: ", summary_lookup[["many_to_many_crosswalk_rows"]]),
  paste0("- ZCTAs without a primary CTPD: ", summary_lookup[["zctas_without_ctpd"]]),
  paste0("- ZCTAs below 99% total resolved coverage: ", summary_lookup[["zctas_below_99pct_total_coverage"]]),
  paste0("- ZCTAs above 101% total overlap (hierarchical/overlapping CTPDs): ", summary_lookup[["zctas_above_101pct_total_overlap"]]),
  "",
  "## Automated checks",
  "",
  review_text,
  "",
  "## Important interpretation notes",
  "",
  "- IRNs are stored as six-character strings with leading zeros.",
  "- CTPD membership is resolved by directed graph reachability, so lead-district chains and cycles are retained and diagnosed.",
  "- Area shares use Ohio-clipped ZCTA area in NAD83 / Conus Albers (EPSG:5070).",
  "- The latest Census cartographic-boundary ZCTA vintage is 2020; school-district and county geometry is 2025.",
  "- OEDS records 200600 and 200602 are correctional/non-geographic and are not assigned traditional-district boundaries.",
  "- See ctpd_validation.csv for every current CTPD mapping decision and validation_checks.csv for items requiring review."
)
writeLines(report_lines, file.path(output_dir, "validation_report.md"))

message("Build complete. Outputs written to: ", output_dir)
print(run_summary)
print(checks)
