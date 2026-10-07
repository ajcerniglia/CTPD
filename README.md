# Ohio ZIP-to-CTPD crosswalk

This project builds a statewide, area-weighted crosswalk between Ohio Census ZIP Code Tabulation Areas (ZCTAs) and current Ohio Career-Technical Planning Districts (CTPDs).

## Run

From the project root:

```r
source("R/build_ctpd_crosswalk.R")
```

Or from a shell:

```sh
Rscript R/build_ctpd_crosswalk.R .
```

The script downloads Census inputs only when the saved `data_raw/census/*.rds` snapshots are absent.

To refresh the statewide CTPD membership table directly from the public OEDS
website before rebuilding:

```sh
Rscript R/download_ctpd_membership_from_oeds.R .
```

The public organization search is capped at 100 results. The extractor obtains
the full traditional-district list in county batches, normalizes all IRNs as
six-character text, retrieves each CTPD's relationship records, and retains
only current active relationships to open traditional public districts.

## Build the static website

After building the crosswalk, regenerate the GitHub Pages site with:

```sh
Rscript R/build_ctpd_website.R .
```

This reads the GeoPackage and many-to-many crosswalk in `output/`, simplifies the map geometry for the browser, embeds the statewide lookup data in `docs/index.html`, and copies the downloadable crosswalk to `docs/data/`.

The generated `docs/` directory is a complete static site. It does not require Shiny, a database, or a running R process. To publish it with GitHub Pages, configure Pages to deploy the `docs` folder from the repository's main branch.

Website source is kept in `web/index-template.html`. Update that template for design or wording changes, then rerun the website build script. Changes to district membership or boundaries should begin with `R/build_ctpd_crosswalk.R`, followed by `R/build_ctpd_website.R`.

## Inputs and vintages

- `data_raw/oeds_ctpd_member_districts.csv`: current direct CTPD-to-member-district relationships regenerated from the public OEDS organization and relationship APIs.
- `data_raw/oeds_district_ctpd_2026-10-06.csv`: Ohio Educational Directory System public extract generated October 6, 2026. It includes open Traditional Public District and Career Technical Planning District records.
- `data_raw/census/oh_unified_school_districts_2025.rds`: 2025 Census cartographic unified school districts for Ohio.
- `data_raw/census/oh_counties_2025.rds`: 2025 Census cartographic counties for Ohio.
- `data_raw/census/oh_state_2020.rds`: 2020 Ohio cartographic state boundary used to clip the same-vintage ZCTAs without cross-vintage border slivers.
- `data_raw/census/zcta_prefix4_2020_cb.rds`: 2020 Census cartographic ZCTAs beginning with 4, clipped to Ohio during the build. Census does not publish a newer annual cartographic-boundary ZCTA file.

Authoritative validation sources:

- Ohio OEDS Public Extract: <https://oeds.education.ohio.gov/dataextract>
- Ohio Department of Education and Workforce CTPD information: <https://education.ohio.gov/Topics/Career-Tech/Planning-Funding-and-Accountability>
- Census TIGER/Line and cartographic boundaries: <https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html>

## Method

1. Normalize every IRN to a six-character string.
2. Use current active OEDS relationship records to map every open traditional public district directly to its current CTPD.
3. Validate current CTPD and district names and IRNs against the dated OEDS extract. No fuzzy matching, hierarchy traversal, or hand-coded membership overrides are required.
4. Match all current traditional districts to Census school-district geometry by normalized name plus an overlapping designated county. Five reviewed aliases handle known naming differences.
5. Dissolve member-district geometry by current CTPD.
6. Clip ZCTAs to Ohio, intersect them with CTPD polygons in EPSG:5070, and calculate overlap area, share of the Ohio portion of each ZCTA, and share of each CTPD.
7. Assign each ZCTA to the CTPD with the greatest overlap area. Ties are deterministic by six-digit CTPD IRN.

No positive-area intersection is dropped from the many-to-many table.

## Main outputs

- `output/zcta_ctpd_crosswalk_many_to_many.csv`: every positive-area ZCTA–CTPD intersection.
- `output/zcta_ctpd_primary.csv`: one primary CTPD per Ohio-intersecting ZCTA, or an explicit unassigned status.
- `output/ctpd_member_districts.csv`: fully resolved current CTPD-to-member mapping.
- `output/ctpd_validation.csv`: current-list match method, seed, member count, and geometry status.
- `output/ohio_ctpd_spatial.gpkg`: CTPD boundaries, member districts, primary ZCTAs, full intersection geometry, and district validation layers.
- `output/validation_map.png`: statewide district-membership coverage and multiplicity map with dissolved CTPD outlines.
- `output/validation_report.md`, `output/validation_checks.csv`, and the other diagnostic CSVs: audit trail and review flags.

## Validation notes

- OEDS CTPDs `200600` and `200602` are correctional/non-geographic and have no traditional school-district boundary.
- The public OEDS relationships assign all 611 open traditional districts exactly once across 93 geographic CTPDs.
- `output/oeds_vs_legacy_membership_changes.csv` records the nine pair-level differences between the earlier derived hierarchy and the current direct OEDS relationships.

Refresh the OEDS extract and rerun the public relationship extractor together when authoritative membership data for new or reorganized CTPDs become available.
