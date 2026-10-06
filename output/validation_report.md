# Ohio ZIP-to-CTPD validation report

Generated: 2026-10-06 11:31:21 EDT

## Summary

- Current open OEDS CTPDs: 95
- Resolved geographic CTPDs: 93
- Unresolved geographic CTPDs: 0
- Non-geographic correctional CTPDs: 2
- Traditional districts matched to Census: 611
- Ohio-intersecting ZCTAs: 1233
- Many-to-many crosswalk rows: 2270
- ZCTAs without a primary CTPD: 0
- ZCTAs below 99% total resolved coverage: 3
- ZCTAs above 101% total overlap (hierarchical/overlapping CTPDs): 14

## Automated checks

- All automated validation checks passed.

## Important interpretation notes

- IRNs are stored as six-character strings with leading zeros.
- CTPD membership is resolved by directed graph reachability, so lead-district chains and cycles are retained and diagnosed.
- Area shares use Ohio-clipped ZCTA area in NAD83 / Conus Albers (EPSG:5070).
- The latest Census cartographic-boundary ZCTA vintage is 2020; school-district and county geometry is 2025.
- OEDS records 200600 and 200602 are correctional/non-geographic and are not assigned traditional-district boundaries.
- See ctpd_validation.csv for every current CTPD mapping decision and validation_checks.csv for items requiring review.
