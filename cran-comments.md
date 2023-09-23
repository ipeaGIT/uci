## R CMD check results

── R CMD check results ──────────────────────────────────────────────────────────────────────────── uci 0.3.0 ────
Duration: 1m 3.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


* Major changes
  * `uci()` now accepts columns with `NA` values. In such cases, `NA`s are replaced with zero as they represent zero activities.
  * improved documentation
  * The package now uses Queen contiguity (`spdep::poly2nb(geo, queen=TRUE)`) to determine neighboring polygons whe considering spatial link distances.
  * Update vignette