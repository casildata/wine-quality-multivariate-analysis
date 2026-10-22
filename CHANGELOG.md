# Changelog: from the original project to this release

## 2026-10-22: review fixes

- Outliers: the report no longer claims every later step is re-run without flagged wines (only the PCA is), and a new check repeats both rules with the five skewed variables on a log scale (136 classical vs. 524 robust flags; masking holds, but heavy tails inflate both counts).
- Baselines: the best single measurement for red vs. white (total sulfur dioxide, AUC 0.953) and an alcohol-only row for good white wine (AUC 0.752 vs. 0.784 for the full LDA).
- Effect size for the white-wine clusters (η² = 0.067) and a note that the two-factor model is weak (29.0% of variance; three variables load on neither factor).
- `R/check_packages.R` no longer installs packages; it prints the install command and stops. `digest` added so the data check runs on R older than 4.5.
- The MCD seed is now 1, set once in `02_outliers.R`. The robust total is unchanged at 761 (322 red, 439 white, was 324 and 437); the box-plot rule's overlap with the robust flags moves from 32.5% to 32.6%.
- Every other earlier number is unchanged; runtime is now about 30 seconds.

## Rebuild for publication (2026)

Adapted from a graduate team project. This is a solo rewrite: no code from other team
members is included, and the analysis was rebuilt from scratch in base R.

**Improvements**
- Mahalanobis outliers now use the upper χ² tail (`d2 > qchisq(0.999, 11)`); script 02 reproduces the difference from the original rule.
- Each wine carries a stable `id` for outlier tracking.
- Added robust (MCD) distance, a PCA outlier-sensitivity check, silhouette-based choice of k, adjusted Rand index, parallel analysis, Heywood-case detection and cross-validated LDA with AUC.
- Factor analysis runs on white wine only and excludes density (collinear with sugar and alcohol).
- Repository-relative paths; dependencies cut to ggplot2, patchwork, MASS and cluster.
- Added `run_all.R`, the generated report and figures, and a data README with license and citation.
- Raw CSVs are the exact UCI files (an earlier copy had `alcohol` rounded in 40 rows), checked against SHA-256 checksums before every run; results unchanged.

**Not included**
- Teammates' drafts and code; `historical.csv` (unknown source and license).

## Original project (2021)
- Graduate team project in Multivariate Analysis, Texas Tech University, on the UCI Wine Quality data
