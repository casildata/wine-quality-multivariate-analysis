# Changelog: from the original project to this release

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
