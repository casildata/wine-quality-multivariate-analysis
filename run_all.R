# Reproduce every table and figure from the repository root:
#   Rscript run_all.R
# Packages: listed in r-packages.txt; the run stops with the install command if any are missing.
source("R/check_packages.R")
source("R/check_data.R")     # raw CSVs must match the UCI checksums
source("R/helpers.R")
source("R/report.R")

dir.create("outputs", showWarnings = FALSE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

wine <- load_wine()

pipeline <- c(
  "scripts/01_explore.R",
  "scripts/02_outliers.R",
  "scripts/03_pca.R",
  "scripts/04_clustering_mds.R",
  "scripts/05_factor_analysis.R",
  "scripts/06_discriminant_analysis.R"
)
for (s in pipeline) {
  message("==> ", s)
  source(s, echo = FALSE)
}
report_write("docs/results.md")
message("Results written to docs/results.md")
