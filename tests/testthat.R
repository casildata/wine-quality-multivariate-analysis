# Run the unit tests from the repository root:
#   Rscript tests/testthat.R
# Loading testthat here (not only from CI) keeps it in renv.lock.
library(testthat)
test_dir("tests/testthat", stop_on_failure = TRUE)
