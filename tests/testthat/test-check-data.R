source("../../R/check_data.R")

copy_raw <- function() {
  tmp <- tempfile()
  dir.create(tmp)
  file.copy(list.files("../../data/raw", full.names = TRUE), tmp)
  tmp
}

test_that("the committed raw files match their checksums", {
  skip_if(is.na(sha256_file("../../data/raw/SHA256SUMS")), "needs R 4.5+ or digest")
  expect_true(check_data("../../data/raw"))
})

test_that("a changed value is rejected", {
  skip_if(is.na(sha256_file("../../data/raw/SHA256SUMS")), "needs R 4.5+ or digest")
  tmp <- copy_raw()
  lines <- readLines(file.path(tmp, "winequality-red.csv"))
  lines[2] <- sub("^7.4", "7.5", lines[2])
  writeLines(lines, file.path(tmp, "winequality-red.csv"))
  expect_error(check_data(tmp), "winequality-red.csv")
})

test_that("Windows line endings are rejected", {
  skip_if(is.na(sha256_file("../../data/raw/SHA256SUMS")), "needs R 4.5+ or digest")
  tmp <- copy_raw()
  path <- file.path(tmp, "winequality-white.csv")
  con <- file(path, "wb")
  writeLines(readLines(path), con, sep = "\r\n")
  close(con)
  expect_error(check_data(tmp), "winequality-white.csv")
})
