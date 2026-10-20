# Stop before the analysis if a raw CSV differs from the UCI original.
# data/raw/SHA256SUMS holds the checksums of the files in the UCI download
# (same format as `sha256sum -c`). Run from the repository root.

sha256_file <- function(path) {
  if (exists("sha256sum", envir = asNamespace("tools"))) {   # R 4.5+
    return(unname(get("sha256sum", envir = asNamespace("tools"))(path)))
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(path, algo = "sha256", file = TRUE))
  }
  NA_character_
}

check_data <- function(dir = "data/raw") {
  sums <- read.table(file.path(dir, "SHA256SUMS"), col.names = c("sha256", "file"),
                     colClasses = "character")
  got <- vapply(file.path(dir, sums$file), sha256_file, character(1))
  if (anyNA(got)) {
    warning("Raw data not verified: SHA-256 needs R 4.5+ or the digest package.",
            call. = FALSE)
    return(invisible(FALSE))
  }
  bad <- sums$file[got != sums$sha256]
  if (length(bad) > 0) {
    stop("Raw data doesn't match the UCI originals (data/raw/SHA256SUMS): ",
         paste(bad, collapse = ", "),
         "\nRe-download from https://archive.ics.uci.edu/dataset/186/wine+quality",
         call. = FALSE)
  }
  invisible(TRUE)
}

check_data()
