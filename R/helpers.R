# Data loading and small statistics helpers shared by the scripts.

CHEM_VARS <- c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar",
               "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density",
               "pH", "sulphates", "alcohol")
TYPE_COLOURS <- c(red = "#9b2c2c", white = "#d69e2e")

load_wine <- function(dir = "data/raw") {
  read_one <- function(type) {
    d <- read.csv(file.path(dir, paste0("winequality-", type, ".csv")), sep = ";")
    names(d) <- gsub(".", "_", names(d), fixed = TRUE)
    d <- d[, c(CHEM_VARS, "quality")]
    d$type <- type
    d
  }
  wine <- rbind(read_one("red"), read_one("white"))
  wine$type <- factor(wine$type, levels = c("red", "white"))
  wine$id <- seq_len(nrow(wine))
  wine
}

pretty_var <- function(v) gsub("_", " ", v)

# Agreement between two partitions, corrected for chance (1 = identical, ~0 = random).
adjusted_rand <- function(a, b) {
  tab <- table(a, b)
  c2 <- function(x) sum(choose(x, 2))
  s_ij <- c2(tab)
  s_a <- c2(rowSums(tab))
  s_b <- c2(colSums(tab))
  expected <- s_a * s_b / choose(sum(tab), 2)
  (s_ij - expected) / ((s_a + s_b) / 2 - expected)
}

# Area under the ROC curve via the rank (Mann-Whitney) formula.
auc <- function(score, positive) {
  r <- rank(score)
  n1 <- sum(positive)
  n0 <- sum(!positive)
  (sum(r[positive]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
