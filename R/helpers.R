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

# Rows that repeat an earlier row exactly (all measurements, quality and type).
is_repeat <- function(d) duplicated(d[, c(CHEM_VARS, "quality", "type")])

# One key per distinct lab profile, so identical wines can be kept together.
profile_key <- function(d) do.call(paste, c(d[CHEM_VARS], sep = "|"))

# Random fold labels, one per row.
make_folds <- function(n, folds = 10, seed = 1) {
  set.seed(seed)
  sample(rep(seq_len(folds), length.out = n))
}

# Fold labels drawn per group, so all rows of a group land in the same fold.
group_folds <- function(group, folds = 10, seed = 1) {
  g <- unique(group)
  make_folds(length(g), folds, seed)[match(group, g)]
}

# Out-of-fold scores from fit_predict(X_train, y_train, X_test).
cv_predict <- function(fit_predict, X, y, fold) {
  score <- numeric(nrow(X))
  for (f in sort(unique(fold))) {
    test <- fold == f
    score[test] <- fit_predict(X[!test, , drop = FALSE], y[!test], X[test, , drop = FALSE])
  }
  score
}

# LDA posterior probability of the second class, for cv_predict().
lda_fit_predict <- function(X_train, y_train, X_test) {
  predict(MASS::lda(X_train, grouping = y_train), X_test)$posterior[, 2]
}

# Share of positives caught at the threshold that keeps the given share of negatives
# below it (read from the cross-validated scores, like a point on the ROC curve).
sensitivity_at <- function(score, positive, specificity = 0.9) {
  mean(score[positive] > quantile(score[!positive], specificity, names = FALSE))
}
