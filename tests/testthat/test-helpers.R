source("../../R/helpers.R", chdir = TRUE)

test_that("adjusted_rand is 1 for identical partitions and ignores label names", {
  expect_equal(adjusted_rand(c(1, 1, 2, 2), c("a", "a", "b", "b")), 1)
  expect_lt(abs(adjusted_rand(rep(1:2, 50), rep(1:2, each = 50))), 0.05)
})

test_that("auc matches perfect, reversed and tied rankings", {
  y <- c(FALSE, FALSE, TRUE, TRUE)
  expect_equal(auc(c(0.1, 0.2, 0.8, 0.9), y), 1)
  expect_equal(auc(c(0.9, 0.8, 0.2, 0.1), y), 0)
  expect_equal(auc(rep(0.5, 4), y), 0.5)
})

test_that("load_wine reads both UCI files with the expected shape", {
  wine <- load_wine("../../data/raw")
  expect_equal(dim(wine), c(6497, 14))
  expect_equal(as.vector(table(wine$type)), c(1599, 4898))
  expect_equal(sum(is_repeat(wine)), 1177)
})

test_that("grouped folds keep every copy of a profile in one fold", {
  group <- c("a", "a", "b", "c", "c", "c", "d", "e", "f", "g")
  fold <- group_folds(group, folds = 3)
  expect_true(all(tapply(fold, group, function(f) length(unique(f))) == 1))
  expect_setequal(unique(fold), 1:3)
})

test_that("cv_predict never scores a row with a model that saw it", {
  # Each test row's score is the number of training rows equal to it: 0 if no leak
  X <- matrix(1:20, ncol = 1)
  seen <- function(X_train, y_train, X_test) sapply(X_test[, 1], function(v) sum(X_train[, 1] == v))
  expect_equal(cv_predict(seen, X, rep(0, 20), make_folds(20, folds = 5)), rep(0, 20))
})

test_that("sensitivity_at reads a point off the ROC curve", {
  score <- c(1:10, 6:15)                      # 10 negatives, then 10 positives
  positive <- rep(c(FALSE, TRUE), each = 10)
  expect_equal(sensitivity_at(score, positive, 0.9), 0.6)   # threshold 9.1: 10..15 caught
  expect_equal(sensitivity_at(score, positive, 0), 1)
})

test_that("the outlier rule uses the upper chi-square tail", {
  set.seed(1)
  X <- matrix(rnorm(2000 * 3), ncol = 3)
  X[1, ] <- c(8, 8, 8)                        # one planted outlier
  d2 <- mahalanobis(X, colMeans(X), cov(X))
  expect_gt(d2[1], qchisq(0.999, 3))         # flagged by the fixed rule
  expect_false(pchisq(d2[1], 3) < 0.001)     # missed by the original rule
})
