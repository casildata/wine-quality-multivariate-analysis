# Linear discriminant analysis with 10-fold cross-validation.
#   (a) red vs. white: how separable are the types?
#   (b) white wine, "good" (quality >= 7) vs. the rest: can chemistry predict expert ratings?

library(MASS)
library(ggplot2)

cv_lda <- function(X, y, folds = 10, seed = 1) {
  set.seed(seed)
  fold <- sample(rep(seq_len(folds), length.out = nrow(X)))
  post <- numeric(nrow(X))
  for (f in seq_len(folds)) {
    test <- fold == f
    m <- lda(X[!test, ], grouping = y[!test])
    post[test] <- predict(m, X[test, ])$posterior[, 2]
  }
  post
}
metrics <- function(post, y, positive, threshold = 0.5) {
  pred <- post >= threshold
  truth <- y == positive
  data.frame(accuracy = pct(mean(pred == truth)),
             sensitivity = pct(mean(pred[truth])),
             specificity = pct(mean(!pred[!truth])),
             balanced_accuracy = pct((mean(pred[truth]) + mean(!pred[!truth])) / 2),
             AUC = sprintf("%.3f", auc(post, truth)))
}

# ---- (a) red vs. white -------------------------------------------------------------
X <- scale(wine[, CHEM_VARS])
post_type <- cv_lda(X, wine$type)
m_type <- metrics(post_type, wine$type, "white")
lda_type <- lda(X, grouping = wine$type)
coef_type <- lda_type$scaling[, 1]
coef_type <- coef_type[order(-abs(coef_type))]

# ---- (b) good vs. not, white wine ---------------------------------------------------
w <- wine[wine$type == "white", ]
Xw <- scale(w[, CHEM_VARS])
good <- factor(ifelse(w$quality >= 7, "good", "other"), levels = c("other", "good"))
base_rate <- mean(good == "good")
post_good <- cv_lda(Xw, good)
m_good_05 <- metrics(post_good, good, "good", 0.5)
m_good_br <- metrics(post_good, good, "good", base_rate)
lda_good <- lda(Xw, grouping = good)
coef_good <- lda_good$scaling[, 1]
coef_good <- coef_good[order(-abs(coef_good))]

res_tbl <- rbind(
  cbind(task = "Red vs. white (all wines)", threshold = "0.5", m_type),
  cbind(task = "Good (7+) vs. other (white)", threshold = "0.5", m_good_05),
  cbind(task = "Good (7+) vs. other (white)", threshold = sprintf("%.2f (base rate)", base_rate), m_good_br))

coef_tbl <- data.frame(variable = pretty_var(names(coef_good)),
                       coefficient = round(coef_good, 2), row.names = NULL)

dens <- data.frame(posterior = post_good, group = good)
p_lda <- ggplot(dens, aes(posterior, fill = group)) +
  geom_density(alpha = 0.55, colour = NA) +
  geom_vline(xintercept = c(0.5, base_rate), linetype = c(2, 3), colour = "grey30") +
  scale_fill_manual(values = c(other = "#a0aec0", good = "#2f855a"),
                    labels = c(other = "quality 3–6", good = "quality 7–9")) +
  labs(title = "Cross-validated LDA: P(good) for white wines",
       subtitle = "Dashed: 0.5 threshold · Dotted: base-rate threshold",
       x = "Posterior probability of 'good'", y = "Density", fill = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "top")
save_plot(p_lda, "lda_quality_posterior.png", 7.5, 4.5)

report_add("## 6. Discriminant analysis (10-fold cross-validated)\n\n",
           paste(md_table(res_tbl), collapse = "\n"), "\n\n",
           "**Red vs. white is almost perfectly separable**: the largest standardized ",
           "discriminant weights are ", paste(sprintf("%s (%+.2f)", pretty_var(names(coef_type)[1:3]),
                                                  coef_type[1:3]), collapse = ", "), ".\n\n",
           "**Quality is much harder.** Only ", pct(base_rate), " of white wines score 7+. At the ",
           "default 0.5 threshold LDA looks accurate but misses most good wines (low sensitivity); ",
           "lowering the threshold to the base rate trades accuracy for balanced accuracy. ",
           "The AUC shows how far the chemistry alone can rank wines. Standardized discriminant ",
           "weights for 'good':\n\n",
           paste(md_table(coef_tbl), collapse = "\n"), "\n\n",
           "Density and residual sugar get large opposite-signed weights because they are ",
           "strongly collinear with alcohol; read them together, not one at a time.\n\n",
           "![LDA posterior](figures/lda_quality_posterior.png)\n")
