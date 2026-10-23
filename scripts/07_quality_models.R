# Can a more flexible model predict "good" (quality >= 7) white wine better than LDA,
# and do the repeated rows inflate the answer?
#   Random folds put copies of the same lab profile on both sides of a split; grouped
#   folds keep every copy in one fold. Same models, same 10 folds otherwise.

w <- wine[wine$type == "white", ]
X <- as.matrix(w[, CHEM_VARS])
y <- factor(ifelse(w$quality >= 7, "good", "other"), levels = c("other", "good"))
is_good <- y == "good"

key <- profile_key(w)
folds <- list(random = make_folds(nrow(w)), grouped = group_folds(key))
# A row has a twin across the split when its profile appears in more than one fold
twin_across <- sapply(folds, function(f) {
  n_folds <- tapply(f, key, function(x) length(unique(x)))
  mean(n_folds[key] > 1)
})
conflicting <- sum(tapply(w$quality, key, function(q) length(unique(q)) > 1))

logit_fp <- function(X_train, y_train, X_test) {
  m <- glm(y_train ~ ., family = binomial, data = data.frame(X_train, y_train))
  predict(m, data.frame(X_test), type = "response")
}
rf_fp <- function(X_train, y_train, X_test) {
  m <- randomForest::randomForest(X_train, y_train, ntree = 500)
  predict(m, X_test, type = "prob")[, "good"]
}
models <- list(
  "Alcohol only (LDA)" = list(fp = lda_fit_predict, cols = "alcohol"),
  "LDA" = list(fp = lda_fit_predict, cols = CHEM_VARS),
  "Logistic regression" = list(fp = logit_fp, cols = CHEM_VARS),
  "Random forest (500 trees)" = list(fp = rf_fp, cols = CHEM_VARS))

set.seed(7)   # random forest draws; the folds have their own seed
scores <- lapply(models, function(m) lapply(folds, function(f)
  cv_predict(m$fp, X[, m$cols, drop = FALSE], y, f)))

model_tbl <- data.frame(
  model = names(models),
  auc_random_folds = sapply(scores, function(s) sprintf("%.3f", auc(s$random, is_good))),
  auc_grouped_folds = sapply(scores, function(s) sprintf("%.3f", auc(s$grouped, is_good))),
  inflation = sapply(scores, function(s) sprintf("%+.3f", auc(s$random, is_good) - auc(s$grouped, is_good))),
  good_wines_caught_at_90pct_specificity = sapply(scores, function(s) pct(sensitivity_at(s$grouped, is_good))),
  row.names = NULL)
print(model_tbl)

auc_of <- function(model, scheme) auc(scores[[model]][[scheme]], is_good)
rf <- "Random forest (500 trees)"

report_add("## 7. Better models, and the duplicate audit (white wine, good vs. other)\n\n",
           format(sum(duplicated(key)), big.mark = ","), " of the ", format(nrow(w), big.mark = ","),
           " white wines repeat an earlier lab profile exactly, and ",
           if (conflicting == 0) "none of the repeated profiles carries two different quality scores" else
             paste(conflicting, "repeated profiles carry two different quality scores"),
           ", so these look like repeated records rather than different wines that happen to ",
           "measure the same. With random ",
           "10-fold cross-validation (as in section 6), ", pct(twin_across["random"]),
           " of white wines have an identical twin in another fold. **Grouped folds** keep every ",
           "copy of a profile in one fold (", pct(twin_across["grouped"]), " across folds).\n\n",
           paste(md_table(model_tbl), collapse = "\n"), "\n\n",
           "**The random forest is the best model, but random folds overstate it:** AUC ",
           sprintf("%.3f", auc_of(rf, "random")), " with random folds against ",
           sprintf("%.3f", auc_of(rf, "grouped")), " with grouped folds. A forest can memorize a ",
           "profile and recognize its copy in the test fold; LDA and logistic regression are too ",
           "rigid to do that, so their scores barely move. Section 6's LDA numbers are therefore ",
           "safe, while any flexible model on this data needs grouped (or deduplicated) folds.\n\n",
           "Even the leak-free forest is a modest ranker (AUC ", sprintf("%.3f", auc_of(rf, "grouped")),
           "): at a threshold that keeps 90% of other wines below it, it catches ",
           pct(sensitivity_at(scores[[rf]]$grouped, is_good)), " of good wines. The threshold is read ",
           "from the cross-validated scores, so treat that figure as slightly optimistic.\n")
