# Multivariate outlier detection, run separately for red and white wine.
# Compares the original rule (box-plot outlier in 2+ variables), classical Mahalanobis
# distance, and robust Mahalanobis distance (minimum covariance determinant).

library(ggplot2)
library(MASS)

p <- length(CHEM_VARS)
cutoff <- qchisq(0.999, df = p)   # upper tail: far from the centre = outlier
mcd_seed <- 1                     # MCD searches random subsets

flag_type <- function(d) {
  X <- as.matrix(d[, CHEM_VARS])
  # Original rule: outside the box-plot whiskers on two or more variables
  iqr_hits <- rowSums(sapply(CHEM_VARS, function(v) d[[v]] %in% boxplot.stats(d[[v]])$out))
  classical <- mahalanobis(X, colMeans(X), cov(X))
  set.seed(mcd_seed)
  mcd <- cov.rob(X, method = "mcd")
  robust <- mahalanobis(X, mcd$center, mcd$cov)
  data.frame(id = d$id, type = d$type, quality = d$quality,
             iqr_2plus = iqr_hits >= 2,
             d2_classical = classical, classical = classical > cutoff,
             d2_robust = robust, robust = robust > cutoff,
             # what the original code flagged: lower tail of the chi-square, i.e. the
             # wines CLOSEST to the centre
             original_rule = pchisq(classical, df = p) < 0.001)
}
flags <- do.call(rbind, lapply(split(wine, wine$type), flag_type))
write.csv(flags, "outputs/outlier_flags.csv", row.names = FALSE)

summ <- do.call(rbind, lapply(split(flags, flags$type), function(f) data.frame(
  type = as.character(f$type[1]), wines = nrow(f),
  boxplot_rule = sum(f$iqr_2plus),
  classical_mahalanobis = sum(f$classical),
  robust_mcd = sum(f$robust),
  both_mahalanobis = sum(f$classical & f$robust),
  original_lower_tail_rule = sum(f$original_rule))))
print(summ)

agree <- with(flags, data.frame(
  comparison = c("Box-plot rule vs. robust MCD", "Classical vs. robust MCD"),
  share_of_robust_flags_also_caught = pct(c(mean(iqr_2plus[robust]), mean(classical[robust])))))

# ---- Chi-square Q-Q plot of distances: classical vs. robust -------------------------
qq <- do.call(rbind, lapply(split(flags, flags$type), function(f) {
  n <- nrow(f)
  q <- qchisq(ppoints(n), df = p)
  rbind(data.frame(type = f$type[1], method = "Classical", theoretical = q, observed = sort(f$d2_classical)),
        data.frame(type = f$type[1], method = "Robust (MCD)", theoretical = q, observed = sort(f$d2_robust)))
}))
p_qq <- ggplot(qq, aes(theoretical, observed, colour = method)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = 2) +
  geom_hline(yintercept = cutoff, colour = "grey30", linetype = 3) +
  geom_point(size = 0.6, alpha = 0.6) +
  facet_wrap(~type, labeller = labeller(type = function(x) paste(x, "wine"))) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = c("#2b6cb0", "#c53030")) +
  labs(title = "Chi-square Q-Q plot of squared Mahalanobis distances",
       subtitle = sprintf("Dotted line: 99.9th percentile of chi-square(%d) = %.1f", p, cutoff),
       x = "Chi-square quantile (log scale)", y = "Ordered squared distance (log scale)", colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "top")
save_plot(p_qq, "outlier_chisq_qq.png", 9, 4.8)

outliers_classical <- flags$id[flags$classical]
d_orig <- with(flags[flags$original_rule, ], range(d2_classical))

# ---- How much do the counts depend on the choices? ------------------------------------
# Raw MCD isn't scaled to the chi-square: rescale its distances so their median matches.
# Repeated rows count the same profile several times. Heavy right tails push wines past
# the cut-off, so repeat both rules with the skewed variables on a log scale.
count_flags <- function(d, seed) {
  X <- as.matrix(d[, CHEM_VARS])
  set.seed(seed)
  mcd <- cov.rob(X, method = "mcd")
  c(classical = sum(mahalanobis(X, colMeans(X), cov(X)) > cutoff),
    robust = sum(mahalanobis(X, mcd$center, mcd$cov) > cutoff))
}
LOG_VARS <- c("residual_sugar", "chlorides", "free_sulfur_dioxide", "sulphates", "volatile_acidity")
wine_log <- wine
wine_log[LOG_VARS] <- lapply(wine_log[LOG_VARS], log)
by_type <- function(d) sapply(split(d, d$type), count_flags, seed = mcd_seed)
log_counts <- by_type(wine_log)
unique_counts <- by_type(wine[!is_repeat(wine), ])
rescaled <- sapply(split(flags, flags$type), function(f)
  sum(f$d2_robust / (median(f$d2_robust) / qchisq(0.5, p)) > cutoff))

variant_rows <- list(
  "Classical (above)" = summ$classical_mahalanobis,
  "Robust MCD (above)" = summ$robust_mcd,
  "Robust MCD, median rescaled to chi-square" = rescaled,
  "Classical, repeated rows removed" = unique_counts["classical", ],
  "Robust MCD, repeated rows removed" = unique_counts["robust", ],
  "Classical, skewed variables logged" = log_counts["classical", ],
  "Robust MCD, skewed variables logged" = log_counts["robust", ])
variants <- data.frame(variant = names(variant_rows),
                       red = sapply(variant_rows, `[`, 1), white = sapply(variant_rows, `[`, 2),
                       row.names = NULL)
variants$total <- variants$red + variants$white
ratios <- c(sum(summ$robust_mcd) / sum(summ$classical_mahalanobis),
            sum(rescaled) / sum(summ$classical_mahalanobis),
            sum(unique_counts["robust", ]) / sum(unique_counts["classical", ]),
            sum(log_counts["robust", ]) / sum(log_counts["classical", ]))

report_add("## 2. Multivariate outliers\n\n",
           "Each type is screened separately (their centres differ). A wine is flagged when its ",
           "squared Mahalanobis distance exceeds the 99.9th percentile of chi-square(", p, ") = ",
           sprintf("%.1f", cutoff), ".\n\n",
           paste(md_table(summ), collapse = "\n"), "\n\n",
           paste(md_table(agree), collapse = "\n"), "\n\n",
           "The classical distance uses a mean and covariance that the outliers themselves inflate ",
           "(*masking*), so it flags far fewer wines than the robust MCD estimate. The heavy upper ",
           "tail in the Q-Q plot shows that the 11-variable normal model doesn't hold (several ",
           "variables, such as chlorides and sulphates, are strongly right-skewed). Flagged wines are ",
           "kept; section 3 re-runs the PCA without the classical outliers to check that they don't ",
           "drive the components.\n\n",
           "**How much do the counts depend on the choices?** The raw MCD distances sit above the ",
           "chi-square line even in the middle of the Q-Q plot, so they are rescaled to match its ",
           "median; repeated rows are dropped; and the five most skewed variables (",
           paste(pretty_var(LOG_VARS), collapse = ", "), ") are put on a log scale:\n\n",
           paste(md_table(variants), collapse = "\n"), "\n\n",
           "Masking holds in every comparison (the robust estimate flags ",
           sprintf("%.1f", min(ratios)), "–", sprintf("%.1f", max(ratios)),
           " times as many wines as the classical one), but the robust count itself ranges from ",
           min(variants$total[grepl("^Robust", variants$variant)]), " to ",
           max(variants$total[grepl("^Robust", variants$variant)]),
           ". Heavy tails, not only masking, push wines past the cut-off.\n\n",
           "**Bug in the original analysis, fixed:** the original script flagged wines with `pchisq(d2) < 0.001`, ",
           "the *lower* tail. That selects the most typical wines (here ", sum(summ$original_lower_tail_rule),
           " wines with d² between ", sprintf("%.2f", d_orig[1]), " and ", sprintf("%.2f", d_orig[2]),
           "), not the most unusual ones.\n\n",
           "![Chi-square Q-Q](figures/outlier_chisq_qq.png)\n")
