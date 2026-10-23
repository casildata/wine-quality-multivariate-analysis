# Exploratory analysis: how do red and white wines differ, and which variables move together?

library(ggplot2)

n_dup <- sum(is_repeat(wine))
counts <- as.data.frame(table(type = wine$type))
quality_tbl <- as.data.frame.matrix(table(wine$type, wine$quality))
quality_tbl <- cbind(type = rownames(quality_tbl), quality_tbl)

# Standardized mean difference (pooled SD): which chemistry separates the two types?
smd <- sapply(CHEM_VARS, function(v) {
  r <- wine[wine$type == "red", v]
  w <- wine[wine$type == "white", v]
  pooled <- sqrt(((length(r) - 1) * var(r) + (length(w) - 1) * var(w)) / (length(r) + length(w) - 2))
  (mean(r) - mean(w)) / pooled
})
diff_tbl <- data.frame(
  variable = pretty_var(CHEM_VARS),
  red_median = sapply(CHEM_VARS, function(v) as.character(signif(median(wine[wine$type == "red", v]), 4))),
  white_median = sapply(CHEM_VARS, function(v) as.character(signif(median(wine[wine$type == "white", v]), 4))),
  std_mean_diff = round(smd, 2),
  row.names = NULL
)
diff_tbl <- diff_tbl[order(-abs(diff_tbl$std_mean_diff)), ]
print(diff_tbl)

# ---- Box plots by type ----------------------------------------------------------
long <- do.call(rbind, lapply(CHEM_VARS, function(v)
  data.frame(variable = pretty_var(v), value = wine[[v]], type = wine$type)))
long$variable <- factor(long$variable, levels = diff_tbl$variable)
p_box <- ggplot(long, aes(type, value, fill = type)) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3, width = 0.6) +
  facet_wrap(~variable, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = TYPE_COLOURS, guide = "none") +
  labs(title = "Physicochemical profile by wine type",
       subtitle = "Ordered by standardized mean difference (largest separation first)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11)
save_plot(p_box, "boxplots_by_type.png", 10, 7)

# ---- Correlation heatmap --------------------------------------------------------
R <- cor(wine[, c(CHEM_VARS, "quality")])
cor_long <- expand.grid(x = colnames(R), y = colnames(R), stringsAsFactors = FALSE)
cor_long$r <- R[cbind(cor_long$x, cor_long$y)]
cor_long$x <- factor(pretty_var(cor_long$x), levels = pretty_var(colnames(R)))
cor_long$y <- factor(pretty_var(cor_long$y), levels = rev(pretty_var(colnames(R))))
p_cor <- ggplot(cor_long, aes(x, y, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.8) +
  scale_fill_gradient2(low = "#2b6cb0", mid = "white", high = "#c53030", limits = c(-1, 1)) +
  labs(title = "Correlation matrix (all 6,497 wines)", x = NULL, y = NULL, fill = "r") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
save_plot(p_cor, "correlation_heatmap.png", 8, 7)

upper <- which(upper.tri(R[CHEM_VARS, CHEM_VARS]), arr.ind = TRUE)
pairs_tbl <- data.frame(
  pair = paste(pretty_var(CHEM_VARS[upper[, 1]]), "×", pretty_var(CHEM_VARS[upper[, 2]])),
  r = round(R[CHEM_VARS, CHEM_VARS][upper], 2))
pairs_tbl <- head(pairs_tbl[order(-abs(pairs_tbl$r)), ], 5)
quality_cor <- sort(R["quality", CHEM_VARS], decreasing = TRUE)

report_add("## 1. Exploration\n\n",
           format(nrow(wine), big.mark = ","), " wines (",
           format(counts$Freq[1], big.mark = ","), " red, ", format(counts$Freq[2], big.mark = ","), " white), ",
           "11 physicochemical measurements and a sensory quality score (median of at least ",
           "three expert ratings). ", format(n_dup, big.mark = ","), " rows repeat an earlier row ",
           "exactly. They are kept, as in the original study; sections 2 and 3 show how much they ",
           "move the outlier counts and the components, and section 7 shows why model evaluation ",
           "has to keep the copies together.\n\n",
           "**Quality scores by type** (most wines score 5–6; the scale is used narrowly):\n\n",
           paste(md_table(quality_tbl), collapse = "\n"), "\n\n",
           "**What separates red from white?** Standardized mean difference (red − white, in pooled SDs):\n\n",
           paste(md_table(diff_tbl), collapse = "\n"), "\n\n",
           "![Box plots](figures/boxplots_by_type.png)\n\n",
           "**Strongest correlations between measurements:**\n\n",
           paste(md_table(pairs_tbl), collapse = "\n"), "\n\n",
           "Alcohol is the measurement most correlated with quality (r = ",
           sprintf("%.2f", quality_cor["alcohol"]), "); density is the most negative (r = ",
           sprintf("%.2f", quality_cor["density"]), "), largely because alcohol lowers density.\n\n",
           "![Correlation heatmap](figures/correlation_heatmap.png)\n")
