# Unsupervised structure: k-means, hierarchical clustering and classical MDS.
# Do natural groups in the chemistry recover wine type, and do they say anything about quality?

library(ggplot2)
library(cluster)

Z <- scale(wine[, CHEM_VARS])

# ---- k-means: choose k by within-cluster SS and silhouette --------------------------
set.seed(42)
sil_idx <- sample(nrow(Z), 2000)           # silhouette needs a distance matrix; subsample
d_sil <- dist(Z[sil_idx, ])
k_grid <- 2:8
km_fits <- lapply(k_grid, function(k) kmeans(Z, centers = k, nstart = 25, iter.max = 50))
k_tbl <- data.frame(
  k = k_grid,
  within_ss = round(sapply(km_fits, `[[`, "tot.withinss")),
  avg_silhouette = round(sapply(km_fits, function(f)
    mean(silhouette(f$cluster[sil_idx], d_sil)[, "sil_width"])), 3),
  ari_vs_type = round(sapply(km_fits, function(f) adjusted_rand(f$cluster, wine$type)), 3))
best_k <- k_tbl$k[which.max(k_tbl$avg_silhouette)]
print(k_tbl)

km2 <- km_fits[[1]]
km2_tab <- table(cluster = km2$cluster, type = wine$type)
km2_acc <- sum(apply(km2_tab, 1, max)) / nrow(wine)

# ---- Hierarchical clustering (Ward) on a stratified sample -------------------------
set.seed(7)
hc_idx <- unlist(lapply(split(seq_len(nrow(wine)), wine$type), function(i) sample(i, 750)))
hc <- hclust(dist(Z[hc_idx, ]), method = "ward.D2")
hc_ari <- sapply(2:4, function(k) adjusted_rand(cutree(hc, k), wine$type[hc_idx]))

# ---- Within white wine: do chemistry clusters differ in quality? -------------------
white <- wine$type == "white"
Zw <- scale(wine[white, CHEM_VARS])
set.seed(11)
km_w <- kmeans(Zw, centers = 3, nstart = 25)
qual_tbl <- do.call(rbind, lapply(sort(unique(km_w$cluster)), function(cl) {
  idx <- km_w$cluster == cl
  centre <- km_w$centers[cl, ]
  top <- names(sort(abs(centre), decreasing = TRUE))[1:2]
  data.frame(cluster = cl, wines = sum(idx),
             mean_quality = round(mean(wine$quality[white][idx]), 2),
             share_quality_7plus = pct(mean(wine$quality[white][idx] >= 7)),
             profile = paste(sprintf("%s %s", ifelse(centre[top] > 0, "high", "low"),
                                     pretty_var(top)), collapse = ", "))
}))
qual_tbl <- qual_tbl[order(-qual_tbl$mean_quality), ]
qual_anova <- summary(aov(wine$quality[white] ~ factor(km_w$cluster)))[[1]]
qual_anova_p <- qual_anova[["Pr(>F)"]][1]
# Effect size: with n = 4,898 any difference is "significant", so report the share of
# quality variance the clusters explain (eta squared)
qual_eta2 <- qual_anova[["Sum Sq"]][1] / sum(qual_anova[["Sum Sq"]])

# ---- Classical MDS on the same sample ----------------------------------------------
mds <- cmdscale(dist(Z[hc_idx, ]), k = 2)
pc_sample <- prcomp(Z[hc_idx, ])$x[, 1:2]
mds_pca_r <- abs(cor(mds[, 1], pc_sample[, 1]))  # Euclidean classical MDS == PCA scores

mds_df <- data.frame(dim1 = mds[, 1], dim2 = mds[, 2], type = wine$type[hc_idx],
                     cluster = factor(cutree(hc, 2)))
p_mds <- ggplot(mds_df, aes(dim1, dim2, colour = type, shape = cluster)) +
  geom_point(size = 1.4, alpha = 0.7) +
  scale_colour_manual(values = TYPE_COLOURS) +
  scale_shape_manual(values = c(16, 4)) +
  labs(title = "Classical MDS (1,500 wines, stratified sample)",
       subtitle = "Colour: true type · Shape: Ward cluster (k = 2)",
       x = "Dimension 1", y = "Dimension 2", colour = "Type", shape = "Cluster") +
  theme_minimal(base_size = 11)
p_k <- ggplot(k_tbl, aes(k, avg_silhouette)) +
  geom_line(colour = "#4a5568") + geom_point(size = 2.5, colour = "#2b6cb0") +
  labs(title = "Choosing k (k-means)", subtitle = "Average silhouette width, 2,000-wine sample",
       x = "Number of clusters k", y = "Average silhouette") +
  theme_minimal(base_size = 11)
save_plot(patchwork::wrap_plots(p_k, p_mds, widths = c(1, 1.5)), "clustering_mds.png", 12, 5)

report_add("## 4. Clustering and multidimensional scaling\n\n",
           "**k-means on all wines** (standardized, 25 random starts):\n\n",
           paste(md_table(k_tbl), collapse = "\n"), "\n\n",
           "The average silhouette is highest at k = ", best_k, ". With k = 2 the clusters ",
           "match wine type for ", pct(km2_acc), " of wines (adjusted Rand index ",
           sprintf("%.2f", k_tbl$ari_vs_type[1]), "): **the chemistry separates red and white ",
           "without being told which is which.** Ward hierarchical clustering on a stratified ",
           "sample agrees (ARI at k = 2: ", sprintf("%.2f", hc_ari[1]), ").\n\n",
           "**Within white wine**, three chemistry clusters differ in quality (ANOVA p ",
           if (qual_anova_p < 1e-4) "< 0.0001" else sprintf("= %.4f", qual_anova_p),
           ", η² = ", sprintf("%.3f", qual_eta2), "):\n\n",
           paste(md_table(qual_tbl), collapse = "\n"), "\n\n",
           "The differences are real but modest: the clusters explain ", pct(qual_eta2),
           " of the variation in quality. Clustering finds *styles* of wine, not quality ",
           "grades. Section 6 tests prediction directly.\n\n",
           "**MDS check:** classical MDS on Euclidean distances of standardized data is ",
           "mathematically equivalent to PCA; the first MDS dimension correlates ",
           sprintf("%.3f", mds_pca_r), " with PC1 scores of the same wines.\n\n",
           "![Clustering and MDS](figures/clustering_mds.png)\n")
