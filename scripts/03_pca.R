# Principal component analysis on standardized measurements (all wines).

library(ggplot2)

pca <- prcomp(wine[, CHEM_VARS], scale. = TRUE)
var_explained <- pca$sdev^2 / sum(pca$sdev^2)
n_kaiser <- sum(pca$sdev^2 > 1)

var_tbl <- data.frame(component = paste0("PC", seq_along(var_explained)),
                      eigenvalue = round(pca$sdev^2, 2),
                      variance = pct(var_explained),
                      cumulative = pct(cumsum(var_explained)))

loadings <- pca$rotation[, 1:3]
# Fix sign so PC1 is positive for white wine (scores easier to read)
scores <- pca$x[, 1:3]
for (k in 1:3) {
  if (k == 1 && mean(scores[wine$type == "white", 1]) < 0 ||
      k > 1 && loadings[which.max(abs(loadings[, k])), k] < 0) {
    loadings[, k] <- -loadings[, k]
    scores[, k] <- -scores[, k]
  }
}
load_tbl <- data.frame(variable = pretty_var(CHEM_VARS), round(loadings, 2), row.names = NULL)

describe_pc <- function(k, n = 3) {
  l <- loadings[, k]
  pos <- names(sort(l[l > 0.25], decreasing = TRUE))[seq_len(min(n, sum(l > 0.25)))]
  neg <- names(sort(l[l < -0.25]))[seq_len(min(n, sum(l < -0.25)))]
  paste0("**PC", k, "** (", pct(var_explained[k]), "): high ",
         if (length(pos)) paste(pretty_var(pos), collapse = ", ") else "(none)",
         "; low ", if (length(neg)) paste(pretty_var(neg), collapse = ", ") else "(none)")
}

# ---- Scree ----------------------------------------------------------------------
scree <- data.frame(pc = seq_along(var_explained), eig = pca$sdev^2)
p_scree <- ggplot(scree, aes(pc, eig)) +
  geom_col(fill = "#4a5568", width = 0.7) +
  geom_hline(yintercept = 1, linetype = 2, colour = "#c53030") +
  scale_x_continuous(breaks = scree$pc) +
  labs(title = "Scree plot", subtitle = "Dashed line: Kaiser criterion (eigenvalue = 1)",
       x = "Principal component", y = "Eigenvalue") +
  theme_minimal(base_size = 11)

# ---- Biplot: scores coloured by type, loading arrows ---------------------------
arrow_scale <- 8
arrows <- data.frame(var = pretty_var(CHEM_VARS),
                     x = loadings[, 1] * arrow_scale, y = loadings[, 2] * arrow_scale)
# Labels sit just past the arrow tip; spread out the ones whose arrows nearly coincide
arrows$lx <- arrows$x * 1.1 + 0.25 * sign(arrows$x)
arrows$ly <- arrows$y * 1.1
nudge_y <- c(free_sulfur_dioxide = -0.35, total_sulfur_dioxide = 0.3,
             chlorides = -0.45, fixed_acidity = 0.35, sulphates = -0.2)
arrows$ly <- arrows$ly + ifelse(CHEM_VARS %in% names(nudge_y), nudge_y[CHEM_VARS], 0)
p_bi <- ggplot(data.frame(scores, type = wine$type), aes(PC1, PC2)) +
  geom_point(aes(colour = type), size = 0.5, alpha = 0.35) +
  geom_segment(data = arrows, aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow(length = unit(0.15, "cm")), colour = "grey15") +
  geom_text(data = arrows, aes(lx, ly, label = var, hjust = ifelse(x > 0, 0, 1)),
            size = 2.9, fontface = "bold", colour = "grey10") +
  scale_colour_manual(values = TYPE_COLOURS) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  coord_cartesian(xlim = c(-7, 7), ylim = c(-6, 6)) +
  labs(title = "PCA biplot", x = sprintf("PC1 (%s)", pct(var_explained[1])),
       y = sprintf("PC2 (%s)", pct(var_explained[2])), colour = NULL) +
  theme_minimal(base_size = 11)

save_plot(patchwork::wrap_plots(p_scree, p_bi, widths = c(1, 1.6)), "pca_scree_biplot.png", 12, 5.5)

# ---- Sensitivity: does removing outliers change the components? -----------------
pca_clean <- prcomp(wine[!wine$id %in% outliers_classical, CHEM_VARS], scale. = TRUE)
stability <- sapply(1:3, function(k) abs(cor(pca$rotation[, k], pca_clean$rotation[, k])))
pca_unique <- prcomp(wine[!is_repeat(wine), CHEM_VARS], scale. = TRUE)
stability_unique <- sapply(1:3, function(k) abs(cor(pca$rotation[, k], pca_unique$rotation[, k])))
pc1_type_sep <- abs(cor(scores[, 1], wine$type == "white"))

report_add("## 3. Principal component analysis\n\n",
           "Standardized variables (they're on very different scales). ", n_kaiser,
           " components have eigenvalue > 1; the first two explain ",
           pct(sum(var_explained[1:2])), " and the first ", n_kaiser, " explain ",
           pct(sum(var_explained[1:n_kaiser])), " of the variance.\n\n",
           paste(md_table(var_tbl[1:6, ]), collapse = "\n"), "\n\n",
           "Loadings (signs fixed so PC1 is positive for white wine):\n\n",
           paste(md_table(load_tbl), collapse = "\n"), "\n\n",
           "- ", describe_pc(1), ". PC1 is the **red vs. white axis**: its scores correlate ",
           sprintf("%.2f", pc1_type_sep), " with wine type, although type was never given to the PCA.\n",
           "- ", describe_pc(2), ". PC2 is a **body/ripeness axis** (sugar and density against alcohol).\n",
           "- ", describe_pc(3), ".\n\n",
           "**Robustness:** re-running without the ", length(outliers_classical),
           " classical outliers leaves the loadings almost unchanged (|r| between old and new loadings: PC1 ",
           sprintf("%.3f", stability[1]), ", PC2 ", sprintf("%.3f", stability[2]), ", PC3 ",
           sprintf("%.3f", stability[3]), "). Dropping the ", format(sum(is_repeat(wine)), big.mark = ","),
           " repeated rows also leaves them almost unchanged (PC1 ", sprintf("%.3f", stability_unique[1]), ", PC2 ",
           sprintf("%.3f", stability_unique[2]), ", PC3 ", sprintf("%.3f", stability_unique[3]), ").\n\n",
           "![PCA](figures/pca_scree_biplot.png)\n")
