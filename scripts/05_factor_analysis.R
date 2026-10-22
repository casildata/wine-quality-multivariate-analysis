# Exploratory factor analysis (maximum likelihood, varimax) on white wine.
# One wine type only: pooling red and white would make "type" the dominant factor.
# Density is left out: it is almost a linear function of sugar and alcohol, which drives
# the ML solution to uniquenesses of zero (a Heywood case).

library(ggplot2)

white_wine <- wine[wine$type == "white", ]
density_r2 <- summary(lm(density ~ residual_sugar + alcohol, data = white_wine))$r.squared
FA_VARS <- setdiff(CHEM_VARS, "density")
Xw <- white_wine[, FA_VARS]
Rw <- cor(Xw)

# Parallel analysis: keep factors whose eigenvalue beats the 95th percentile of
# eigenvalues from random normal data of the same size.
set.seed(2021)
n <- nrow(Xw); p <- ncol(Xw)
rand_eig <- replicate(50, eigen(cor(matrix(rnorm(n * p), n, p)), only.values = TRUE)$values)
pa <- data.frame(factor = 1:p, observed = eigen(Rw, only.values = TRUE)$values,
                 random_95th = apply(rand_eig, 1, quantile, 0.95))
n_pa <- which(pa$observed <= pa$random_95th)[1] - 1

# Fit with the parallel-analysis count; step down if ML hits a Heywood case
# (factanal stops uniquenesses at a 0.005 lower bound instead of warning).
fit_fa <- function(k) {
  f <- tryCatch(factanal(Xw, factors = k, rotation = "varimax"),
                error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(f) && min(f$uniquenesses) < 0.01) NULL else f
}
n_factors <- n_pa
fa <- fit_fa(n_factors)
while (is.null(fa) && n_factors > 1) {
  n_factors <- n_factors - 1
  fa <- fit_fa(n_factors)
}
print(fa, cutoff = 0.3)

L <- unclass(fa$loadings)
colnames(L) <- paste0("F", seq_len(ncol(L)))
load_tbl <- data.frame(variable = pretty_var(rownames(L)),
                       apply(L, 2, function(col) ifelse(abs(col) >= 0.3, sprintf("%.2f", col), "")),
                       communality = round(1 - fa$uniquenesses, 2), row.names = NULL)
ss <- colSums(L^2)
fa_var <- data.frame(factor = colnames(L), ss_loadings = round(ss, 2),
                     variance_explained = pct(ss / p), row.names = NULL)

name_factor <- function(k) {
  top <- names(sort(abs(L[, k]), decreasing = TRUE))[1:3]
  paste0(colnames(L)[k], ": ", paste(sprintf("%s (%+.2f)", pretty_var(top), L[top, k]), collapse = ", "))
}

# Loadings heatmap
fl <- expand.grid(variable = rownames(L), factor = colnames(L), stringsAsFactors = FALSE)
fl$loading <- L[cbind(fl$variable, fl$factor)]
fl$variable <- factor(pretty_var(fl$variable), levels = rev(pretty_var(rownames(L))))
p_fa <- ggplot(fl, aes(factor, variable, fill = loading)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(abs(loading) >= 0.3, sprintf("%.2f", loading), "")), size = 3.2) +
  scale_fill_gradient2(low = "#2b6cb0", mid = "white", high = "#c53030", limits = c(-1, 1)) +
  labs(title = "Factor loadings, white wine (ML, varimax)",
       subtitle = "Loadings below 0.3 hidden", x = NULL, y = NULL, fill = "Loading") +
  theme_minimal(base_size = 11)
save_plot(p_fa, "factor_loadings.png", 6.5, 5.5)

report_add("## 5. Exploratory factor analysis (white wine)\n\n",
           "Density is excluded: residual sugar and alcohol alone explain ",
           pct(density_r2), " of its variance, and keeping it forces uniquenesses to zero. ",
           "Parallel analysis on the remaining ", p, " variables retains ", n_pa, " factors",
           if (n_factors < n_pa) paste0("; maximum likelihood hit a Heywood case at ", n_pa,
                                        ", so ", n_factors, " were fitted") else "", ".\n\n",
           paste(md_table(head(transform(pa, observed = round(observed, 2),
                                         random_95th = round(random_95th, 2)), 6)), collapse = "\n"), "\n\n",
           paste(md_table(load_tbl), collapse = "\n"), "\n\n",
           paste(md_table(fa_var), collapse = "\n"), "\n\n",
           paste0("- ", sapply(seq_len(ncol(L)), name_factor), collapse = "\n"), "\n\n",
           "The model is weak: together the factors explain ", pct(sum(ss) / p),
           " of the variance, and ", paste(pretty_var(names(which(1 - fa$uniquenesses < 0.1))), collapse = ", "),
           " load on neither (communality below 0.1). Most of the white-wine chemistry varies ",
           "independently rather than through a few shared factors.\n\n",
           "The likelihood-ratio test of ", n_factors, " factors gives p = ",
           format.pval(fa$PVAL, digits = 2), ". With n = ", format(n, big.mark = ","),
           " the test rejects any parsimonious model, so the choice rests on parallel analysis ",
           "and interpretability.\n\n",
           "![Factor loadings](figures/factor_loadings.png)\n")
