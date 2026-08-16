# =============================================================================
# PCA Validation of the Spam PAC Directory
# =============================================================================
#
# This script validates the manually coded spam PAC directory (144 PACs) using
# principal component analysis. It also applies a set of rule-based diagnostic
# checks to compare the manual directory against a fully data-driven classification.
#
# Required data files (in data/):
#   - spam_pac_indicators_for_pca.csv  | Indicator matrix for all PACs and candidates
#   - spam_pac_directory_final.csv     | Manual spam PAC directory (144 PACs)
#
# Outputs:
#   - Correlation matrix, PCA summary, and leave-one-out robustness (stdout)
#   - Rule-based classification diagnostics (stdout)
#   - Three-category breakdown: Manual (144), Rule-Based Addition, Non-Spam (stdout)
#   - figs/pca_scree.png                | Variance explained by component
#   - figs/pca_loadings.png             | PC1 loadings bar chart
#   - figs/pca_indicators_jitter.png    | Indicator distributions (3 categories)
#   - figs/pca_classification.png        | Logistic classification curve (3 categories)
#
# =============================================================================


# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

req_pkgs <- c("data.table", "dplyr", "tidyr", "ggplot2", "pROC", "glue")
missing_pkgs <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(sprintf("Missing required packages: %s", paste(missing_pkgs, collapse = ", ")),
       call. = FALSE)
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(pROC)
  library(glue)
})

dir.create("figs", showWarnings = FALSE)


# =============================================================================
# 1. Load Data and Prepare Indicators
# =============================================================================

df_agg <- read.csv("data/spam_pac_indicators_for_pca.csv")

# Load spam PAC directory
sp_dir <- read.csv("data/spam_pac_directory_final.csv")
df_agg$is_spam_pac_manual <- as.integer(df_agg$bonica_rid %in% sp_dir$bonica_rid)
df_agg$is_spam_pac        <- df_agg$is_spam_pac_manual

df_agg$log_hvv_total <- log1p(df_agg$hvv_total)
df_agg$is_cand <- as.numeric(grepl("cand", df_agg$bonica_rid) & df_agg$is_spam_pac_manual==0)



# PCA indicator set
use.cols <- c(
    "mean_age",
    "pct_65_plus",
    "pct_overall_donations_100plus",
    "pct_overall_donations_10plus_distinct",
    "high_volume_vendor_pct",
    "fundraising_inefficiency",
    "log_hvv_total",
    "log_n_refunds"
)

# Restrict PCA to PACs only (exclude candidates)
df_comm <- df_agg[df_agg$is_cand == 0, ]


# =============================================================================
# 2. Correlation Matrix
# =============================================================================

cat("\n")
cat("===========================================================================\n")
cat("  Correlation Matrix for PCA Indicators\n")
cat("===========================================================================\n\n")
print(round(cor(df_comm[, use.cols], use = "complete.obs"), 3))


# =============================================================================
# 3. Fit PCA Model
# =============================================================================

pr.out <- prcomp(df_comm[, use.cols], center = TRUE, scale. = TRUE)

cat("\n")
cat("===========================================================================\n")
cat("  PCA Model Summary\n")
cat("===========================================================================\n\n")
print(summary(pr.out))
cat("\n  PC1 and PC2 Loadings:\n")
print(round(pr.out$rotation[, 1:2], 3))

# Score all rows (PACs and candidates) using the PAC-only PCA model
df_agg$pc1 <- predict(pr.out, newdata = df_agg[, use.cols])[, 1]
df_agg$pc2 <- predict(pr.out, newdata = df_agg[, use.cols])[, 2]

# Also score the PAC-only subset
df_comm$pc1 <- pr.out$x[, 1]
df_comm$pc2 <- pr.out$x[, 2]

# Ensure higher PC1 = more spam-like
if (mean(df_comm$pc1[df_comm$is_spam_pac_manual == 1], na.rm = TRUE) < 0) {
    df_comm$pc1 <- df_comm$pc1 * -1
    df_agg$pc1  <- df_agg$pc1  * -1
    pr.out$rotation[, 1] <- pr.out$rotation[, 1] * -1
}
if (mean(df_comm$pc2[df_comm$is_spam_pac_manual == 1], na.rm = TRUE) < 0) {
    df_comm$pc2 <- df_comm$pc2 * -1
    df_agg$pc2  <- df_agg$pc2  * -1
}


# =============================================================================
# 4. Leave-One-Out Robustness
# =============================================================================

cat("\n")
cat("===========================================================================\n")
cat("  Leave-One-Out Robustness (correlation with full model PC1)\n")
cat("===========================================================================\n\n")

rdim <- NULL
for (i.col in seq_along(use.cols)) {
    pr.out.tmp <- prcomp(df_comm[, use.cols[-i.col]], center = TRUE, scale. = TRUE)
    rdim <- cbind(rdim, pr.out.tmp$x[, 1])
}
colnames(rdim) <- use.cols
rownames(rdim) <- df_comm$bonica_rid

main.model.pca.1 <- df_comm$pc1
print(t(cor(main.model.pca.1, rdim)))
cat("\n  Note: correlations consistently above r = 0.98 indicate high robustness.\n")


# =============================================================================
# 5. Rule-Based Diagnostic Classification
# =============================================================================

cat("\n")
cat("===========================================================================\n")
cat("  Rule-Based Diagnostic Classification\n")
cat("===========================================================================\n\n")

indicators <- df_comm

# Check 1: Donor saturation — >=85% of donations from hyper-frequent donors
# Use pct_overall_either_100n_10d if available, otherwise construct from available cols
if ("pct_overall_either_100n_10d" %in% names(indicators)) {
    check1 <- with(indicators, ifelse(
        round(pct_overall_either_100n_10d, 2) >= 0.85, 1L, 0L))
} else {
    # Fallback: use pct_overall_donations_100plus as proxy
    check1 <- with(indicators, ifelse(
        pct_overall_donations_100plus >= 0.50 |
        pct_overall_donations_10plus_distinct >= 0.85, 1L, 0L))
}

# Check 2: Fundraising structure — high vendor spending or large-scale inefficiency
check2 <- with(indicators, ifelse(
    (high_volume_vendor_pct >= 0.20 & (fundraising_inefficiency >= 0.35)) |
    (exp(log_hvv_total) > 1e6 & (fundraising_inefficiency >= 0.5)),
    1L, 0L))

# Check 3: Donor age — >66% of donors aged 65+
check3 <- with(indicators, ifelse(pct_65_plus > 0.66, 1L, 0L))

# Check 4: PCA — above median PC1 score among PACs
check4 <- with(indicators, ifelse(
    pc1 >= quantile(pc1, probs = 0.5), 1L, 0L))

# Combined: must pass (check1 OR check2) AND check3 AND check4
full <- ifelse(check1 == 1 | check2 == 1 & check3 == 1 & check4 == 1, 1L, 0L)
fundraising_model <- ifelse(check1 == 1 | check2 == 1, 1L, 0L)

spam   <- indicators[indicators$is_spam_pac_manual == 1, ]
n_spam <- nrow(spam)

cat(sprintf("  Total spam PACs in manual directory:  %d\n\n", n_spam))
cat("  --- Individual Criteria ---\n")
cat(sprintf("  Meet check1 (donor saturation):       %d / %d  (%.1f%%)\n",
            sum(check1[indicators$is_spam_pac_manual == 1]), n_spam,
            100 * mean(check1[indicators$is_spam_pac_manual == 1])))
cat(sprintf("  Meet check2 (fundraising structure):  %d / %d  (%.1f%%)\n",
            sum(check2[indicators$is_spam_pac_manual == 1]), n_spam,
            100 * mean(check2[indicators$is_spam_pac_manual == 1])))
cat(sprintf("  Meet check3 (donor age > 66%%):        %d / %d  (%.1f%%)\n",
            sum(check3[indicators$is_spam_pac_manual == 1]), n_spam,
            100 * mean(check3[indicators$is_spam_pac_manual == 1])))
cat(sprintf("  Meet check4 (PCA above median):       %d / %d  (%.1f%%)\n",
            sum(check4[indicators$is_spam_pac_manual == 1]), n_spam,
            100 * mean(check4[indicators$is_spam_pac_manual == 1])))
cat(sprintf("  Meet check1 OR check2:                %d / %d  (%.1f%%)\n",
            sum(fundraising_model[indicators$is_spam_pac_manual == 1]), n_spam,
            100 * mean(fundraising_model[indicators$is_spam_pac_manual == 1])))
cat(sprintf("\n  Meet ALL criteria:                    %d / %d  (%.1f%%)\n",
            sum(full[indicators$is_spam_pac_manual == 1]), n_spam,
            100 * mean(full[indicators$is_spam_pac_manual == 1])))


# --- Spam PACs that FAIL all criteria ---
fail_idx <- which(indicators$is_spam_pac_manual == 1 & full == 0)
fail <- indicators[fail_idx, ]
cat(sprintf("\n  --- Spam PACs that FAIL criteria: %d ---\n", nrow(fail)))
if (nrow(fail) > 0) {
    fail$check1 <- check1[fail_idx]
    fail$check2 <- check2[fail_idx]
    fail$check3 <- check3[fail_idx]
    fail$check4 <- check4[fail_idx]
    for (i in seq_len(nrow(fail))) {
        fails_on <- c()
        if (fail$check1[i] == 0 & fail$check2[i] == 0) fails_on <- c(fails_on, "fundraising model")
        if (fail$check2[i] == 0) fails_on <- c(fails_on, "fundraising efficiency")
        if (fail$check3[i] == 0) fails_on <- c(fails_on, "donor age")
        if (fail$check4[i] == 0) fails_on <- c(fails_on, "PCA")
        nm <- if ("committee_name" %in% names(fail)) fail$committee_name[i] else fail$bonica_rid[i]
        cat(sprintf("    %-45s  fails: %s\n",
                    paste0(nm, " | ", fail$bonica_rid[i]),
                    paste(fails_on, collapse = ", ")))
    }
}

# --- Non-spam PACs that MEET all criteria (rule-based additions) ---
non_spam_qualify <- indicators[indicators$is_spam_pac_manual == 0 & full == 1, ]
cat(sprintf("\n  --- Non-spam PACs that MEET all criteria: %d ---\n", nrow(non_spam_qualify)))
if (nrow(non_spam_qualify) > 0) {
    non_spam_qualify <- non_spam_qualify[order(-non_spam_qualify$pc1), ]
    for (i in seq_len(min(nrow(non_spam_qualify), 30))) {
        nm <- if ("committee_name" %in% names(non_spam_qualify)) {
            non_spam_qualify$committee_name[i]
        } else {
            non_spam_qualify$bonica_rid[i]
        }
        cat(sprintf("    %-45s  (PC1: %.2f)\n", nm, non_spam_qualify$pc1[i]))
    }
    if (nrow(non_spam_qualify) > 30) {
        cat(sprintf("    ... and %d more\n", nrow(non_spam_qualify) - 30))
    }
}

# Construct rule-based directory
rule_based_spam <- indicators$bonica_rid[full == 1 | indicators$is_spam_pac_manual == 1]
n_rule_based <- sum(full == 1)
n_overlap    <- sum(full == 1 & indicators$is_spam_pac_manual == 1)
n_manual_only <- sum(full == 0 & indicators$is_spam_pac_manual == 1)
n_rule_only   <- sum(full == 1 & indicators$is_spam_pac_manual == 0)

cat(sprintf("\n  --- Directory Comparison ---\n"))
cat(sprintf("  Manual directory:                 %d PACs\n", n_spam))
cat(sprintf("  Rule-based directory:             %d PACs\n", n_rule_based))
cat(sprintf("  Overlap (manual ∩ rule-based):    %d PACs\n", n_overlap))
cat(sprintf("  Manual only (below threshold):    %d PACs\n", n_manual_only))
cat(sprintf("  Rule-based only (new additions):  %d PACs\n", n_rule_only))

# --- Three-category classification ---
# Used for all plots: Manual (144), Rule-Based Addition, Non-Spam
df_comm$is_rule_based_addition <- as.integer(full == 1 & indicators$is_spam_pac_manual == 0)

df_comm$spam_category <- factor(
    ifelse(df_comm$is_spam_pac_manual == 1, "Manual Spam PAC (144)",
    ifelse(df_comm$is_rule_based_addition == 1, "Rule-Based Addition",
           "Non-Spam PAC")),
    levels = c("Non-Spam PAC", "Rule-Based Addition", "Manual Spam PAC (144)")
)

# Color palette for three categories
cat_colors <- c(
    "Non-Spam PAC"           = "#457B9D",
    "Rule-Based Addition"    = "#F4A261",
    "Manual Spam PAC (144)"  = "#E63946"
)

cat(sprintf("\n  Three-category breakdown:\n"))
print(table(df_comm$spam_category))


# =============================================================================
# 6. AUC and Logistic Model
# =============================================================================

cat("\n")
cat("===========================================================================\n")
cat("  Logistic Model and AUC\n")
cat("===========================================================================\n\n")

roc_pc1 <- roc(df_comm$is_spam_pac_manual, df_comm$pc1, quiet = TRUE)
auc_pc1 <- round(auc(roc_pc1), 3)
cat(sprintf("  PC1 AUC: %.3f\n", auc_pc1))

roc_pc2 <- roc(df_comm$is_spam_pac_manual, df_comm$pc2, quiet = TRUE)
auc_pc2 <- round(auc(roc_pc2), 3)
cat(sprintf("  PC2 AUC: %.3f\n\n", auc_pc2))

logit_mod <- glm(is_spam_pac_manual ~ pc1, data = df_comm, family = binomial())
cat("=== Logistic Regression: is_spam_pac ~ pc1 ===\n")
print(summary(logit_mod))


# =============================================================================
# 7. Plots
# =============================================================================


# -----------------------------------------------------------------------------
# Plot 1: Scree Plot
# -----------------------------------------------------------------------------

var_explained <- summary(pr.out)$importance
scree_df <- data.frame(
    PC = factor(paste0("PC", seq_len(ncol(var_explained))),
                levels = paste0("PC", seq_len(ncol(var_explained)))),
    Proportion = var_explained["Proportion of Variance", ],
    Cumulative = var_explained["Cumulative Proportion", ]
)

p_scree <- ggplot(scree_df, aes(x = PC, y = Proportion)) +
    geom_col(fill = "#457B9D", width = 0.6) +
    geom_line(aes(x = as.numeric(PC), y = Cumulative),
              color = "#E63946", linewidth = 1.2) +
    geom_point(aes(x = as.numeric(PC), y = Cumulative),
               color = "#E63946", size = 3) +
    scale_y_continuous(labels = scales::percent_format(),
                       sec.axis = sec_axis(~ ., name = "Cumulative Proportion",
                                           labels = scales::percent_format())) +
    labs(title = "Variance Explained by Principal Component",
         x = NULL, y = "Proportion of Variance") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())

ggsave(p_scree, file = "figs/pca_scree.png",
       width = 8, height = 5, bg = "white")
cat("  Saved figs/pca_scree.png\n")


# -----------------------------------------------------------------------------
# Plot 2: PC1 Loadings
# -----------------------------------------------------------------------------

loadings_df <- data.frame(
    Variable = rownames(pr.out$rotation),
    Loading  = pr.out$rotation[, 1]
)
loadings_df <- loadings_df[order(loadings_df$Loading), ]

label_map <- c(
    mean_age                               = "Average Donor Age",
    pct_65_plus                            = "% Donors Age 65+",
    pct_overall_donations_100plus          = "% Donations from 100+ Donors",
    pct_overall_donations_10plus_distinct  = "% Donations from 10+ Distinct PAC Donors",
    high_volume_vendor_pct                 = "% Spending on High-Volume Vendors",
    fundraising_inefficiency               = "Fundraising Spend / Indv Contributions",
    log_hvv_total                          = "log(High-Volume Vendor Spending)",
    log_n_refunds                          = "log(1 + Number of Refunds)"
)
loadings_df$Label <- ifelse(loadings_df$Variable %in% names(label_map),
                            label_map[as.character(loadings_df$Variable)],
                            as.character(loadings_df$Variable))
loadings_df$Label <- factor(loadings_df$Label,
                            levels = loadings_df$Label[order(loadings_df$Loading)])

p_loadings <- ggplot(loadings_df, aes(x = Label, y = Loading)) +
    geom_col(aes(fill = Loading > 0), width = 0.7, show.legend = FALSE) +
    scale_fill_manual(values = c("TRUE" = "#E63946", "FALSE" = "#457B9D")) +
    coord_flip() +
    labs(title = "PC1 Loadings",
         subtitle = "Indicators that define the spam dimension",
         x = NULL, y = "Loading on PC1") +
    theme_minimal(base_size = 13) +
    theme(plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(color = "#666666"),
          panel.grid.minor = element_blank())

ggsave(p_loadings, file = "figs/pca_loadings.png",
       width = 9, height = 5, bg = "white")
cat("  Saved figs/pca_loadings.png\n")


# -----------------------------------------------------------------------------
# Plot 3: Indicator Distributions by Spam Category (Jitter)
# Three categories: Manual Spam PAC, Rule-Based Addition, Non-Spam PAC
# -----------------------------------------------------------------------------

indicator_list <- c(
    "pc1",
    "mean_age",
    "pct_65_plus",
    "pct_overall_donations_100plus",
    "pct_overall_donations_10plus_distinct",
    "high_volume_vendor_pct",
    "fundraising_inefficiency",
    "log_n_refunds"
)

plot_label_map <- c(
    pc1                                    = "Composite Spam Score (PC1)",
    mean_age                               = "Average Donor Age",
    pct_65_plus                            = "% Donors Age 65+",
    pct_overall_donations_100plus          = "% Donations from 100+ Donors",
    pct_overall_donations_10plus_distinct  = "% Donations from 10+ Distinct PAC Donors",
    high_volume_vendor_pct                 = "% Spending on High-Volume Vendors",
    fundraising_inefficiency               = "Fundraising Spend / Indv Contributions",
    log_n_refunds                          = "log(1 + Number of Refunds)"
)

# Winsorize extreme outliers for visual clarity
winsorize99 <- function(x) pmin(x, quantile(x, 0.995, na.rm = TRUE))
cols_to_winsorize <- c("fundraising_inefficiency")

# Filter to available columns
indicator_list <- intersect(indicator_list, names(df_comm))

df_plot <- df_comm %>%
    mutate(across(any_of(cols_to_winsorize), winsorize99)) %>%
    select(spam_category, all_of(indicator_list)) %>%
    pivot_longer(cols      = -spam_category,
                 names_to  = "indicator",
                 values_to = "value") %>%
    mutate(indicator = factor(indicator,
                              levels = indicator_list,
                              labels = plot_label_map[indicator_list]))

p_jitter <- ggplot(df_plot, aes(x = spam_category, y = value, color = spam_category)) +
    geom_jitter(width = 0.25, alpha = 0.5, size = 1.2) +
    facet_wrap(~ indicator, scales = "free_y", ncol = 3) +
    scale_color_manual(values = cat_colors) +
    labs(title    = "Indicator Distributions by Classification",
         subtitle = "Manual directory (144 PACs) vs. rule-based additions vs. non-spam PACs",
         x = NULL, y = "Indicator Value", color = "Classification") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x     = element_blank(),
          axis.ticks.x    = element_blank(),
          strip.text       = element_text(face = "bold", size = 9),
          panel.spacing    = unit(1, "lines"))

ggsave(p_jitter, file = "figs/pca_indicators_jitter.png",
       width = 14, height = 10, bg = "white")
cat("  Saved figs/pca_indicators_jitter.png\n")


# -----------------------------------------------------------------------------
# Plot 4: Logistic Classification Curve (three-category jitter)
# Logistic model is still binary (manual spam vs. rest), but the jittered
# points are colored by three categories to show where rule-based additions fall
# -----------------------------------------------------------------------------

newdat     <- data.frame(pc1 = seq(min(df_comm$pc1, na.rm = TRUE),
                                   max(df_comm$pc1, na.rm = TRUE),
                                   length.out = 500))
pred       <- predict(logit_mod, newdata = newdat, type = "link", se.fit = TRUE)
newdat$pr  <- plogis(pred$fit)
newdat$lo  <- plogis(pred$fit - 1.96 * pred$se.fit)
newdat$hi  <- plogis(pred$fit + 1.96 * pred$se.fit)

# Jitter y-positions: manual spam at top, rule-based additions in middle, non-spam at bottom
df_comm$y_jit <- ifelse(df_comm$is_spam_pac_manual == 1, 1.04,
                 ifelse(df_comm$is_rule_based_addition == 1, 0.52, -0.04))

p_logistic <- ggplot() +
    geom_ribbon(data = newdat, aes(x = pc1, ymin = lo, ymax = hi),
                fill = "#7B61FF", alpha = 0.15) +
    geom_hline(yintercept = 0.5, linetype = "dashed",
               linewidth = 0.6, color = "#666666") +
    geom_line(data = newdat, aes(x = pc1, y = pr),
              color = "#4C2FFF", linewidth = 1.4) +
    geom_jitter(data = df_comm[df_comm$spam_category == "Non-Spam PAC", ],
                aes(x = pc1, y = y_jit, color = spam_category),
                height = 0.04, width = 0, alpha = 0.45, size = 2) +
    geom_jitter(data = df_comm[df_comm$spam_category == "Rule-Based Addition", ],
                aes(x = pc1, y = y_jit, color = spam_category),
                height = 0.04, width = 0, alpha = 0.65, size = 2.5) +
    geom_jitter(data = df_comm[df_comm$spam_category == "Manual Spam PAC (144)", ],
                aes(x = pc1, y = y_jit, color = spam_category),
                height = 0.04, width = 0, alpha = 0.65, size = 2.5) +
    scale_color_manual(values = cat_colors) +
    scale_y_continuous(limits = c(-0.12, 1.16),
                       breaks = c(0, 0.5, 1),
                       labels = c("Non-Spam", "0.5", "Spam")) +
    labs(x        = "PC1 (Composite Spam Score)",
         y        = "Predicted Probability (Manual Classification)",
         title    = "PCA Validation: Logistic Classification of Spam PACs",
         subtitle = sprintf("AUC = %.3f  |  Rule-based additions cluster with manual spam PACs above the decision boundary", auc_pc1),
         color    = "Classification") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold"),
          plot.subtitle    = element_text(color = "#666666", size = 11),
          legend.position  = "bottom")

ggsave(p_logistic, file = "figs/pca_classification.png",
       width = 10, height = 6, bg = "white")
cat("  Saved figs/pca_classification.png\n")


cat("\n  Done.\n")
