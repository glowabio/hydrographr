# =============================================================================
# Quality check of the prediction table
# =============================================================================

# Inspect variable names
names(predict_table)

# Check number of sub-catchments matches expected
ifelse(
  nrow(predict_table) == nrow(subc_ids),
  "The number of rows in the prediction table matches the number of sub-catchments.",
  "Mismatch detected: the number of sub-catchments and prediction table rows differ."
)

# Check for missing values in each column
colSums(is.na(predict_table))

# Simplify climate variable names for readability
colnames(predict_table) <- gsub("_1981-2010_observed", "", colnames(predict_table))

# =============================================================================
# Collinearity assessment
# =============================================================================

# Drop near-zero variance land cover classes not expected in Greek freshwater
# subcatchments (mangroves, moss/lichen, sparse vegetation, water bodies as cover)
lc_drop <- c("c130_y2020", "c150_y2020", "c190_y2020", "c200_y2020")

predict_table <- predict_table %>%
  dplyr::select(-any_of(lc_drop))

# Select numeric predictor variables (mean values only)
# - ends_with("_mean"): climate and hydrography aggregated means
# - ends_with("2020"): land cover proportional cover per subcatchment
numeric_vars <- predict_table %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(ends_with("_mean") | ends_with("2020"))

# Compute and visualize pairwise correlation matrix
# High correlations (|r| > 0.7) indicate potential redundancy
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "lower",
         tl.cex = 0.6, diag = FALSE,
         col = COL2("RdBu", 200))  # diverging palette: blue = negative, red = positive

# -----------------------------------------------------------------------------
# VIF-based variable selection
# Sequentially removes the variable with the highest VIF until all
# remaining predictors fall below the threshold (th = 10)
# -----------------------------------------------------------------------------
vif_results <- vifstep(numeric_vars, th = 10)

# Inspect retained variables and their final VIF values
vif_results@results

selected_vars <- vif_results@results$Variables
selected_vars

# =============================================================================
# Subset prediction table to non-collinear predictors
# =============================================================================
predict_table <- predict_table %>%
  dplyr::select(any_of(c("subcID", selected_vars)))

# Final check
names(predict_table)
