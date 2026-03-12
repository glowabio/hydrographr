#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_dispersal_classification.R
# Estimate species dispersal distances using fishmove,
# bin into quantile classes, assign dispersal probability (PD)
#
# Strategy:
#   1. Run fishmove(L, AR, SO, T) for 11 species with known aspect ratio
#   2. Fit model: log(sigma_mob) ~ log(TL) + sqrt(SO) to predict dispersal
#      for species without AR data (R² = 0.985)
#   3. Classify all species into 7 dispersal classes (PD = 0.3 to 0.9)
#
# References:
#   Radinger & Wolter (2014) Patterns and predictors of fish dispersal in rivers
#   Baldan et al. (2022) riverconn: an R package to assess river connectivity
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(readxl)
library(data.table)
# devtools::install_github("cran/fishmove")
library(fishmove)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
T_REF          <- 3650   # 10-year dispersal window (population-level; Mekong study)
SO_DEFAULT     <- 5      # default stream order for species without occurrence data
N_DISP_CLASSES <- 7      # number of dispersal classes
PD_MIN         <- 0.3    # minimum dispersal probability
PD_MAX         <- 0.9    # maximum dispersal probability


# ============================================================
# STEP 1: Read trait data
# ============================================================

fish_traits <- read_xlsx(
  "points_original/fish/Fish distributional & traits data (1).xlsx",
  sheet = "Traits"
) %>%
  dplyr::rename(species = Species) %>%
  dplyr::select(species, max_TL, Caudal_fin, Migration) %>%
  dplyr::mutate(
    Caudal_fin_label = case_when(
      Caudal_fin == 1 ~ "Rounded",
      Caudal_fin == 2 ~ "Truncated",
      Caudal_fin == 3 ~ "Emarginate",
      Caudal_fin == 4 ~ "Forked",
      Caudal_fin == 5 ~ "Heterocercal",
      Caudal_fin == 6 ~ "Pointed",
      TRUE ~ "Unknown"
    ),
    Migration_label = case_when(
      Migration == 0 ~ "Non-migratory",
      Migration == 1 ~ "Potamodromous",
      Migration == 2 ~ "Long_distance",
      TRUE ~ "Unknown"
    )
  )

message("Trait data: ", nrow(fish_traits), " species")


# ============================================================
# STEP 2: Get species-specific stream order from occurrences
# ============================================================

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv")

species_stream_order <- occurrences %>%
  group_by(species) %>%
  dplyr::summarise(
    max_SO = max(strahler, na.rm = TRUE),
    n_obs  = n(),
    .groups = "drop"
  )

fish_traits <- fish_traits %>%
  left_join(species_stream_order, by = "species")

# Fill missing stream order with default
missing_SO <- fish_traits %>% filter(is.na(max_SO))
if (nrow(missing_SO) > 0) {
  message("WARNING: ", nrow(missing_SO), " species have no stream order data. Using default SO=", SO_DEFAULT)
  fish_traits <- fish_traits %>%
    mutate(max_SO = ifelse(is.na(max_SO), SO_DEFAULT, max_SO))
}


# ============================================================
# STEP 3: Read aspect ratio data and average duplicates
# ============================================================

ar_data <- read.csv("traits/AspectRatioData.csv") %>%
  group_by(species) %>%
  summarize(AR = mean(AR, na.rm = TRUE), .groups = "drop") %>%
  mutate(species = gsub(" ", "_", species))

# Handle name mismatch
ar_data <- ar_data %>%
  mutate(species = ifelse(species == "Gasterosteus_aculeatus",
                          "Gasterosteus_gymnurus", species))

message("AR data for ", nrow(ar_data), " species")


# ============================================================
# STEP 4: Run fishmove for calibration species (with AR)
# ============================================================

calibration <- fish_traits %>%
  inner_join(ar_data, by = "species")

message("Calibration species (with TL + AR): ", nrow(calibration))

fm_results <- data.frame()

for (i in 1:nrow(calibration)) {
  sp    <- calibration$species[i]
  tl_mm <- calibration$max_TL[i] * 10  # fishmove expects mm
  ar    <- calibration$AR[i]
  so    <- calibration$max_SO[i]

  cat("  fishmove:", sp, "(L=", tl_mm, "mm, AR=", round(ar, 2), ", SO=", so, ")\n")

  tryCatch({
    fm <- fishmove(L = tl_mm, AR = ar, SO = so, T = T_REF,
                   rep = 200, seed = 42)

    fm_results <- rbind(fm_results, data.frame(
      species      = sp,
      sigma_mob_m  = fm$pred.fishmove["fit", "sigma_mob", , , , ]
    ))
  }, error = function(e) cat("    ERROR:", conditionMessage(e), "\n"))
}


# ============================================================
# STEP 5: Fit predictive model for species without AR
# Model: log(sigma_mob) ~ log(TL) + sqrt(SO)
# This mirrors fishmove's internal regression structure minus AR
# ============================================================

cal_data <- fm_results %>%
  left_join(fish_traits, by = "species") %>%
  mutate(log_sigma_mob = log(sigma_mob_m),
         log_TL  = log(max_TL),
         sqrt_SO = sqrt(max_SO))

best_model <- lm(log_sigma_mob ~ log_TL + sqrt_SO, data = cal_data)

cat("\n--- Predictive model summary ---\n")
print(summary(best_model))


# ============================================================
# STEP 6: Predict dispersal for ALL species
# ============================================================

predict_data <- fish_traits %>%
  mutate(log_TL  = log(max_TL),
         sqrt_SO = sqrt(max_SO))

predictions <- predict(best_model, newdata = predict_data,
                       interval = "prediction", level = 0.95)

fish_traits <- fish_traits %>%
  mutate(
    sigma_mob_predicted = exp(predictions[, "fit"]),
    sigma_mob_lwr       = exp(predictions[, "lwr"]),
    sigma_mob_upr       = exp(predictions[, "upr"]),
    # Use fishmove value for calibration species, prediction for the rest
    sigma_mob_final = ifelse(
      species %in% fm_results$species,
      fm_results$sigma_mob_m[match(species, fm_results$species)],
      sigma_mob_predicted
    ),
    source = ifelse(species %in% fm_results$species,
                    "AR available", "AR unavailable")
  )


# ============================================================
# STEP 7: Bin into dispersal classes (Mekong method)
# 7 quantile-based classes mapped to PD = 0.3 to 0.9
# ============================================================

quantile_breaks <- quantile(fish_traits$sigma_mob_final,
                            probs = seq(0, 1, length.out = N_DISP_CLASSES + 1),
                            na.rm = TRUE)

pd_values <- seq(PD_MIN, PD_MAX, length.out = N_DISP_CLASSES)

fish_traits <- fish_traits %>%
  mutate(
    dispersal_class = cut(sigma_mob_final,
                          breaks = quantile_breaks,
                          labels = FALSE,
                          include.lowest = TRUE),
    dispersal_prob = pd_values[dispersal_class]
  )


# ============================================================
# STEP 8: Save output
# ============================================================

fish_dis_class <- fish_traits %>%
  dplyr::select(
    species,
    distance = sigma_mob_final,
    dispersal_prob,
    dispersal_class,
    source,
    max_TL,
    max_SO,
    Migration_label,
    Caudal_fin_label
  ) %>%
  arrange(species)

fwrite(fish_dis_class, "traits/fish_dis_class.txt", sep = "\t")
message("\nfish_dis_class.txt saved")


# ============================================================
# STEP 9: Diagnostic plots
# ============================================================

# Plot 1: Calibration -- fishmove (with AR) vs predicted (without AR)
cal_check <- cal_data %>%
  mutate(predicted = exp(predict(best_model)))

p1 <- ggplot(cal_check, aes(x = sigma_mob_m, y = predicted)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = species), hjust = -0.1, vjust = -0.5, size = 2.5) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "sigma_mob (m) — with AR",
       y = "sigma_mob (m) — without AR",
       title = "Calibration: with vs without Aspect Ratio") +
  theme_bw()

# Plot 2: All species dispersal by migration type and data source
p2 <- ggplot(fish_dis_class, aes(x = max_TL, y = distance / 1000,
                                 color = Migration_label, shape = source)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10() + scale_x_log10() +
  labs(x = "Max total length (cm)",
       y = "Dispersal distance (km, log scale)",
       color = "Migration",
       title = "Dispersal estimates for all Greek freshwater fish") +
  theme_bw()

cowplot::plot_grid(p1, p2, ncol = 1, align = "v") %>%
  ggsave("traits/dispersal_diagnostics.png", ., width = 10, height = 12)

cat("\nPlots saved: traits/dispersal_diagnostics.png\n")


# ============================================================
# STEP 10: Summary
# ============================================================

cat("\n========== SUMMARY ==========\n")
cat("Calibration species (AR available):", sum(fish_dis_class$source == "AR available"), "\n")
cat("Predicted species (AR unavailable):", sum(fish_dis_class$source == "AR unavailable"), "\n\n")

cat("Dispersal distance (km) by migration type:\n")
fish_dis_class %>%
  group_by(Migration_label) %>%
  summarize(
    n = n(),
    median_km = round(median(distance / 1000), 1),
    min_km = round(min(distance / 1000), 1),
    max_km = round(max(distance / 1000), 1),
    .groups = "drop"
  ) %>% print()

cat("\nDispersal probability distribution:\n")
print(table(fish_dis_class$dispersal_prob))
