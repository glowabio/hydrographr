#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_pci_figures.R
#
# Figure for the Population Connectivity Index results (seven Sarantaporos
# fish species): Fragmentation Index per species (ranked bar chart),
# coloured by dispersal/migration tier (0.3 / 0.6 / 0.9).
#
# NOTE: a current-vs-future PCI scatter was dropped because PCI under the
# current scenario is ~1.0 for every species (the single existing dam
# leaves the network near-fully connected), so the comparison carries no
# information on the current axis. The Fragmentation Index already encodes
# the connectivity loss caused by the planned dams.
#
# REQUIRES
#   tidyverse, data.table
#
# INPUT
#   connectivity/pci/fi_summary.txt        (species, PCI_current, PCI_future, FI)
#   traits/fish_dis_class.txt              (dispersal_prob, Migration_label)
#
# OUTPUT
#   connectivity/pci/fig_fi_barplot.png
#
# LOCATION
#   workflows/<connectivity_module>/03_pci_figures.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(data.table)

# ============================================================
# FIX: prevent MASS::select etc. from masking dplyr verbs
# ============================================================
select   <- dplyr::select
rename   <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- BASE_DIR
setwd(BASE_DIR)

# ============================================================
# READ INPUTS
# ============================================================
fi_summary <- fread("connectivity/pci/fi_summary.txt")

fish_dis_class <- fread("traits/fish_dis_class.txt") %>%
  dplyr::select(species, dispersal_prob, Migration_label)

fi_plot <- fi_summary %>%
  dplyr::left_join(fish_dis_class, by = "species") %>%
  dplyr::mutate(
    species_lab = gsub("_", " ", species),
    # tier label keyed to dispersal_prob (set in 01_dispersal_estimation.R)
    tier = factor(
      dplyr::case_when(
        dispersal_prob == 0.3 ~ "Non-migratory (0.3)",
        dispersal_prob == 0.6 ~ "Potamodromous (0.6)",
        dispersal_prob == 0.9 ~ "Long-distance (0.9)",
        TRUE ~ "Other"
      ),
      levels = c("Non-migratory (0.3)", "Potamodromous (0.6)", "Long-distance (0.9)")
    )
  )

# Tier colours: soft pastel qualitative set (neutral, non-semantic).
# Pink / lilac / grey-blue, low -> high dispersal.
tier_cols <- c("Non-migratory (0.3)" = "#F4C7D9",   # soft pink
               "Potamodromous (0.6)" = "#CDB7E0",   # lilac
               "Long-distance (0.9)" = "#A9C4D9")   # grey-blue
pal <- brewer.pal(3, "BuGn")



# ============================================================
# FIGURE: Fragmentation Index per species
# ============================================================
p_fi <- ggplot(fi_plot,
               aes(x = reorder(species_lab, FI), y = FI, fill = tier)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", FI)),
            hjust = -0.15, size = 3) +
  coord_flip() +
  scale_fill_manual(values = pal, name = "Dispersal tier", drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Fragmentation Index (% connectivity loss)") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"),
        legend.position = "top")

# ============================================================
# WRITE  (png() per project convention, not ggsave)
# ============================================================
dir.create("connectivity/pci", recursive = TRUE, showWarnings = FALSE)

png("connectivity/pci/fig_fi_barplot.png",
    width = 1900, height = 1200, res = 300)
print(p_fi)
dev.off()

message("Figure written: connectivity/pci/fig_fi_barplot.png")
