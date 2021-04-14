#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 26, 2021
# Purpose:       3.0 EDA - exploratory data analysis
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  This script conducts some initial exploratory data analysis including:
#       - Identification of most- and least-populated municipalities in the
#         top-quartile of CINEP-reported FARC activities.
#       - Estimation of Cohen's Kappa yearly, in grouped years, and in a
#         cross-section.
#       - Confusion matrices and summary statistics for all dependent and
#         independent variables.
#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ADMINISTRATIVE                                                          ----
#-----------------------------------------------------------------------------#

#---------------------------#
# Clear working environment
#---------------------------#
rm(list = ls())

#---------------------------#
# Load required packages
#---------------------------#
library(tidyverse)
library(sf)
library(kableExtra)

#---------------------------#
# Load data
#---------------------------#
load("data/data_variables.RData")

#---------------------------#
# Functions
#---------------------------#
source("Scripts/Functions/kappa_cm.R")


#-----------------------------------------------------------------------------#
# SETUP                                                                   ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Create data string for plot filenames
# ----------------------------------- #
d <- str_remove_all(Sys.Date(),"-")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# FARC - MOST ACTIVE MUNICIPALITIES                                       ----
#-----------------------------------------------------------------------------#
# Identify municipalities in data with the highest numbers of FARC events as
# reported in CINEP data.

# ----------------------------------- #
# Subset and tidy data for mapping
# ----------------------------------- #
active_farc <- colombia_pn %>%
  st_drop_geometry() %>%
  mutate(popden = pop_sum / area_km2) %>%
  group_by(Department, Municipality) %>%
  summarize(cinep   = sum(cinep),
            icews   = sum(icews),
            ged     = sum(ged),
            popden  = mean(popden),
            capdist = distance_bogota_km[1],
            .groups = "keep") %>%
  ungroup() %>%
  filter(cinep > 5) %>%
  arrange(capdist) %>%
  slice(1:5, (n()-4):n()) %>%
  mutate(rank = 1:n()) %>%
  mutate(
    x = as_factor(case_when(rank %in% 1:5 ~ "Journalistic Proximity",
                            TRUE ~ "Journalistic Remoteness")),
    nx = case_when(Municipality %in% c("Ovejas", "San Luis","Alvarado","Dolores",
                                       "El Carmen de Bolivar")~ -1e6,
                   TRUE ~ 1e6),
    ny = case_when(Municipality %in% c("El Carmen de Bolivar", "Valledupar",
                                       "Tibu","Arauca") ~ 1e6,
                   Municipality == "Alvarado" ~ -1e6,
                   Municipality == "Samana" ~ -6e5,
                   Municipality == "San Francisco" ~ -2e5,
                   Municipality == "Dolores" ~ -1.5e6,
                   TRUE ~ 0),
   LABEL = Municipality) %>%
  mutate(nx = case_when(Municipality %in% c("San Francisco", "Samana") ~ -2e6,
                        TRUE ~ nx))
# ----------------------------------- #


# ----------------------------------- #
# Most active - table
# ----------------------------------- #
active_farc %>%
  select(c(Department:capdist,x)) %>%
  kbl(format = "pipe", digits = 2,
      col.names = c("Department","Municipality","CINEP","ICEWS","GED",
                    "Pop. Density","Capital Dist.", "Class"))

# active_farc %>%
#   select(c(Department:capdist,x)) %>%
#   kbl(format = "latex", digits = 2,
#       col.names = c("Department","Municipality","CINEP","ICEWS","GED",
#                     "Pop. Density","Capital Dist.", "Class")) %>%
#   kable_classic_2(full_width = F) %>%
#   cat(., file = "Plots/EDA/CINEP-top-10-mun-table.txt")

# active_farc %>%
#   select(c(Department:capdist,x)) %>%
#   kbl(format = "html", digits = 2,
#       col.names = c("Department","Municipality","CINEP","ICEWS","GED",
#                     "Pop. Density","Capital Dist.", "Class")) %>%
#   kable_classic_2(full_width = F) %>%
#   save_kable(file = "Plots/EDA/CINEP-top-10-mun-table.html")
# ----------------------------------- #


# ----------------------------------- #
# Most active - map
# ----------------------------------- #
active_farc <- colombia_pn %>%
  filter(year == 2002) %>%
  select(Department, Municipality) %>%
  left_join(., active_farc, by = c("Department", "Municipality"))


bogota <- st_sf(data.frame(Name = "Bogota",
                geom = st_sfc(st_point(c(-74.09854840698665,4.647941523654987)),
                                 crs = "+proj=longlat"))) %>%
  st_transform(., crs = st_crs(active_farc))


mp <- ggplot(data = active_farc) +
  geom_sf(aes(fill = x), color = "gray50", size = 0.05) +
  geom_sf(data = bogota, shape = "\u2b50", size = 10, aes(fill = "Bogota")) +
  scale_fill_manual(values       = c("white","#00bfc4", "#f8766d"),
                    na.value     = "transparent",
                    na.translate = FALSE) +
  guides(fill = guide_legend(override.aes = list(shape    = c("", "", "\u2b50"),
                                                 size     = rep((10 / .pt),3),
                                                 linetype = rep("blank",3) ),
                             nrow    = 2,
                             byrow   = TRUE,
                             reverse = TRUE) ) +
  scale_x_continuous(limits = c(-2.8e6,-6.5e5)) +
  scale_y_continuous(limits = c(3157019.2, 5e6)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        plot.title       = element_text(size = (12 / .pt), hjust = 0),
        plot.subtitle    = element_text(size = (10 / .pt))) +
  # labs(title = "Remoteness: Distance for international journalists traveling from Bogota",
       # subtitle = "Top 10 Colombain municipalities based on CINEP-reported FARC activities") +
  ggrepel::geom_label_repel(
    aes(label = LABEL, geometry = geometry),
    label.size = NA,
    fill = "transparent",
    size = (10 / .pt),
    segment.size = 0.25,
    segment.color = "gray40",
    segment.linetype = "dashed",
    stat = "sf_coordinates",
    nudge_x = active_farc$nx,
    nudge_y = active_farc$ny,
    na.rm   = TRUE,
    box.padding = .25,
    force = 1
  )

# mp
ggsave(filename = sprintf("Plots/EDA/CINEP-top-10-mun-map-%s.png",d),
       plot     = mp,
       units    = "in",
       width    = 5.0,
       height   = 5.5,
       dpi      = 340)

# saving also as an rdata obj to "results/table" dir to see how it looks in pdf
# save(mp, file = "Results/Tables/CO-Map.Rdata")

# So... of course latex does not support the unicode star in the ggplot object.
ggsave(filename = "Results/Tables/Map.png",
       plot     = mp,
       units    = "in",
       width    = 4.0,
       height   = 4.0,
       dpi      = 340)
# ----------------------------------- #

rm(active_farc, mp)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SUMMARY STATISTICS                                                      ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Summary Statistics - Yearly
# ----------------------------------- #
tmp <- colombia_pn %>%
  st_drop_geometry() %>%
  select(year, cinep, cinep_bin, icews, icews_bin, ged, ged_bin) %>%
  group_by(year) %>%
  summarize(MEAN_CINEP     = mean(cinep),
            MEAN_CINEP.BIN = mean(cinep_bin),
            MIN_CINEP      = min(cinep),
            MIN_CINEP.BIN  = min(cinep_bin),
            MAX_CINEP      = max(cinep),
            MAX_CINEP.BIN  = max(cinep_bin),
            SD_CINEP       = sd(cinep),
            SD_CINEP.BIN   = sd(cinep_bin),

            MEAN_ICEWS     = mean(icews),
            MEAN_ICEWS.BIN = mean(icews_bin),
            MIN_ICEWS      = min(icews),
            MIN_ICEWS.BIN  = min(icews_bin),
            MAX_ICEWS      = max(icews),
            MAX_ICEWS.BIN  = max(icews_bin),
            SD_ICEWS       = sd(icews),
            SD_ICEWS.BIN   = sd(icews_bin),

            MEAN_GED     = mean(ged),
            MEAN_GED.BIN = mean(ged_bin),
            MIN_GED      = min(ged),
            MIN_GED.BIN  = min(ged_bin),
            MAX_GED      = max(ged),
            MAX_GED.BIN  = max(ged_bin),
            SD_GED       = sd(ged),
            SD_GED.BIN   = sd(ged_bin)
            ) %>%
  pivot_longer(.,
               cols = !contains("year"),
               names_to = c("stat","var"),
               names_sep = "_",
               values_to = "vals") %>%
  pivot_wider(.,
              names_from = "stat",
              values_from = "vals")

# To arrange by counts and binary outcomes separately
tmp <- tmp %>%
  mutate(bin = case_when(str_detect(var, "BIN") ~ 1, TRUE ~ 0)) %>%
  arrange(year, bin, var)

tmp[,2:6] %>%
  kbl(digits = 3) %>%
  kable_styling(bootstrap_options = "striped", full_width = TRUE) %>%
  pack_rows(index = table(tmp$year))

rm(tmp)
# ----------------------------------- #


# ----------------------------------- #
# Summary Statistics - Grouped years
# ----------------------------------- #
tmp <- colombia_yg %>%
  st_drop_geometry() %>%
  select(year_grouped, cinep, cinep_bin, icews, icews_bin, ged, ged_bin) %>%
  group_by(year_grouped) %>%
  summarize(MEAN_CINEP     = mean(cinep),
            MEAN_CINEP.BIN = mean(cinep_bin),
            MIN_CINEP      = min(cinep),
            MIN_CINEP.BIN  = min(cinep_bin),
            MAX_CINEP      = max(cinep),
            MAX_CINEP.BIN  = max(cinep_bin),
            SD_CINEP       = sd(cinep),
            SD_CINEP.BIN   = sd(cinep_bin),

            MEAN_ICEWS     = mean(icews),
            MEAN_ICEWS.BIN = mean(icews_bin),
            MIN_ICEWS      = min(icews),
            MIN_ICEWS.BIN  = min(icews_bin),
            MAX_ICEWS      = max(icews),
            MAX_ICEWS.BIN  = max(icews_bin),
            SD_ICEWS       = sd(icews),
            SD_ICEWS.BIN   = sd(icews_bin),

            MEAN_GED     = mean(ged),
            MEAN_GED.BIN = mean(ged_bin),
            MIN_GED      = min(ged),
            MIN_GED.BIN  = min(ged_bin),
            MAX_GED      = max(ged),
            MAX_GED.BIN  = max(ged_bin),
            SD_GED       = sd(ged),
            SD_GED.BIN   = sd(ged_bin)
  ) %>%
  pivot_longer(.,
               cols = !contains("year_grouped"),
               names_to = c("stat","var"),
               names_sep = "_",
               values_to = "vals") %>%
  pivot_wider(.,
              names_from = "stat",
              values_from = "vals")

# To arrange by counts and binary outcomes separately
tmp <- tmp %>%
  mutate(bin = case_when(str_detect(var, "BIN") ~ 1, TRUE ~ 0)) %>%
  arrange(year_grouped, bin, var)

tmp[,2:6] %>%
  kbl(digits = 3) %>%
  kable_styling(bootstrap_options = "striped", full_width = TRUE) %>%
  pack_rows(index = table(tmp$year_grouped))

rm(tmp)
# ----------------------------------- #


# ----------------------------------- #
# Summary Statistics - Cross section
# ----------------------------------- #
tmp <- colombia_cs %>%
  st_drop_geometry() %>%
  select(cinep, cinep_bin, icews, icews_bin, ged, ged_bin) %>%
  summarize(MEAN_CINEP     = mean(cinep),
            MEAN_CINEP.BIN = mean(cinep_bin),
            MIN_CINEP      = min(cinep),
            MIN_CINEP.BIN  = min(cinep_bin),
            MAX_CINEP      = max(cinep),
            MAX_CINEP.BIN  = max(cinep_bin),
            SD_CINEP       = sd(cinep),
            SD_CINEP.BIN   = sd(cinep_bin),

            MEAN_ICEWS     = mean(icews),
            MEAN_ICEWS.BIN = mean(icews_bin),
            MIN_ICEWS      = min(icews),
            MIN_ICEWS.BIN  = min(icews_bin),
            MAX_ICEWS      = max(icews),
            MAX_ICEWS.BIN  = max(icews_bin),
            SD_ICEWS       = sd(icews),
            SD_ICEWS.BIN   = sd(icews_bin),

            MEAN_GED     = mean(ged),
            MEAN_GED.BIN = mean(ged_bin),
            MIN_GED      = min(ged),
            MIN_GED.BIN  = min(ged_bin),
            MAX_GED      = max(ged),
            MAX_GED.BIN  = max(ged_bin),
            SD_GED       = sd(ged),
            SD_GED.BIN   = sd(ged_bin)
  ) %>%
  pivot_longer(.,
               cols = everything(),
               names_to = c("stat","var"),
               names_sep = "_",
               values_to = "vals") %>%
  pivot_wider(.,
              names_from = "stat",
              values_from = "vals")

# To arrange by counts and binary outcomes separately
tmp <- tmp %>%
  mutate(bin = case_when(str_detect(var, "BIN") ~ 1, TRUE ~ 0)) %>%
  arrange(bin, var)

tmp[,1:5] %>%
  kbl(digits = 3) %>%
  kable_styling(bootstrap_options = "striped", full_width = TRUE)

rm(tmp)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CLEAN-UP                                                                ----
#-----------------------------------------------------------------------------#
rm(list = ls())
#-----------------------------------------------------------------------------#




