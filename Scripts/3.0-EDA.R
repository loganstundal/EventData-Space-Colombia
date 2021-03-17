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
            .groups = "keep")

# quantile(active_farc$cinep, probs = 0.75)

active_farc <- active_farc %>%
  ungroup() %>%
  filter(cinep > 10) %>%
  # arrange(desc(popden)) %>%
  arrange(capdist) %>%

  # Slice top and bottom 5
  slice(1:5, (n()-4):n()) %>%
  mutate(rank = 1:n()) %>%
  mutate(

         # ----------------------------------- #
         # Remoteness via "distance - Bogota"
         # ----------------------------------- #
         # x = as_factor(case_when(rank %in% 1:5 ~ "Closest",
         #                         TRUE ~ "Furthest")),
         x = as_factor(case_when(rank %in% 1:5 ~ "Journalistically Near",
                                 TRUE ~ "Journalistically Remote")),
         nx    = case_when(Municipality %in% c("El Carmen de Bolivar",
                                               "Tibu",
                                               "Tame") ~ 1e6,
                           TRUE ~ -5e5),
         ny   = case_when(Municipality %in% c("El Carmen de Bolivar",
                                              "Tibu",
                                              "Tame",
                                              "Medio Atrato",
                                              "San Carlos",
                                              "Dabeiba",
                                              "Samana") ~ 1.25e6,
                          TRUE ~ -1e6),
         # ----------------------------------- #


         # ----------------------------------- #
         # Remoteness via "population density
         # ----------------------------------- #
         # x = as_factor(case_when(rank %in% 1:5 ~ "Highest Pop. Den.",
         #                         TRUE ~ "Lowest Pop, Den.")),
         # Nudge factors for map (assigned values in meters)
         # nx    = case_when(Municipality %in% c("El Carmen de Bolivar",
         #                                       "Tibu",
         #                                       "Tame",
         #                                       "San Jose Guaviare") ~ 1e6,
         #                   TRUE ~ -5e5),
         # ny   = case_when(Municipality %in% c("El Carmen de Bolivar",
         #                                      "Tibu",
         #                                      "Tame",
         #                                      "Medio Atrato",
         #                                      "San Carlos",
         #                                      "Samana") ~ 1e6,
         #                  TRUE ~ -1e6),
         # ----------------------------------- #

         LABEL = Municipality,

         ) %>%
  mutate(nx = case_when(Municipality == "San Vicente del Caguan" ~ 5e5,
                        TRUE ~ nx))
  # mutate(
  #        # Distance - Bogota
  #       ny = case_when(Municipality == "Vista Hermosa" ~ -1.2e6,
  #                      Municipality == "Acevedo" ~ -8e5,
  #                      Municipality == "Samana" ~ 1.15e6,
  #                      TRUE ~ ny),
  #       nx = case_when(Municipality == "Samana" ~ -3e5,
  #                      TRUE ~ nx)

         # Population density
         # ny = case_when(Municipality == "Vista Hermosa" ~ -1.2e6,
         #                Municipality == "Acevedo" ~ -8e5,
         #                Municipality == "Samana" ~ 1.15e6,
         #                TRUE ~ ny),
         # nx = case_when(Municipality == "Samana" ~ -3e5,
         #                TRUE ~ nx)

         # )
# ----------------------------------- #


# ----------------------------------- #
# Most active - table
# ----------------------------------- #
active_farc[,1:6] %>% kbl(format = "pipe", digits = 2)

active_farc[,1:7] %>% kbl(format = "html", digits = 2) %>%
  save_kable("Plots/EDA/cineptmp.html")
# ----------------------------------- #


# ----------------------------------- #
# Most active - map
# ----------------------------------- #
active_farc <- colombia_pn %>%
  filter(year == 2002) %>%
  select(Department, Municipality) %>%
  left_join(., active_farc, by = c("Department", "Municipality"))

mp <- ggplot(data = active_farc) +
  geom_sf(aes(fill = x), color = "gray10", size = 0.05) +
  scale_fill_discrete(na.value = "transparent") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank(),
        axis.title = element_blank()) +
  scale_fill_discrete(na.translate = FALSE) +
  ggrepel::geom_label_repel(
    aes(label = LABEL, geometry = geometry),
    stat = "sf_coordinates",
    nudge_x = active_farc$nx,
    nudge_y = active_farc$ny,
    na.rm   = TRUE,
    box.padding = 2,
    force = 40
  ) +
  labs(title = "Remoteness: Distance for international journalists traveling from Bogota",
       subtitle = "Top 10 Colombain municipalities based on CINEP-reported FARC activities")
mp
ggsave(filename = sprintf("Plots/EDA/High-CINEP-Sample-%s.png",d),
       plot     = mp,
       units    = "in",
       width    = 12,
       height   = 12,
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




