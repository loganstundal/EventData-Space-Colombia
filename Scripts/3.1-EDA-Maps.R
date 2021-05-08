#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 27, 2021
# Purpose:       3.1: EDA - Maps
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  This script produces figures for the distribution and spatial variation
#  of dependent variables used in the analysis.
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
library(biscale)

#---------------------------#
# Load data
#---------------------------#
load("data/data_variables.RData")



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
# DISTRIBUTIONS                                                           ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Yearly
# ----------------------------------- #
tmp <- colombia_pn %>%
  st_drop_geometry() %>%
  filter(bogota_dummy == 0) %>%
  select(year, cinep, icews, ged) %>%
  pivot_longer(.,
               cols = cinep:ged,
               names_to = "var",
               values_to = "val") %>%
  mutate(year = as_factor(year))

ggplot(data = tmp , aes(x = val, fill = var)) +
  geom_bar() +
  # geom_bar(position = position_dodge2(preserve = "single")) +
  facet_wrap(~ year, nrow = 2, ncol = 4)

rm(tmp)
# ----------------------------------- #

# ----------------------------------- #
# Years-Grouped
# ----------------------------------- #
tmp <- colombia_yg %>%
  st_drop_geometry() %>%
  filter(bogota_dummy == 0) %>%
  select(year_grouped, cinep, icews, ged) %>%
  pivot_longer(.,
               cols = cinep:ged,
               names_to = "var",
               values_to = "val") %>%
  mutate(year = as_factor(year_grouped))

ggplot(data = tmp , aes(x = val, fill = var)) +
  geom_bar() +
  # geom_bar(position = position_dodge2(preserve = "single")) +
  facet_wrap(~ year, nrow = 1, ncol = 4)

rm(tmp)
# ----------------------------------- #


# ----------------------------------- #
# Cross-section
# ----------------------------------- #
tmp <- colombia_cs %>%
  st_drop_geometry() %>%
  filter(bogota_dummy == 0) %>%
  select(cinep, icews, ged) %>%
  pivot_longer(.,
               cols = cinep:ged,
               names_to = "var",
               values_to = "val")

ggplot(data = tmp , aes(x = val, fill = var)) +
  geom_bar()
  # geom_bar(position = position_dodge2(preserve = "single")) +

rm(tmp)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SPATIAL VARIATION                                                       ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# EVENT / NO-EVENT
# ----------------------------------- #
colombia0 <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_COL_0_sf.rds'))

colombia0 <- colombia0 %>% st_crop(xmin = -80.00, xmax = -66.87,
                                   ymin = -04.22, ymax =  14.00)

# colombia0 <- st_transform(colombia0, crs = st_crs(colombia_cs))
plt_dat <- colombia_cs %>%
  select(cinep_bin, icews_bin, ged_bin) %>%
  mutate_at(.vars = vars(cinep_bin:ged_bin),.funs = ~case_when(.x == 1 ~ "Event", TRUE ~ "No Event")) %>%
  pivot_longer(.,
               cols      = cinep_bin:ged_bin,
               names_to  = "var",
               values_to = "val") %>%
  mutate(var = str_to_upper(str_split(var, "_") %>% map(1))) %>%
  mutate(var = factor(var, levels = c("ICEWS","GED","CINEP"))) %>%
  st_set_geometry(., "geometry") %>%
  st_transform(., crs = st_crs(colombia0))

events <- ggplot(data = plt_dat) +
  geom_sf(aes(fill = val), color = "gray60", size = 0.05) +
  geom_sf(data = colombia0, fill = NA, color = "black", size = 0.1) +
  geom_point(aes(x = -74.06456, y = 4.709534,
                 color = "Bogota"), shape = 24, fill = "black",
             size = 1) +
  scale_color_manual(values = "white") +
  scale_fill_manual(values = c("gray30", "gray70")) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  facet_wrap(~ var, ncol = 3) +
  theme_minimal() +
  theme(axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title     = element_blank(),
        legend.position  = "bottom",
        legend.key.size  = unit(3, "mm"),
        panel.background = element_rect(fill = NA, color = "black", size = 0.1),
        strip.background = element_rect(fill = "gray90", color = "black", size = 0.1)) +
  labs(title = "Reported FARC events by source")

ggsave(plot  = events,
       file  = 'Results/Plots/Map-Observed.png',
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')
# ----------------------------------- #


# ----------------------------------- #
# Yearly
# ----------------------------------- #
# ICEWS
mp <- ggplot(data = colombia_pn) +
  geom_sf(aes(fill = icews_bias_fct),
          color = "gray30",
          size  = 0.05) +
  facet_wrap(~year, nrow = 2, ncol = 4) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = "ICEWS",
       subtitle = "Reporting bias relative to CINEP")

# mp
# ggsave(filename = sprintf("Plots/EDA/Map-ICEWS-Yearly-%s.png",d),
#        units    = "in",
#        width    = 8,
#        height   = 8,
#        dpi      = 340)
# rm(mp)

# GED
mp <- ggplot(data = colombia_pn) +
  geom_sf(aes(fill = ged_bias_fct),
          color = "gray30",
          size  = 0.05) +
  facet_wrap(~year, nrow = 2, ncol = 4) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = "GED",
       subtitle = "Reporting bias relative to CINEP")

# mp
# ggsave(filename = sprintf("Plots/EDA/Map-GED-Yearly-%s.png",d),
#        units    = "in",
#        width    = 8,
#        height   = 8,
#        dpi      = 340)
# rm(mp)
# ----------------------------------- #


# ----------------------------------- #
# Years-Grouped
# ----------------------------------- #
tmp <- colombia_yg %>%
  st_drop_geometry() %>%
  select(ID_Mun, year_grouped, icews_bias_fct, ged_bias_fct) %>%
  pivot_longer(.,
               cols = !matches(c("ID_Mun", "year_grouped")),
               values_to = "val",
               names_to  = "var") %>%
  left_join(., colombia_cs["ID_Mun"], by = "ID_Mun") %>%
  st_set_geometry(., "geometry")


mp <- ggplot(data = tmp) +
  geom_sf(aes(fill = val),
          color = "gray30",
          size  = 0.05) +
  facet_wrap(~var + year_grouped, nrow = 2, ncol = 4) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = "ICEWS & GED",
       subtitle = "Reporting bias relative to CINEP")

# mp
# ggsave(filename = sprintf("Plots/EDA/Map-BOTH-YearGrouped-%s.png",d),
#        units    = "in",
#        width    = 8,
#        height   = 8,
#        dpi      = 340)
# rm(tmp,mp)
# ----------------------------------- #


# ----------------------------------- #
# Cross-Section
# ----------------------------------- #
tmp <- colombia_cs %>%
  st_drop_geometry() %>%
  select(ID_Mun, icews_bias_fct, ged_bias_fct) %>%
  pivot_longer(.,
               cols = !matches(c("ID_Mun")),
               values_to = "val",
               names_to  = "var") %>%
  left_join(., colombia_cs["ID_Mun"], by = "ID_Mun") %>%
  st_set_geometry(., "geometry")

mp <- ggplot(data = tmp) +
  geom_sf(aes(fill = val),
          color = "gray30",
          size  = 0.05) +
  facet_wrap(~var, ncol = 2) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = "ICEWS & GED",
       subtitle = "Reporting bias relative to CINEP")

# mp
# ggsave(filename = sprintf("Plots/EDA/Map-BOTH-CrossSection-%s.png",d),
#        units    = "in",
#        width    = 8,
#        height   = 8,
#        dpi      = 340)
# rm(tmp,mp)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CLEAN-UP                                                                ----
#-----------------------------------------------------------------------------#
rm(list=ls())
#-----------------------------------------------------------------------------#



