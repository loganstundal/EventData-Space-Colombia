#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 23, 2021
# Purpose:       Note
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#
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
load("Data/Colombia_panel.Rdata")


#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#
dat <- dat %>%
  dplyr::select(c(ID_Mun, Department, Municipality, year),
         starts_with(c("icews","ged","cinep")),
         -ends_with("stand"))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SUMMARY STATISTICS FOR BINS
#-----------------------------------------------------------------------------#
sapply(dat %>%
         dplyr::select(starts_with(c("icews","ged","cinep"))) %>%
         st_drop_geometry(),
       function(x){summary(x)},
       simplify = FALSE)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# BIN DATA                                                                ----
#-----------------------------------------------------------------------------#
dat2 <- dat %>%
  mutate(bin_icews = case_when(icews_farc < 1 ~ "0",
                               icews_farc >= 1 & icews_farc < 3 ~ "1-2",
                               icews_farc >= 3 & icews_farc <= 5 ~ "3-5",
                               icews_farc >= 6 ~ "6+"),

         bin_ged   = case_when(ged_farc < 1 ~ "0",
                               ged_farc >= 1 & ged_farc < 3 ~ "1-2",
                               ged_farc >= 3 & ged_farc <= 5 ~ "3-5",
                               ged_farc >= 6 ~ "6+"),

         bin_cinep = case_when(cinep_farc < 1 ~ "0",
                               cinep_farc >= 1 & cinep_farc < 3 ~ "1-2",
                               cinep_farc >= 3 & cinep_farc <= 5 ~ "3-5",
                               cinep_farc >= 6 ~ "6+")) %>%
  mutate(across(starts_with("bin"), ~as_factor(.))) %>%
  dplyr::select(c(ID_Mun, Department, Municipality, year),
                starts_with(c("bin")))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MAPS
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# ICEWS
# ----------------------------------- #
icews <- ggplot(data = dat2) +
  geom_sf(aes(fill = bin_icews), size = 0.05, color = "gray80") +
  facet_wrap(~ year,
             nrow = 2,
             ncol = 4) +
  scale_fill_viridis_d(name = "Events") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        strip.background = element_rect(fill  = "gray80"),
        strip.text       = element_text(color = "black"),
        legend.position  = "bottom",
        legend.direction = "horizontal") +
  labs(title = "ICEWS")


# ----------------------------------- #
# GED
# ----------------------------------- #
ged <- ggplot(data = dat2) +
  geom_sf(aes(fill = bin_ged), size = 0.05, color = "gray80") +
  facet_wrap(~ year,
             nrow = 2,
             ncol = 4) +
  scale_fill_viridis_d(name = "Events") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        strip.background = element_rect(fill  = "gray80"),
        strip.text       = element_text(color = "black"),
        legend.position  = "bottom",
        legend.direction = "horizontal") +
  labs(title = "GED")


# ----------------------------------- #
# CINEP
# ----------------------------------- #
cinep <- ggplot(data = dat2) +
  geom_sf(aes(fill = bin_cinep), size = 0.05, color = "gray80") +
  facet_wrap(~ year,
             nrow = 2,
             ncol = 4) +
  scale_fill_viridis_d(name = "Events") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        strip.background = element_rect(fill  = "gray80"),
        strip.text       = element_text(color = "black"),
        legend.position  = "bottom",
        legend.direction = "horizontal") +
  labs(title = "CINEP")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE PLOTS                                                              ----
#-----------------------------------------------------------------------------#

# dirpath <- "Plots/Panel-Maps-20210223/"
# dir.create(dirpath)

ggsave(plot     = icews,
       filename = paste0(dirpath, "icews", ".png"),
       width    = 8.0,
       height   = 8.0,
       dpi      = 320)

ggsave(plot     = ged,
       filename = paste0(dirpath, "ged", ".png"),
       width    = 8.0,
       height   = 8.0,
       dpi      = 320)

ggsave(plot     = cinep,
       filename = paste0(dirpath, "cinep", ".png"),
       width    = 8.0,
       height   = 8.0,
       dpi      = 320)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SUMMARY STATISTICS PER YEAR
#-----------------------------------------------------------------------------#

dat3 <- dat %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarize(
    mean_ICEWS = mean(icews_farc),
    "n>0_ICEWS"= sum(icews_farc > 0),
    max_ICEWS  = max(icews_farc),
    # min_ICEWS  = min(icews_farc),
    sd_ICEWS   = sd(icews_farc),

    mean_GED = mean(ged_farc),
    "n>0_GED"= sum(ged_farc > 0),
    max_GED  = max(ged_farc),
    # min_GED  = min(ged_farc),
    sd_GED   = sd(ged_farc),

    mean_CINEP = mean(cinep_farc),
    "n>0_CINEP"= sum(cinep_farc > 0),
    max_CINEP  = max(cinep_farc),
    # min_CINEP  = min(cinep_farc),
    sd_CINEP   = sd(cinep_farc)
    ) %>%
  pivot_longer(.,
               cols   = !matches("year"),
               names_to  = c("Statistic", "Variable"),
               names_sep = "_",
               values_to = "Value") %>%
  pivot_wider(.,
              names_from = Statistic,
              values_from = Value) %>%
  arrange(Variable, year)


dat3

# ----------------------------------- #
# ICEWS
# ----------------------------------- #
dat3 %>%
  filter(Variable == "ICEWS") %>%
  dplyr::select(-Variable) %>%
  kbl(caption = "ICEWS",
      format = "html", digits = 3) %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  cat(., file = paste0(dirpath, "ICEWS", ".txt"))

# ----------------------------------- #
# GED
# ----------------------------------- #
dat3 %>%
  filter(Variable == "GED") %>%
  dplyr::select(-Variable) %>%
  kbl(caption = "GED",
      format = "html", digits = 3) %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  cat(., file = paste0(dirpath, "GED", ".txt"))


# ----------------------------------- #
# CINEP
# ----------------------------------- #
dat3 %>%
  filter(Variable == "CINEP") %>%
  dplyr::select(-Variable) %>%
  kbl(caption = "CINEP",
      format = "html", digits = 3) %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  cat(., file = paste0(dirpath, "CINEP", ".txt"))












