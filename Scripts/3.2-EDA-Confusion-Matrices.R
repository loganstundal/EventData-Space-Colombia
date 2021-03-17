#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          March 17, 2021
# Purpose:       3.2 EDA - Confusion Matrices
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  This script produces various confusion matrices in cross-sectional and
#  time-slices as requested by R1.
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

# Identify remote vs non-remote municipalities according to distance from
# Bogota (i.e., "journalistic distance", or the distance an international
# journalist would need to travel to report a story which would be used in
# ICEWS or GED).

# Identify municipalities in data with the highest numbers of FARC events as
# reported in CINEP data.

# ----------------------------------- #
# Subset and tidy data for mapping
# ----------------------------------- #
active_farc <- colombia_pn %>%
  st_drop_geometry() %>%
  group_by(Department, Municipality) %>%
  summarize(cinep   = sum(cinep),
            icews   = sum(icews),
            ged     = sum(ged),
            capdist = distance_bogota_km[1],
            .groups = "keep") %>%
  ungroup() %>%
  filter(cinep > 5) %>%
  arrange(capdist) %>%
  slice(1:5, (n()-4):n()) %>%
  mutate(rank = 1:n()) %>%
  mutate(class = case_when(rank %in% 1:5 ~ "Non-remote", TRUE ~ "Remote"),
         ID    = paste(Department, Municipality, sep = "-"))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CROSS-SECTION                                                           ----
#-----------------------------------------------------------------------------#
confusion_cs <- colombia_cs %>%
  left_join(., active_farc[,c("Department", "Municipality","class")],
            by  = c("Department", "Municipality")) %>%
  drop_na(class) %>%
  select(Municipality, class, cinep, icews, ged) %>%
  mutate(across(c(cinep, icews, ged), ~case_when(.x > 0 ~ 1, TRUE ~ 0), .names = "{col}"))

table(confusion_cs$cinep,
      confusion_cs$icews,
      confusion_cs$class)

table(confusion_cs$cinep,
      confusion_cs$ged,
      confusion_cs$class)

#-----------------------------------------------------------------------------#
# TIME-SLICES                                                             ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Grouping variable setup
# ----------------------------------- #
colombia_pn <- colombia_pn %>%
  mutate(year_slice = case_when(year %in% 2002:2004 ~ "2002-2004",
                                year %in% 2005:2007 ~ "2005-2007",
                                year %in% 2008:2009 ~ "2008-2009"))
# ----------------------------------- #


# ----------------------------------- #
# Group-year confusion matrices
# ----------------------------------- #
confusion_ts <- colombia_pn %>%
  left_join(., active_farc[,c("Department", "Municipality","class")],
            by  = c("Department", "Municipality")) %>%
  drop_na(class) %>%
  select(Municipality, class, year_slice, year, cinep, icews, ged) %>%
  mutate(across(c(cinep, icews, ged), ~case_when(.x > 0 ~ 1, TRUE ~ 0), .names = "{col}"))

table(confusion_ts$cinep,
      confusion_ts$icews,
      confusion_ts$class,
      confusion_ts$year_slice)

table(confusion_ts$cinep,
      confusion_ts$ged,
      confusion_ts$class,
      confusion_ts$year_slice)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
