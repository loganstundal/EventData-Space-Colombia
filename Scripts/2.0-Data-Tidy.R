#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 25, 2021
# Purpose:       2.0 Data-Tidy
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  Tidy data used in models and all analysis.
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
library(sp)

#---------------------------#
# Load data
#---------------------------#
# Administrative units
load('data/colombia_admin.rdata')

# Event data
event_farc  <- read_csv('data/event-data/gedicews_FARC_20190908.csv',
                        col_types = cols(.default = "d"))
cinep       <- haven::read_dta('data/event-data/CINEP_HRV_Farc.dta')

# Covariates
{forest      <- read_csv('data/covariates/Colombia_ForestCover_municipality_Panel.csv') %>%
  dplyr::select(YEAR, admin2Pcod, mean) %>%
  rename(year   = YEAR,
         ID_Mun = admin2Pcod,
         google_ee_forest_per = mean) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))

pop <- read_csv('data/covariates/Colombia_Population_municipality_Panel.csv') %>%
  dplyr::select(YEAR, admin2Pcod, sum) %>%
  rename(year   = YEAR,
         ID_Mun = admin2Pcod,
         google_ee_pop_sum = sum) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))

tri <- read_csv('data/covariates/Colombia_tri_municipality_mean.csv') %>%
  dplyr::select(admin2Pcod, mean) %>%
  rename(ID_Mun = admin2Pcod,
         google_terrain_ri_mean_m = mean) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))

nl <- read_csv("data/covariates/Colombia_NightLights_municipality_Panel.csv") %>%
  dplyr::select(YEAR, admin2Pcod, NL) %>%
  rename(year = YEAR,
         ID_Mun = admin2Pcod,
         google_ee_nl_mean = NL) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))}



#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#

# First submission draft years: seq(2002,2009)
# Set years here for ease:
yr_min <- 2000
yr_max <- 2009

# ----------------------------------- #
# Google Earth Engine
# ----------------------------------- #
# [forest, pop, tri, nightlights]
ge <- left_join(pop, forest, by  = c("year", "ID_Mun")) %>%
  left_join(., tri, by = c("ID_Mun")) %>%
  left_join(., nl,  by = c("year", "ID_Mun")) %>%
  filter(ID_Mun != 88001) %>% #'San Andres Y Providencia'
  group_by(ID_Mun) %>%
  mutate(google_ee_forest_per = imputeTS::na_interpolation(google_ee_forest_per)) %>%
  ungroup() %>%
  filter(year >= yr_min, year <= yr_max)

ge <- ge %>%
  mutate(across(matches("google_ee_nl_mean"), ~ scales::rescale(., to = c(1, 100)))) %>%
  rename_at(.vars = vars(starts_with(c("google_ee_", "google_"))),
            ~ str_remove_all(., "google_ee_|google_")) %>%
  mutate(pop_sum_ln = log(pop_sum)) %>%
  dplyr::select(year, ID_Mun, order(colnames(.)))

rm(forest, pop, tri, nl)
# ----------------------------------- #


# ----------------------------------- #
# Event Data - ICEWS / GED
# ----------------------------------- #
event_farc <- event_farc %>%
  replace(is.na(.), 0) %>%
  dplyr::select(year,latitude,longitude, sort(names(.))) %>%
  mutate(icews_farc       = rebcivicews + rebcivicews,
         # icews_farc_stand = rebcivicewsstand + rebgovicewsstand,
         ged_farc         = rebcivged + rebgovged,
         # ged_farc_stand   = rebcivgedstand + rebgovgedstand
         ) %>%
  dplyr::select(latitude, longitude, year,
                starts_with(c("icews","ged"))) %>%
  filter(year >= yr_min, year <= yr_max)

coordinates(event_farc) <- c("longitude","latitude")
proj4string(event_farc) <- CRS("+proj=longlat +datum=WGS84")
event_farc  <- event_farc %>%
  st_as_sf() %>%
  st_transform(., st_crs(colombia))


# Workflow notes:
"
1. Perform spatial join using st_intersection to identify all events
   uniquely within a municipality.
2. Identify those with missing ID_Mun names (do not interset)
   - some of these are directly on a border.
   - subset these and drop from original successful intersect cases
3. Perform spatial join using 1km distance
   - drop any remaining points which are well outside CO municipalities.
   - bind these back to original success cases.
"

events <- st_join(x    = event_farc,
                  y    = colombia,
                  join = st_intersects)

out_of_bounds <- events[is.na(events$ID_Mun),]

events2 <- st_join(x    = out_of_bounds %>% select(year, starts_with(c("icews","ged"))),
                   y    = colombia,
                   join = st_is_within_distance,
                   dist = 1e3) %>%
  drop_na(ID_Mun)

# This ultimately makes little difference - all DVs coded as 0; clean up later or remove.

events <- events %>%
  drop_na(ID_Mun) %>%
  bind_rows(., events2) %>%
  select(ID_Mun, year, starts_with(c("icews","ged"))) %>%
  group_by(year, ID_Mun) %>%
  summarize(across(starts_with(c("icews","ged")), sum))

rm(out_of_bounds, events2, event_farc)

# sapply(events[3:6], function(x){summary(x)})
# sapply(events,      function(x){table(is.na(x))})
# ----------------------------------- #


# ----------------------------------- #
# Event Data - CINEP
# ----------------------------------- #
cinep <- cinep %>%
  rename(ID_Mun     = id_2,
         cinep_farc = CINEP_hrv) %>%
  dplyr::select(ID_Mun, year, cinep_farc) %>%
  filter(year >= yr_min, year <= yr_max)
# ----------------------------------- #


#-----------------------------------------------------------------------------#
# NOTE - I really really fucked the dv up in the first draft...
# cinep_new <- cinep %>%
#   group_by(id_2) %>%
#   summarise(cinep_farc = sum(CINEP_hrv),
#             year       = year[1]) %>%
#   rename(ID_Mun = id_2)
#
#
# cinep_old <- cinep %>%
#   group_by(id_2) %>%
#   summarise(cinep_farc = n(),
#             year       = year[1]) %>%
#   rename(ID_Mun = id_2)
#
# tst <- data.frame(x = rep(1:3, each = 4),
#                   y = sample(1:3, size = 12, replace = T))
# tst %>%
#   group_by(x) %>%
#   summarize(test = sum(y),
#             old  = n())
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# PREP SF PANEL                                                           ----
#-----------------------------------------------------------------------------#
colombia_panel <- do.call(bind_rows,
                          replicate(n        = length(yr_min:yr_max),
                                    expr     = colombia,
                                    simplify = F))
colombia_panel$year <- rep(yr_min:yr_max, each = nrow(colombia))

colombia <- colombia_panel
rm(colombia_panel)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MERGE ALL                                                               ----
#-----------------------------------------------------------------------------#
colombia <- colombia %>%
  left_join(., cinep,  by = c("year","ID_Mun")) %>%
  left_join(., events, by = c("year","ID_Mun")) %>%
  left_join(., ge,     by = c("year","ID_Mun")) %>%
  mutate(across(starts_with(c("cinep","icews","ged")),
                ~replace_na(.x, 0)))

rm(yr_min, yr_max, cinep, events, ge)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# BINARY OUTCOMES, BOGOTA DUMMY, & COLUMN ORDERING                        ----
#-----------------------------------------------------------------------------#
colombia <- colombia %>%
  mutate(
         # Bogota dummy
         bogota_dummy = ifelse(Department == 'Bogota D.C.', 1, 0),

         # Binary outcomes
         icews_farc_bin    = as.integer(icews_farc > 0),
         ged_farc_bin      = as.integer(ged_farc > 0),
         cinep_farc_bin    = as.integer(cinep_farc > 0)) %>%

  mutate(icews_cinep_under = case_when(icews_farc_bin == 0 & cinep_farc_bin == 1 ~ 1, TRUE ~ 0),
         icews_cinep_bias  = case_when(icews_farc_bin != cinep_farc_bin ~ 1, TRUE ~ 0),

         ged_cinep_under   = case_when(ged_farc_bin == 0 & cinep_farc_bin == 1 ~ 1, TRUE ~ 0),
         ged_cinep_bias    = case_when(ged_farc_bin != cinep_farc_bin ~ 1, TRUE ~ 0),

         icews_ged_under   = case_when(icews_farc_bin == 0 & ged_farc_bin == 1 ~ 1, TRUE ~ 0),
         icews_ged_bias    = case_when(icews_farc_bin != ged_farc_bin ~ 1, TRUE ~ 0),

         # Factors for mapping
         icews_bias_fct = as_factor(case_when(icews_farc_bin == 0 & cinep_farc_bin == 1 ~ 'Underreport',
                                              icews_farc_bin == 1 & cinep_farc_bin == 0 ~ 'Overreport',
                                              icews_farc_bin == cinep_farc_bin ~ 'Agree')),

         ged_bias_fct = as_factor(case_when(ged_farc_bin == 0 & cinep_farc_bin == 1 ~ 'Underreport',
                                            ged_farc_bin == 1 & cinep_farc_bin == 0 ~ 'Overreport',
                                            ged_farc_bin == cinep_farc_bin ~ 'Agree'))) %>%
  mutate(icews_bias_fct = suppressWarnings({fct_relevel(icews_bias_fct,
                                                        levels = c("Underreport", "Overreport", "Agree"))}),
         ged_bias_fct   = suppressWarnings({fct_relevel(ged_bias_fct,
                                                        levels = c("Underreport", "Overreport", "Agree"))})) %>%
  # Arrange columns
  select(ID_Mun, Department, Municipality, year,
         starts_with(c("cinep","icews","ged")),
         sort(names(.)))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(colombia, file = "data/data_variables.RData")
rm(list = ls())
