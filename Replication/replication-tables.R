#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          July 26, 2021
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
library()

#---------------------------#
# Set working directory
#---------------------------#
setwd()

#---------------------------#
# Load data
#---------------------------#
# load("results-tables.Rdata")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY RESULTS FOR TABLES                                                 ----
#-----------------------------------------------------------------------------#


tidy_vals <- list()
for(yr in yr_grp){
  for(dv in dvs){
    bs <- res_vals[[yr]][["betas"]][[dv]]
    hy <- res_vals[[yr]][["hyper"]][[dv]]

    llik <- res_vals[[yr]][["lliks"]][[dv]]

    vl <- bind_rows(bs, hy)
    vl <- bind_cols(vl,
                    "model" = dv,
                    "years" = yr,
                    "lliks" = llik,
                    "n"     = "1116")

    id <- paste(dv, yr, sep = "_._")
    tidy_vals[[id]] <- vl
  }
};rm(yr, dv, bs, hy, llik, vl, id)

# Bind parameters to DF
tidy_vals <- bind_rows(tidy_vals)


tab_vals <- tidy_vals %>%
  mutate(across(c(!!cent, lb, ub, lliks),
                ~format(round(.x, 3), nsmall = 3))) %>%
  mutate(hpd = sprintf("[%s, %s]", lb, ub)) %>%
  dplyr::select(variable, !!cent, hpd, model, years) %>%
  pivot_longer(.,
               cols     = c(!!cent, hpd),
               names_to = "type") %>%
  pivot_wider(.,
              id_cols = c(variable, type, years),
              names_from = model,
              values_from = value)

lliks_n <- tidy_vals %>%
  group_by(model, years) %>%
  summarize(lliks = lliks[1],
            n     = n[1],
            .groups = "keep") %>%
  ungroup() %>%
  mutate(across(c(lliks),
                ~format(round(.x, 3), nsmall = 3))) %>%
  dplyr::select(lliks, n, model, years) %>%
  pivot_longer(.,
               cols = c(lliks, n),
               names_to = "variable") %>%
  pivot_wider(.,
              id_cols = c(variable, years),
              names_from = model,
              values_from = value) %>%
  mutate(type = NA)


tab_vals <- bind_rows(tab_vals, lliks_n) %>%
  dplyr::select(-type) %>%
  mutate(variable = case_when(variable == "intercept" ~ "Intercept",
                              variable == "dist"      ~ "Dist. Bogota, km (log)",
                              variable == "pop"       ~ "Population (log)",
                              variable == "tri"       ~ "TRI",
                              variable == "lliks"     ~ "LogLik",
                              variable == "n"         ~ "N",
                              TRUE ~ variable))

rm(lliks_n)
















#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
#-----------------------------------------------------------------------------#
