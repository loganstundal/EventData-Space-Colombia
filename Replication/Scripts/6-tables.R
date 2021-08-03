#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          July 26, 2021
# Purpose:       Replicates main models table results located in Appendix
#                Tables A5-A8
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

#---------------------------#
# Load required packages
#---------------------------#
library(dplyr)
library(tidyr)
library(kableExtra)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Results/Replication-Estimates/parameter-data.Rdata")
#---------------------------#

#---------------------------#
# Local functions
#---------------------------#
local_table <- function(models, caption, model_names = NULL){
  # ----------------------------------- #
  # Description
  # ----------------------------------- #
  # Takes model parameter data (exported as `parameter_data` from
  # 2-models-spde.R) which has been tidied into a formatted data frame (done
  # below) and produces a table with model parameter estimates and credibility
  # intervals
  # ----------------------------------- #
  if(is.null(model_names)){
    model_names <- c("", "ICEWS", "GED", "CINEP",
                     "ICEWS - Underreporing","GED - Underreporting")
  }

  kbl_table <-  kbl(x         = models,
                    caption   = caption,
                    format    = "pipe",
                    escape    = FALSE,
                    col.names = model_names,
                    align     = c("l","c","c","c","c","c"))

  return(kbl_table)
}
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY RESULTS FOR TABLES                                                 ----
#-----------------------------------------------------------------------------#
tidy_vals <- list()
for(yr in yr_grp){
  for(dv in dvs){
    bs <- parameter_data[[yr]][["betas"]][[dv]]
    hy <- parameter_data[[yr]][["hyper"]][[dv]]

    llik <- parameter_data[[yr]][["lliks"]][[dv]]

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
  mutate(across(c(median, lb, ub, lliks),
                ~format(round(.x, 3), nsmall = 3))) %>%
  mutate(hpd = sprintf("[%s, %s]", lb, ub)) %>%
  dplyr::select(variable, median, hpd, model, years) %>%
  pivot_longer(.,
               cols     = c(median, hpd),
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
# CONSTRUCT TABLES                                                        ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# 2002-2009
# ----------------------------------- #
tab_vals %>%
  dplyr::filter(years == "2002-2009") %>%
  dplyr::select(-years) %>%
  local_table(models = ., caption = "Table A.5: SPDE: 2002-2009") %>%
  save_kable("Results/Replication-Tables/table_appendix_5.txt")
# ----------------------------------- #


# ----------------------------------- #
# 2002-2004
# ----------------------------------- #
tab_vals %>%
  dplyr::filter(years == "2002-2004") %>%
  dplyr::select(-years) %>%
  local_table(models = ., caption = "Table A.6: SPDE: 2002-2004") %>%
  save_kable("Results/Replication-Tables/table_appendix_6.txt")
# ----------------------------------- #


# ----------------------------------- #
# 2005-2007
# ----------------------------------- #
tab_vals %>%
  dplyr::filter(years == "2005-2007") %>%
  dplyr::select(-years) %>%
  local_table(models = ., caption = "Table A.7: SPDE: 2005-2007") %>%
  save_kable("Results/Replication-Tables/table_appendix_7.txt")
# ----------------------------------- #


# ----------------------------------- #
# 2008-2009
# ----------------------------------- #
tab_vals %>%
  dplyr::filter(years == "2008-2009") %>%
  dplyr::select(-years) %>%
  local_table(models = ., caption = "Table A.8: SPDE: 2008-2009") %>%
  save_kable("Results/Replication-Tables/table_appendix_8.txt")
# ----------------------------------- #
#-----------------------------------------------------------------------------#

rm(list=ls())
