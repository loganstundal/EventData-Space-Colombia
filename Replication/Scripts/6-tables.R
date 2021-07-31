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
load("../Results/Replication-Estimates/parameter-data.Rdata")
#---------------------------#

#---------------------------#
# Local functions
#---------------------------#
custom_table <- function(data, cap, footnote = NULL){
  kbl(x         = data,
      caption   = cap,
      escape    = FALSE,
      col.names = linebreak(c("", "ICEWS", "GED", "CINEP", "ICEWS\nUnderreporing","GED\nUnderreporting"),
                            align = "c"),
      align     = c("l","c","c","c","c","c"),
      position  = "!ht",
      booktabs  = TRUE) %>%
    kable_styling(font_size = 10) %>%
    collapse_rows(columns     = 1,
                  valign      = "middle",
                  latex_hline = "none") %>%
    row_spec(8,  extra_latex_after = "\\cline{1-6}") %>%
    row_spec(14, extra_latex_after = "\\cline{1-6}") %>%
    footnote(general = footnote, footnote_as_chunk = T)
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
                              TRUE ~ variable)) %>%
  dplyr::mutate(across(contains(c("icews","ged","cinep")),
                       ~cell_spec(.x, font_size = ifelse(stringr::str_detect(.x, "\\["), 8, 10))))

rm(lliks_n)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CONSTRUCT TABLES                                                        ----
#-----------------------------------------------------------------------------#

# Note - here custom_table() will output to html format in R-Studio's viewer
# pane. In an rmarkdown file being compiled to pdf, custom_table() will
# default to a LateX formatted output.

# ----------------------------------- #
# 2002-2009
# ----------------------------------- #
fn  <- "Point estimates reflect posterior median, 95% HPD in brackets."
tmp <- tab_vals %>%
  dplyr::filter(years == cap) %>%
  dplyr::select(-years)
custom_table(data = tmp, cap = "2002-2009", footnote = fn)
# ----------------------------------- #


# ----------------------------------- #
# 2002-2004
# ----------------------------------- #
fn  <- "Point estimates reflect posterior median, 95% HPD in brackets."
tmp <- tab_vals %>%
  dplyr::filter(years == cap) %>%
  dplyr::select(-years)
custom_table(data = tmp, cap = "2002-2004", footnote = fn)
# ----------------------------------- #


# ----------------------------------- #
# 2005-2007
# ----------------------------------- #
fn  <- "Point estimates reflect posterior median, 95% HPD in brackets."
tmp <- tab_vals %>%
  dplyr::filter(years == cap) %>%
  dplyr::select(-years)
custom_table(data = tmp, cap = "2005-2007", footnote = fn)
# ----------------------------------- #


# ----------------------------------- #
# 2008-2009
# ----------------------------------- #
fn  <- "Point estimates reflect posterior median, 95% HPD in brackets."
tmp <- tab_vals %>%
  dplyr::filter(years == cap) %>%
  dplyr::select(-years)
custom_table(data = tmp, cap = "2008-2009", footnote = fn)
# ----------------------------------- #
#-----------------------------------------------------------------------------#
