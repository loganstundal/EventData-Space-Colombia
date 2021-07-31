#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          July 27, 2021
# Purpose:       Replicates main models results presented in Figures 3 & 4
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
library(stringr)
library(forcats)
library(ggplot2)
library(cowplot)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("../Results/Replication-Estimates/parameter-data.Rdata")
#---------------------------#

#---------------------------#
# Local functions
#---------------------------#
custom_plot <- function(data, colors){
  ggplot(data = data,
         aes(y = y)) +
    geom_point(aes(x = median, color = model), size = 1.0) +
    geom_errorbar(aes(xmin = lb, xmax = ub, color = model),
                  width = 0.1, size = 0.5) +
    scale_color_manual(values = {{colors}}) +
    facet_wrap(~years, ncol = 3) +
    theme_minimal() +
    theme(axis.title         = element_blank(),
          panel.border       = element_rect(colour = "black",
                                            fill   = NA,
                                            size   = 0.3),
          panel.grid         = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted",
                                            color    = "gray70",
                                            size     = 0.2),
          strip.background   = element_rect(fill     = NA,
                                            linetype = "solid",
                                            size     = 0.3),
          strip.text         = element_text(size = 10),
          legend.position    = "none")}
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

g <- bind_rows(tidy_vals) %>%
  filter(variable != "intercept") %>%
  dplyr::select(variable:years) %>%
  filter(years != "2008-2009") %>%
  mutate(type_var = case_when(variable %in% c("Kappa","Sigma","Range") ~ variable,
                              TRUE                                     ~ "main"),
         type_mod = case_when(str_detect(model, "_bin") ~ "Event",
                              TRUE                      ~ "Under"),
         model = case_when(str_starts(model, "cinep") ~ "CINEP",
                           str_starts(model, "icews") ~ "ICEWS",
                           str_starts(model, "ged")   ~ "GED"),
         y = case_when(variable == "dist"      ~ 1,
                       variable == "pop"       ~ 2,
                       variable == "tri"       ~ 3,
                       TRUE                    ~ 1)) %>%
  mutate(type_var = factor(type_var, levels = c("main","Kappa","Sigma","Range")),
         y = case_when(str_detect(model, "ICEWS") ~ y - 0.2,
                       str_detect(model, "GED")   ~ y + 0.2,
                       TRUE ~ y),
         model = factor(model, levels = c("CINEP","ICEWS","GED")),
         years = factor(years, levels = c("2002-2009", "2002-2004",
                                          "2005-2007", "2008-2009")))
# ----------------------------------- #
#-----------------------------------------------------------------------------#




#-----------------------------------------------------------------------------#
# CONSTRUCT PLOTS - OBSERVED EVENTS                                       ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# EVENT MODELS - FIGURE 3
# ----------------------------------- #
# Regressor parameters
event_main <- custom_plot(data = g %>% filter(type_var == "main",
                                              type_mod == "Event"),
                          colors = model_colors) +
  scale_y_continuous(breaks = 1:3,
                     labels = c("Dist. Bogota, km (log)",
                                "Population (log)",
                                "TRI"))

# SPDE parameters
# KAPPA
event_spde_kappa <- custom_plot(data = g %>% filter(type_var == "Kappa",
                                                    type_mod == "Event"),
                                colors = model_colors) +
  scale_y_continuous(breaks = 1, labels = "Kappa") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())

# SIGMA
event_spde_sigma <- custom_plot(data = g %>% filter(type_var == "Sigma",
                                                    type_mod == "Event"),
                                colors = model_colors) +
  scale_y_continuous(breaks = 1, labels = "Sigma") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())

# RANGE
event_spde_range <- custom_plot(data = g %>% filter(type_var == "Range",
                                                    type_mod == "Event"),
                                colors = model_colors) +
  scale_y_continuous(breaks = 1, labels = "Range") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank(),
        axis.text.x      = element_text(hjust = 1))

# LEGEND (extract from Range and update range to no legend)
leg              <- get_legend(event_spde_range)
event_spde_range <- event_spde_range + theme(legend.position = "none")

# Event - full plot
event_final_plot <- plot_grid(event_main,
                              event_spde_kappa,
                              event_spde_sigma,
                              event_spde_range,leg,
                              rel_heights = c(3, 1, 1, 1, .3),
                              nrow  = 5,
                              align = "v",
                              axis  = "l")
# ----------------------------------- #


# ----------------------------------- #
# UNDERREPORTING MODELS - FIGURE 4
# ----------------------------------- #
# Regressor parameters
under_main <- custom_plot(data = g %>% filter(type_var == "main",
                                              type_mod == "Under"),
                          colors = model_colors) +
  scale_y_continuous(breaks = 1:3,
                     labels = c("Dist. Bogota, km (log)",
                                "Population (log)",
                                "TRI"))

# SPDE parameters
# KAPPA
under_spde_kappa <- custom_plot(data = g %>% filter(type_var == "Kappa",
                                                    type_mod == "Under"),
                                colors = model_colors) +
  scale_y_continuous(breaks = 1, labels = "Kappa") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())

# SIGMA
under_spde_sigma <- custom_plot(data = g %>% filter(type_var == "Sigma",
                                                    type_mod == "Under"),
                                colors = model_colors) +
  scale_y_continuous(breaks = 1, labels = "Sigma") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank())

# RANGE
under_spde_range <- custom_plot(data = g %>% filter(type_var == "Range",
                                                    type_mod == "Under"),
                                colors = model_colors) +
  scale_y_continuous(breaks = 1, labels = "Range") +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank(),
        axis.text.x      = element_text(hjust = 1))

# LEGEND (extract from Range and update range to no legend)
leg              <- get_legend(under_spde_range)
under_spde_range <- under_spde_range + theme(legend.position = "none")

# under - full plot
under_final_plot <- plot_grid(under_main,
                              under_spde_kappa,
                              under_spde_sigma,
                              under_spde_range,leg,
                              rel_heights = c(3, 1, 1, 1, .3),
                              nrow  = 5,
                              align = "v",
                              axis  = "l")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
ggsave(filename = "../Results/Replication-Figures/figure_main_3.png",
       plot     = event_final_plot,
       width    = 6.5,
       height   = 6.5,
       units    = "in",
       dpi      = 380)

ggsave(filename = "../Results/Replication-Figures/figure_main_4.png",
       plot     = under_final_plot,
       width    = 6.5,
       height   = 6.5,
       units    = "in",
       dpi      = 380)

rm(list = ls())
#-----------------------------------------------------------------------------#

