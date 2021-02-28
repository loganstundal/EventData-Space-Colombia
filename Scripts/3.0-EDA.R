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
kappa_cm <- function(observed,
                     pred   = NULL,
                     p_bin  = NULL,
                     cutoff = 0.5,
                     sig    = 0.95){
  # Cohen's Kappa
  # Kappa = 1 at perfect agreement, 0 (or negative) at no agreement or
  # agreement worse than random chance.

  # observed - true values
  # p_bin    - Reported values
  # pred     - fn can take predicted probabilities to convert to binary
  #            outcomes (if pred, fn uses "cutoff" to define binary vals.)
  # cutoff   - value above which a predicted probability is coded as an event
  # sig      - significance values for reported confidence intervals

  if(is.null(p_bin)){
    p_bin <- ifelse(pred >= cutoff, 1, 0)
  }

  tab <- table(observed, p_bin)

  p_agree  = (tab[1] + tab[4]) / sum(tab)
  p_random = (((tab[1] + tab[3]) / sum(tab)) * ((tab[1] + tab[2]) / sum(tab))) +
    (((tab[2] + tab[4]) / sum(tab)) * ((tab[3] + tab[4]) / sum(tab)))

  k    = (p_agree - p_random) / (1 - p_random)
  se_k = sqrt(( p_agree * (1 - p_agree) ) / ( length(observed) * (1 - p_random)^2  ))

  crit = qnorm(1 - (1-sig)/2)

  k_lci = k - crit*se_k
  k_uci = k + crit*se_k

  return(data.frame('Kappa_lci' = k_lci,
                    'Kappa'     = k,
                    'Kappa_uci' = k_uci))
}



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
            .groups = "keep")

quantile(active_farc$cinep, probs = 0.75)

active_farc <- active_farc %>%
  ungroup() %>%
  filter(cinep > 1) %>%
  arrange(desc(popden)) %>%

  # Slice top and bottom 4
  slice(1:4, (n()-3):n()) %>%
  mutate(rank = 1:n()) %>%
  mutate(x = as_factor(case_when(rank %in% 1:4 ~ "Highest Pop. Den.",
                                 TRUE ~ "Lowest Pop, Den.")),
         LABEL = Municipality,

         # Nudge factors for map (assigned values in meters)
         nx    = case_when(Municipality %in% c("Sincelejo",
                                               "Medellin",
                                               "Bogota D.C.",
                                               "Paz de Ariporo") ~ 1e6,
                           TRUE ~ 0),
         ny   = case_when(Municipality %in% c("Sincelejo",
                                              "Medellin",
                                              "Bogota D.C.",
                                              "Paz de Ariporo") ~ 1e6,
                          TRUE ~ -1e6)
         )
# ----------------------------------- #


# ----------------------------------- #
# Most active - table
# ----------------------------------- #
active_farc[,1:6] %>% kbl(format = "pipe", digits = 2)
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
    nudge_y = active_farc$ny
  )

# mp
# ggsave(filename = sprintf("Plots/EDA/High-CINEP-Sample-%s.png",d),
#        units    = "in",
#        width    = 8,
#        height   = 8,
#        dpi      = 340)
# ----------------------------------- #
rm(active_farc, mp)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# COHEN'S KAPPA - YEARLY                                                  ----
#-----------------------------------------------------------------------------#
yrs <- sort(unique(colombia_pn$year))

# ----------------------------------- #
# Kappa by year - ICEWS
# ----------------------------------- #
icews <- sapply(yrs, function(x){
  tmp <- colombia_pn %>% filter(year == x)
  return(kappa_cm(observed = tmp$cinep_bin,
                  p_bin    = tmp$icews_bin))
}, simplify = FALSE)

icews <- bind_rows(icews) %>%
  mutate(year = yrs,
         var  = 'ICEWS') %>%
  select(year, Kappa_lci, Kappa, Kappa_uci, var)
# ----------------------------------- #


# ----------------------------------- #
# Kappa by year - GED
# ----------------------------------- #
ged <- sapply(yrs, function(x){
  tmp <- colombia_pn %>% filter(year == x)
  return(kappa_cm(observed = tmp$cinep_bin,
                  p_bin    = tmp$ged_bin))
}, simplify = FALSE)

ged <- bind_rows(ged) %>%
  mutate(year = yrs,
         var  = 'GED') %>%
  select(year, Kappa_lci, Kappa, Kappa_uci, var)
# ----------------------------------- #


# ----------------------------------- #
# Tidy plotting data and plot
# ----------------------------------- #
plt_dat <- bind_rows(icews, ged) %>%
  mutate(x = case_when(var == "ICEWS" ~ year - 0.1,
                       TRUE ~ year + 0.1))

plt <- ggplot(data = plt_dat, aes(x = x)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "gray70", size = 0.5) +
  geom_errorbar(aes(ymin = Kappa_lci,
                    ymax = Kappa_uci,
                    color = var)) +
  geom_point(aes(y = Kappa, color = var)) +
  scale_x_continuous(breaks = plt_dat$year,
                     labels = plt_dat$year) +
  theme_bw() +
  theme(panel.grid       = element_blank(),
        legend.title     = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        axis.title.x     = element_blank()) +
  labs(title    = "Inter-rating Reliability: Cohen's Kappa",
       subtitle = "Measures of ICEWS and GED coding reliability relative to CINEP Ground Truth")

# plt
# ggsave(filename = sprintf("Plots/EDA/Kappa-Yearly-%s.png",d),
#        units    = "in",
#        width    = 8,
#        height   = 6,
#        dpi      = 340)

rm(icews, ged, plt_dat, plt, yrs)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# COHEN'S KAPPA - GROUPED YEARS                                           ----
#-----------------------------------------------------------------------------#

yrs <- sort(unique(colombia_yg$year_grouped))

# ----------------------------------- #
# Kappa by grouped years - ICEWS
# ----------------------------------- #
icews <- sapply(yrs, function(x){
  tmp <- colombia_yg %>% filter(year_grouped == x)
  return(kappa_cm(observed = tmp$cinep_bin,
                  p_bin    = tmp$icews_bin))
}, simplify = FALSE)

icews <- bind_rows(icews) %>%
  mutate(year = yrs,
         var  = 'ICEWS',
         x    = 1:n()) %>%
  select(year, Kappa_lci, Kappa, Kappa_uci, var, x)
# ----------------------------------- #


# ----------------------------------- #
# Kappa by grouped years - GED
# ----------------------------------- #
ged <- sapply(yrs, function(x){
  tmp <- colombia_yg %>% filter(year_grouped == x)
  return(kappa_cm(observed = tmp$cinep_bin,
                  p_bin    = tmp$ged_bin))
}, simplify = FALSE)

ged <- bind_rows(ged) %>%
  mutate(year = yrs,
         var  = 'GED',
         x    = 1:n()) %>%
  select(year, Kappa_lci, Kappa, Kappa_uci, var, x)
# ----------------------------------- #


# ----------------------------------- #
# Tidy plotting data and plot
# ----------------------------------- #
plt_dat <- bind_rows(icews, ged) %>%
  mutate(x = case_when(var == "ICEWS" ~ x - 0.1,
                       TRUE ~ x + 0.1))

plt <- ggplot(data = plt_dat, aes(x = x)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "gray70", size = 0.5) +
  geom_errorbar(aes(ymin = Kappa_lci,
                    ymax = Kappa_uci,
                    color = var)) +
  geom_point(aes(y = Kappa, color = var)) +
  scale_x_continuous(breaks = 1:nrow(plt_dat),
                     labels = plt_dat$year) +
  theme_bw() +
  theme(panel.grid       = element_blank(),
        legend.title     = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        axis.title.x     = element_blank()) +
  labs(title    = "Inter-rating Reliability: Cohen's Kappa",
       subtitle = "Measures of ICEWS and GED coding reliability relative to CINEP Ground Truth")

# plt
# ggsave(filename = sprintf("Plots/EDA/Kappa-GroupedYears-%s.png",d),
#        units    = "in",
#        width    = 8,
#        height   = 6,
#        dpi      = 340)

rm(icews, ged, plt_dat, plt, yrs)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# COHEN'S KAPPA - CROSS SECTION                                           ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Kappa - ICEWS
# ----------------------------------- #
icews <- kappa_cm(observed = colombia_cs$cinep_bin,
                  p_bin    = colombia_cs$icews_bin)

icews <- icews %>%
  mutate(var  = 'ICEWS',
         x    = 1) %>%
  select(Kappa_lci, Kappa, Kappa_uci, var, x)
# ----------------------------------- #


# ----------------------------------- #
# Kappa - ICEWS
# ----------------------------------- #
ged <- kappa_cm(observed = colombia_cs$cinep_bin,
                p_bin    = colombia_cs$ged_bin)

ged <- ged %>%
  mutate(var  = 'GED',
         x    = 1) %>%
  select(Kappa_lci, Kappa, Kappa_uci, var, x)
# ----------------------------------- #



# ----------------------------------- #
# Tidy plotting data and plot
# ----------------------------------- #
plt_dat <- bind_rows(icews, ged) %>%
  mutate(x = case_when(var == "ICEWS" ~ x - 0.1,
                       TRUE ~ x + 0.1))

plt <- ggplot(data = plt_dat, aes(x = x)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "gray70", size = 0.5) +
  geom_errorbar(aes(ymin = Kappa_lci,
                    ymax = Kappa_uci,
                    color = var),
                width = 0.05) +
  geom_point(aes(y = Kappa, color = var)) +
  scale_x_continuous(breaks = plt_dat$x,
                     labels = plt_dat$var,
                     limits = c(0.75, 1.25)) +
  theme_bw() +
  theme(panel.grid       = element_blank(),
        legend.title     = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        axis.title.x     = element_blank()) +
  labs(title    = "Inter-rating Reliability: Cohen's Kappa",
       subtitle = "Measures of ICEWS and GED coding reliability relative to CINEP Ground Truth")

# plt
# ggsave(filename = sprintf("Plots/EDA/Kappa-CrossSection-%s.png",d),
#        units    = "in",
#        width    = 6,
#        height   = 6,
#        dpi      = 340)

rm(icews, ged, plt_dat, plt)
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# CONFUSION MATRICES                                                      ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Confusion Matrices - Yearly
# ----------------------------------- #
yrs <- unique(colombia_pn$year)

tmp <- sapply(yrs, function(y){
  dat <- colombia_pn %>% filter(year == y)
  sapply(c('icews_bin','ged_bin'), function(x){
    caret::confusionMatrix(as_factor(dat$cinep_bin),
                           as_factor(dat[[x]]))
  }, simplify = FALSE)
}, simplify = FALSE)

names(tmp) <- paste("Year", yrs, sep = "_")
tmp["Year_2002"]
rm(tmp, yrs)
# ----------------------------------- #


# ----------------------------------- #
# Confusion Matrices - Grouped years
# ----------------------------------- #
yrs <- unique(colombia_yg$year_grouped)

tmp <- sapply(yrs, function(y){
  dat <- colombia_yg %>% filter(year_grouped == y)
  sapply(c('icews_bin','ged_bin'), function(x){
    caret::confusionMatrix(as_factor(dat$cinep_bin),
                           as_factor(dat[[x]]))
  }, simplify = FALSE)
}, simplify = FALSE)

names(tmp) <- paste("Year", yrs, sep = "_")
tmp["Year_2002-03"]
rm(tmp, yrs)
# ----------------------------------- #


# ----------------------------------- #
# Confusion Matrices - Cross section
# ----------------------------------- #
tmp <- sapply(c('icews_bin','ged_bin'), function(x){
  caret::confusionMatrix(as_factor(colombia_cs$cinep_bin),
                         as_factor(colombia_cs[[x]]))
}, simplify = FALSE)

tmp
rm(tmp,)
# ----------------------------------- #


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




