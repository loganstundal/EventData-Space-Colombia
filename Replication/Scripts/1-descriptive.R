#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          July 28, 2021
# Purpose:       Produces tables and figures in paper section 3.2
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Produces: Figure 1 and 2 as well as Tables 1 and 2 in main paper and
#           Table 1 from Appendix
#
#-----------------------------------------------------------------------------#

#!/usr/bin/env Rscript

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
library(forcats)
library(stringr)
library(ggplot2)
library(ggrepel)
library(sf)
library(purrr)
library(caret)
library(kableExtra)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/farc_events.Rdata")
load("Data/colombia.Rdata")
load("Data/colombia2.Rdata")
#---------------------------#

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
  se_k = sqrt((p_agree * (1 - p_agree)) / (length(observed) * (1 - p_random)^2))

  crit = qnorm(1 - (1-sig)/2)

  k_lci = k - crit*se_k
  k_uci = k + crit*se_k

  return(data.frame('Kappa_lci' = k_lci,
                    'Kappa'     = k,
                    'Kappa_uci' = k_uci))
}

confusion_matrix <- function(data, x, reference = "cinep", title = NULL){
  # This function prints a formatted confusion matrix with accuracy and
  # Cohen's Kappa statistics
  cfm <- caret::confusionMatrix(data = data[[x]], reference = data[[reference]],
                                positive = "1")

  tabs <- t(cfm$table)
  colnames(tabs) <- rownames(tabs) <- c("No Event","Event")
  acc  <- cfm$overall[c("AccuracyLower","Accuracy","AccuracyUpper")] %>%
    bind_rows() %>%
    mutate(stat = round(Accuracy*100, 2),
           ci   = sprintf("[%s, %s]",
                          round(AccuracyLower*100, 2),
                          round(AccuracyUpper*100, 2))) %>%
    select(stat,ci)
  kap  <- kappa_cm(observed = data[[reference]], p_bin = data[[x]]) %>%
    mutate(stat = round(Kappa, 2),
           ci   = sprintf("[%s, %s]",
                          round(Kappa_lci, 2),
                          round(Kappa_uci, 2))) %>%
    select(stat, ci)

  acc_kap <- bind_rows(acc, kap) %>% mutate(z = c("Accuracy","Kappa")) %>%
    tibble::column_to_rownames(var = "z")

  {
    cat(paste(rep("_",40), collapse = ""));cat("\n")
    if(!is.null(title)){
      cat(sprintf("\t%s\t\n", str_to_upper(title)))
    } else{
      cat(sprintf("\t%s\t\n",str_to_upper(x)))
      }
    print(tabs);cat("\n")
    cat(paste(rep("-",30), collapse = ""));cat("\n")
    print(acc_kap);cat("\n")
    cat(paste(rep("_",40), collapse = ""));cat("\n\n\n")
    }
}

# Custom map theme
map_theme <- theme(axis.text  = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   panel.grid = element_blank(),
                   legend.title     = element_blank(),
                   legend.position  = "bottom",
                   legend.key.size  = unit(3, "mm"),
                   legend.key       = element_rect(color = NA, fill = NA),
                   panel.background = element_rect(fill  = NA,
                                                   color = "black",
                                                   size  = 0.1),
                   strip.background = element_rect(fill  = "gray90",
                                                   color = "black",
                                                   size  = 0.1))
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Figure 1: Observed FARC Events (full cross-section)
#-----------------------------------------------------------------------------#
# Tidy data
plt_dat <- dat$`2002-2009` %>%
  select(cinep_bin, icews_bin, ged_bin, department, municipality) %>%

  # Recode for plot legend
  mutate(across(.cols = contains("_bin"),
                .fns  = ~case_when(.x == 1 ~ "Event",
                                   TRUE    ~ "No Event"))) %>%

  # Expand to use facet_wrap()
  pivot_longer(.,
               cols      = cinep_bin:ged_bin,
               names_to  = "var",
               values_to = "val") %>%
  mutate(var = str_to_upper(str_split(var, "_") %>% map(1))) %>%
  mutate(var = factor(var, levels = c("ICEWS","GED","CINEP"))) %>%

  # Join spatial data
  left_join(., colombia2, by = c("department", "municipality")) %>%
  st_set_geometry(., "geometry")

events <- ggplot(data = plt_dat) +
  geom_sf(aes(fill = val), color = "gray60", size = 0.05) +
  geom_sf(data = colombia, fill = NA, color = "black", size = 0.1) +
  geom_point(aes(x = -74.06456, y = 4.709534,
                 color = "Bogota"), shape = 24, fill = "black",
             size = 1) +
  scale_color_manual(values = "white") +
  scale_fill_manual(values = c("gray30", "gray70")) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  facet_wrap(~ var, ncol = 3) +
  map_theme +
  labs(title = "Reported FARC events by source")
# ----------------------------------- #


# ----------------------------------- #
# Export Figure 1 map
# ----------------------------------- #
ggsave(plot  = events,
       file  = 'Results/Replication-Figures/figure_main_1.png',
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')
# ----------------------------------- #

# Clean up
rm(events, plt_dat, colombia)
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# FIGURE 2: FARC - Most active municipalities
#-----------------------------------------------------------------------------#
# Identify municipalities in data with the highest numbers of FARC events as
# reported in CINEP data.

# ----------------------------------- #
# Subset and tidy data for mapping
# ----------------------------------- #
active_farc <- dat$`2002-2009` %>%
  mutate(popden = pop_sum / area_km2) %>%
  group_by(department, municipality) %>%
  summarize(cinep   = sum(cinep),
            icews   = sum(icews),
            ged     = sum(ged),
            popden  = mean(popden),
            capdist = distance_bogota_km[1],
            .groups = "keep") %>%
  ungroup() %>%
  filter(cinep > 5) %>%
  arrange(capdist) %>%
  slice(1:5, (n()-4):n()) %>%
  mutate(rank = 1:n()) %>%
  # Arrange label position based on map projection in meters - done by
  # trial and error to get labels to print nicely.
  mutate(
    x = as_factor(case_when(rank %in% 1:5 ~ "Journalistic Proximity",
                            TRUE ~ "Journalistic Remoteness")),
    nx = case_when(municipality %in% c("Ovejas","San Luis","Alvarado","Dolores",
                                       "El Carmen de Bolivar")~ -1e6,
                   TRUE ~ 1e6),
    ny = case_when(municipality %in% c("El Carmen de Bolivar", "Valledupar",
                                       "Tibu","Arauca") ~ 1e6,
                   municipality == "Alvarado" ~ -1e6,
                   municipality == "Samana" ~ -6e5,
                   municipality == "San Francisco" ~ -2e5,
                   municipality == "Dolores" ~ -1.5e6,
                   TRUE ~ 0),
    LABEL = municipality) %>%
  mutate(nx = case_when(municipality %in% c("San Francisco", "Samana") ~ -2e6,
                        TRUE ~ nx))
# ----------------------------------- #


# ----------------------------------- #
# Appendix Table 1 & Export
# ----------------------------------- #
tableA1 <- active_farc %>%
  select(c(department:capdist,x)) %>%
  kbl(format = "pipe", digits = 2,
      col.names = c("Department","Municipality","CINEP","ICEWS","GED",
                    "Pop. Density","Capital Dist.", "Class"))
tableA1

save_kable(x = tableA1, file = "Results/Replication-Tables/table_appendix_1.txt")
# ----------------------------------- #


# ----------------------------------- #
# Create Figure 2 map - most active FARC locations [remote and non-remote]
# ----------------------------------- #
# Join spatial data to most-active FARC
active_farc_sf <- colombia2 %>%
  left_join(., active_farc, by = c("department", "municipality")) %>%
  st_transform(., crs = "+proj=aea  +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60
                         +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")

# Identify projected Bogota coordinates
bogota <- st_sf(data.frame(Name = "Bogota",
                           geom = st_sfc(st_point(c(-74.09854,4.64794)),
                                         crs = "+proj=longlat"))) %>%
  st_transform(., crs = st_crs(active_farc_sf)) %>%
  st_coordinates() %>% as.data.frame()

# Produce Map
mp <- ggplot(data = active_farc_sf) +
  geom_sf(aes(fill = x), color = "gray50", size = 0.05) +
  geom_point(data = bogota, aes(x = X, y = Y,
                 color = "Bogota"), shape = 24, fill = "black",
             size = 2) +
  scale_color_manual(values = "white") +
  scale_fill_manual(values       = c("#00bfc4", "#f8766d"),
                    na.value     = "transparent",
                    na.translate = FALSE) +
  guides(fill = guide_legend(nrow    = 2,
                             byrow   = TRUE,
                             reverse = TRUE)) +
  scale_x_continuous(limits = c(-2.8e6,-6.5e5)) +
  scale_y_continuous(limits = c(3157019.2, 5e6)) +
  map_theme +
  geom_label_repel(
    aes(label = LABEL, geometry = geometry),
    label.size       = NA,
    fill             = "transparent",
    size             = (10 / .pt),
    segment.size     = 0.25,
    segment.color    = "gray40",
    segment.linetype = "dashed",
    stat             = "sf_coordinates",
    nudge_x          = active_farc_sf$nx,
    nudge_y          = active_farc_sf$ny,
    na.rm            = TRUE)

# ----------------------------------- #
# Export Figure 2 map
# ----------------------------------- #
ggsave(filename = "Results/Replication-Figures/figure_main_2.png",
       plot     = mp,
       units    = "in",
       width    = 4.0,
       height   = 4.0,
       dpi      = 340)
# ----------------------------------- #

# Clean up
rm(active_farc_sf, bogota, colombia2, map_theme, mp, tableA1)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Confusion Matrices - setup
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Confusion matrices data
# ----------------------------------- #
# Change class values in active_farc for code clarity:
active_farc <- active_farc %>%
  mutate(class = case_when(x == "Journalistic Proximity" ~ "Non-remote",
                           x == "Journalistic Remoteness" ~ "Remote"))

# Within particular years
confusion <- dat$Yearly_panel %>%
  left_join(., active_farc[,c("department", "municipality","class")],
            by  = c("department", "municipality")) %>%
  mutate(year_slice = case_when(year %in% 2002:2004 ~ "2002-2004",
                                year %in% 2005:2007 ~ "2005-2007",
                                year %in% 2008:2009 ~ "2008-2009")) %>%
  drop_na(class) %>%
  select(municipality, year, year_slice ,class, cinep, icews, ged) %>%
  mutate(across(.cols = cinep:ged,
                .fns  = ~case_when(.x > 0 ~ 1, TRUE ~ 0), .names = "{col}")) %>%
  mutate(across(.cols = cinep:ged,
                .fns  = as_factor))

# Across all years
full_cs <- dat$`2002-2009` %>%
  left_join(., active_farc[,c("department", "municipality","class")],
            by  = c("department", "municipality")) %>%
  select(municipality, class, cinep, icews, ged) %>%
  mutate(across(.cols = cinep:ged,
                .fns  = ~case_when(.x > 0 ~ 1, TRUE ~ 0), .names = "{col}")) %>%
  mutate(across(.cols = cinep:ged,
                .fns  = as_factor))

rm(list = setdiff(ls(),c("confusion","kappa_cm","confusion_matrix","full_cs")))
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CONFUSION MATRICES
#-----------------------------------------------------------------------------#
# 1. TABLE 1 -  Full cross-sectional table
#               [sum to 1116]
# 2. TABLE 2 -	One event per year over year groups
#               [sum to 15 (5 municipalities * 3 yrs)]
#-----------------------------------------------------------------------------#


sink(file = "Results/Replication-Tables/table_main_1.txt")
#-----------------------------------------------------------------------------#
# 1.	TABLE 1: Municipality-Event Confusion Matrix, 2002-2009
#-----------------------------------------------------------------------------#
confusion_matrix(data = full_cs ,
                 x    = "icews",
                 title = "Table 1 (2002-2009); ICEWS")

confusion_matrix(data = full_cs,
                 x    = "ged",
                 title = "Table 1 (2002-2009); GED")

#-----------------------------------------------------------------------------#
sink()


sink(file = "Results/Replication-Tables/table_main_2.txt")
#-----------------------------------------------------------------------------#
# TABLE 2 ~ Municipality-Event Confusion Matrices for Selected Municipalities
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Non-remote municipalities
# ----------------------------------- #
confusion_matrix(data = confusion %>%
                   filter(class  == "Non-remote"),
                 x = "icews",
                 title = "Non-remote, ICEWS, 2002-2009")
confusion_matrix(data = confusion %>%
                   filter(class  == "Non-remote"),
                 x = "ged",
                 title = "Non-remote, GED, 2002-2009")

confusion_matrix(data = confusion %>%
                   filter(year_slice == "2002-2004",
                          class  == "Non-remote"),
                 x = "icews",
                 title = "Non-remote, ICEWS, 2002-2004")
confusion_matrix(data = confusion %>%
                   filter(year_slice == "2002-2004",
                          class  == "Non-remote"),
                 x = "ged",
                 title = "Non-remote, GED, 2002-2004")

confusion_matrix(data = confusion %>%
                   filter(year_slice == "2005-2007",
                          class  == "Non-remote"),
                 x = "icews",
                 title = "Non-remote, ICEWS, 2005-2007")
confusion_matrix(data = confusion %>%
                   filter(year_slice == "2005-2007",
                          class  == "Non-remote"),
                 x = "ged",
                 title = "Non-remote, GED, 2005-2007")
# ----------------------------------- #


# ----------------------------------- #
# Remote municipalities
# ----------------------------------- #
confusion_matrix(data = confusion %>%
                   filter(class  == "Remote"),
                 x = "icews",
                 title = "Remote, ICEWS, 2002-2009")
confusion_matrix(data = confusion %>%
                   filter(class  == "Remote"),
                 x = "ged",
                 title = "Remote, GED, 2002-2009")

confusion_matrix(data = confusion %>%
                   filter(year_slice == "2002-2004",
                          class  == "Remote"),
                 x = "icews",
                 title = "Remote, ICEWS, 2002-2004")
confusion_matrix(data = confusion %>%
                   filter(year_slice == "2002-2004",
                          class      == "Remote"),
                 x = "ged",
                 title = "Remote, GED, 2002-2004")

confusion_matrix(data = confusion %>%
                   filter(year_slice == "2005-2007",
                          class      == "Remote"),
                 x = "icews",
                 title = "Remote, ICEWS, 2005-2007")
confusion_matrix(data = confusion %>%
                   filter(year_slice == "2005-2007",
                          class      == "Remote"),
                 x = "ged",
                 title = "Remote, GED, 2005-2007")
# ----------------------------------- #
#-----------------------------------------------------------------------------#
sink()
rm(list=ls())
