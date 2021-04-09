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
# Subset and tidy data
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

# active_farc[,1:8] %>% kbl(format = "pipe", digits = 2)
# ----------------------------------- #


# ----------------------------------- #
# Confusion matrices data
# ----------------------------------- #
confusion <- colombia_pn %>%
  left_join(., active_farc[,c("Department", "Municipality","class")],
            by  = c("Department", "Municipality")) %>%
  mutate(year_slice = case_when(year %in% 2002:2004 ~ "2002-2004",
                                year %in% 2005:2007 ~ "2005-2007",
                                year %in% 2008:2009 ~ "2008-2009")) %>%
  drop_na(class) %>%
  select(Municipality, year, year_slice ,class, cinep, icews, ged) %>%
  mutate(across(c(cinep, icews, ged), ~case_when(.x > 0 ~ 1, TRUE ~ 0), .names = "{col}"))

rm(list = c(ls(pattern = "colombia"), "active_farc"))
# ----------------------------------- #

# Iteration variables
cl <- unique(confusion$class)
ys <- unique(confusion$year_slice)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CONFUSION MATRICES
#-----------------------------------------------------------------------------#
# 1.	One event per year over 8 years; adds to 40	       [2 tables total]
# 2.	One event over 8 years; adds to 5		               [2 tables total]
# 3.	One event per year in time slice; add to 15 or 10  [6 tables total]
# 4.	One event over time slice years; adds to 5	       [6 tables total]
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# 1.	One event per year over 8 years; adds to 40	        2 tables total] ----
#-----------------------------------------------------------------------------#
for(cls in cl){
  cat(sprintf("Class:%s\n\n",cls))
  cat("             ICEWS               GED              \n")

  tmp <- confusion %>% filter(class == !!{cls}) %>% select(cinep, icews, ged) %>%
    mutate(across(everything(), ~factor(.x, levels = c("0","1"))))

  res <- sapply(c("icews", "ged"), function(x){
    cfm <- caret::confusionMatrix(data = tmp[[x]], reference = tmp$cinep, positive = "1")

    tabs <- cfm$table
    acc  <- round(cfm$overall[c("AccuracyLower","Accuracy","AccuracyUpper")] * 100, 1)

    kap  <- round(kappa_cm(observed = tmp$cinep, p_bin = tmp[[x]]),2)

    return(list("table"    = t(tabs),
                "accuracy" = acc,
                "kappa"    = kap))
  }, simplify = FALSE)

  tab <- cbind(res$icews$table, res$ged$table)
  colnames(tab) <- rep(c("No Event", "Event"),2)
  rownames(tab) <- rep(c("No Event", "Event"),1)

  acc <- data.frame(rbind(res$icews$accuracy,res$ged$accuracy)) %>%
    mutate(CI = sprintf("[%s, %s]", AccuracyLower, AccuracyUpper)) %>%
    select(Accuracy, CI)
  rownames(acc) <- c("ICEWS","GED")

  kap <- data.frame(rbind(res$icews$kappa,res$ged$kappa)) %>%
    mutate(CI = sprintf("[%s, %s]", Kappa_lci, Kappa_uci)) %>%
    select(Kappa, CI)
  rownames(kap) <- c("ICEWS","GED")

  print(tab);cat(rep("\n",2))
  print(acc);cat(rep("\n",2))
  print(kap);cat(rep("\n",2))
  cat(rep("\n",6))
};rm(cls,tmp, res, tab, acc, kap)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# 2.	One event over 8 years; adds to 5		               [2 tables total]
#-----------------------------------------------------------------------------#
for(cls in cl){
  cat(sprintf("Class:%s\n\n",cls))
  cat("             ICEWS               GED              \n")

  tmp <- confusion %>% filter(class == !!{cls}) %>%
    select(Municipality,cinep, icews, ged) %>%
    group_by(Municipality) %>%
    summarize(across(everything(), ~sum(.x))) %>%
    ungroup() %>%
    select(cinep:ged) %>%
    mutate(across(everything(), ~case_when(.x > 0 ~ 1, TRUE ~ 0))) %>%
    mutate(across(everything(), ~factor(.x, levels = c("0","1"))))

  res <- sapply(c("icews", "ged"), function(x){
    cfm <- caret::confusionMatrix(data = tmp[[x]], reference = tmp$cinep, positive = "1")

    tabs <- cfm$table
    acc  <- round(cfm$overall[c("AccuracyLower","Accuracy","AccuracyUpper")] * 100,2)

    kap  <- round(kappa_cm(observed = tmp$cinep, p_bin = tmp[[x]]),2)

    return(list("table"    = t(tabs),
                "accuracy" = acc,
                "kappa"    = kap))
  }, simplify = FALSE)

  tab <- cbind(res$icews$table, res$ged$table)
  colnames(tab) <- rep(c("No Event", "Event"),2)
  rownames(tab) <- rep(c("No Event", "Event"),1)

  acc <- data.frame(rbind(res$icews$accuracy,res$ged$accuracy)) %>%
    mutate(CI = sprintf("[%s, %s]", AccuracyLower, AccuracyUpper)) %>%
    select(Accuracy, CI)
  rownames(acc) <- c("ICEWS","GED")

  kap <- data.frame(rbind(res$icews$kappa,res$ged$kappa)) %>%
    mutate(CI = sprintf("[%s, %s]", Kappa_lci, Kappa_uci)) %>%
    select(Kappa, CI)
  rownames(kap) <- c("ICEWS","GED")

  print(tab);cat(rep("\n",2))
  print(acc);cat(rep("\n",2))
  print(kap);cat(rep("\n",2))
  cat(rep("\n",6))
};rm(cls, tmp, res, tab, acc, kap)
#-----------------------------------------------------------------------------#




#-----------------------------------------------------------------------------#
# 3.	One event per year in time slice; add to 15 or 10  [6 tables total]
#-----------------------------------------------------------------------------#
for(cls in cl){
  for(ysl in ys){
    cat(sprintf("Class:%s\nYear-Slice:%s\n",cls,ysl))
    cat("             ICEWS               GED              \n")

    tmp <- confusion %>% filter(class == !!{cls},
                                year_slice == !!{ysl}) %>%
      select(cinep:ged) %>%
      mutate(across(everything(), ~case_when(.x > 0 ~ 1, TRUE ~ 0))) %>%
      mutate(across(everything(), ~factor(.x, levels = c("0","1"))))

    res <- sapply(c("icews", "ged"), function(x){
      cfm <- caret::confusionMatrix(data = tmp[[x]], reference = tmp$cinep, positive = "1")

      tabs <- cfm$table
      acc  <- round(cfm$overall[c("AccuracyLower","Accuracy","AccuracyUpper")] * 100,2)

      kap  <- round(kappa_cm(observed = tmp$cinep, p_bin = tmp[[x]]),2)

      return(list("table"    = t(tabs),
                  "accuracy" = acc,
                  "kappa"    = kap))
    }, simplify = FALSE)

    tab <- cbind(res$icews$table, res$ged$table)
    colnames(tab) <- rep(c("No Event", "Event"),2)
    rownames(tab) <- rep(c("No Event", "Event"),1)

    acc <- data.frame(rbind(res$icews$accuracy,res$ged$accuracy)) %>%
      mutate(CI = sprintf("[%s, %s]", AccuracyLower, AccuracyUpper)) %>%
      select(Accuracy, CI)
    rownames(acc) <- c("ICEWS","GED")

    kap <- data.frame(rbind(res$icews$kappa,res$ged$kappa)) %>%
      mutate(CI = sprintf("[%s, %s]", Kappa_lci, Kappa_uci)) %>%
      select(Kappa, CI)
    rownames(kap) <- c("ICEWS","GED")

    print(tab);cat(rep("\n",2))
    print(acc);cat(rep("\n",2))
    print(kap);cat(rep("\n",2))
    cat(rep("\n",6))
  }
};rm(cls, ysl, tmp, res, tab, acc, kap)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# 4.	One event over time slice years; adds to 5	       [6 tables total]
#-----------------------------------------------------------------------------#
for(cls in cl){
  for(ysl in ys){
    cat(sprintf("Class:%s\nYear-Slice:%s\n",cls,ysl))
    cat("             ICEWS               GED              \n")

    tmp <- confusion %>% filter(class == !!{cls},
                                year_slice == !!{ysl}) %>%
      select(Municipality,cinep, icews, ged) %>%
      group_by(Municipality) %>%
      summarize(across(everything(), ~sum(.x))) %>%
      ungroup() %>%
      select(cinep:ged) %>%
      mutate(across(everything(), ~case_when(.x > 0 ~ 1, TRUE ~ 0))) %>%
      mutate(across(everything(), ~factor(.x, levels = c("0","1"))))

    res <- sapply(c("icews", "ged"), function(x){
      cfm <- caret::confusionMatrix(data = tmp[[x]], reference = tmp$cinep, positive = "1")

      tabs <- cfm$table
      acc  <- round(cfm$overall[c("AccuracyLower","Accuracy","AccuracyUpper")] * 100,2)

      kap  <- round(kappa_cm(observed = tmp$cinep, p_bin = tmp[[x]]),2)

      return(list("table"    = t(tabs),
                  "accuracy" = acc,
                  "kappa"    = kap))
    }, simplify = FALSE)

    tab <- cbind(res$icews$table, res$ged$table)
    colnames(tab) <- rep(c("No Event", "Event"),2)
    rownames(tab) <- rep(c("No Event", "Event"),1)

    acc <- data.frame(rbind(res$icews$accuracy,res$ged$accuracy)) %>%
      mutate(CI = sprintf("[%s, %s]", AccuracyLower, AccuracyUpper)) %>%
      select(Accuracy, CI)
    rownames(acc) <- c("ICEWS","GED")

    kap <- data.frame(rbind(res$icews$kappa,res$ged$kappa)) %>%
      mutate(CI = sprintf("[%s, %s]", Kappa_lci, Kappa_uci)) %>%
      select(Kappa, CI)
    rownames(kap) <- c("ICEWS","GED")

    print(tab);cat(rep("\n",2))
    print(acc);cat(rep("\n",2))
    print(kap);cat(rep("\n",2))
    cat(rep("\n",6))
  }
};rm(cls, ysl, tmp, res, tab, acc, kap)
#-----------------------------------------------------------------------------#

























# NOTE - ONLY USING 5 REMOTE, 5 NON-REMOTE MUNICIPALITIES FOR THESE PLOTS BELOW

#-----------------------------------------------------------------------------#
# COHEN'S KAPPA - CROSS-SECTIONS                                          ----
#-----------------------------------------------------------------------------#

pltdat1 <- confusion_cs %>%
  filter(class == "Remote") %>%
  select(cinep, icews) %>%
  {kappa_cm(observed = .$cinep,
            p_bin    = .$icews)} %>%
  mutate(var  = 'ICEWS') %>%
  select(Kappa_lci, Kappa, Kappa_uci, var)

pltdat2 <- confusion_cs %>%
  filter(class == "Remote") %>%
  select(cinep, ged) %>%
  {kappa_cm(observed = .$cinep,
            p_bin    = .$ged)} %>%
  mutate(var  = 'GED') %>%
  select(Kappa_lci, Kappa, Kappa_uci, var)

pltdat <- bind_rows(pltdat1, pltdat2)

ggplot(data =pltdat, aes(x = c(0,1))) +
  geom_point(aes(y = Kappa, color = var)) +
  geom_errorbar(aes(ymin = Kappa_lci, ymax = Kappa_uci, color = var),
                width = 0.2) +
  xlim(-0.2,1.2) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray70", size = 0.5),
        axis.title = element_blank()) +
  labs(y = "Cohen's Kappa") +
  scale_x_continuous(breaks = c(0,1),
                     labels = pltdat$var)


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
}, simplify = FALSE) %>%
  bind_rows(.) %>%
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
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
