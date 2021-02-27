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
#  This script produces figures for summary statistics tables as well as
#  for simple Kappa analysis tables.
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

#---------------------------#
# Load data
#---------------------------#
load("data/data_variables.RData")


#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())




"
Logan, do you think we could subset the data to municipalities in the highest quartile of
FARC violence (per CINEP) during our period, and then identify from that subset the top
four and bottom four in terms of population/population density? I could take a stab at
this tomorrow if you are tied up estimating the two year time slice models. But if we did
this, we could then show those eight municipality names to Jen, asking her if any of them
stand out as particularly (in)appropriate to consider? Note: I like the idea of doing this
for more than one rural-urban municipality pairing, I suspect what we'll find (even if we
can't report all of it in the main paper) is that the strengths/weaknesses of ICEWS vs.
GED vs. CINEP change a lot depending on the municipality pairing considered.

"
library(kableExtra)

tmp <- colombia %>%
  st_drop_geometry() %>%
  select(year, Department, Municipality, cinep_farc, area_km2, pop_sum) %>%
  mutate(popden = pop_sum / area_km2) %>%
  group_by(Department, Municipality) %>%
  summarize(cinep = sum(cinep_farc),
            popden = mean(popden),
            .groups = "keep")


# quantile(tmp$cinep)
# table(tmp$cinep > 15)

tmp <- tmp %>%
  filter(cinep > 15) %>%
  arrange(desc(popden)) %>%
  mutate(x = as_factor("Sample"),
         LABEL = Municipality,
         nx    = case_when(Municipality %in% c("El Carmen de Bolivar",
                                               "Tibu",
                                               "Tame",
                                               "San Carlos",
                                               "Samana") ~ 1e6,
                           TRUE ~ 0),
         ny   = case_when(Municipality %in% c("El Carmen de Bolivar",
                                              "Tibu",
                                              "Tame",
                                              "San Carlos",
                                              "Samana") ~ 1e6,
                          TRUE ~ -1e6))

tmp[,1:4] %>% kbl(format = "simple", digits = 2)


tmp <- colombia %>%
  filter(year == 2000) %>%
  select(Department, Municipality) %>%
  left_join(., tmp, by = c("Department", "Municipality"))

mp <- ggplot(data = tmp) +
  geom_sf(aes(fill = x), color = "gray10", size = 0.05) +
  scale_fill_discrete(na.value = "transparent") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank()) +
  # geom_sf_label(aes(label = LABEL))
  ggrepel::geom_label_repel(
    # data = nc[c(1:3, 10:14), ],
    aes(label = LABEL, geometry = geometry),
    stat = "sf_coordinates",
    nudge_x = tmp$nx,
    nudge_y = tmp$ny
  )

ggsave(filename = "Documents/High-CINEP-Sample.png",
       units    = "in",
       width    = 8,
       height   = 8,
       dpi      = 340)

#-----------------------------------------------------------------------------#
# CONFUSION MATRICES ----------------------------------------------------------


kappa_cm <- function(observed,
                     pred   = NULL,
                     p_bin  = NULL,
                     cutoff = 0.5,
                     sig    = 0.95,
                     silent = TRUE){


"
If the raters are in complete agreement then {kappa=1}.
If there is no agreement among the raters other than what would be expected
by chance, {kappa =0}.
It is possible for the statistic to be negative, which implies that there is
no effective agreement between the two raters or the agreement is worse than
random.

https://en.wikipedia.org/wiki/Cohen%27s_kappa

"

  if(is.null(pred) & is.null(p_bin)){
    stop('Either "pred" or "p_bin" are required.')
  }

  if(is.null(p_bin)){
    p_bin = ifelse(pred >= cutoff, 1, 0)
  }

  tab = table(observed, p_bin)

  p_agree  = (tab[1] + tab[4]) / sum(tab)
  p_random = (((tab[1] + tab[3]) / sum(tab)) * ((tab[1] + tab[2]) / sum(tab))) +
    (((tab[2] + tab[4]) / sum(tab)) * ((tab[3] + tab[4]) / sum(tab)))

  k    = (p_agree - p_random) / (1 - p_random)
  se_k = sqrt(( p_agree * (1 - p_agree) ) / ( length(observed) * (1 - p_random)^2  ))

  crit = qnorm(1 - (1-sig)/2)

  k_lci = k - crit*se_k
  k_uci = k + crit*se_k

  return(list('Kappa' = k,
              'Kappa_lci' = k_lci,
              'Kappa_uci' = k_uci))

  do.call(sprintf, c(list('Kappa: %s\n
                Kappa CI: [%s, %s]')),
          paste(round(c(k, k_lci, k_uci),3)))

  if(silent == FALSE){
    cat(sprintf('Kappa: %s\nKappa CI: [%s, %s]', round(k,3),round(k_lci,3),round(k_uci,3)))
  }
}



# ----------------------------------- #
# Double checking my kappa fn

# Validating my previous code
icews[5,]
tst <- colombia %>% filter(year == 2004) %>%
  select(icews_farc_bin, cinep_farc_bin) %>%
  st_drop_geometry() %>%
  as_data_frame() %>%
  mutate_all(., as_factor)

psych::cohen.kappa(x = tst)

#data may be explicitly categorical
x <- c("red","yellow","blue","red")
y <- c("red",  "blue", "blue" ,"red")
xy.df <- data.frame(x,y)
ck <- psych::cohen.kappa(xy.df)
ck
ck$agree

kappa_cm(observed = xy.df$y, p_bin = xy.df$x,silent = F)

# ----------------------------------- #





#-----------------------------------------------------------------------------#
# kappy by year
yrs <- sort(unique(colombia$year))

# ----------------------------------- #
# Kappa by year - ICEWS
# ----------------------------------- #
icews <- sapply(yrs, function(x){
  tmp <- colombia %>% filter(year == x)
  return(kappa_cm(observed = tmp$cinep_farc_bin,
                  p_bin    = tmp$icews_farc_bin))
})

icews <- icews %>%
  t %>%
  apply(., 2, as.numeric) %>%
  as.data.frame() %>%
  mutate(year = yrs) %>%
  select(year, Kappa_lci, Kappa, Kappa_uci)
# ----------------------------------- #


# ----------------------------------- #
# Kappa by year - GED
# ----------------------------------- #
ged <- sapply(yrs, function(x){
  tmp <- colombia %>% filter(year == x)
  return(kappa_cm(observed = tmp$cinep_farc_bin,
                  p_bin    = tmp$ged_farc_bin))
})

ged <- ged %>%
  t %>%
  apply(., 2, as.numeric) %>%
  as.data.frame() %>%
  mutate(year = yrs) %>%
  select(year, Kappa_lci, Kappa, Kappa_uci)
# ----------------------------------- #

icews$var <- "icews"
ged$var   <- "ged"

plt_dat <- bind_rows(icews, ged) %>%
  mutate(x = case_when(var == "icews" ~ year - 0.1,
                       TRUE ~ year + 0.1))

ggplot(data = plt_dat, aes(x = x)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "gray70", size = 0.5) +
  geom_errorbar(aes(ymin = Kappa_lci,
                    ymax = Kappa_uci,
                    color = var)) +
  geom_point(aes(y = Kappa, color = var)) +
  scale_x_continuous(breaks = plt_dat$year,
                     labels = plt_dat$year) +
  scale_color_discrete(labels = c("GED","ICEWS")) +
  theme_bw() +
  theme(panel.grid       = element_blank(),
        legend.title     = element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        axis.title.x     = element_blank()) +
  labs(title    = "Inter-rating Reliability: Cohen's Kappa",
       subtitle = "Measures of ICEWS and GED coding reliability relative to CINEP Ground Truth")



#-----------------------------------------------------------------------------#




# Kappa statistics
d <- sapply(c('icews_farc_bin','ged_farc_bin'), function(x){
  kappa_cm(observed = colombia$cinep_farc_bin,
           p_bin    = colombia[[x]])
}) %>% t %>% apply(., 2, as.numeric) %>% as.data.frame













kappa_plot <- {ggplot(data = d, aes(x = c(0,0))) +
    geom_point(aes(y = Kappa, colour = c('ICEWS','GED'))) +
    geom_errorbar(aes(ymin = Kappa_lci,
                      ymax = Kappa_uci,
                      colour = c('ICEWS','GED')),
                  width = 0.10) +
    geom_hline(aes(yintercept = 0), linetype = 'solid', size = 1, color = 'gray70') +
    scale_x_continuous(name   = '',
                       limits = c(-0.25, 0.25)) +
    scale_y_continuous(name   = '',
                       limits = c(-0.01, 0.5)) +
    # scale_color_manual(values = c('ICEWS' = cols[1],
    #                               'GED'   = cols[2])) +
    # coord_equal(expand = FALSE) +
    theme_minimal() +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.title       = element_blank(),
          axis.ticks.x       = element_blank(),
          axis.text.x        = element_blank(),
          axis.title.x       = element_blank(),
          legend.position    = 'bottom',
          legend.direction   = 'horizontal') +
    labs(title    = 'Kappa values and 95% confidence intervals',
         subtitle = 'ICEWS and GED reliabilty compared to CINEP',
         caption  = sprintf('Plot date: %s.', format(Sys.Date(), "%B %d, %Y")))
}
ggsave(filename = 'paper drafts/memo_20200325/plots_20200325/RawData_Kappa.png',
       plot     = kappa_plot,
       width    = 5,
       height   = 7,
       dpi      = 320)

# ----------------------------------- #

# Confusion Matrices
lapply(c('icews_farc_bin','ged_farc_bin'), function(x){
  caret::confusionMatrix(as_factor(colombia$cinep_farc_bin), as_factor(colombia[[x]]))
})









