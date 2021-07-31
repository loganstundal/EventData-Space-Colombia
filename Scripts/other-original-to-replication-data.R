
rm(list= ls())
load("data/data_variables.RData")
rm(colombia_yg)

colombia <- colombia_pn %>%
  as.data.frame() %>%
  mutate(yr_grp = case_when(year %in% 2002:2004 ~ "2002-2004",
                            year %in% 2005:2007 ~ "2005-2007",
                            year %in% 2008:2009 ~ "2008-2009")) %>%
  group_by(Department, Municipality, yr_grp) %>%
  summarize(across(c(cinep, icews, ged), sum),
            across(c(distance_bogota_km_ln, terrain_ri_mean_m,
                     pop_sum, pop_sum_ln, area_km2, area_km2_ln, distance_bogota_km,
                     centroid_mun_long, centroid_mun_lat), ~.x[1]),
            pop_sum_ln = mean(pop_sum_ln),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(across(c(cinep, icews, ged), ~case_when(.x > 0 ~ 1, TRUE ~ 0), .names = "{col}_bin")) %>%
  mutate(icews_cinep_under = case_when(icews_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         icews_cinep_over  = case_when(icews_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
         icews_cinep_bias  = case_when(icews_bin != cinep_bin ~ 1, TRUE ~ 0),

         ged_cinep_under   = case_when(ged_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         ged_cinep_over    = case_when(ged_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
         ged_cinep_bias    = case_when(ged_bin != cinep_bin ~ 1, TRUE ~ 0))
# ----------------------------------- #


# ----------------------------------- #
colombia <- colombia_cs %>%
  st_drop_geometry() %>%
  mutate(icews_cinep_under = case_when(icews_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         icews_cinep_over  = case_when(icews_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
         icews_cinep_bias  = case_when(icews_bin != cinep_bin ~ 1, TRUE ~ 0),

         ged_cinep_under   = case_when(ged_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         ged_cinep_over    = case_when(ged_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
         ged_cinep_bias    = case_when(ged_bin != cinep_bin ~ 1, TRUE ~ 0)) %>%
  mutate(yr_grp = "2002-2009") %>%
  select(names(colombia)) %>%
  bind_rows(colombia, .)

rm(colombia_cs)
# ----------------------------------- #


# ----------------------------------- #
# Tidy variable
colombia %<>% mutate(yr_grp = factor(yr_grp,
                                     levels = c("2002-2009",
                                                "2002-2004",
                                                "2005-2007",
                                                "2008-2009")))

# ID To use in many apply function calls below:
yr_grp <- as.character(unique(colombia$yr_grp))
# ----------------------------------- #


# ----------------------------------- #
# Arrange data for lat-long alignment
# ----------------------------------- #
dat <- sapply(yr_grp, function(x){
  colombia %>% filter(yr_grp == x) %>%
    arrange(Department, Municipality, yr_grp) %>%
    janitor::clean_names() %>%
    select(-contains(c("bias", "over"))) %>%
    select(department, municipality, yr_grp,
           starts_with(c("cinep", "icews", "ged")),
           contains(c("area","distance","pop_","terrain")),
           starts_with("centroid"))
}, simplify = FALSE)
rm(colombia)
# ----------------------------------- #


# ----------------------------------- #
dat$Yearly_panel <- colombia_pn %>%
  janitor::clean_names() %>%
  select(department, municipality, year, cinep, cinep_bin, icews, icews_bin,
         icews_cinep_under, ged, ged_bin, ged_cinep_under, area_km2,
         area_km2_ln, distance_bogota_km_ln, distance_bogota_km, pop_sum,
         pop_sum_ln, terrain_ri_mean_m, centroid_mun_long, centroid_mun_lat)

rm(colombia_pn)
# ----------------------------------- #


yr_grp <- names(dat[1:4])
save(dat, file = "../Data/farc_events.Rdata")
