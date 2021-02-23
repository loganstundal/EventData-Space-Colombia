#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          May 23, 2020                                                 
# Purpose:       INLA SPDE geostatistical models                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#                                                                             
#                                                                             
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Clear working directory   
#---------------------------#
rm(list = ls())

#---------------------------#
# Load required libraries   
#---------------------------#
library(tidyverse)
library(INLA)
library(sf)

#---------------------------#
# Set working directory     
#---------------------------#
setwd('C:/Users/logan/GoogleDrive/UMN/RESEARCH/RA_John/Event_Data_Project')

#---------------------------#
# Load data                 
#---------------------------#
load('data/Colombia.Rdata')
rm(list = setdiff(ls(),c('colombia', 'colombia_lvl0')))

#---------------------------#
# Load functions            
#---------------------------#
map_prep <- function(mesh, boundary, res = 100){
  meshvert       = rbind(c(mesh$loc[,1],mesh$loc[,2]))
  proj           = inla.mesh.projector(mesh, projection = "longlat", 
                                       dims = c(res,res))
  
  boundary  = st_transform(boundary, crs = "+proj=longlat +datum=WGS84")
  boundary  = as_Spatial(from = boundary, 
                         IDs  = 1:nrow(boundary))
  
  e              = expand.grid(proj$x,proj$y)
  coordinates(e) = c("Var1","Var2")
  proj4string(e) = CRS(proj4string(boundary))
  e              = e[boundary]
  ins            = as.matrix(e@coords)
  
  return(list('proj'     = proj,
              'ins'      = ins,
              'boundary' = boundary))
}

post_gmrf <- function(model, map_prep, n_breaks = 8, map_title = NULL){
  proj = map_prep$proj
  
  # Resolve mean predictions with mesh vertices
  xmean       = inla.mesh.project(projector = proj,
                                  field     = model$summary.random$spatial.field$mean)
  xmean[!map_prep$ins] = NA
  
  # Extract values for the probability of FARC attack
  probsurf    =  binomial(link='probit')$linkinv(xmean)
  
  # Create breaks
  if(length(n_breaks) == 1){
  breaks = extended_breaks(n = n_breaks)(probsurf)  
  } else{
    breaks = n_breaks
  } 
  
  # Convert probability surfaces to raster objects
  colnames(probsurf) = proj$x
  rownames(probsurf) = proj$y
  probsurf           = raster(x   = probsurf, 
                              xmn = min(proj$x),
                              xmx = max(proj$x),
                              ymn = min(proj$y),
                              ymx = max(proj$y),
                              crs = CRS(proj4string(map_prep$boundary)))
  probsurf = mask(probsurf, map_prep$boundary)
  probsurf = as.data.frame(probsurf, xy = TRUE) %>% rename(z = layer)

  # Produce ggplot map objects - posterior probabilities
  map_prob = ggplot() +
    geom_raster(data = probsurf,
                aes(x    = x,
                    y    = y,
                    fill = z),
                alpha = 0.85) +
    labs(title = map_title) +
    scale_fill_gradientn(name     = '',
                         colours  = viridis::magma(n = length(breaks)),
                         na.value = 'transparent',
                         breaks   = breaks, 
                         labels   = scales::percent,
                         limits   = c(0, 1)) +
    geom_sf(data = st_as_sf(map_prep$boundary), fill = 'transparent') +
    theme_minimal() +
    theme(axis.text  = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.margin=unit(c(t = 0, l = 0, b = -1, r = 0),"cm"))
  
  return(map_prob)
}

gplot <- function(plot1, plot2, label = NULL){
  tmp = ggdraw(xlim = c(0, 120), ylim = c(0, 100)) +
    draw_plot(plot   = plot1,
              x      = 10,
              y      = 15,
              width  = 50,
              height = 85) +
    draw_plot(plot   = plot2,
              x      = 60,
              y      = 15,
              width  = 50, 
              height = 85)
  
  if(!is.null(label)){
    tmp = tmp + draw_label(label = paste0(sprintf('Plot date: %s.\n',format(Sys.Date(),"%B %d, %Y")), label),
                           x = 100, 
                           y = 008,
                           size = 10,
                           hjust = 1)
  } else{
    tmp  = tmp + draw_label(label = paste0(sprintf('Plot date: %s.',format(Sys.Date(),"%B %d, %Y"))),
                            x = 100, 
                            y = 008,
                            size = 10,
                            hjust = 1)
  }
  return(tmp)
} 

my_theme <- function(){
  theme_void() + 
    theme(panel.grid       = element_blank(),
          plot.title       = element_text(hjust = 0),
          legend.position  = 'bottom',
          legend.direction = 'horizontal',
          legend.key.width = unit(0.5,'in'))
}

# TIDY DATA -------------------------------------------------------------------
# CONSTRUCT DISAGREEMENT VARIABLES
colombia <- colombia %>%
  mutate(icews_farc_bin = as.integer(icews_farc > 0),
         ged_farc_bin   = as.integer(ged_farc > 0),
         cinep_bin      = as.integer(cinep_farc > 0)) %>%
  mutate(icews_cinep_under = case_when(icews_farc_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         icews_cinep_over  = case_when(icews_farc_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
         icews_cinep_bias  = case_when(icews_farc_bin != cinep_bin ~ 1, TRUE ~ 0),
         
         ged_cinep_under   = case_when(ged_farc_bin == 0 & cinep_bin == 1 ~ 1, TRUE ~ 0),
         ged_cinep_over    = case_when(ged_farc_bin == 1 & cinep_bin == 0 ~ 1, TRUE ~ 0),
         ged_cinep_bias    = case_when(ged_farc_bin != cinep_bin ~ 1, TRUE ~ 0),
         
         icews_ged_under   = case_when(icews_farc_bin == 0 & ged_farc_bin == 1 ~ 1, TRUE ~ 0),
         icews_ged_bias    = case_when(icews_farc_bin != ged_farc_bin ~ 1, TRUE ~ 0),
         
         icews_cinep_full_bias = case_when(icews_farc_bin == 0 & cinep_bin == 1 ~ 'Underreport',
                                           icews_farc_bin == 1 & cinep_bin == 0 ~ 'Overreport',
                                           icews_farc_bin == cinep_bin ~ 'Agree'),
         
         ged_cinep_full_bias = case_when(ged_farc_bin == 0 & cinep_bin == 1 ~ 'Underreport',
                                         ged_farc_bin == 1 & cinep_bin == 0 ~ 'Overreport',
                                         ged_farc_bin == cinep_bin ~ 'Agree'),
         
         distance_bogota_km_ln = log(distance_bogota_km),
         google_ee_pop_sum_ln  = log(google_ee_pop_sum),
         google_tri_ln         = log(google_terrain_ri_mean_m))


# INLA ANALYSIS ---------------------------------------------------------------
colcoord <- cbind(colombia$centroid_mun_long, colombia$centroid_mun_lat)
border   <- inla.mesh.segment(colcoord)
mesh     <- inla.mesh.2d(loc.domain = border$loc, max.edge = c(1.6))
nv       <- mesh$n
A        <- inla.spde.make.A(mesh = mesh,
                             loc  = as.matrix(cbind(colombia$centroid_mun_long,
                                                    colombia$centroid_mun_lat)))

# SPDE ------------------------------ #
spde <- inla.spde2.matern(mesh, alpha=2)

# MODEL SETUP ----------------------- #
formula <- y ~ -1 + intercept + dist + pop + tri + f(spatial.field, model=spde)

dvs <- c(
         # 'icews_cinep_under','ged_cinep_under',
         'icews_cinep_over', 'ged_cinep_over'
         # 'icews_cinep_bias', 'ged_cinep_bias'
         )

# dvs <- c('icews_farc_bin')

stacks <- lapply(dvs, function(x){
  inla.stack(data    = list(y = colombia[[x]]),
             A       = list(A,1,1,1), 
             effects = list(c(list(intercept = rep(1,nv)),
                              inla.spde.make.index("spatial.field", spde$n.spde)),
                            dist = colombia$distance_bogota_km_ln,
                            pop  = colombia$google_ee_pop_sum_ln,
                            tri  = colombia$google_terrain_ri_mean_m),
             tag='spde')
});names(stacks) <- dvs

# ESTIMATION ------------------------ #
inla_mods <- lapply(1:length(dvs), function(x){
  print(sprintf('Working on model DV: %s', dvs[x]))
  inla(formula = formula, 
       data    = inla.stack.data(stacks[[x]], spde = spde),
       family  = 'binomial',
       control.family    = list(link = 'probit'),
       control.predictor = list(A = inla.stack.A(stacks[[x]]), compute = TRUE),
       control.compute   = list(waic   = TRUE, 
                                config = TRUE))
})


inla_icews_under <- inla(formula,
                         data = inla.stack.data(stacks[['icews_farc_bin']],
                                                spde = spde),
                         family = 'binomial',
                         control.family    = list(link   = "probit"),
                         control.predictor = list(A      = inla.stack.A(stacks[['icews_farc_bin']]), compute = TRUE),
                         control.compute   = list(waic   = TRUE,
                                                  config = TRUE))

inla_icews_bias <- inla(formula,
                        data = inla.stack.data(stacks[['icews_cinep_bias']],
                                               spde = spde),
                        family = 'binomial',
                        control.family    = list(link   = "probit"),
                        control.predictor = list(A      = inla.stack.A(stacks[['icews_cinep_bias']]), compute = TRUE),
                        control.compute   = list(waic   = TRUE,
                                                 config = TRUE))

inla_ged_under <- inla(formula,
                       data = inla.stack.data(stacks[['ged_cinep_under']],
                                              spde = spde),
                       family = 'binomial',
                       control.family    = list(link   = "probit"),
                       control.predictor = list(A      = inla.stack.A(stacks[['ged_cinep_under']]), compute = TRUE),
                       control.compute   = list(waic   = TRUE,
                                                config = TRUE))

inla_ged_bias <- inla(formula,
                      data = inla.stack.data(stacks[['ged_cinep_bias']],
                                             spde = spde),
                      family = 'binomial',
                      control.family    = list(link   = "probit"),
                      control.predictor = list(A      = inla.stack.A(stacks[['ged_cinep_bias']]), compute = TRUE),
                      control.compute   = list(waic   = TRUE,
                                               config = TRUE))


inla_icews_under$summary.fixed[,c(1,3,5)]
inla_ged_under$summary.fixed[,c(1,3,5)]

inla_icews_bias$summary.fixed[,c(1,3,5)]
inla_ged_bias$summary.fixed[,c(1,3,5)]

#-----------------------------------------------------------------------------#
# MAPS
# ----------------------------------- #
model_map_prep <- map_prep(mesh = mesh, boundary = colombia_lvl0, res = 300)
# ----------------------------------- #

icews_under <- post_gmrf(model     = inla_icews_under,
                         map_prep  = model_map_prep, 
                         n_breaks  = seq(0,1.0,0.2),
                         map_title = 'ICEWS: Posterior Probability')
icews_under <- icews_under + labs(subtitle = 'Underreporting') + my_theme()
icews_bias  <- post_gmrf(model     = inla_icews_bias,
                         map_prep  = model_map_prep, 
                         n_breaks  = seq(0,1.0,0.2),
                         map_title = 'ICEWS: Posterior Probability')
icews_bias  <- icews_bias + labs(subtitle = 'Bias') + my_theme()


ged_under <- post_gmrf(model     = inla_ged_under,
                       map_prep  = model_map_prep, 
                       n_breaks  = seq(0,1.0,0.2),
                       map_title = 'GED: Posterior Probability')
ged_under <- ged_under + labs(subtitle = 'Underreporting') + my_theme()
ged_bias  <- post_gmrf(model     = inla_ged_bias,
                       map_prep  = model_map_prep, 
                       n_breaks  = seq(0,1.0,0.2),
                       map_title = 'GED: Posterior Probability')
ged_bias  <- ged_bias + labs(subtitle = 'Bias') + my_theme()


under     <- gplot(icews_under, ged_under,
                   label = paste0('Underreporting: Source does not report a FARC event where CINEP does.'))
bias      <- gplot(icews_bias, ged_bias,
                   label = paste0('Bias: Any disagreement between source and CINEP'))


ggsave(filename = 'Plots/PLOTS_DV_COV/Posterior_Underreport.png',
       plot     = under,
       width    = 16,
       height   = 10,
       dpi      = 350,
       units    = 'in')

ggsave(filename = 'Plots/PLOTS_DV_COV/Posterior_Bias.png',
       plot     = bias,
       width    = 16,
       height   = 10,
       dpi      = 350,
       units    = 'in')


# SAVE ------------------------------------------------------------------------
# save()
# rm(list = ls())