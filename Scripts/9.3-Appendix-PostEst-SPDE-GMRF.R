#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          May 06, 2021
# Purpose:       9.3 - Appendix - SPDE Posterior GMRF Maps
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
library(INLA)
library(tidyverse)
library(sf)
library(scales)
library(cowplot)

#---------------------------#
# Load data
#---------------------------#
load("Results/inla-mods.Rdata")

#---------------------------#
# Functions
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

post_surf <- function(model,
                      map_prep,
                      type  = NULL,
                      probs = FALSE){
  require(raster)
  proj = map_prep$proj

  # Resolve mean predictions with mesh vertices
  require(MatrixModels)
  xmean       = inla.mesh.project(projector = proj,
                                  field     = model$summary.random$spatial.field[[type]])
  xmean[!map_prep$ins] = NA

  # Extract values for the probability of FARC attack
  if(probs){
    surf  =  binomial(link='probit')$linkinv(xmean)
  } else{
    surf  =  xmean
  }

  # Convert probability surfaces to raster objects
  colnames(surf) = proj$x
  rownames(surf) = proj$y
  surf           = raster(x   = surf,
                          xmn = min(proj$x),
                          xmx = max(proj$x),
                          ymn = min(proj$y),
                          ymx = max(proj$y),
                          crs = CRS(proj4string(map_prep$boundary)))
  surf = mask(surf, map_prep$boundary)
  surf = as.data.frame(surf, xy = TRUE) %>% rename(z = layer)

  return(surf)
}

custom_scale <- function(surfaces,
                         n_breaks){
  the_breaks = extended_breaks(only.loose = T)(surfaces)
  the_limits = c(min(the_breaks),max(the_breaks))

  return(list('breaks' = the_breaks,
              'limits' = the_limits))
}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SETUP                                                                   ----
#-----------------------------------------------------------------------------#
colcoord <- cbind(dat$`2002-2004`$centroid_mun_long,
                  dat$`2002-2004`$centroid_mun_lat)
border   <- inla.mesh.segment(colcoord)
mesh     <- inla.mesh.2d(loc.domain = border$loc, max.edge = c(1.6))

rm(colcoord, border)

colombia0 <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_COL_0_sf.rds'))

colombia0 <- colombia0 %>% st_crop(xmin = -80.00, xmax = -66.87,
                                   ymin = -04.22, ymax =  14.00)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MAPS                                                                    ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Construct common data for all maps
# ----------------------------------- #
model_map_prep <- map_prep(mesh = mesh, boundary = colombia0, res = 180)

mods <- inla_mods$`2002-2009`[1:3]
names(mods) <- c("ICEWS","GED","CINEP")
# ----------------------------------- #


# ----------------------------------- #
# Posterior predicted probabilities
# ----------------------------------- #
probs <- lapply(mods, function(x){
  post_surf(model    = x,
            map_prep = model_map_prep,
            type     = 'mean',
            probs    = TRUE)
})

plt_dat <- bind_rows(probs, .id = "groups") %>%
  mutate(groups = factor(groups, levels = c("ICEWS", "GED", "CINEP")))

probs <- ggplot(data = plt_dat) +
  geom_raster(aes(x    = x,
                  y    = y,
                  fill = z),
              alpha = 0.85) +

  geom_point(aes(x = -74.06456, y = 4.709534), shape = 24, fill = 'black',color = 'white',
             size = 2) +

  scale_fill_gradientn(colours  = viridis::magma(n = 8),
                       na.value = 'transparent',
                       breaks   = seq(0,1,0.2),
                       labels   = scales::percent,
                       limits   = c(0,1),
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  geom_sf(data = st_as_sf(colombia0), fill = 'transparent', colour = 'black', size = 0.1) +
  theme_minimal() +
  theme(axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title     = element_blank(),
        panel.background = element_rect(fill = NA, color = "black", size = 0.1),
        strip.background = element_rect(fill = "gray90", color = "black", size = 0.1)) +
  facet_wrap(facets = ~groups, ncol = 3) +
  labs(title = expression(paste('Posterior probability, ', pi['s'])))
# ----------------------------------- #


# ----------------------------------- #
# Posterior means
# ----------------------------------- #
means <- lapply(mods, function(x){
  post_surf(model    = x,
            map_prep = model_map_prep,
            type     = 'mean',
            probs    = FALSE)
})

mean_scale_vals = custom_scale(as.numeric(unlist(sapply(means, '[','z'))), 8)

plt_dat <- bind_rows(means, .id = "groups") %>%
  mutate(groups = factor(groups, levels = c("ICEWS", "GED", "CINEP")))

means <- ggplot(data = plt_dat) +
  geom_raster(aes(x    = x,
                  y    = y,
                  fill = z),
              alpha = 0.85) +

  geom_point(aes(x = -74.06456, y = 4.709534), shape = 24, fill = 'black',color = 'white',
             size = 2) +

  scale_fill_gradientn(colours  = viridis::magma(n = 8),
                       na.value = 'transparent',
                       breaks   = mean_scale_vals$breaks,
                       # labels   = scales::percent,
                       limits   = mean_scale_vals$limits,
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  geom_sf(data = st_as_sf(colombia0), fill = 'transparent', colour = 'black', size = 0.1) +
  theme_minimal() +
  theme(axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title     = element_blank(),
        panel.background = element_rect(fill = NA, color = "black", size = 0.1),
        strip.background = element_rect(fill = "gray90", color = "black", size = 0.1)) +
  facet_wrap(facets = ~groups, ncol = 3) +
  labs(title = expression(paste('Posterior mean ', xi['s'])))
# ----------------------------------- #


# ----------------------------------- #
# Posterior Variance
# ----------------------------------- #
vrs <- lapply(mods, function(x){
  post_surf(model    = x,
            map_prep = model_map_prep,
            type     = 'sd',
            probs    = FALSE)
})

vrs_scale_vals = custom_scale(as.numeric(unlist(sapply(vrs, '[','z'))), 8)

plt_dat <- bind_rows(vrs, .id = "groups") %>%
  mutate(groups = factor(groups, levels = c("ICEWS", "GED", "CINEP")))

vrs <- ggplot(data = plt_dat) +
  geom_raster(aes(x    = x,
                  y    = y,
                  fill = z),
              alpha = 0.85) +

  geom_point(aes(x = -74.06456, y = 4.709534), shape = 24, fill = 'black',color = 'white',
             size = 2) +

  scale_fill_gradientn(colours  = viridis::magma(n = 8),
                       na.value = 'transparent',
                       breaks   = vrs_scale_vals$breaks,
                       # labels   = scales::percent,
                       limits   = vrs_scale_vals$limits,
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  geom_sf(data = st_as_sf(colombia0), fill = 'transparent', colour = 'black', size = 0.1) +
  theme_minimal() +
  theme(axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title     = element_blank(),
        panel.background = element_rect(fill = NA, color = "black", size = 0.1),
        strip.background = element_rect(fill = "gray90", color = "black", size = 0.1)) +
  facet_wrap(facets = ~groups, ncol = 3) +
  labs(title = expression(paste('Posterior variance ', sigma[xi['s']]^2)))
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# FINAL PLOT                                                              ----
#-----------------------------------------------------------------------------#
final_plot <- plot_grid(probs, means, vrs, nrow = 3, align = "v", axis = "l")
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
ggsave(plot  = probs,
       file  = 'Results/Plots/Map-spde-Probs.png',
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')

ggsave(plot  = means,
       file  = 'Results/Plots/Map-spde-Means.png',
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')

ggsave(plot  = vrs,
       file  = 'Results/Plots/Map-spde-Vars.png',
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')

ggsave(plot  = final_plot,
       file  = 'Results/Plots/Map-spde-full_plot.png',
       dpi   = 320,
       width = 6.5,
       height= 9.0,
       units = 'in')

rm(list = ls())
dev.off()
#-----------------------------------------------------------------------------#



