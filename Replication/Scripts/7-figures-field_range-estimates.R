#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          July 26, 2021
# Purpose:       Note
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
library(INLA)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(raster)
library(cowplot)
library(scales)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Results/Replication-Estimates/field-data.Rdata")
load("Results/Replication-Estimates/range-data.Rdata")
load("Data/colombia.Rdata")
#---------------------------#

#---------------------------#
# Functions
#---------------------------#
field_prep <- function(mesh, boundary, proj){
  # This function prepares inputs for plotting the GMRF - specifically it
  # identifies which estimates fall within the mesh boundary ("ins")
  boundary  = st_transform(boundary, crs = "+proj=longlat +datum=WGS84")
  boundary  = as_Spatial(from = boundary,
                         IDs  = 1:nrow(boundary))

  e              = expand.grid(proj$x,proj$y)
  coordinates(e) = c("Var1","Var2")
  proj4string(e) = CRS(suppressWarnings({proj4string(boundary)}))
  e              = e[boundary]
  ins            = as.matrix(e@coords)

  return(list('proj'     = proj,
              'ins'      = ins,
              'boundary' = boundary))
}


field_surf <- function(field,
                       field_prep,
                       probs = FALSE){
  # This function takes field data and returns a data frame that contains
  # xy coordinates and a "z" vector that corresponds to field estiamtes.

  require(raster)

  # Grab the field projection object from "field_prep" and set projection obs
  # in "field" which extend beyond the projected area as NA
  proj <- field_prep$proj
  field[!field_prep$ins] <- NA

  # IF probability is true, convert linear-additive means to probabilities
  # using the probit inverse link
  if(probs){
    surf  =  binomial(link='probit')$linkinv(field)
  } else{
    surf  =  field
  }

  # Convert surfaces to raster objects and return rasters as data frames for
  # ggplot
  colnames(surf) = proj$x
  rownames(surf) = proj$y
  surf           = raster(x   = surf,
                          xmn = min(proj$x),
                          xmx = max(proj$x),
                          ymn = min(proj$y),
                          ymx = max(proj$y),
                          crs = CRS(suppressWarnings({proj4string(field_prep$boundary)})))
  surf = mask(surf, field_prep$boundary)
  surf = as.data.frame(surf, xy = TRUE) %>% rename(z = layer) %>%
    drop_na(z)

  return(surf)
}


field_map <- function(field_df, title, breaks, limits, percent = FALSE){
  # Produce ggplot maps of GMRF fields. This is done to consistently enforce
  # plot space. ggplot grobs shift to accomodate changing plot margins due to
  # distinct title and legend character spacing. This function, in conjunction
  # with adjust_labs() resolves this shifting

  # Create adjusted labels with white space padding on right to prevent
  # legend shifting in plots
  # nb.: max_length = 5; " 100%" in one plot = 5 chars. (4 + leading space))
  if(!percent){
    adj_labs <- adjust_labs(label = breaks, max_length = 5)
  } else{
    adj_labs <- adjust_labs(label = percent(breaks, accuracy = 1),
                            max_length = 5)
  }


  plt <- ggplot(field_df) +
    geom_raster(aes(x    = x,
                    y    = y,
                    fill = z),
                alpha = 0.85) +
    geom_point(aes(x = -74.06456, y = 4.709534), shape = 24,
               fill = 'black', color = 'white',
               size = 1) +
    geom_sf(data = st_as_sf(colombia), fill = 'transparent',
            colour = 'black', size = 0.5) +
    scale_fill_gradientn(colours  = viridis::magma(n = 8),
                         na.value = 'transparent',
                         breaks   = breaks,
                         limits   = {limits},
                         labels   = adj_labs,
                         guide    = guide_colorbar(frame.colour = "black",
                                                   ticks.colour = "black")) +
    theme(plot.background  = element_blank(),
          axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          panel.grid       = element_blank(),
          legend.title     = element_blank(),
          plot.title       = element_text(size = rel(1), debug = TRUE),
          panel.background = element_rect(fill = NA, color = "black",
                                          size = 0.1),
          strip.background = element_rect(fill = "gray90", color = "black",
                                          size = 0.1)) +
    facet_wrap(facets = ~var, ncol = 3)

  # Extract plot legend
  plt_leg <- get_legend(plt)

  # Remove original legend
  plt <- plt + theme(legend.position = "none")

  # Redraw plot with fixed relative widths between plot and legend
  plt <- plot_grid(plt, plt_leg, ncol = 2, rel_widths = c(1, 0.15), align = "l")

  # Draw plot with title to impose strice relative title height
  plt <- ggdraw(plt, ylim = c(0, 1.1)) +
    draw_label({title}, x = 0.03, y = 1.04, hjust = 0)
  return(plt)
}


custom_scale <- function(surfaces,
                         n_breaks){
  # Returns break points and limits for GMRF surfaces to uses as inputs
  # in ggplot scale
  the_breaks = extended_breaks(only.loose = T)(surfaces)
  the_limits = c(min(the_breaks),max(the_breaks))

  return(list("breaks" = the_breaks,
              "limits" = the_limits))
}


adjust_labs <- function(label, max_length, pad_char = " ",
                        add_rhs = 0,  # Additional rhs or lhs padding if needed
                        add_lhs = 0
                        ){
  # Helper function to adjust padding in ggplot legend labels. Need white-space
  # on right to fill-out each label to 5 characters so legend does not shift
  # between plots

  # Limited error handling:
  if(max_length<max(nchar(label))){stop("max_length argument is shorter than shortest label length")}

  # Enforce label as character
  label <- as.character(label)

  # Calculate left padding:
  lp <- grepl(pattern = "-", x = label)
  lp <- ifelse(lp, 0, 1) + add_lhs

  # Calculate right padding:
  rp <- max_length - nchar(label) - lp + add_rhs

  # Create padding:
  res <- sapply(list(lp, rp), function(x){
    sapply(x, function(ws){
      paste0(rep(pad_char, ws), collapse = "")
    })
  }) %>% as.data.frame %>%
    rename(lp = 1, rp = 2) %>%
    mutate(lab  = label) %>%
    mutate(lab2 = sprintf("%s%s%s",lp,lab,rp)) %>%
    pull(lab2)

  return(res)
}


matern_plot <- function(data,
                        range,
                        breaks = NULL){

  if(is.null(breaks)){
    deg    <- 2*pi*6371/360
    breaks <- c(0, 10, 50, 100, 150) / deg
  }

  plt <- ggplot(data = data, aes(x=x)) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = "ICEWS"), alpha = 0.3) +
    geom_line(aes(y = y)) +

    geom_vline(aes(xintercept = range), linetype = "dashed", size = 0.1, color = "gray50") +
    geom_hline(aes(yintercept = 0.1),       linetype = "dashed", size = 0.1, color = "gray50") +

    scale_x_continuous(name   = "Distance [km]",
                       limits = c(0,max(data$x)),
                       breaks = breaks,
                       labels = function(x){x * deg},
                       expand = c(0,0)) +

    scale_y_continuous(name   = "Matern covariance function",
                       limits = c(0,1),
                       breaks = c(seq(0, 1, 0.25), 0.1)) +

    theme_minimal() +
    theme(legend.position  = "bottom",
          legend.direction = "horizontal",
          legend.title     = element_blank(),
          panel.grid       = element_blank(),
          panel.background = element_rect(fill = NA, color = "black", size = 0.1))

  return(plt)
}
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MAPS                                                                    ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Construct common data for all maps
# ----------------------------------- #
model_field_prep <- field_prep(mesh     = mesh_list$mesh,
                               proj     = mesh_list$projector,
                               boundary = colombia)
# ----------------------------------- #


# ----------------------------------- #
# Create field data frames for mapping
# ----------------------------------- #
fields <- sapply(c("ICEWS", "GED", "CINEP"), function(dv){
  sapply(c("mean","probs","sd"), function(stat_type){
    if(stat_type == "probs"){
      stat_type  <- "mean"
      prob_check <- TRUE
    } else{
      prob_check <- FALSE
    }
    field_surf(field    = field_data[[dv]][[stat_type]],
              field_prep = model_field_prep,
              probs    = prob_check)
  }, simplify = FALSE)
}, simplify = FALSE)

# Collect fields by type for mapping with facet_wrap()
# glimpse(fields)

field_mean  <- fields %>% purrr::map("mean")  %>% bind_rows(., .id = "var")
field_probs <- fields %>% purrr::map("probs") %>% bind_rows(., .id = "var")
field_var   <- fields %>% purrr::map("sd")    %>% bind_rows(., .id = "var")

rm(fields)
# ----------------------------------- #


# ----------------------------------- #
# Create maps
# ----------------------------------- #
# Field mean
field_mean_scale <- custom_scale(field_mean$z, 8)
field_means_map  <- field_map(field_df = field_mean,
                              title    = expression(paste('Posterior mean ', xi['s'])),
                              breaks   = field_mean_scale$breaks,
                              limits   = field_mean_scale$limits)

# Field probability
field_probs_map <- field_map(field_df = field_probs,
                             title    = expression(paste('Posterior probability, ', pi['s'])),
                             breaks   = seq(0, 1, 0.2),
                             limits   = c(0,1),
                             percent  = TRUE)

# Field variance
field_var_scale <- custom_scale(field_var$z, 8)
field_var_map   <- field_map(field_df = field_var,
                             title    = expression(paste('Posterior variance ', sigma[xi['s']]^2)),
                             breaks   = field_var_scale$breaks,
                             limits   = field_var_scale$limits)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# FINAL PLOT                                                              ----
#-----------------------------------------------------------------------------#
figureA8 <- plot_grid(field_means_map,field_probs_map,field_var_map,
                      nrow = 3, align = "h", axis = "l", rel_widths = 1)
# figureA8
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE FIELD FIGURES                                                      ----
#-----------------------------------------------------------------------------#
ggsave(plot  = field_probs_map,
       file  = "Results/Replication-Figures/figure_appendix_8_1.png",
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')

ggsave(plot  = field_means_map,
       file  = "Results/Replication-Figures/figure_appendix_8_2.png",
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')

ggsave(plot  = field_var_map,
       file  = "Results/Replication-Figures/figure_appendix_8_3.png",
       dpi   = 320,
       width = 6.5,
       height= 3.0,
       units = 'in')

ggsave(plot  = figureA8,
       file  = "Results/Replication-Figures/figure_appendix_8.png",
       dpi   = 320,
       width = 6.5,
       height= 9.0,
       units = 'in')
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# GMRF Range Estimates
#-----------------------------------------------------------------------------#
# Extract plot data frames
range_df <- range_data %>% map("df") %>% bind_rows(., .id = "var")
range_df <- range_df %>%
  mutate(range = case_when(var == "ICEWS" ~ range_data$ICEWS$range,
                           var == "GED"   ~ range_data$GED$range,
                           var == "CINEP" ~ range_data$CINEP$range))

# Breaks: 0 - 500km (converted to decimal degrees)
breaks <- c(0, 50, 100, 200, 300, 400, 500) / (2*pi*6371/360)

figureA9 <- ggplot(data = range_df, aes(x=x)) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = var), alpha = 0.2) +
  geom_line(aes(y = y, color = var), size = 0.25) +

  geom_vline(aes(xintercept = range), linetype = "dashed", size = 0.2, color = "gray50") +
  geom_hline(aes(yintercept = 0.1),   linetype = "dashed", size = 0.2, color = "gray50") +

  scale_x_continuous(name   = "Distance [km]",
                     limits = c(0,max(breaks)),
                     breaks = breaks,
                     labels = function(x){x * (2*pi*6371/360)},
                     expand = c(0,0)) +

  scale_y_continuous(name   = "Matern covariance function",
                     limits = c(0,1),
                     breaks = c(seq(0, 1, 0.25), 0.1)) +

  # scale_fill_viridis_d() +
  scale_fill_manual(values = model_colors) +
  scale_color_manual(values = rep("black",3)) +

  theme(legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_blank(),
        panel.grid       = element_blank(),
        panel.background = element_rect(fill = NA, color = "black", size = 0.1),
        strip.background = element_rect(fill = "gray95", color = "black", size = 0.1),
        plot.margin      = unit(c(1,3,1,1), "mm")) +
  facet_wrap(~ var, nrow = 3) +
  labs(title    = "Spatial correlation decay",
       subtitle = "SPDE - Observed FARC Events")

ggsave(plot     = figureA9,
       filename = "Results/Replication-Figures/figure_appendix_9.png",
       width    = 4.5,
       height   = 8.0,
       units    = "in",
       dpi      = 350)

rm(list = ls())
#-----------------------------------------------------------------------------#
