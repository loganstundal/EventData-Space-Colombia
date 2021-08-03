#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          August 01, 2021
# Purpose:       Master script for either:
#                1 - installing all packages used in scripts 1-7
#                2 - executing scripts 1-7 sequentially
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Assumptions:
#
#  - You have `Rscript` available on your system path environmental variables
#  - You have opened "0-master.R" from within the "Replication/Scripts"
#    directory or you have set manually set your working directory to
#    "Replication/Scripts"
#
#-----------------------------------------------------------------------------#


# ----------------------------------- #
# Install all required packages
# ----------------------------------- #
# Install INLA - not on CRAN
# install.packages("https://inla.r-inla-download.org/R/stable/src/contrib/INLA_21.02.23.tar.gz",
#                  repos  = NULL,
#                  method = "libcurl")

# Install CRAN packages
# install.packages(c("cowplot","dplyr","forcats","ggplot2","ggrepel","kableExtra",
#                    "ProbitSpatial","pROC","purrr","raster","sandwich","scales",
#                    "sf","spdep","stringr","tibble","tidyr"))
# ----------------------------------- #


# ----------------------------------- #
# Execute replication scripts
# ----------------------------------- #
# Set working directory to main "Replication/" directory
setwd("../")

# Get file names for scripts 1-7
target_files <- list.files("Scripts")
target_files <- target_files[grepl("[1-7]", target_files)]

# Create a list to record execution times:
script_times <- list()

# Run replication scripts:
{
  t_start_global <- Sys.time()
  for(i in target_files){
    cat(sprintf("Working on: %s\n", i))

    t_start_local <- Sys.time()

    system(command = sprintf("Rscript %s/Scripts/%s", getwd(), i),
           show.output.on.console = FALSE)

    t_end_local <- Sys.time()

    script_times[[i]] <- t_end_local - t_start_local
  };rm(i)
  t_end_global <- Sys.time()
}

# Total execution time for all 7 scripts:
t_end_global - t_start_global

# Individually:
script_times
# ----------------------------------- #
#-----------------------------------------------------------------------------#
