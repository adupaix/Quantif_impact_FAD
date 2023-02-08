#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2023-02-08
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Template of the small script used to launch the study
#'              
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

#' empty environment
rm(list = ls())
invisible(gc())

config_name = "ex_cfg.R"

#'@Scripts_base_directory: script root path 
BASE_DIR <- "path_to/Quantif_Impact_FAD/"
setwd(dir = BASE_DIR)

# Sourcing the config file and the main file 
source(file.path(BASE_DIR, "config", config_name))
source(file.path(BASE_DIR, "main.R"))