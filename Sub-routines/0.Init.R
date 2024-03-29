#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: sub-routine to initialize the study
#' Loads libraries
#' Loads subfunctions
#' Generate output files names
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

#' ***************
#' Get libraries:
#' ***************
source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("plyr", "dplyr", "tidyr", "ggplot2", 'sf',
                     "maps", "ggpubr", "ks", "tictoc", "raster")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)


# ~ ----
# ~ ----

# Generate the output path
OUTPUT_PATH <- file.path(OUTPUT_PATH, paste(COMMIT_NAME, DENSITY_FROM, sep = '_'))
if(!dir.exists(OUTPUT_PATH)){dir.create(OUTPUT_PATH, recursive = T, showWarnings = F)}

# get world data for maps
if (any(BUILD_MAPS)){world <- map_data("world")}

#so dates are printed in english
Sys.setlocale("LC_TIME", "en_US.UTF-8") # For Unix
# Sys.setlocale("LC_TIME", "English") # For Windows

vars_map_density <- paste0("DENSITY_BUOYS_", toupper(VARS_DENSITY))
vars_raw_map_density <- paste0("raw_buoy_density_", tolower(VARS_DENSITY))

# if (compare_with_ABBI){resolution = 10}

#' **********
# 0. Functions ----
#' **********
source(file.path(FUNC_PATH, "subfunctions.R"))

#' *****
# 0. Names ----
#' *****

Output_names <- list()

#' Names of 1. CATs
#' ----------------
dir.create(file.path(OUTPUT_PATH, "1.CATs"), showWarnings = F)
for (i in ARRAY_TYPE){
  # names of the files containing all the CAT data for the different types of arrays
  Output_names$cats[[i]]$csv <- file.path(OUTPUT_PATH, "1.CATs", paste0(i,"_cats.csv"))
  # Plots of the proportion of CATdiff and CATreturn
  Output_names$cats[[i]]$plot_proportion <- file.path(OUTPUT_PATH, "1.CATs", paste0(i, "_plot_proportions.png"))
  # Plots of the distributions of CATs
  Output_names$cats[[i]]$plot_distribution <- file.path(OUTPUT_PATH, "1.CATs", paste0(i, "_plot_distribution.png"))
}
Output_names$cats$summary <- file.path(OUTPUT_PATH, "1.CATs", "summary.csv")


#' Names of 1. Regression
#' ----------------------
#' files containing information on the regressions performed
dir.create(file.path(OUTPUT_PATH, "1.Regression"), showWarnings = F)
for (i in ARRAY_TYPE){
  Output_names$regression[[i]] <- generate.output.paths.Regression(path = file.path(OUTPUT_PATH, "1.Regression", i))
}


#' Names of 2. Buoy density
#' ------------------------
#' files obtained from the IOTC data on buoy density
dir.create(file.path(OUTPUT_PATH, "2.Buoy_density"), showWarnings = F)
Output_names$buoy_density$timeseries <- file.path(OUTPUT_PATH, "2.Buoy_density", "timeseries.png")
Output_names$buoy_density$raw_csv <- file.path(OUTPUT_PATH, "2.Buoy_density", "Formatted_buoy_data_with_raw_densities.csv")
Output_names$buoy_density$csv <- file.path(OUTPUT_PATH, "2.Buoy_density", "Formatted_buoy_data.csv")
for (i in 1:length(VARS_DENSITY)){
  Output_names$buoy_density$maps[[as.character(YEAR)]][[i]] <- file.path(OUTPUT_PATH, "2.Buoy_density", paste0("maps_",VARS_DENSITY[i],"_", YEAR,".png"))
}


#' Names of 3_4.Prediction
#' ----------------------
#' Maps of the predicted CATs and percentage of time associated obtained from the different arrays
Output_names$prediction$cats <- list()
dir.create(file.path(OUTPUT_PATH, "3_4.Prediction", "3.CATs"), recursive = T, showWarnings = F)
dir.create(file.path(OUTPUT_PATH, "3_4.Prediction", "4.Percent"), showWarnings = F)
Output_names$prediction$cats <- list()
Output_names$prediction$percent <- list()
for (j in VARS_DENSITY){
  Output_names$prediction$cats[[j]] <- list()
  Output_names$prediction$percent[[j]] <- list()
  for (k in ARRAY_TYPE){
    Output_names$prediction$cats[[j]][[k]] <- list()
    for (l in CAT_TYPE){
      Output_names$prediction$cats[[j]][[k]][[l]] <- file.path(OUTPUT_PATH, "3_4.Prediction", "3.CATs",
                                                               paste("maps", l,
                                                                     COLOR_SCALE_TRANSFORMATION,
                                                                     "max",MAX_DISPLAYED_CAT,
                                                                     "res",RESOLUTION,
                                                                     YEAR,j,k,".png",
                                                                     sep = "_"))
      
      Output_names$prediction$percent[[j]][[k]] <- file.path(OUTPUT_PATH, "3_4.Prediction", "4.Percent",
                                                             paste0("maps_res",RESOLUTION,
                                                                    "_",YEAR,"_",j,"_",k, ".png"))
    }
  }
}

# Values of predicted CATs and percentages of time spent associated, stored in a .csv
Output_names$prediction$csv <- file.path(OUTPUT_PATH, '3_4.Prediction', paste0("predictions_res",RESOLUTION,".csv"))

#' Names of 5.Fishing_pressure
#' --------------------------
dir.create(file.path(OUTPUT_PATH, "5.Fishing_pressure"), showWarnings = F)
analysis_5 <- c("all_FOBs", "owned_FOBs")
for (i in analysis_5){
  dir.create(file.path(OUTPUT_PATH, "5.Fishing_pressure", i), showWarnings = F)
  Output_names$fishing_pressure[[i]] <- generate.output.paths.FOB_sets(path = file.path(OUTPUT_PATH, "5.Fishing_pressure", i))
}