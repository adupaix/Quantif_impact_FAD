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

srcUsedPackages <- c("plyr", "dplyr", "tidyr", "ggplot2", "ggspatial", 'sf',
                     "ggpubr")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)

# ~ ----
# ~ ----

if (any(build_maps)){world <- map_data("world")}

#so dates are printed in english
Sys.setlocale("LC_TIME", "en_US.UTF-8") # For Unix
# Sys.setlocale("LC_TIME", "English") # For Windows

vars_map_density <- paste0("DENSITY_BUOYS_", toupper(vars_map_density))

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
try(dir.create(file.path(OUTPUT_PATH, "1.CATs")), silent = T)
# names of the files containing all the CAT data for the different types of arrays
Output_names$cats$square$csv <- file.path(OUTPUT_PATH, "1.CATs", "square_cats.csv")
Output_names$cats$square_rd$csv <- file.path(OUTPUT_PATH, "1.CATs", "square_rd_cats.csv")
Output_names$cats$random$csv <- file.path(OUTPUT_PATH, "1.CATs", "random_cats.csv")
Output_names$cats$summary <- file.path(OUTPUT_PATH, "1.CATs", "summary.csv")
# Plots of the proportion of CATdiff and CATreturn
Output_names$cats$square$plot_proportion <- file.path(OUTPUT_PATH, "1.CATs", "square_plot_proportions.png")
Output_names$cats$square_rd$plot_proportion <- file.path(OUTPUT_PATH, "1.CATs", "square_rd_plot_proportions.png")
Output_names$cats$random$plot_proportion <- file.path(OUTPUT_PATH, "1.CATs", "random_plot_proportions.png")
# Plots of the distributions of CATs
Output_names$cats$square$plot_distribution <- file.path(OUTPUT_PATH, "1.CATs", "square_plot_distribution.png")
Output_names$cats$square_rd$plot_distribution <- file.path(OUTPUT_PATH, "1.CATs", "square_rd_plot_distribution.png")
Output_names$cats$random$plot_distribution <- file.path(OUTPUT_PATH, "1.CATs", "random_plot_distribution.png")

#' Names of 1. Regression
#' ----------------------
#' files containing information on the regressions performed
try(dir.create(file.path(OUTPUT_PATH, "1.Regression")), silent = T)
Output_names$regression$square <- generate.output.paths.Regression(path = file.path(OUTPUT_PATH, "1.Regression", "square"))
Output_names$regression$square_rd <- generate.output.paths.Regression(path = file.path(OUTPUT_PATH, "1.Regression", "square_rd"))
Output_names$regression$random <- generate.output.paths.Regression(path = file.path(OUTPUT_PATH, "1.Regression", "random"))

Output_names$regression$plot <- file.path(OUTPUT_PATH, "1.Regression", "general_plot.png")
Output_names$regression$plot2 <- file.path(OUTPUT_PATH, "1.Regression", "random_and_square_plot.png")

# files with the nearest neighbor plots 
# try(dir.create(file.path(OUTPUT_PATH, "NN_plots")), silent = T)
# Output_names$nn_plot_square1 <- file.path(OUTPUT_PATH, "NN_plots", "nn_square1.png")
# Output_names$nn_plot_square2 <- file.path(OUTPUT_PATH, "NN_plots", "nn_square2.png")
# Output_names$nn_plot_square_rd1 <- file.path(OUTPUT_PATH, "NN_plots", "nn_square_rd1.png")
# Output_names$nn_plot_square_rd2 <- file.path(OUTPUT_PATH, "NN_plots", "nn_square_rd2.png")
# Output_names$nn_plot_random1 <- file.path(OUTPUT_PATH, "NN_plots", "nn_random1.png")
# Output_names$nn_plot_random2 <- file.path(OUTPUT_PATH, "NN_plots", "nn_random2.png")


#' Names of 2. Buoy density
#' ------------------------
#' files obtained from the IOTC data on buoy density
try(dir.create(file.path(OUTPUT_PATH, "2.Buoy_density")), silent = T)
Output_names$buoy_density$timeseries <- file.path(OUTPUT_PATH, "2.Buoy_density", "timeseries.png")
for (i in 1:length(fct)){
  Output_names$buoy_density$maps$`2020`[[i]] <- file.path(OUTPUT_PATH, "2.Buoy_density", paste0("maps_",fct[i],"_2020.png"))
  Output_names$buoy_density$maps$`2021`[[i]] <- file.path(OUTPUT_PATH, "2.Buoy_density", paste0("maps_",fct[i],"_2021.png"))
}


#' Names of 3_4.Prediction
#' ----------------------
#' Maps of the predicted CATs and percentage of time associated obtained from the different arrays
Output_names$prediction$cats <- list()
try(dir.create(file.path(OUTPUT_PATH, "3_4.Prediction", "3.CATs"), recursive = T), silent = T)
try(dir.create(file.path(OUTPUT_PATH, "3_4.Prediction", "4.Percent")), silent = T)
for (i in as.character(2020:2021)){
  Output_names$prediction$cats[[i]] <- list()
  Output_names$prediction$percent[[i]] <- list()
  for (j in fct){
    Output_names$prediction$cats[[i]][[j]] <- list()
    Output_names$prediction$percent[[i]][[j]] <- list()
    for (k in array_type){
      Output_names$prediction$cats[[i]][[j]][[k]] <- list()
      for (l in cat_type){
        Output_names$prediction$cats[[i]][[j]][[k]][[l]] <- file.path(OUTPUT_PATH, "3_4.Prediction", "3.CATs",
                                                                      paste("maps", l,
                                                                            color_scale_transformation,
                                                                            "max",max_displayed_cat,
                                                                            "res",resolution,
                                                                            i,j,k,".png",
                                                                            sep = "_"))
        
        Output_names$prediction$percent[[i]][[j]][[k]] <- file.path(OUTPUT_PATH, "3_4.Prediction", "4.Percent",
                                                                    paste0("maps_res",resolution,
                                                                           "_",i,"_",j,"_",k, ".png"))
      }
    }
  }
}
# Values of predicted CATs and percentages of time spent associated, stored in a .csv
Output_names$prediction$csv <- file.path(OUTPUT_PATH, '3_4.Prediction', paste0("predictions_res",resolution,".csv"))

#' Names of 5.Fishing_pressure
#' --------------------------
try(dir.create(file.path(OUTPUT_PATH, "5.Fishing_pressure")), silent = T)
Output_names$fishing_pressure$maps <- file.path(OUTPUT_PATH, "5.Fishing_pressure", "number_of_FOB_sets_maps.png")
Output_names$fishing_pressure$maps_kernel <- file.path(OUTPUT_PATH, "5.Fishing_pressure",
                                                       paste0("number_of_FOB_sets_maps_with_",
                                                              percent_contour_kernel,
                                                              "percent_area.png"))
Output_names$fishing_pressure$hist_Pa <- file.path(OUTPUT_PATH, "5.Fishing_pressure",
                                                   "Histogram_percentage_of_associated_time.png")
Output_names$fishing_pressure$hist_CAT <- file.path(OUTPUT_PATH, "5.Fishing_pressure",
                                                       paste0("Histogram_CAT_with_limit_at_",
                                                              max_displayed_cat,
                                                              ".png"))
Output_names$fishing_pressure$contour_list <- file.path(OUTPUT_PATH, "5.Fishing_pressure",
                                                        paste0("List_polygons_",percent_contour_kernel,
                                                               "percent_FOB_sets.rds"))
Output_names$fishing_pressure$maps_Pa_kernel <- file.path(OUTPUT_PATH, "5.Fishing_pressure",
                                                          paste0("Maps_Pa_",percent_contour_kernel,
                                                                 "percent_FOB_sets.png"))
Output_names$fishing_pressure$maps_CAT_kernel <- file.path(OUTPUT_PATH, "5.Fishing_pressure",
                                                          paste0("Maps_CAT_",percent_contour_kernel,
                                                                 "percent_FOB_sets.png"))
Output_names$fishing_pressure$csv <- file.path(OUTPUT_PATH, '5.Fishing_pressure',
                                         paste0("predictions_res",resolution,"_percentFOBsets",percent_contour_kernel,".csv"))