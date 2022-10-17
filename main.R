#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-05-03
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: This is the main script to process FAT albaCoRaW outputs and generate Pa maps
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


#' ***************
# 0. Initialization ----
#' ***************
source(file.path(ROUT_PATH, "0.Init.R"))


#' *************************
# 1. 1/CAT = f(rho) regression ----
#' *************************
#' 1.1 CAT summaries (by array type) and fits
#' 
#' model lists (fit obtained with stats::nls)
models <- list() #will contain a list of the general models
models_diff <- list() #will contain a list of the CATdiff models
models_return <- list() #will contain a list of the CATreturn models
models_proportion <- list() #will contain a list of the R (A/B) models

#' cat_summary lists
cat_summary <- list() #will contain a list of data.frames, each containing the summary of simulated CATs 
cat_diff_summary <- list() #will contain a list of data.frames, each containing the summary of simulated CATdiff
cat_return_summary <- list() #will contain a list of data.frames, each containing the summary of simulated CATreturn 

cat("1. Summarise simulation outputs and fit models\n")
for (array_type.i in c("square","square_rd","random")){
  cat(paste0("    Array type:", array_type.i,"\n"))
  source(file = file.path(ROUT_PATH, "1.Regression.R"))
}

# 1.2. Global regression plot
if (general_summary){
  cat("\n  Generate global summary")
  source(file = file.path(ROUT_PATH, "1.Global_summary.R"))
}


# 2. Buoy density ----
#' ***************
cat("\n\n2. Read buoy density and build maps")
source(file = file.path(ROUT_PATH, "2.Buoy_density.R"))

#' ********************
# 3. Predicted CATs maps ----
#' ********************
## 3.1. Determine proper resolution ----
#'
#' retained spatial resolution: 2°
#' retained temporal resolution: month
#' see Determine_resolution.pdf
#'
#'
## 3.2. Predict CAT ----
cat("\n\n3. Predict expected CAT values")
source(file = file.path(ROUT_PATH, "3.Predict_CAT.R"))

## 3.3 Build prediction maps ----
if (build_maps[2]){
  # for each year in the data
  for (i in 1:length(sort(unique(data$YEAR)))){
    #' for each function chosen as argument (corresponding to the available data:
    #' one or several of min, mean, max)
    for (j in 1:length(fct)){
      #' for each array type
      for (k in 1:length(array_type)){
        for (l in 1:length(cat_type)){
          source(file = file.path(ROUT_PATH, "3.Build_maps.R"))
        }
      }
    }
  }
}

# 4. Prediction of Pa (% of time spent associated) ----
## 4.1 Determine the area containing each cell ----
cat("\n\n4. Predict expected Pa values")
if (considered_crt != "MozSey"){
  data %>% dplyr::mutate(area = considered_crt) -> data
} else if (considered_crt == "MozSey"){
  data %>%
    dplyr::mutate(area = dplyr::if_else(degraded_lon < 50 & degraded_lat < -10, "Moz", "Sey")) -> data
}

## 4.2. Predict Pa ----
source(file = file.path(ROUT_PATH, "4.Predict_Pa.R"))

## 4.3 Build maps of Pa ----
if (build_maps[3]){

  for (i in 1:length(sort(unique(data_predict$YEAR)))){
    for (j in 1:length(fct)){
      for (k in 1:length(array_type)){
        
        source(file = file.path(ROUT_PATH, "4.Build_maps.R"))
      }
    }
  }
}

# 5. Fishing pressure (nb of FOB sets) ----
cat("\n\n4. Read fishing data and build kernels of FOB set density")

source(file.path(ROUT_PATH, "5.FOB_sets.R"))

