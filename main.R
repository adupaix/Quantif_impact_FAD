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
cat("\14")
cat("0. Initializing study\n")
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

cat("\14 1. Summarise simulation outputs and fit models\n")
for (array_type.i in c("square","square_rd","random")){
  cat(paste0("    Array type:", array_type.i,"\n"))
  source(file = file.path(ROUT_PATH, "1.Regression.R"))
}

# 1.2. Global regression plot
if (GENERAL_SUMMARY){
  cat("\n  Generate global summary")
  source(file = file.path(ROUT_PATH, "1.Global_summary.R"))
}


# 2. Buoy density ----
#' ***************
cat("\n\n 2. Read buoy density and build maps\n")
source(file = file.path(ROUT_PATH, "2.Buoy_density.R"))

#' ********************
# 3. Predicted CATs maps ----
#' ********************
## 3.1. Determine proper resolution ----
#'
#' retained spatial resolution: 5°
#' retained temporal resolution: month
#' see Determine_resolution.pdf
#'
#'
## 3.2. Predict CAT ----
cat("\n\n 3. Predict expected CAT values")
cat("\n    Predicting CATs")
source(file = file.path(ROUT_PATH, "3.Predict_CAT.R"))

## 3.3 Build prediction maps ----
if (BUILD_MAPS[2]){
  cat("\n    Building maps\n")
  pb <- txtProgressBar(min = 0,
                       max = length(VARS_DENSITY)*length(ARRAY_TYPE)*length(CAT_TYPE)*12,
                       width = length(VARS_DENSITY)*length(ARRAY_TYPE)*length(CAT_TYPE)*12,
                       style = 3)
  tick <- 0
    #' for each function chosen as argument (corresponding to the available data:
    #' one or several of min, mean, max)
    for (j in 1:length(VARS_DENSITY)){
      #' for each array type
      for (k in 1:length(ARRAY_TYPE)){
        #' for each CAT "type" (CAT return, diff, R and CAT)
        for (l in 1:length(CAT_TYPE)){
          source(file = file.path(ROUT_PATH, "3.Build_maps.R"))
        }
      }
    }
  close(pb)
}


# 4. Prediction of Pa (% of time spent associated) ----
## 4.1 Determine the area containing each cell ----
cat("\n\n 4. Predict expected Pa values")
cat("\n    Predicting Pa")
if (CONSIDERED_CRT != "MozSey"){
  data %>% dplyr::mutate(area = CONSIDERED_CRT) -> data
} else if (CONSIDERED_CRT == "MozSey"){
  data %>%
    dplyr::mutate(area = dplyr::if_else(degraded_lon < 50 & degraded_lat < -10, "Moz", "Sey")) -> data
}

## 4.2. Predict Pa ----
source(file = file.path(ROUT_PATH, "4.Predict_Pa.R"))

## 4.3 Build maps of Pa ----
if (BUILD_MAPS[3]){
  cat("\n    Building maps\n")
  pb <- txtProgressBar(min = 0,
                       max = length(VARS_DENSITY)*length(ARRAY_TYPE)*12,
                       width = length(VARS_DENSITY)*length(ARRAY_TYPE)*12,
                       style = 3)
  tick <- 0
    for (j in 1:length(VARS_DENSITY)){
      for (k in 1:length(ARRAY_TYPE)){
        
        source(file = file.path(ROUT_PATH, "4.Build_maps.R"))
      }
    }
  close(pb)
}

# 5. Fishing pressure (nb of FOB sets) ----
cat("\n\n 5. Read fishing data and build kernels of FOB set density\n")

analysis_5 = "all_FOBs"
source(file.path(ROUT_PATH, "5.FOB_sets.R"))
analysis_5 = "owned_FOBs"
source(file.path(ROUT_PATH, "5.FOB_sets.R"))
