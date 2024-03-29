#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-10-14
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Example of config file structure to process FAT albaCoRaW outputs and generate Pa maps
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

#' generate basic paths
DATA_PATH <- file.path(BASE_DIR,'Data')

OUTPUT_PATH <- file.path(BASE_DIR,"Outputs")
FUNC_PATH <- file.path(BASE_DIR,"Functions")
ROUT_PATH <- file.path(BASE_DIR, "Sub-routines")
RESOURCE_PATH <- file.path(BASE_DIR,"Resources")

# Set seed
set.seed(12345678)

#' *****
# Arguments
#' *****
#' @section General arguments
#' --------------------------

#' DATASETS
#' **********
#' @COMMIT_NAME: name of the input folder containing the output from FAT albaCoRaW.
#' This folder must contain sub folders with the name of the type of array used (among "random", "square", "square_rd") 
#' Possible options:
# COMMIT_NAME = "YFT50"
# COMMIT_NAME = "R02"
# COMMIT_NAME = "YFT50_R02"
COMMIT_NAME = "dd5bd6a"

#' @SIM_OUTPUT_PATH: path to the directory where the FAT albaCoRaW outputs are stored
SIM_OUTPUT_PATH <- file.path(DATA_PATH, "CRW_output", paste0("global_", COMMIT_NAME))


#' @DENSITY_FROM: choose to do the whole study using the buoy densities from the IOTC datasets ("buoys")
#'                or from the FOB densities estimated using the multiplication factor from Dupaix et al. (2021) ("FOB")
#'                or from the LOG densities estimated using the same multiplication factor ("LOG")
DENSITY_FROM <- "buoys"

  #' Then we need the observers data from Ob7
  #' Two files are needed:
  #' One containing all the operations on floating objects
  OBSERVERS_FOBFILE <- file.path(DATA_PATH, "Ob7", "all_operation_on_fobs_observe_fr_ATidd.csv")
  #' One containing all the vessel activities (operations on FOBs, but also sets, etc.)
  OBSERVERS_ACTIVITYFILE <- file.path(DATA_PATH, "Ob7", "all_vessel_activities_observe_fr_indian_20191021.csv")

#' IOTC datasets
#' @IOTC_BUOY_DATA_FILE: To calculate the density of floating objects (in 2.Buoy_density), we rely on the IOTC dataset obtained through the 3BU form
#' this file gives the mean monthly number of buoys per 1 degree cell
#' url: https://iotc.org/WGFAD/02/Data/04-BU 
IOTC_BUOY_DATA_FILE <- file.path(DATA_PATH, "IOTC", "IOTC-2021-WGFAD02-DATA04.csv")

#' To have access to the area which is fished, we build a kernel on the sets performed on FOBs (in 5.FOB_sets)
#' For that we need 2 files:
#' @IOTC_SETS_3FA_FILE: Number of FOB sets from the 3FA form (used for European Union, Japan, Mauritius and Seychelles)
#' @!! the FOB sets of the EU-France are likely to be overestimated (see IOTC-2022-WGFAD03-03)
#' However, to date, the set type is not available through the 3CE form for other European fleets
#' url: https://iotc.org/WGFAD/02/Data/01-FA
IOTC_SETS_3FA_FILE <- file.path(DATA_PATH, "IOTC", "IOTC-2021-WGFAD02-DATA01-FA_Rev1_0.csv")
#' @IOTC_SETS_3CE_FILE: Number of FOB sets from the 3CE form (used for Korea)
#' url: https://iotc.org/WPTT/24DP/Data/05-CESurface
IOTC_SETS_3CE_FILE <- file.path(DATA_PATH, "IOTC", "IOTC-2022-WPTT24(DP)-DATA05-CESurface.csv")

#' @IOTC_CELLREF_FILE: Reference file linking the cell identifier in the above datasets with the longitude and latitude
#' url: https://iotc.org/WGFAD/02/Data/00-CWP
IOTC_CELLREF_FILE <- file.path(DATA_PATH, "IOTC", "IOTC-2021-WGFAD02-DATA00-CWP-REFERENCES.csv")


#' Other general arguments
#' *************************
#' @BUILD_MAPS: choose to build the maps (T) or only to calculate the predictions (F)
#' 1: buoy density maps
#' 2: cat prediction maps
#' 3: percentage of time associated (Pa) maps
#' 4: number of FOB sets maps
#' 5: maps of cat and Pa predictions with kernels
# BUILD_MAPS = c(F,F,T,T,T)
BUILD_MAPS = rep(T, 5)

#' @RESOLUTION: spatial RESOLUTION (in °) at which the calculation will be done
RESOLUTION = 5

#' @YEAR: YEAR of the study @!! until now, only performed on 2020. Sub-routine 5 may need to be adapted depending on the
#' accuracy of IOTC datasets, received through form 3FA and 3CE
YEAR = 2020



#'# @section Parameters of 1. Regression
#' --------------------------
#' @GENERAL_SUMMARY: (bool) choose if generate the summary of all CATs (of all array type)
GENERAL_SUMMARY = F



#'# @section Parameters of 2. Buoys density maps
#' --------------------------
#' @VARS_DENSITY: the IOTC dataset contains the monthly mean/min/max number of operationnal buoys per 1° cell
#'                    VARS_DENSITY allows to choose which densities will be plotted (in sub-routine 2.)
#'                    and which densities will be used to calculate the CAT and Pa predictions (in sub-routines 3 and 4.)
#'                    one of c("mean", "min", "max")
VARS_DENSITY <- c("mean")



#'# @section Parameters of 3. Predicted CATs maps
#' --------------------------
#' @ARRAY_TYPE: one or several of c("square","square_rd","random")
#' @CAT_TYPE: one or several of c("CAT","CATd","CATr","R")
#' 
# ARRAY_TYPE <- c("square", "square_rd", "random")
ARRAY_TYPE <- c("random")
# CAT_TYPE <- c("CAT")
CAT_TYPE <- c("CAT","CATd","CATr","R")

#' @COLOR_SCALE_TRANSFORMATION: transformation of the color scale for the CAT prediction maps
#' one of the built_in transformation of ggplot2 scales
#' see help(scale_fill_gradientn)
COLOR_SCALE_TRANSFORMATION = "log10"

#' @MAX_DISPLAYED_CAT: upper limit (in days) of the CAT displayed on the maps.
#' because when rho -> 0 , CAT -> + inf , we need to fix an upper limit
MAX_DISPLAYED_CAT = 30



#'# @section Parameters of 4. Prediction of Pa ...
#' --------------------------
#' @CONSIDERED_CRT: one of c("IO","MozSey")
#' @CRT: measured CRT values (do not change) based on Govinden et al. (2021) ; https://doi.org/10.1111/fog.12536
CONSIDERED_CRT = "IO"
CRT = c(IO = 6.64,
        Moz = 7.56,
        Sey = 5.86,
        IO45 = 6.64 * 0.45)
#' IO45: Baidai et al. (2020) https://doi.org/10.1093/icesjms/fsaa178 demonstrated that, on average,
#' FADs spend 45% of their time occupied by a tuna aggregation in the Indian Ocean.
#' Hence, in 65% of the cases, a tuna reaches an "empty" FAD. If we consider that tunas associate to meet with conspecifics,
#' in 65% of the times, it won't associate, so the CRT will be null. Govinden et al. (2021) measured CRT of tuna that did associate and did not
#' measure the null CRTs, so under this simple hypothesis, the mean CRT is expected to be equal to 45% of the mean measured CRT by
#' Govinden et al. (2021)


#'# @section Parameters of 5. FOB sets ...
#' --------------------------
#' @PERCENT_CONTOUR_KERNEL: percentage of the FOB sets to consider
#'                          for example, if PERCENT_CONTOUR_KERNEL = 95, after building the kernel density of the number of FOB sets
#'                          we extract the contour of the surface which contains 95% of the sets.
PERCENT_CONTOUR_KERNEL = 95


