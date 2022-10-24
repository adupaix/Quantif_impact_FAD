#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-10-14
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Example of config file structure to process FAT albaCoRaW outputs and generate Pa maps
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

#' empty environment
rm(list = ls())
invisible(gc())

WD <- "/home/adupaix/Documents/These/Axe_2/Quantif_impact_FAD/"

#' generate basic paths
DATA_PATH <- file.path(WD,'Data')

OUTPUT_PATH <- file.path(WD,"Outputs")
FUNC_PATH <- file.path(WD,"Functions")
ROUT_PATH <- file.path(WD, "Sub-routines")
RESOURCE_PATH <- file.path(WD,"Resources")

# Set seed
set.seed(12345678)

#' *****
# Arguments
#' *****
#' @section General arguments
#' --------------------------
#' @commit_name: name of the input folder containing the output from FAT albaCoRaW.
#' This folder must contain sub folders with the name of the type of array used (for example "random") 
#' Possible options:
# commit_name = "YFT50"
# commit_name = "R02"
# commit_name = "YFT50_R02"
commit_name = "dd5bd6a"

#' @build_maps: choose to build the maps (T) or only to calculate the predictions (F)
#' 1: buoy density maps
#' 2: cat prediction maps
#' 3: percentage of time associated (Pa) maps
#' 4: number of FOB sets maps
#' 5: maps of cat and Pa predictions with kernels
# build_maps = c(F,F,T,T,T)
build_maps = rep(T, 5)

#' @resolution: spatial resolution (in °) at which the calculation will be done
resolution = 5

#' @sim_output_path: path to the directory where the FAT albaCoRaW outputs are stored
sim_output_path <- file.path(DATA_PATH, "CRW_output", paste0("global_", commit_name))

#' @year: year of the study @!! until now, only performed on 2020. Sub-routine 5 may need to be adapted depending on the
#' accuracy of IOTC dataset, received through form 3FA and 3CE
year = 2020


#'# @section Parameters of 1. Regression
#' --------------------------
#' @general_summary: (bool) choose if generate the summary of all CATs (of all array type)
general_summary = F

#'# @section Parameters of 2. Buoys density maps
#' --------------------------
#' @vars_map_density: choose which densities will be plotted
#'                   one of c("mean", "min", "max")
vars_map_density <- c("mean")


#'# @section Parameters of 3. Predicted CATs maps
#' --------------------------
#' @fct: one of c(mean, min, max)
#' @array_type: one  or several of c("square","square_rd","random")
#' @cat_type: one or several of c("CAT","CATd","CATr","R")
fct <- c("mean")
# array_type <- c("square", "square_rd", "random")
array_type <- c("random")
# cat_type <- c("CAT")
cat_type <- c("CAT","CATd","CATr","R")

#' @color_scale_transformation: transformation of the color scale for the CAT prediction maps
#' one of the built_in transformation of ggplot2 scales
#' see help(scale_fill_gradientn)
color_scale_transformation = "log10"

#' @max_displayed_cat: upper limit (in days) of the CAT displayed on the maps.
#' because when rho -> 0 , CAT -> + inf , we need to fix an upper limit
max_displayed_cat = 30


#'# @section Parameters of 4. Prediction of Pa ...
#' --------------------------
#' @considered_crt: one of c("IO","MozSey")
#' @crt: measured CRT values (do not change)
considered_crt = "IO"
crt = c(IO = 6.64,
        Moz = 7.56,
        Sey = 5.86,
        IO45 = 6.64 * 0.45) # cf. Govinden et al. (2021) ; https://doi.org/10.1111/fog.12536
#' IO45: Baidai et al. (2020) https://doi.org/10.1093/icesjms/fsaa178 demonstrated that, on average,
#' FADs spend 45% of their time occupied by a tuna aggregation in the IO.
#' Hence, in 65% of the cases, a tuna reaches an "empty" FAD. If we consider that tunas associate to meet with conspecifics,
#' in 65% of the times, it won't associate, so the CRT will be null. Govinden et al. (2021) measured CRT of tuna that did associate and did not
#' measure the null CRTs, so under this simple hypothesis, the mean CRT is expected to be equal to 45% of the mean measured CRT by
#' Govinden et al. (2021)


#'# @section Parameters of 5. Fishing pressure ...
#' --------------------------
#' @percent_contour_kernel: percentage of the fishing pressure to consider
#'                          for example, if percent_contour_kernel = 95, after building the kernel density of the number of FOB sets
#'                          we extract the contour of the surface which contains 95% of the sets.
percent_contour_kernel = 95


#' Run the main script
#' -------------------
source(file.path(WD, "main.R"))