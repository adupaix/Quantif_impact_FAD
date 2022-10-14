#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-05-03
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: This is the main script to process FAT albaCoRaW outputs and generate Pa maps
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
# Arguments ----
#' *****
#' General arguments
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

sim_output_path <- file.path(DATA_PATH, "CRW_output", paste0("global_", commit_name))
OUTPUT_PATH <- file.path(OUTPUT_PATH, commit_name)
if(!dir.exists(OUTPUT_PATH)){dir.create(OUTPUT_PATH, recursive = T)}

#' Parameters of 1. Regression
#' @general_summary: (bool) choose if generate the summary of all CATs (of all array type)
general_summary = F

#'# Parameters of 2. Buoys density maps
#' @vars_map_density: choose which densities will be plotted
#'                   one of c("mean", "min", "max")
vars_map_density <- c("mean")

#'# Parameters of 3. Predicted CATs maps
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

#'# Parameters of 4. Prediction of Pa ...
#' @considered_crt: one of c("IO","MozSey")
#' @crt: measured CRT values (do not change)
considered_crt = "IO45"
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

#'# Parameters of 5. Fishing pressure ...
#' @percent_contour_kernel: percentage of the fishing pressure to consider
#'                          for example, if percent_contour_kernel = 95, after building the kernel density of the number of FOB sets
#'                          we extract the contour of the surface which contains 95% of the sets.
percent_contour_kernel = 95

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

#' cat_summary list
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
# read the FOB set datasets
  #' Number of FOB sets from the 3FA form (European Union, Japan, Mauritius and Seychelles)
  #' @!! the FOB sets of the EU-France are likely to be overestimated (see IOTC-2022-WGFAD03-03)
  #' However, the set type is not available through the 3CE form for other European fleets
sets3FA <- read.csv(file.path(DATA_PATH, "IOTC", "IOTC-2021-WGFAD02-DATA01-FA_Rev1_0.csv"))

  #' Number of FOB sets from the 3CE form (Korea)
sets3CE <- read.csv(file.path(DATA_PATH, "IOTC", "IOTC-2022-WPTT24(DP)-DATA05-CESurface.csv"))


#' Apply the same formatting to the 2 datasets
  #' 3FA: keep the year and fleets of interest, keep only the lines with sets and rename columns
sets3FA %>% dplyr::filter(YEAR == 2020,
                          FLEET_CODE %in% c("EUR", "JPN", "MUS", "SYC"),
                          NUM_SETS_ON_FOB != 0) %>%
  dplyr::select(YEAR, MONTH, FISHING_GROUND_CODE, FLEET_CODE, NUM_SETS_ON_FOB) %>%
  dplyr::mutate(data_source = "FA") -> sets3FA
names(sets3FA) <- toupper(names(sets3FA))

  #' 3CE: select the year and fleets of interest, keep only the log schools catch and the effort expressed as SETS
  #'      rename the columns
levels(sets3CE$Fleet) <- gsub(" ", "", levels(sets3CE$Fleet))
columns <- grep("LS", names(sets3CE))
sets3CE %>% dplyr::filter(Year == 2020,
                          Fleet == "KOR",
                          EffortUnits == "SETS") %>%
  dplyr::select(Fleet:EffortUnits, all_of(columns)) -> sets3CE
columns <- grep("LS", names(sets3CE))
sets3CE$is_LS <- apply(sets3CE, 1, function(x) !all(is.na(x[columns]))) # if the set was a FOB-set (LS), at least one .LS column won't be a NA
sets3CE %>% dplyr::filter(is_LS) %>% dplyr::select(Year, MonthStart, Grid, Fleet, Effort) %>% # keep only the FOB sets and the column of interest
  dplyr::rename("month" = "MonthStart", #rename to have the same formatting as sets3FA
                "fleet_code" = "Fleet",
                "num_sets_on_fob" = "Effort",
                "fishing_ground_code" = "Grid") %>%
  dplyr::mutate(data_source = "CE") -> sets3CE
names(sets3CE) <- toupper(names(sets3CE))

#' Concatenate the two datasets
sets <- rbind(sets3FA, sets3CE)

# merge it with the ref_cells data.frame (read previously in 2.Buoy_density)
sets <- merge(sets, ref_cells, by = "FISHING_GROUND_CODE")
names(sets) <- tolower(names(sets))

# replace the , in the data by .
sets <- lapply(sets, function(chr) sub(pattern = ",", replacement = ".", x = chr))
# change all the columns to characters
sets <- lapply(sets, as.character)
#remove spaces in the strings
sets <- lapply(sets, function(x) gsub(" ", "", x))
# change the columns containing numbers to numeric format
sets <- data.frame(lapply(sets,
                          function(x) if(any(grepl("[0-9]", x))){return(as.numeric(x))} else {return(x)}),
                   stringsAsFactors = F)

sets %>% dplyr::mutate(date = as.Date(paste0(year, "-", month, "-15")),
                       id_unique = paste0(fishing_ground_code,date)) %>%
  dplyr::filter(center_lat > -40, center_lat < 30,
                center_lon > 30, center_lon < 110) -> sets

# dim(sets)[1] # 5760
# sum(sets$num_sets_on_fob) # 11522

## keep only one line per cell-month, with the sum of the number of sets in the cell
sets %>%
  plyr::ddply("id_unique", function(x) sum(x$num_sets_on_fob)) %>%
  left_join(sets %>%
              dplyr::filter(!duplicated(sets$id_unique)),
            by = "id_unique") %>%
  dplyr::select(-num_sets_on_fob) %>%
  dplyr::rename("num_sets_on_fob" = "V1") -> sets
# 
# dim(sets)[1] # 2672
# sum(sets$num_sets_on_fob) # 11522

#' new column in the data frame, which will contain T if the cell is inside
#' the polygons containing chosen_cont % of the FOB sets
data_predict$is_in_contour <- F

if (build_maps[4]){
  # build monthly maps and determine which cells are where the FOB sets density is highest
  plist <- list() # will contain the maps of the number of FOB sets
  plist_kernel <- list() # will contain the maps of the number of FOB sets with the surface in red
  contour_list <- list() # will contain a list of multipolygon of the monthly surface containing a given percentage of FOB sets
  for (m in 1:12){
    #~ filter data to keep only the month of interest
    # set data
    sets %>%
      dplyr::filter(month == m) -> sets.m
    # data_predict (with all the CAT and Pa predictions)
    data_predict %>%
      filter(YEAR == 2020, MONTH == m) -> data.m
    pts.m <- st_as_sf(data.m, coords = c("degraded_lon","degraded_lat")) # create an st_point data frame object
    cells_in_contour <- list() #will contain the line number in pts.m which correspond to cell that are inside the contour
    
    #~ build the map
    ggplot(sets.m)+
      coord_sf(xlim = c(30, 110), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
      geom_tile(aes(x=center_lon, y=center_lat, fill=num_sets_on_fob))+
      scale_fill_gradientn("Number of\nFOB sets",
                           colors=c("black","blue","yellow","red"),
                           limits = c(0,max(sets$num_sets_on_fob)))+
      geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
      ggtitle(format(unique(sets.m$date), format = "%B")) -> plist[[m]]
    
    # format the map
    plist[[m]] <- mise.en.forme.ggplot(plist[[m]])
    
    #~ build the kernel
    # randomly put points in each cell (one point per set)
    sets.m %>% dplyr::select(center_lon, center_lat, num_sets_on_fob) -> df
    names(df) <- c("x","y","layer")
    # duplicate each line depending on the number of sets performed in the cell
    pts <- plyr::ddply(df, c("x","y"), function(x){cbind(rep(x$x, times = x$layer),
                                                         rep(x$y, times = x$layer))})
    # randomly move the points inside the cell
    pts$x <- pts$x + runif(length(pts$x),
                           min = -0.5,
                           max = 0.5)
    pts$y <- pts$y + runif(length(pts$y),
                           min = -0.5,
                           max = 0.5)
    # build the kernel
    kd <- ks::kde(pts[,1:2], compute.cont=TRUE)
    # build the surface containing the chosen percentage (chosen_cont) of FOB sets
    chosen_cont <- 100-percent_contour_kernel
    contour_ <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,
                                      levels=cont[paste0(chosen_cont,"%")]))
    # change it to data frame (for plotting)
    for (i in 1:length(contour_)){
      contour_[[i]] <- data.frame(contour_[[i]])
    }
    
    plist_kernel[[m]] <- plist[[m]]
    
    for (i in 1:length(contour_)){
      plist_kernel[[m]] <- plist_kernel[[m]] +
        geom_path(aes(x, y), data=contour_[[i]], col="red")
      
      #change the df to polygon
      contour_[[i]] <- st_polygon(
        list(
          as.matrix(contour_[[i]][,c("x","y")])
        )
      )
      
      # check which cells are inside the contour
      cells_in_contour[[i]] = st_intersects(contour_[[i]], pts.m)
    }
    
    # store the polygons in contour_list
    contour_list[[m]] <- st_as_sf(month = m, st_sfc(contour_), crs = st_crs(4326))
    
    cells_in_contour <- unlist(cells_in_contour)
    
    # Fill the column of data_predict
    data_predict$is_in_contour[which(data_predict$id_unique %in% pts.m$id_unique[cells_in_contour])] <- T
    
  }
  
  maps <- ggpubr::ggarrange(plotlist = plist,
                            ncol = 4, nrow = 3,
                            align = "hv", labels = "AUTO",
                            common.legend = T,
                            legend = "right")
  ggsave(Output_names$fishing_pressure$maps, maps,
         width = 120*4 + 20,
         height = 105*3, units = "mm")
  
  maps_kernel <- ggpubr::ggarrange(plotlist = plist_kernel,
                                   ncol = 4, nrow = 3,
                                   align = "hv", labels = "AUTO",
                                   common.legend = T,
                                   legend = "right")
  ggsave(Output_names$fishing_pressure$maps_kernel, maps_kernel,
         width = 120*4 + 20,
         height = 105*3, units = "mm")
  
  p1 <- ggplot(data_predict)+
    geom_histogram(aes(x=PREDICTED_MEAN_PERCENT_RANDOM, fill = is_in_contour),
                   binwidth = 5)+
    scale_fill_brewer("Is in the most\nfished cells",palette = "Set1")+
    facet_wrap(~is_in_contour)+
    ylab("Number of cells")+
    xlab(expression(Predicted~P[a]))
  
  ggsave(Output_names$fishing_pressure$hist_Pa, p1, height = 8, width = 6)
  
  p2 <- ggplot(data_predict)+
    geom_histogram(aes(x=PREDICTED_MEAN_CAT_RANDOM, fill = is_in_contour),
                   binwidth = 1)+
    scale_x_continuous(limits = c(0,max_displayed_cat))+
    scale_fill_brewer("Is in the most\nfished cells",palette = "Set1")+
    facet_wrap(~is_in_contour)+
    ylab("Number of cells")+
    xlab("Predicted CAT")
  
  ggsave(Output_names$fishing_pressure$hist_CAT, p2, height = 8, width = 6)
  
  saveRDS(contour_list, Output_names$fishing_pressure$contour_list)
}

## Add fishing pressure surface on prediction maps
if (all(file.exists(gsub("png","rds", Output_names$prediction$cats[["2020"]][["mean"]][["random"]][["CAT"]]),
                    gsub("png","rds", Output_names$prediction$percent[["2020"]][["mean"]][["random"]]),
                    Output_names$fishing_pressure$contour_list)) &
    build_maps[5]){
  
  maps_cat <- readRDS(gsub("png","rds", Output_names$prediction$cats[["2020"]][["mean"]][["random"]][["CAT"]]))
  maps_Pa <- readRDS(gsub("png","rds", Output_names$prediction$percent[["2020"]][["mean"]][["random"]]))
  contour_list <- readRDS(Output_names$fishing_pressure$contour_list)
  
  for (m in 1:length(maps_cat)){
    maps_cat[[m]] <- maps_cat[[m]] +
      geom_sf(data = contour_list[[m]], col = "red", fill = NA) +
      coord_sf(xlim = c(30, 110), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))
    
    maps_Pa[[m]] <- maps_Pa[[m]] +
      geom_sf(data = contour_list[[m]], col = "red", fill = NA) +
      coord_sf(xlim = c(30, 110), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))
  }
  
  Pamaps <- ggpubr::ggarrange(plotlist = maps_Pa[1:(length(maps_Pa))],
                              ncol = 4, nrow = 3,
                              align = "hv", labels = "AUTO",
                              common.legend = T,
                              legend = "right")
  ggsave(Output_names$fishing_pressure$maps_Pa_kernel, Pamaps,
         width = 120*4 + 20,
         height = 105*3, units = "mm")
  
  CATmaps <- ggpubr::ggarrange(plotlist = maps_cat[1:(length(maps_cat))],
                              ncol = 4, nrow = 3,
                              align = "hv", labels = "AUTO",
                              common.legend = T,
                              legend = "right")
  ggsave(Output_names$fishing_pressure$maps_CAT_kernel, CATmaps,
         width = 120*4 + 20,
         height = 105*3, units = "mm")
  
  write.csv(data_predict, file = Output_names$fishing_pressure$csv)
}

