#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Sub-routine to prepare buoy data and build maps
#'
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

## 2.1 Read + prep data ----
cat("    Reading buoy density data\n")
n_buoys <- read.csv(IOTC_BUOY_DATA_FILE,
                    sep = ";",
                    stringsAsFactors = F)
ref_cells <- read.csv(IOTC_CELLREF_FILE,
                      sep = ";",
                      stringsAsFactors = F)


data <- merge(n_buoys, ref_cells)
data <- data.frame(apply(data, 2, function(chr) sub(pattern = ",", replacement = ".", x = chr)))
data <- data.frame(apply(data, 2, function(fct) as.numeric(as.character(fct))))

data %>% dplyr::mutate(DENSITY_BUOYS_MIN = N_BUOYS_MIN / WATER_AREA_KM2,
                       DENSITY_BUOYS_MEAN = N_BUOYS_MEAN / WATER_AREA_KM2,
                       DENSITY_BUOYS_MAX = N_BUOYS_MAX / WATER_AREA_KM2) %>%
  dplyr::filter(WATER_AREA_KM2 > 6000) %>% # filter depending on the area to remove coastal cells which can have extreme values
  dplyr::mutate(DATE = as.Date(paste0(YEAR, "-", MONTH, "-01"))) %>%
  dplyr::filter(YEAR == get("YEAR", envir = globalenv())) -> data

data %>% dplyr::mutate(degraded_lat = floor(CENTER_LAT/RESOLUTION)*RESOLUTION + RESOLUTION/2,
                       degraded_lon = floor(CENTER_LON/RESOLUTION)*RESOLUTION + RESOLUTION/2,
                       id_unique = paste(YEAR, MONTH, degraded_lat, degraded_lon, sep = "_")) -> data

# Get the ratio between FAD and NLOG
if (DENSITY_FROM != "buoys"){
  cat("    Calculating LOG/FAD from observers data\n")
  STARTING_YEAR_FOR_RATIO_CALCULATION <- 2014
  ratio_file_name <- paste0("LOG_over_FAD_res", RESOLUTION, "_",
                            STARTING_YEAR_FOR_RATIO_CALCULATION ,"-",
                            YEAR, ".csv")
  if(!file.exists(file.path(OUTPUT_PATH, "2.Buoy_density", ratio_file_name))){
    source(file.path(ROUT_PATH, "2.1.Ratio_LOG_FAD.R"))
  }
  multiplication_factor <- read.csv2(file.path(OUTPUT_PATH, "2.Buoy_density", ratio_file_name))
  
  dplyr::left_join(data, multiplication_factor, by = c("MONTH" = "month", "degraded_lon" = "x", "degraded_lat" = "y")) %>%
    dplyr::mutate(raw_buoy_density_min = DENSITY_BUOYS_MIN,
                  raw_buoy_density_mean = DENSITY_BUOYS_MEAN,
                  raw_buoy_density_max = DENSITY_BUOYS_MAX) %>%
    dplyr::mutate(multiply_by = case_when(DENSITY_FROM == "LOG" ~ Log_over_Fad,
                                          DENSITY_FROM == "FOB" ~ 1+Log_over_Fad)) %>%
    dplyr::mutate(DENSITY_BUOYS_MIN = DENSITY_BUOYS_MIN * multiply_by,
                  DENSITY_BUOYS_MEAN = DENSITY_BUOYS_MEAN * multiply_by,
                  DENSITY_BUOYS_MAX = DENSITY_BUOYS_MAX * multiply_by) %>%
    dplyr::select(-multiply_by) -> data
} else {
  data %>%
    dplyr::mutate(raw_buoy_density_min = DENSITY_BUOYS_MIN,
                  raw_buoy_density_mean = DENSITY_BUOYS_MEAN,
                  raw_buoy_density_max = DENSITY_BUOYS_MAX) -> data
}

write.csv(data, file = Output_names$buoy_density$raw_csv, row.names = F)

# filter and select columns to build data frame used in the rest of the study
vars <- c("id_unique", "YEAR", "MONTH", "DATE", "degraded_lon", "degraded_lat",
          "WATER_AREA_KM2", "DENSITY_BUOYS_MIN", "DENSITY_BUOYS_MEAN", "DENSITY_BUOYS_MAX")
if(DENSITY_FROM != "buoys"){vars <- c(vars, "Log_over_Fad")}

data %>%
  dplyr::filter(!is.na(DENSITY_BUOYS_MEAN)) %>%
  dplyr::select(all_of(vars)) -> data_for_following_subroutines

write.csv(data_for_following_subroutines,
          file = Output_names$buoy_density$csv, row.names = F)

## 2.2. Density timeseries ----
if (BUILD_MAPS[1]){
  cat("    Building maps\n")
  data %>%
    plyr::ddply(.variables = "DATE",
                function(x) mean(x$raw_buoy_density_mean)) %>%
    dplyr::rename("mean" = "V1") %>%
    dplyr::left_join(data %>% plyr::ddply(.variables = "DATE",
                                          function(x) mean(x$raw_buoy_density_min)) %>%
                       dplyr::rename("min" = "V1"),
                     by = "DATE") %>%
    dplyr::left_join(data %>% plyr::ddply(.variables = "DATE", function(x) mean(x$raw_buoy_density_max)) %>%
                       dplyr::rename("max" = "V1"),
                     by = "DATE") %>%
    tidyr::pivot_longer(cols = c("min","mean","max")) %>%
    ggplot(aes(x=DATE, y = value, col = name))+
    geom_point()+
    geom_line(alpha = 0.3)+
    ylab("Density (buoys.km-2)")+
    xlab("Date") +
    scale_color_brewer("Type", palette = "Set1") -> p
  
  ggsave(Output_names$buoy_density$timeseries, p, width = 8, height = 8)
  
  ## 2.3. Density maps ----
  
  for (j in 1:length(vars_map_density)){
    # Maps of raw density data
    maps <- build.density.maps(data,
                               var_map_density = vars_raw_map_density[j],
                               lon_var_name = "CENTER_LON",
                               lat_var_name = "CENTER_LAT")
    
    ggsave(sub("maps","Raw_maps",Output_names$buoy_density$maps[j]), maps,
           width = 120*4 + 20,
           height = 105*3, units = "mm")
    
    # Maps of densities obtained with multiplication factor
    maps <- build.density.maps(data %>%
                                 dplyr::filter(!is.na(!!rlang::sym(vars_map_density[j]))),
                               var_map_density = vars_map_density[j],
                               lon_var_name = "degraded_lon",
                               lat_var_name = "degraded_lat")
    
    ggsave(Output_names$buoy_density$maps[j], maps,
           width = 120*4 + 20,
           height = 105*3, units = "mm")
    
  }
  
}