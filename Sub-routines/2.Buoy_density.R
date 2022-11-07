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

n_buoys <- read.csv(file.path(DATA_PATH, "IOTC", "IOTC-2021-WGFAD02-DATA04.csv"),
                    sep = ";",
                    stringsAsFactors = F)
ref_cells <- read.csv(file.path(DATA_PATH, "IOTC", "IOTC-2021-WGFAD02-DATA00-CWP-REFERENCES.csv"),
                      sep = ";",
                      stringsAsFactors = F)


data <- merge(n_buoys, ref_cells)
data <- data.frame(apply(data, 2, function(chr) sub(pattern = ",", replacement = ".", x = chr)))
data <- data.frame(apply(data, 2, function(fct) as.numeric(as.character(fct))))

data %>% dplyr::mutate(DENSITY_BUOYS_MIN = N_BUOYS_MIN / WATER_AREA_KM2,
                       DENSITY_BUOYS_MEAN = N_BUOYS_MEAN / WATER_AREA_KM2,
                       DENSITY_BUOYS_MAX = N_BUOYS_MAX / WATER_AREA_KM2) %>%
  dplyr::filter(WATER_AREA_KM2 > 6000) %>% # filter depending on the area to remove coastal cells which sometimes have extreme values
  dplyr::mutate(DATE = as.Date(paste0(YEAR, "-", MONTH, "-01"))) %>%
  dplyr::filter(YEAR == get("YEAR", envir = globalenv())) -> data

## 2.2. Density timeseries ----
if (BUILD_MAPS[1]){
  data %>% plyr::ddply(.variables = "DATE", function(x) mean(x$DENSITY_BUOYS_MEAN)) %>%
    dplyr::rename("mean" = "V1") %>%
    dplyr::left_join(data %>% plyr::ddply(.variables = "DATE", function(x) mean(x$DENSITY_BUOYS_MIN)) %>%
                       dplyr::rename("min" = "V1")) %>%
    dplyr::left_join(data %>% plyr::ddply(.variables = "DATE", function(x) mean(x$DENSITY_BUOYS_MAX)) %>%
                       dplyr::rename("max" = "V1")) %>%
    tidyr::pivot_longer(cols = c("min","mean","max")) %>%
    ggplot(aes(x=DATE, y = value, col = name))+
    geom_point()+
    geom_line(alpha = 0.3)+
    ylab("Density (buoys.km-2)")+ xlab("Date") +
    scale_color_brewer("Type", palette = "Set1") -> p
  
  ggsave(Output_names$buoy_density$timeseries, p, width = 8, height = 8)
  
  ## 2.3. Density maps ----
  
  map <- list()
  months_in_data <- sort(unique(data$DATE))
  # maxs <- c(0.009, 0.004, 0.025)
  
  for (j in 1:length(vars_map_density)){
    lims = c(min(data[,vars_map_density[j]]), max(data[,vars_map_density[j]]))
    for (i in 1:length(unique(data$DATE))){
      data %>% dplyr::filter(DATE == months_in_data[i]) %>%
        # dplyr::filter((!!rlang::sym(vars_map_density[j])) < maxs[j]) %>%
        ggplot()+
        coord_sf(xlim = c(30, 110), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
        geom_tile(aes(x=CENTER_LON, y=CENTER_LAT, fill=(!!rlang::sym(vars_map_density[j]))))+
        scale_fill_gradientn(colors=c("black","blue","yellow","red"),
                             limits = lims)+
        # limits = c(0,maxs[j]))+
        geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
        ggtitle(format(months_in_data[i], format = "%Y-%m")) -> map[[i]]
      
      map[[i]] <- mise.en.forme.ggplot(map[[i]])
    }
    
    
    maps <- ggpubr::ggarrange(plotlist = map[1:12],
                              ncol = 4, nrow = 3,
                              align = "hv", labels = "AUTO",
                              common.legend = T,
                              legend = "right")
    
    ggsave(Output_names$buoy_density$maps[j], maps,
           width = 120*4 + 20,
           height = 105*3, units = "mm")
    
  }
}