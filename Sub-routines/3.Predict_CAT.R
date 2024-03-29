#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Sub-routine to predict the expected mean CAT values in the Western Indian Ocean 
#'
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

data <- read.csv(file = Output_names$buoy_density$csv)

data %>% plyr::ddply(.variables = "id_unique",
                     function(x) sum(x$DENSITY_BUOYS_MEAN * x$WATER_AREA_KM2) / sum(x$WATER_AREA_KM2) ) %>%
  dplyr::rename("mean_degraded_density" = "V1") %>%
  dplyr::left_join(data %>% plyr::ddply(.variables = "id_unique",
                                        function(x) sum(x$DENSITY_BUOYS_MIN * x$WATER_AREA_KM2) / sum(x$WATER_AREA_KM2) ) %>%
                     dplyr::rename("min_degraded_density" = "V1"), by = "id_unique") %>%
  dplyr::left_join(data %>% plyr::ddply(.variables = "id_unique",
                                        function(x) sum(x$DENSITY_BUOYS_MAX * x$WATER_AREA_KM2) / sum(x$WATER_AREA_KM2) ) %>%
                     dplyr::rename("max_degraded_density" = "V1"), by = "id_unique") %>%
  dplyr::right_join(data, by = "id_unique") -> data

# predict the mean CATs
# VARS_DENSITY <- c("min","mean","max")
for (j in 1:length(VARS_DENSITY)){
  for (array_type.k in ARRAY_TYPE){
    varname <- paste("PREDICTED",
                      toupper(VARS_DENSITY[j]),
                      CAT_TYPE,
                      toupper(array_type.k),
                      sep = "_")
    
    fct_name <- paste0(VARS_DENSITY[j],"_degraded_density")
    
    for (k in 1:length(CAT_TYPE)){
      data %>% dplyr::mutate(V1 = cat.formula(rho = data[[fct_name]],
                                          model_diff = models_diff[[array_type.k]],
                                          model_return = models_return[[array_type.k]],
                                          model_proportion = models_proportion[[array_type.k]],
                                          out_var=CAT_TYPE[k])) -> data
      names(data)[which(names(data)=="V1")] <- varname[k]
    }
    
  }
}
