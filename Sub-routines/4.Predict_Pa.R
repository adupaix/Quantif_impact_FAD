#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Sub-routine to predict the expected Pa values in the Western Indian Ocean 
#'
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

for (j in 1:length(VARS_DENSITY)){
  for (k in 1:length(ARRAY_TYPE)){
    catvarname <- paste0("PREDICTED_",
                         toupper(VARS_DENSITY[j]),
                         "_CAT_",
                         toupper(ARRAY_TYPE[k]))
    percentvarname <- paste0("PREDICTED_",
                             toupper(VARS_DENSITY[j]),
                             "_PERCENT_",
                             toupper(ARRAY_TYPE[k]))
    
    data %>% dplyr::mutate(V1 = 100 * CRT[area] / (CRT[area] + !!rlang::sym(catvarname))) %>%
      dplyr::mutate(V1 = case_when(is.na(!!rlang::sym(catvarname)) ~ 0,
                                   !is.na(!!rlang::sym(catvarname)) ~ V1)) -> data
    names(data)[which(names(data)=="V1")] <- percentvarname
    
    data <- col.to.discrete(data = data,
                            steps = seq(0,100,10),
                            col_name = percentvarname,
                            new_col_name = paste0(percentvarname, "_d"))
    
  }
}

#' @cleaning: select only columns of interest
vars <- c("id_unique", "YEAR", "MONTH", "DATE", "degraded_lon", "degraded_lat",
          "mean_degraded_density", "max_degraded_density", "min_degraded_density")
for (i in 1:length(ARRAY_TYPE)){
  vars <- c(vars, grep(toupper(ARRAY_TYPE[i]), names(data), value = T))
}

data %>% dplyr::select(all_of(vars)) %>%
  dplyr::filter(!duplicated(id_unique)) -> data_predict

write.csv(data_predict, Output_names$prediction$csv)