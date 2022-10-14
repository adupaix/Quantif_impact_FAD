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

for (j in 1:length(fct)){
  for (k in 1:length(array_type)){
    catvarname <- paste0("PREDICTED_",
                         toupper(fct[j]),
                         "_CAT_",
                         toupper(array_type[k]))
    percentvarname <- paste0("PREDICTED_",
                             toupper(fct[j]),
                             "_PERCENT_",
                             toupper(array_type[k]))
    
    data %>% dplyr::mutate(V1 = 100 * crt[area] / (crt[area] + !!rlang::sym(catvarname))) -> data
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
for (i in 1:length(array_type)){
  vars <- c(vars, grep(toupper(array_type[i]), names(data), value = T))
}

data %>% dplyr::select(all_of(vars)) %>%
  dplyr::filter(!duplicated(id_unique)) -> data_predict

write.csv(data_predict, Output_names$prediction$csv)