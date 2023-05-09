#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Sub-routine to generate the maps of the predicted mean CAT
#'
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


# rename the variable of interest to simplify ggplot construction
varname <- c(paste("PREDICTED",
                    toupper(VARS_DENSITY[j]),
                    CAT_TYPE[l],
                    toupper(ARRAY_TYPE[k]),
                    sep = "_"))
data %>% dplyr::rename("toplot" = dplyr::all_of(varname[1])) -> sub_data

months_in_data <- sort(unique(sub_data$DATE))
cat_maps <- list()

# Build a map for each month
for (m in 1:length(unique(sub_data$DATE))){
  
  sub_data %>% dplyr::filter(!duplicated(id_unique) & DATE == months_in_data[m]) %>%
    dplyr::filter(toplot < MAX_DISPLAYED_CAT) %>%
    ggplot()+
    coord_sf(xlim = c(30, 110), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
    geom_tile(aes(x=degraded_lon, y=degraded_lat, fill=toplot))+
    scale_fill_gradientn(paste("Predicted",CAT_TYPE[l], "\n(days)"),
                         trans = ifelse(CAT_TYPE[l] == "R", "identity",COLOR_SCALE_TRANSFORMATION),
                         colors=c("black","blue","yellow","red"),
                         breaks = c(1, 2, 4, 8, 15, MAX_DISPLAYED_CAT),
                         limits = c(0.5, ifelse(CAT_TYPE[l] == "R", NA, MAX_DISPLAYED_CAT)))+
    geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
    ggtitle(format(months_in_data[m], format = "%B")) -> cat_maps[[m]]
  
  
  cat_maps[[m]] <- mise.en.forme.ggplot(cat_maps[[m]])
  
  tick <- tick + 1
  setTxtProgressBar(pb, value = tick)
}


catmaps <- ggpubr::ggarrange(plotlist = cat_maps,
                             ncol = 4, nrow = 3,
                             align = "hv", labels = "AUTO",
                             common.legend = T,
                             legend = "right")
ggsave(Output_names$prediction$cats[[VARS_DENSITY[j]]][[ARRAY_TYPE[k]]][[CAT_TYPE[l]]], catmaps,
       width = 120*4 + 20,
       height = 105*3, units = "mm")

saveRDS(cat_maps,
        file = gsub("png","rds", Output_names$prediction$cats[[VARS_DENSITY[j]]][[ARRAY_TYPE[k]]][[CAT_TYPE[[l]]]]))
