#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Sub-routine to build the maps of the expected Pa (percentage of time spent associated)
#'
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


varname <- paste0("PREDICTED_",
                  toupper(fct[j]),
                  "_PERCENT_",
                  toupper(array_type[k]), "_d")
data_predict[,varname] <- factor(data_predict[,varname]) #drops the unused factors
my_colors <- scales::viridis_pal()(length(levels(data_predict[,varname]))+1) # get the colors (+1 to drop the yellow at the end of the palette)

data_predict %>%
  dplyr::rename("toplot" = dplyr::all_of(varname)) -> sub_data

months_in_data <- sort(unique(sub_data$DATE))
percent_maps <- list()

for (l in 1:length(unique(sub_data$DATE))){
  
  sub_data %>% dplyr::filter(!duplicated(id_unique) & DATE == months_in_data[l]) %>%
    # dplyr::filter(toplot < max_displayed_cat) %>%
    # dplyr::filter((!!rlang::sym(vars[j])) < maxs[j]) %>%
    ggplot()+
    coord_sf(xlim = c(30, 110), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
    geom_tile(aes(x=degraded_lon, y=degraded_lat, fill=toplot))+
    # scale_fill_gradientn("Predicted\n% of time\nassociated",
    #                      colors=c("black","blue","yellow","red"),
    #                      n.breaks=10,
    #                      limits = c(0,100))+
    # limits = c(0,maxs[j]))+
    scale_fill_manual(expression(P[a]~"(%)"),
                      values = my_colors,
                      aesthetics = "fill",
                      drop = F)+
    geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
    ggtitle(format(months_in_data[l], format = "%Y-%m")) -> percent_maps[[l]]
  
  
  percent_maps[[l]] <- mise.en.forme.ggplot(percent_maps[[l]])
}

percentmaps <- ggpubr::ggarrange(plotlist = percent_maps[1:(length(percent_maps))],
                                 ncol = 4, nrow = 3,
                                 align = "hv", labels = "AUTO",
                                 common.legend = T,
                                 legend = "right")
ggsave(Output_names$prediction$percent[[fct[j]]][[array_type[k]]], percentmaps,
       width = 120*4 + 20,
       height = 105*3, units = "mm")
saveRDS(percent_maps,
        file = gsub("png","rds",Output_names$prediction$percent[[fct[j]]][[array_type[k]]]))
