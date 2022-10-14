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


varname <- c(paste("PREDICTED",
                    toupper(fct[j]),
                    cat_type[l],
                    toupper(array_type[k]),
                    sep = "_"))

data %>% dplyr::filter(YEAR == sort(unique(data$YEAR))[i]) %>%
  dplyr::rename("toplot" = dplyr::all_of(varname[1])) -> sub_data

months_in_data <- sort(unique(sub_data$DATE))
cat_maps <- list()

for (m in 1:length(unique(sub_data$DATE))){
  
  # sub_data %>% dplyr::filter(!duplicated(id_unique) & DATE == months_in_data[m]) %>%
  #   dplyr::filter(toplot < max_displayed_cat) %>%
  #   ggplot()+
  #   geom_histogram(aes(x=toplot), binwidth = 2, color = "grey20", fill = "grey90")+
  #   scale_y_continuous(n.breaks = 5)+
  #   theme(panel.background = element_rect(fill = "white", color = "black"),
  #         panel.grid = element_blank(),
  #         plot.background = element_rect(fill = "white", color = "black"),
  #         axis.title = element_blank()) -> p1
  
  sub_data %>% dplyr::filter(!duplicated(id_unique) & DATE == months_in_data[m]) %>%
    dplyr::filter(toplot < max_displayed_cat) %>%
    # dplyr::filter((!!rlang::sym(vars[j])) < maxs[j]) %>%
    ggplot()+
    coord_sf(xlim = c(30, 110), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
    geom_tile(aes(x=degraded_lon, y=degraded_lat, fill=toplot))+
    scale_fill_gradientn(paste("Predicted",cat_type[l], "\n(days)"),
                         trans = ifelse(cat_type[l] == "R", "identity",color_scale_transformation),
                         colors=c("black","blue","yellow","red"),
                         # n.breaks=10,
                         breaks = c(1, 2, 4, 8, 15, max_displayed_cat),
                         limits = c(1, ifelse(cat_type[l] == "R", NA, max_displayed_cat)))+
    geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
    ggtitle(format(months_in_data[m], format = "%B")) -> cat_maps[[m]]
  
  
  cat_maps[[m]] <- mise.en.forme.ggplot(cat_maps[[m]])
  
  # cat_maps[[1]][[l]] <- cowplot::ggdraw(cat_maps[[1]][[l]])+cowplot::draw_plot(p1, 0.5, 0.2, 0.25, 0.2)
  # cat_maps[[2]][[l]] <- cowplot::ggdraw(cat_maps[[2]][[l]])+cowplot::draw_plot(p2, 0.5, 0.2, 0.25, 0.2)
}


catmaps <- ggpubr::ggarrange(plotlist = cat_maps,
                             ncol = 4, nrow = 3,
                             align = "hv", labels = "AUTO",
                             common.legend = T,
                             legend = "right")
ggsave(Output_names$prediction$cats[[as.character(sort(unique(data$YEAR))[i])]][[fct[j]]][[array_type[k]]][[cat_type[l]]], catmaps,
       width = 120*4 + 20,
       height = 105*3, units = "mm")

saveRDS(cat_maps,
        file = gsub("png","rds", Output_names$prediction$cats[[as.character(sort(unique(data$YEAR))[i])]][[fct[j]]][[array_type[k]]][[cat_type[[l]]]]))
