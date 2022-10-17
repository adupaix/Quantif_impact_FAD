#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-10-17
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: sub-routine to take the number of FOB sets into account
#' Build a kernel density estimation on the number of FOB sets (from IOTC 2 data sources)
#' Build a surface of a given percentage of this density
#' Build maps of predicted CAT and Pa with the surface on it
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

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