#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2023-03-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Script to compare predictions from FOB and LOG densities
#'              
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

#' empty environment
rm(list = ls())
invisible(gc())

#'@Scripts_base_directory: script root path 
BASE_DIR <- "/home/adupaix/Documents/These/Axe_2/Quantif_impact_FAD/"

#' generate basic paths
DATA_PATH <- file.path(BASE_DIR,'Data')

OUTPUT_PATH <- file.path(BASE_DIR,"Outputs")
FUNC_PATH <- file.path(BASE_DIR,"Functions")
ROUT_PATH <- file.path(BASE_DIR, "Sub-routines")
RESOURCE_PATH <- file.path(BASE_DIR,"Resources")

#' ***************
#' Get libraries:
#' ***************
source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("plyr", "dplyr", "tidyr", "ggplot2", "ggpubr")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)

FOLDER_NAME <- "dd5bd6a"
# FOLDER_NAME <- "YFT50_R02"

FOB_TYPE1 <- "FOB"
FOB_TYPE2 <- "LOG"

#' Output folder names
#' ***************
#'   study with FOB densities
#' FOB_DIR_NAME <- paste0(FOLDER_NAME,FOB_TYPE1)
#' #'   study with LOG densities
#' buoys_DIR_NAME <- "dd5bd6a_buoys"


# Read predictions from outputs
var <- c("id_unique", "YEAR", "MONTH", "DATE", "degraded_lon", "degraded_lat",
         "mean_degraded_density","PREDICTED_MEAN_CAT_RANDOM", "PREDICTED_MEAN_PERCENT_RANDOM")
read.predict.data <- function(type, dir_name){
  return(
    read.csv(file.path(OUTPUT_PATH, dir_name,
                       "3_4.Prediction", "predictions_res5.csv")) %>%
      dplyr::select(dplyr::all_of(var)) %>%
      dplyr::rename("density" = "mean_degraded_density",
                    "CAT" = "PREDICTED_MEAN_CAT_RANDOM",
                    "Pa" = "PREDICTED_MEAN_PERCENT_RANDOM") %>%
      dplyr::mutate(FOB_type = type)
  )
}
data <- dplyr::bind_rows(read.predict.data(FOB_TYPE1, paste0(FOLDER_NAME,"_",FOB_TYPE1)),
                         read.predict.data(FOB_TYPE2, paste0(FOLDER_NAME,"_",FOB_TYPE2))) %>%
  dplyr::mutate(FOB_type = factor(FOB_type, levels = c(FOB_TYPE1,FOB_TYPE2)))

# filtering cells which are in common
id_to_keep1 <- as.character(unique(data[which(data$FOB_type == FOB_TYPE1),"id_unique"]))
id_to_keep2 <- as.character(unique(data[which(data$FOB_type == FOB_TYPE2),"id_unique"]))
id_to_keep <- c(id_to_keep1,id_to_keep2)[duplicated(c(id_to_keep1,id_to_keep2))]
data %>%
  dplyr::filter(id_unique %in% id_to_keep)-> data

## Generating comparison histograms

p1 <- ggplot(data = data)+ 
  geom_histogram(mapping = aes(x = density, y =..count.., fill=as.factor(FOB_type)),
                 alpha = 0.6,
                 position = "identity")+
  scale_x_continuous(limits = c(0,3.1*10**-3))+
  scale_fill_brewer("FOB type", palette = "Set1")+
  ylab("Number of cells")+xlab(expression(Density~(km^-2)))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))

p2 <- ggplot(data = data)+ 
  geom_histogram(mapping = aes(x = CAT, y =..count.., fill=as.factor(FOB_type)),
                 binwidth = 2, alpha = 0.6,
                 position = "identity")+
  scale_x_continuous(limits = c(0,50))+
  scale_fill_brewer("FOB type", palette = "Set1")+
  ylab("Number of cells")+xlab(expression(Predicted~CAT(rho)~(days)))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        axis.title.y = element_blank())

p3 <- ggplot(data = data)+ 
  geom_histogram(mapping = aes(x = Pa, y =..count.., fill=as.factor(FOB_type)),
                 binwidth = 5, alpha = 0.6,
                 position = "identity")+
  # scale_x_continuous(limits = c(0,30))+
  scale_fill_brewer("FOB type", palette = "Set1")+
  ylab("Number of cells")+xlab(expression(Predicted~P[a](rho)~("%")))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        axis.title.y = element_blank())
 

fig <- ggpubr::ggarrange(p1,p2,p3,
                         ncol = 3,
                         labels = "AUTO",
                         label.y = 1.03,
                         common.legend = T,
                         legend = "top")

dir.create(file.path(OUTPUT_PATH, paste0("Comparison_",FOB_TYPE1,"_",FOB_TYPE2,"_",FOLDER_NAME)), showWarnings = F)
ggsave(file.path(OUTPUT_PATH, paste0("Comparison_",FOB_TYPE1,"_",FOB_TYPE2,"_",FOLDER_NAME), "Figure5.png"), fig,
       width = 10, height = 6)

## Summary table
data %>%
  plyr::ddply(.variables = "FOB_type", function(x)
              c(mRho = mean(x$density, na.rm = T),
                seRho = sd(x$density, na.rm = T) / nrow(x),
                mCAT = mean(x$CAT, na.rm = T),
                seCAT = sd(x$CAT, na.rm = T)/nrow(x),
                mPa = mean(x$Pa, na.rm = T),
                sePa = sd(x$Pa, na.rm = T)/nrow(x))) -> summary_comparison

write.csv2(summary_comparison,
           file = file.path(OUTPUT_PATH,
                            paste0("Comparison_",FOB_TYPE1,"_",FOB_TYPE2,"_",FOLDER_NAME),
                            "summary_comparison.csv"),
           row.names = F)

