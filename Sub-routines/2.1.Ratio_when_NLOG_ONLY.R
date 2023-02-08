#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2023-02-08
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Script to calculate the ratio between NLOG and FAD from observers data
#'                Used to estimate the NLOG densities if no DFAD where deployed
#'
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

###FONCTIONS NECESSAIRES
source(file.path(FUNC_PATH, "NlogFadRatio",'1.nDaysPerMonth.R'))
source(file.path(FUNC_PATH, "NlogFadRatio", '2.PrepObs.R'))
source(file.path(FUNC_PATH, "NlogFadRatio", "2.RatioLogOverFad.R"))

# OUTPUT_PATH2 <- file.path(OUTPUT_PATH, "2.LOG_over_FAD_from_Ob7")
# try(dir.create(OUTPUT_PATH2, recursive = T), silent = T)

Ob7<-read.csv(OBSERVERS_FOBFILE, header=T, sep=",")
Ob7 <- prep.obs(Ob7)

ttob7 <- read.csv(OBSERVERS_ACTIVITYFILE, header = T, sep = ",")

ratio <- list()


for (i in 1:12){
  r = ratio.log.fad(Ob7, ttob7, year = STARTING_YEAR_FOR_RATIO_CALCULATION:YEAR,
                    month = i, log.type = "LOG", gsize = RESOLUTION,
                    Ob7_preped = T)
  
  ratio[[i]] <- as.data.frame(r, xy = T) %>%
    dplyr::mutate(month = i)
  
}

df <- bind_rows(ratio) %>%
  dplyr::filter(!is.na(ratio)) %>%
  dplyr::rename("Log_over_Fad" = "ratio") -> df

ratio_file_name <- paste0("LOG_over_FAD_res", RESOLUTION, "_",
                          STARTING_YEAR_FOR_RATIO_CALCULATION ,"-",
                          YEAR, ".csv")

write.csv2(df, file.path(OUTPUT_PATH, "2.Buoy_density", ratio_file_name),
           row.names = F)

