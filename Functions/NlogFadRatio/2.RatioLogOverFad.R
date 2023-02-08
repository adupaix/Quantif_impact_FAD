#########################################################################################
                              #   RATIO FUNCTION  #
#########################################################################################
#'@AUTHOR: Amael DUPAIX
#'@contact: amael.dupaix@ens-lyon.fr
#'
#' last modification: 2023-02-03
#' -----------------

#======================================================================================#
#                               RASTER DES RATIOS LOG/FAD                              #
#======================================================================================#
#  ARGUMENTS:                                                                          #
# Ob7 (data.frame) : donnees observateurs (operations sur objets flottants)            #
# ttob7 (data.frame) : donnees observateurs (toutes activitees)                        #
# year (num) : annees dont on veut la carte. c() pour un pool des annees               #
# log.type (chr) : choix entre "ALOG", "NLOG" et "LOG" (a la fois NLOG et ALOG)        #
# month (vector) : mois dont on veut la carte. c() pool tous les mois                  #
# gsize (num) : choix de la resolution, pour des cellules de gsizexgsize degrees       #
# Ob7_preped (log) : Ob7 fourni est brut (F) ou c'est le res de prep.obs() (T)         #
#                                                                                      #
# return: ggplot                                                                       #
#--------------------------------------------------------------------------------------#

ratio.log.fad <- function(Ob7, ttob7, year = c(), log.type = "LOG", month = c(),
                          gsize = 10, Ob7_preped=10){
  
  ### 1. PREPARATION DES DONNEES ----
  #------------------------------------------------------------------------------  
  if (Ob7_preped == FALSE){
    Ob7 <- prep.obs(Ob7)
  }
  
  ### 2. SELECTION DONNEES D'INTERET ----
  #------------------------------------------------------------------------------   
  ## A. SELECTION COLONNES
  data<-data.frame(Ob7$year,Ob7$latitude,Ob7$longitude,Ob7$obj_conv,Ob7$observation_date)
  names(data)<-c("year","latitude","longitude","fob_type","observation_date")
  
  ttob7 <- data.frame(ttob7$year,ttob7$vessel_name,ttob7$observation_date,
                      ttob7$latitude,ttob7$longitude)
  names(ttob7) <- c("year","vessel_name","observation_date","latitude","longitude")
  
  ## B. EXTRACTION DES DONNEES DE L'ANNEE ----
  if (!is.null(year)){
    data<-data[data$year %in% year,]
    ttob7 <- ttob7[ttob7$year %in% year,]
  }
  
  ## C. SUPPRIME LES LIGNES CONTENANT NULL, FOB, LOG ou des COORDONNEES NULLES-----
  data<-data[data$fob_type!="NULL",]
  data<-data[data$fob_type!="FOB",]
  if (log.type != "LOG"){
    data<-data[data$fob_type!="LOG",]
  }
  data<-data[data$latitude!=0 | data$longitude!=0,]
  ttob7<-ttob7[ttob7$latitude!=0 | ttob7$longitude!=0,]
  
  ## D. SELECTION DES DONNEES DU TRIMESTRE ----
  if (!is.null(month)){

    data <- subset.month(data, month)
    ttob7 <- subset.month(ttob7, month)

  }
  
  ## E. TRI POUR GARDER UNE OBS PAR J DANS TTES OBS ----
  ttob7$id_unique <- paste(ttob7$vessel_name,ttob7$observation_date)
  ttob7 <- ttob7[!duplicated(ttob7$id_unique),]
  
  ## F. NOMBRE TOTAL D'OBJET-----
  n.fad <- dim(data[data$fob_type=="FAD",])[1]
  if (log.type=="ALOG"){
    n.log <- dim(data[data$fob_type=="ALOG",])[1]
  } else if (log.type=="NLOG"){
    n.log <- dim(data[data$fob_type=="NLOG",])[1]
  } else if (log.type=="LOG"){
    n.log <- dim(data[data$fob_type%in%c("NLOG","ALOG","LOG"),])[1]
  }
  ### 3. CREATION DES RASTERS-----
  #---------------------------------------------------------------------------------------
  ## A. creation d'un raster, avec les bonnes coordonnees, et dont la grille contient des 0-----
  r <- create.raster(gsize)
  r_fad <- create.raster(gsize)
  eff_obs <- create.raster(gsize)
  
  ## B. COMPTE DES OCCURENCES PAR CELLULE-----
  # i. FAD-----
  fad<-data[data$fob_type=="FAD",]
  
  r_fad <- fill.raster(fad, r_fad)
  
  # ii. LOG-----
  if(log.type=="ALOG"){
    alog<-data[data$fob_type=="ALOG",]
    
    r <- fill.raster(alog, r)
    
  }else if(log.type=="NLOG"){
    nlog<-data[data$fob_type=="NLOG",]
    
    r <- fill.raster(nlog, r)
    
  } else if (log.type=="LOG"){
    nlog<-data[data$fob_type %in% c("NLOG","ALOG","LOG"),]
    
    r <- fill.raster(nlog, r)
  }
  
  # iii. Effort ----
  eff_obs <- fill.raster(ttob7, eff_obs)
  
  ## C. REMPLACE LES VALEURS DES CELLULES AVEC MOINS DE 10 JOURS D OBSERVATION PAR DES NA ----
  r <- del.low.eff(r, eff_obs, gsize)
  r_fad <- del.low.eff(r_fad, eff_obs, gsize)
  
  ## D. CALCUL DU RATIO LOG/FAD-----
  r[] <- r[]/r_fad[]
  
  names(r)<-"ratio"
  
  return(r)
  
}


#' Sub-functions of @ratio_log_fad()
#' ***********************************
## SELECTION DES DONNEES DES MOIS D'INTERET

subset.month <- function(data, month){
  data$observation_month <- as.numeric(strftime(data$observation_date, "%m"))
  data <- subset(data, data$observation_month %in% month)
  
  return(data)
}


## REMPLACE LES VALEURS DES CELLULES AVEC MOINS DE 10 JOURS D OBSERVATION PAR DES NA

del.low.eff <- function(rast, eff_obs, gsize){
  for (i in 1:length(rast@data@values)) {
    if (eff_obs@data@values[i]<6){
      rast@data@values[i]<-NA
    }
  }
  return(rast)
}


## COMPTE DES OCCURENCES PAR CELLULE DE RASTER
fill.raster <- function(data, rast){
  for (i in 1:dim(data)[1]) {
    coord<-c(data$longitude[i],data$latitude[i])
    rast[cellFromXY(rast,coord)]<-rast[cellFromXY(rast,coord)]+1
  }
  return(rast)
}


#######################################################################################
#                     CREATE AN EMPTY RASTER IN THE INDIAN OCEAN                      #
#######################################################################################
# ARGUMENTS:                                                                          #
# gsize (num): size of the grid cells, in degree                                      #
#######################################################################################


create.raster <- function(gsize){
  
  r <- raster(
    res = gsize,
    xmn = 20,
    xmx = 100,
    ymn = -40,
    ymx = 40
  )
  
  r[] <- 0
  names(r) <- "occ"
  
  return(r)
}



