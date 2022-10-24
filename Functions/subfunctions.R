#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: file containing the functions used in the main script 
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

# function to format the Western Indian Ocean maps
mise.en.forme.ggplot <- function(p){
  p <- p + xlab("Longitude") +
    ylab("Latitude") +
    # echelle distance
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
    # fleche nord
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true", 
                                      pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                                      style = north_arrow_fancy_orienteering) +
    # legende a l interieur
    theme(panel.border = element_rect(colour = "black", fill=NA),
          # legend.position = c(1,0),
          # legend.justification = c(1,0),
          legend.background = element_rect(fill="white", linetype = "solid", colour = "black"),
          legend.title.align = .5,
          panel.background = element_rect(fill = "grey40"),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
}

generate.summary <- function(CATs){
  plyr::ddply(CATs, "distance_min", summarise, mean_CAT = mean(CAT), m = mean(one_over_CAT), sd = sd(one_over_CAT),
              mean_dist = mean(distFAD)) -> cat_summary
  plyr::ddply(CATs, "distance_min", function(x) nrow(x)) -> cat_summary2
  
  merge(cat_summary, cat_summary2) %>%
    dplyr::rename("n" = "V1") -> cat_summary
  
  cat_summary %>% mutate(rho = 1/(distance_min)^2, se = sd/sqrt(n)) -> cat_summary
  
  return(cat_summary)
}

distribution.plot <- function(CATs){
  ggplot()+
    geom_violin(data = CATs, aes(x=distance_min, y = one_over_CAT, group = distance_min))
}

nls.and.plot <- function(cat_summary,
                         plot_name,
                         plot_rds_name,
                         nls_name,
                         cat_type){
  x <- seq(0,max(cat_summary$rho),10^-5)
  
  if (cat_type == "diff"){
    model <- nls(mean_CAT ~ a * distance_min**b, data = cat_summary, start = list(a=0.1, b=1))
    y <- coef(model)[1] / (x ^ (coef(model)[2]/2))
  } else if (cat_type == "return"){
    model <- nls(mean_CAT ~ 1 + a * distance_min**b, data = cat_summary, start = list(a=0.1, b=1))
    y <- 1 + coef(model)[1] / (x ^ (coef(model)[2]/2))
  }
  
  
  
  p <- ggplot()+
    # geom_smooth(data = cat_summary, aes(x=rho, y=1/mean_CAT), method = "lm", formula = y ~ 0 + x, se = F)+
    # geom_errorbar(data = cat_summary, aes(x=rho, ymin = m - sd/sqrt(n), ymax = m + sd/sqrt(n)), alpha = 0.2)+
    geom_point(data = cat_summary, aes(x=rho, y=mean_CAT))+
    # geom_text(x = 1*max(cat_summary$rho)/5,
    #           y = 4*max(1/cat_summary$mean_CAT)/5,
    #           label = lm_eqn(lm_model), parse = TRUE)+
    geom_line(aes(x=x,y=y), col = "red")+
    ylim(0, ifelse(cat_type == "diff", 50, 30))+
    xlab(expression(rho ~ (km^-1)))
  
  ggsave(plot_name, p,
         height = 10, width = 10)
  # saveRDS(lm_model, lm_name)
  saveRDS(model, nls_name)
  saveRDS(p, plot_rds_name)
  
  return(model)
}

read.cats <- function(sim_output_path,
                      array_type){
  
  out_dirs <- list.dirs(file.path(sim_output_path, array_type), recursive = F)
  dists <- as.numeric(unlist(lapply(strsplit(out_dirs, "distFAD"), function(x) strsplit(x[2], "_")[[1]][1])))
  seeds <- as.numeric(unlist(lapply(strsplit(out_dirs, "seed"), function(x) strsplit(x[2], "_")[[1]][1])))
  
  if (file.exists(Output_names$cats[[which(names(Output_names$cats) == array_type)]]$csv)){
    cat("        Reading existing csv file\n")
    CATs <- read.csv(Output_names$cats[[which(names(Output_names$cats) == array_type)]]$csv)
  } else {
    cat("        Reading CATs from simulation outputs\n")
    CATs <- list()
    
    for (i in 1:length(dists)){
      CATs[[i]] <- read.csv(file.path(out_dirs[i], "CATs", "CATs_array.csv"),
                            sep = " ", header = F)
      names(CATs[[i]]) <- c("id_tuna","nstep_b","nstep_e", "FAD_b", "FAD_e", "CAT", "distFAD", "nn_nb", "is_a_CAT")
      
      # nn_nb <- apply(CATs[[i]], 1, get.nn.nb.square, dist.i = dists[i])
      
      
      CATs[[i]] %>% dplyr::select(id_tuna, CAT, distFAD, nn_nb) %>%
        mutate(seed = seeds[i],
               distance_min = dists[i]) -> CATs[[i]]
    }
    
    CATs <- bind_rows(CATs) %>% mutate(rho = 1/(distance_min)^2,
                                       one_over_CAT = 1/CAT)
    
    write.csv(CATs, Output_names$cats[[which(names(Output_names$cats) == array_type)]]$csv)
  }
  
  return(CATs)
}

generate.output.paths.Regression <- function(path){
  if(!dir.exists(path)){dir.create(path)}
  l <- list()
  l$plot <- file.path(path, "general_fit.png")
  l$plot_rds <- file.path(path, "general_fit.rds")
  # l$rds <- file.path(path, "lm.rds")
  # l$summary <- file.path(path, "lm.txt")
  
  l$proportion$plot <- file.path(path, "model_proportion.png")
  l$proportion$plot_rds <- file.path(path, "plot_proportion.rds")
  l$proportion$nls_rds <- file.path(path, "model_proportion.rds")
  l$proportion$model_summary <- file.path(path, "model_proportion.txt")
  
  l$diff$plot <- file.path(path, "model_diff.png")
  l$diff$plot_rds <- file.path(path, "plot_diff.rds")
  l$diff$nls_rds <- file.path(path, "model_diff.rds")
  # l$diff$lm_rds <- file.path(path, "model_lm_diff.rds")
  l$diff$model_summary <- file.path(path, "model_diff.txt")
  
  l$return$plot <- file.path(path, "model_return.png")
  l$return$nls_rds <- file.path(path, "model_return.rds")
  l$return$plot_rds <- file.path(path, "plot_return.rds")
  # l$return$lm_rds <- file.path(path, "model_lm_return.rds")
  l$return$model_summary <- file.path(path, "model_return.txt")
  
  l$general$nls_rds <- file.path(path, "model_general_simplefit.rds")
  l$general$model_summary <- file.path(path, "model_general_simplefit.txt")
  
  return(l)
}

nls.and.plot.proportion <- function(cat_return_summary,
                                    cat_diff_summary,
                                    plots_name,
                                    plot_rds_name,
                                    nls_name){
  
  merge(cat_return_summary, cat_diff_summary, by = c("distance_min", "rho")) %>%
    dplyr::select(distance_min, rho, n.x, n.y) %>%
    dplyr::rename("CATret" = "n.x", "CATdiff" = "n.y") %>%
    dplyr::mutate(tot = CATdiff + CATret) %>%
    tidyr::pivot_longer(-c(rho,tot,distance_min), names_to = "type", values_to = "n") %>%
    dplyr::mutate(p = n/tot) -> toplot
  
  p1 <- ggplot(toplot)+
    geom_area(aes(x = rho, y = p, fill = type),  alpha = 0.7)+
    scale_fill_brewer("CAT type", palette = "Set1")+
    scale_y_continuous(n.breaks = 10)+
    geom_point(data = toplot %>% dplyr::filter(type == "CATret"), aes(x = rho, y = p), color = "grey40")+
    ylab("Proportion of each CAT type")+
    xlab("FAD density (km-2)")
  
  ggsave(plots_name[1], p1,
         height = 10, width = 10)
  
  ratio <- plyr::ddply(toplot, c("distance_min","rho"), function(x) x[which(x$type == "CATdiff"), "n"] / x[which(x$type == "CATret"), "n"]) %>%
    dplyr::rename("R" = "V1")
  
  # model <- nls(R ~ a * rho**c * exp(b * rho) , data = ratio, start = list(a = 150, b = 400, c = 0.1),
  #              control = nls.control(maxiter = 300))
  model <- nls(R ~ a * distance_min**(-2*c) * exp(b / distance_min**2) , data = ratio, start = list(a = 150, b = 200, c = 0.1),
               control = nls.control(maxiter = 300))
  
  a = coef(model)[1]
  b = coef(model)[2]
  c = coef(model)[3]
  
  p2 <- ggplot()+
    geom_point(data = ratio, aes(x = rho, y = R))+
    geom_function(fun = function(x) a * x ** c * exp(b * x), col = "red")+
    xlab(expression(rho ~ (km^-1)))
  
  ggsave(plots_name[2], p2,
         height = 10, width = 10)
  saveRDS(model, nls_name)
  saveRDS(p2, plot_rds_name)
  
  return(model)
}

plot.CAT.distribution <- function(CATs){
  
  ggplot()+
    geom_violin(data = CATs, 
                aes(x=factor(distance_min, levels = sort(unique(distance_min), decreasing = T)),
                    y = one_over_CAT,
                    group = distance_min))+
    scale_y_continuous(limits = c(0, 5))+
    xlab("Mean interFAD distance (km)")+
    ylab("1/CAT (days-1)")
  
  
}


# save both the simplefit model (CAT = a/rho^b)
# and the plot of the more complex fit (obtained with the formula CAT = (R*CATdiff + CATreturn)/(1+R))
general.fit <- function(model_diff, model_return, model_proportion,
                        cat_summary,
                        plot_name,
                        plot_rds_name,
                        nls_name){
  # x <- seq(min(cat_summary$rho),max(cat_summary$rho),10^-5)
  # y = ( a * ad * exp(b * x) * x ^ (c + br/2) + x ^ ((br + bd) / 2) + ar * x ^ (bd/2) ) / (x ^ ((br + bd)/2) * (1 + a * x**c * exp(b * x)) )
  
  simple_model <- nls(mean_CAT ~ a / rho ** b, data = cat_summary, start = list(a=1,b=1))
  alpha = coef(simple_model)[1]
  betha = coef(simple_model)[2]
  
  saveRDS(simple_model, nls_name)

  
  p <- ggplot()+
    geom_point(data = cat_summary, aes(x = rho, y = mean_CAT))+
    geom_function(fun = cat.formula,
                  args = list(model_diff=model_diff,
                              model_return=model_return,
                              model_proportion=model_proportion,
                              out_var="CAT"),
                  col = "blue")+
    # geom_function(fun = function(x) alpha / x ** betha, col = "blue")+
    ylab("CAT (days)")+
    xlab(expression(rho ~ (km^-1)))
  
  ggsave(plot_name, p, width = 8, height = 8)
  saveRDS(p, plot_rds_name)
  
  return(simple_model)
}

#' @out_var: one of "CAT", "CATd", "CATr" or "R"
cat.formula <- function(rho, model_diff, model_return, model_proportion, out_var){
  ad = coef(model_diff)[1]
  bd = coef(model_diff)[2]
  ar = coef(model_return)[1]
  br = coef(model_return)[2]
  a = coef(model_proportion)[1]
  b = coef(model_proportion)[2]
  c = coef(model_proportion)[3]
  
  R = a * rho ** c * exp(b * rho)
  CATd = ad * rho ** (-bd/2)
  CATr = 1 + ar * rho ** (-br/2)
  CAT = (R * CATd + CATr)/(1+R)
  
  return(get(out_var))
  
}

col.to.discrete <- function(data, steps, col_name, new_col_name){
  
  #Rajoute une colonne a data, qu'on va completer avec les valeurs des categories
  data$discrete <- as.character(NA)
  
  for (i in 1:(length(steps)-1)){ # pour chaque pas
    data %>% mutate(discrete = case_when( #on rempli la colonne discrete avec une chaine de charactere
      #par exemple, si on en est au pas = 0.02, on remplace le NA de la colonne discrete par "0.02-0.05" si col_name est compris dans [0.02,0.05]
      !!rlang::sym(col_name) >= steps[i] & !!rlang::sym(col_name) < steps[i+1] & is.na(discrete) ~ paste0(steps[i],"-",steps[i+1]),
      !is.na(discrete) ~ discrete
    )) -> data
  }
  
  lvls <- paste0(steps, "-", lead(steps))[1:(length(steps)-1)]
  lvls <- lvls[length(lvls):1]
  data$discrete <- factor(data$discrete, levels = lvls)
  names(data)[which(names(data) == "discrete")] <- new_col_name
  
  return(data)
}
