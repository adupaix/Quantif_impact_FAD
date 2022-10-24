#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Sub-routine to summarise the simulation outputs and fit the CATdiff, CATreturn,
#'              R (A/B) and CAT as a function of FAD density
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

if (array_type.i %in% ARRAY_TYPE | GENERAL_SUMMARY){
  if (file.exists(Output_names$regression[[array_type.i]]$general$nls_rds) & !GENERAL_SUMMARY){
    
    models[[array_type.i]] <- readRDS(Output_names$regression[[array_type.i]]$general$nls_rds)
    models_diff[[array_type.i]] <- readRDS(Output_names$regression[[array_type.i]]$diff$nls_rds)
    models_return[[array_type.i]] <- readRDS(Output_names$regression[[array_type.i]]$return$nls_rds)
    models_proportion[[array_type.i]] <- readRDS(Output_names$regression[[array_type.i]]$proportion$nls_rds)
    
  } else {

    CATs <- read.cats(sim_output_path = SIM_OUTPUT_PATH,
                      array_type = array_type.i)
    cat_summary[[array_type.i]] <- generate.summary(CATs)
    
    p <- plot.CAT.distribution(CATs)
    ggsave(Output_names$cats[[array_type.i]]$plot_distribution, p)
    
# CAT diff ----
    CATs %>% dplyr::filter(nn_nb != 0) -> CAT_diff
    cat_diff_summary[[array_type.i]] <- generate.summary(CAT_diff)
    
    models_diff[[array_type.i]] <- nls.and.plot(cat_summary = cat_diff_summary[[array_type.i]],
                                           plot_name = Output_names$regression[[array_type.i]]$diff$plot,
                                           plot_rds_name = Output_names$regression[[array_type.i]]$diff$plot_rds,
                                           nls_name = Output_names$regression[[array_type.i]]$diff$nls_rds,
                                           cat_type = "diff")
    
    
    sink(type = "output", file = Output_names$regression[[array_type.i]]$diff$model_summary)
    cat(paste0("Summary nls CATdiff ", array_type.i," array\n-----------------------\n"))
    summary(models_diff[[array_type.i]])
    sink()
    
# CAT return ----
    CATs %>% dplyr::filter(nn_nb == 0) -> CAT_return
    cat_return_summary[[array_type.i]] <- generate.summary(CAT_return)
    
    models_return[[array_type.i]] <- nls.and.plot(cat_summary = cat_return_summary[[array_type.i]],
                                             plot_name = Output_names$regression[[array_type.i]]$return$plot,
                                             plot_rds_name = Output_names$regression[[array_type.i]]$return$plot_rds,
                                             nls_name = Output_names$regression[[array_type.i]]$return$nls_rds,
                                             cat_type = "return")
    
    sink(type = "output", file = Output_names$regression[[array_type.i]]$return$model_summary)
    cat("\nSummary nls CATreturn square array\n-----------------------\n")
    summary(models_return[[array_type.i]])
    sink()
    
# Proportion (R) ----
    models_proportion[[array_type.i]] <- nls.and.plot.proportion(cat_return_summary = cat_return_summary[[array_type.i]],
                                                            cat_diff_summary = cat_diff_summary[[array_type.i]],
                                                            plots_name = c(Output_names$cats[[array_type.i]]$plot_proportion,
                                                                           Output_names$regression[[array_type.i]]$proportion$plot),
                                                            plot_rds_name = Output_names$regression[[array_type.i]]$proportion$plot_rds,
                                                            nls_name = Output_names$regression[[array_type.i]]$proportion$nls_rds)
    
    sink(type = "output", file = Output_names$regression[[array_type.i]]$proportion$model_summary)
    cat("\nSummary nls R (A/B) square array\n-----------------------\n")
    summary(models_proportion[[array_type.i]])
    sink()
    
    # generate both the simplefit model (CAT = a/rho^b)
    # and the plot of the more complex fit (obtained with the formula CAT = (R*CATdiff + CATreturn)/(1+R))
    models[[array_type.i]] <- general.fit(model_diff = models_diff[[array_type.i]],
                                     model_return = models_return[[array_type.i]],
                                     model_proportion = models_proportion[[array_type.i]],
                                     cat_summary = cat_summary[[array_type.i]],
                                     plot_name = Output_names$regression[[array_type.i]]$plot,
                                     plot_rds_name = Output_names$regression[[array_type.i]]$plot_rds,
                                     nls_name = Output_names$regression[[array_type.i]]$general$nls_rds)
    
    sink(type = "output", file = Output_names$regression[[array_type.i]]$general$model_summary)
    cat("\nSummary nls CAT square array\n-----------------------\n")
    summary(models[[array_type.i]])
    sink()
    
  }
}
