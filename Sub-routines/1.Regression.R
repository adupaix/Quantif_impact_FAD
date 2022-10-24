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
    
    # 1.1.2. Regression + plot ----
    # # 1.1.2.1. All CATs ---
    # 
    # lm_square <- lm.and.plot(cat_summary = cat_summary_square,
    #                          plot_name = Output_names$regression[[array_type.i]]$plot,
    #                          rds_name = Output_names$regression[[array_type.i]]$rds)
    # 
    # sink(type = "output", file = Output_names$regression[[array_type.i]]$summary, type = "output")
    # cat("Summary lm square array\n-----------------------\n")
    # summary(lm_square)
    # sink()
    
    # 1.1.2.2. CATdiff only ----
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
    
    # 1.1.2.2. CATreturn only ----
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
    
    # 1.1.3. Proportion of each CAT types ----
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

# COMMENTED 1.1.3. Nearest neighbor + plot ----

# CATs %>% plyr::ddply(.variables = c("distance_min","nn_nb"), .fun = nrow) -> nn_summary_square
# 
# nn_summary_square %>% plyr::ddply("distance_min", .fun = function(x) sum(x$V1)) %>%
#   dplyr::rename("ncat_total" = "V1") %>%
#   dplyr::right_join(nn_summary_square, by = "distance_min") %>%
#   dplyr::mutate(percent = 100*V1/ncat_total,
#          density = 1/(distance_min)**2) %>%
#   dplyr::mutate(nn_nb = case_when(nn_nb == 0 ~ "Return to init. FAD",
#                            nn_nb <= 4 ~ "1-4",
#                            nn_nb <= 8 ~ "5-8",
#                            nn_nb <= 12 ~ "9-12",
#                            nn_nb <= 20 ~ "13-20",
#                            nn_nb <= 24 ~ "21-24",
#                            nn_nb >= 25 ~ ">24")) %>%
#   plyr::ddply(.variables = c("distance_min", "ncat_total","nn_nb","density"), function(x) cbind(V1 = sum(x$V1), percent = sum(x$percent)) ) -> nn_summary_square
# 
# nn_summary_square %>% dplyr::filter(nn_nb == ">24") %>%
#   plyr::ddply(c("distance_min", "ncat_total", "density", "nn_nb"), function(x) data.frame(V1 = sum(x$V1),
#                                                                                           percent = sum(x$percent))) %>%
#   select(names(nn_summary_square)) -> nn_summary_square_ab24
# 
# nn_summary_square %>% dplyr::filter(nn_nb != ">24") %>%
#   dplyr::bind_rows(nn_summary_square_ab24) -> nn_summary_square
# 
# nn_summary_square$nn_nb <- factor(nn_summary_square$nn_nb,
#                                   levels = c("Return to init. FAD","1-4","5-8","9-12","13-20","21-24",">24"),
#                                   ordered = T)
# 
# p <- ggplot(data=nn_summary_square, aes(x=density, y=percent)) +
#   # scale_x_continuous(breaks = )+
#   scale_y_continuous(n.breaks = 10)+
#   geom_area(aes(fill = nn_nb), colour="grey") +
#   scale_fill_brewer("Visited FAD", type = "qual", palette = "Set1") +
#   xlab("FAD density (km-2)")+
#   ylab("Percentage of tuna visiting a given object")+
#   theme(panel.background = element_blank())
# ggsave(Output_names$nn_plot_square1, p)
# 
# p <- ggplot(CATs)+
#   geom_histogram(aes(x=nn_nb), binwidth = 1)+
#   facet_wrap(~distance_min)+
#   scale_x_continuous(limits = c(-1,30),
#                      breaks = c(1,5,9,13,21,25,29))+
#   xlab("Nearest neighbor number")
# 
# ggsave(Output_names$nn_plot_square2, p)
