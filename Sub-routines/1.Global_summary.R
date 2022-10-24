#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-22
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description: Sub-routine to generate the global summary on the simulation outputs 
#'
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

for (i in 1:length(ARRAY_TYPE)){
  cat_summary[[i]] %>% mutate(array_type = names(cat_summary)[i]) -> cat_summary[[i]]
  cat_diff_summary[[i]] %>% mutate(array_type = names(cat_diff_summary)[i]) -> cat_diff_summary[[i]]
  cat_return_summary[[i]] %>% mutate(array_type = names(cat_return_summary)[i]) -> cat_return_summary[[i]]
}

cat_summary <- dplyr::bind_rows(cat_summary)
cat_diff_summary <- dplyr::bind_rows(cat_diff_summary)
cat_return_summary <- dplyr::bind_rows(cat_return_summary)

cat_diff_summary %>% dplyr::mutate(cat_type = "diff") %>%
  dplyr::bind_rows(cat_return_summary %>%
                     dplyr::mutate(cat_type = "return")) -> cat_summary_by_type

bind_rows(cat_summary_by_type, cat_summary %>% dplyr::mutate(cat_type = "both")) -> cat_summary

# Save global CAT summary
write.csv(cat_summary,
          Output_names$cats$summary)