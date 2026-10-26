make_education_cat_base <- function(data) {
  
  data <- data %>%
    arrange(.imp, id, treatment_clone, days_from_fdd, visit) %>%
    group_by(id, .imp, treatment_clone) %>%
    mutate(
      mut_number = row_number(), #define order while arranged above
      education_cat_base = case_when(
        #we want to look at education at baseline, with case when it only looks at first row if this is true it does not look further
        #therefore, we assume highest education is true, if for example both education_1 and 3 are imputed with 1 at baseline
        #if education 3 2 1 and 0 are all 0, this will for now return NA
        mut_number == 1 & education_3 == 1 ~ 3,
        mut_number == 1 & education_2 == 1 ~ 2,
        mut_number == 1 & education_1 == 1 ~ 1,
        mut_number == 1 & education_0 == 1 ~ 0,
        .default = NA 
      ),
      #we define education category during follow-up, so with mut_number > 1
      #again, we assume the highest education if several are imputed with 1
      #note, this can be imputed differently at different rows during follow-up
      educ_cat_fu = case_when(
        mut_number > 1 & education_3 == 1 ~ 3,
        mut_number > 1 & education_2 == 1 ~ 2,
        mut_number > 1 & education_1 == 1 ~ 1,
        mut_number > 1 & education_0 == 1 ~ 0,
        .default = NA
      ),
      max_educ_cat_fu = max(educ_cat_fu, na.rm = TRUE), #define highest education during follow-up
      max_educ_cat_fu = if_else(is.infinite(max_educ_cat_fu), NA, max_educ_cat_fu), #if NA, max() returns infinite, we set back to NA
      #if education was missing at baseline, so all categories zero, we take the highest imputed level during follow-up and set that as baseline
      #if education was missing at baseline and also always zero (so NA) during follow-up, we assume education level of zero for baseline
      education_cat_base = case_when(
        is.na(education_cat_base) & !is.na(max_educ_cat_fu) ~ max_educ_cat_fu,
        is.na(education_cat_base) & is.na(max_educ_cat_fu) ~ 0,
        .default = education_cat_base
      )
    ) %>%
    fill(education_cat_base, .direction = "down") %>% #pull the education base level down for every row
    ungroup()
  
  return(data)
    
}
