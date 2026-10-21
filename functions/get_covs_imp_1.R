get_covs_imp_1 <- function (data) {
  
  imp_1 <- data %>%
    filter(.imp == 1) %>%
    mutate(
      visit = sprintf("%02d", as.integer(visit)) #fill with zero so visit 01 etc, max 2 positions long (for visit 10, 11, 12) and make integers
    ) %>%
    arrange(id, treatment_clone, as.integer(visit))
  
  covs <- imp_1[c("age_cat", "demo_male" , "education_cat_base" ,  
                  #clinical
                  "txt_pre_sbp", "txt_pre_dbp" , "cci" , "bmi" ,
                  #dialysis related
                  "ultra_fil_rate" , "txt_per_week" , "catheter_1" , "lab_wktv" , "txt_qb_mean" ,
                  # lab
                  "lab_serum_na" , "lab_potassium" , "lab_phosph" , "lab_albumin" , "lab_crp" ,
                  
                  "lab_hgba1c" , "lab_calcium" , "lab_creatinine" , "lab_hgb" , "lab_pth" , "lab_bicarb" , "lab_ferritin" )]
  
}
