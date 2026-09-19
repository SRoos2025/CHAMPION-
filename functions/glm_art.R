glm_art <- function (i) {
  dat_imp <- filter(data_weighted, .imp == i) # filter each imputed set
  
  #make model for chance of not being censored, inf_cens = 0
  map(visits, function(v) {
    dat_imp_visit <- filter(dat_imp, visit == v)
    
    glm(
      art_cens == 0 ~ 
        #demographic
        age_cat + demo_male + education_cat_base +  
        #clinical
        txt_pre_sbp + txt_pre_dbp + cci + bmi +
        #dialysis related
        ultra_fil_rate + txt_per_week + catheter_1 + lab_wktv + txt_qb_mean + 
        # lab
        lab_serum_na + lab_potassium + lab_phosph + lab_albumin + lab_crp +
        lab_hgba1c + lab_calcium + lab_creatinine + lab_hgb + lab_pth + lab_bicarb + lab_ferritin, 
      data = dat_imp_visit,
      family = binomial()
    )
  })
}
