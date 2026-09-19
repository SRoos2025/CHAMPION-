art_glm_allvisits <- function (i) {
  dat_imp <- filter (dat_imputed_both_clones, .imp == i)
  
  glm(art_cens == 0 ~ 
        #demographic
        age_cat + demo_male + education_cat_base +  
        #clinical
        txt_pre_sbp + txt_pre_dbp + cci + bmi +
        #dialysis related
        ultra_fil_rate + txt_per_week + catheter_1 + lab_wktv + txt_qb_mean +
        # lab
        lab_serum_na + lab_potassium + lab_phosph + lab_albumin + lab_crp +
        lab_hgba1c + lab_calcium + lab_creatinine + lab_hgb + lab_pth + lab_bicarb + lab_ferritin, 
      data = dat_imp,
      family = binomial()
  )
}
