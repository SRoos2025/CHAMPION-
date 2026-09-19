glm_fixed <-  function (i) {
  dat_imp <- filter(data_weighted, .imp == i) # filter each imputed set
  
  #make model for chance of not being censored, inf_cens = 0
  map(visits, function(v) {
    dat_imp_visit <- filter(dat_imp, visit == v)
    
    glm(
      inf_cens == 0 ~  #only fixed effects #demographic
        age_cat + demo_male + education_cat_base +  
        #clinical
        cci,
      data = dat_imp_visit,
      family = binomial()
    )
  })
}
