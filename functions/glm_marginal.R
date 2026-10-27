glm_marginal <-  function (i, data = data, outcome = "inf_cens == 0") {
  dat_imp <- filter(data, .imp == i) # filter each imputed set
  
  #make model for chance of not being censored, inf_cens = 0
  map(visits, function(v) {
    dat_imp_visit <- filter(dat_imp, visit == v)
    
    glm(
      inf_cens == 0 ~ 1, #only intercept 
      data = dat_imp_visit,
      family = binomial()
    )
  })
}


