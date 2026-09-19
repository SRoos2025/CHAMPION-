predict_weight <- function (i) {
  #filter per imputation
  dat_imp <- filter(dat_imputed_both_clones, .imp == i)
  
  #map over visits and put in dataframe using bind rows
  bind_rows(
    map(visits, function(v) {
      dat_imp_visit <- filter (dat_imp, visit == v)
      
      dat_imp_visit <- dat_imp_visit %>%
        mutate(
          .imp = i,
          visit = v,
          ps = predict(
            #take from the list, the corresponding imputation and visit and calculate propensity score
            fit_list[[paste0("imp_", i)]][[as.character(v)]],
            type = "response"
          )
        )
    })
  )
}
