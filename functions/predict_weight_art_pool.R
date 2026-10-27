predict_weight_art_pool <- function (i, data = data) {
  #filter per imputation
  dat_imp <- filter(data, .imp == i)
  
  dat_imp <- dat_imp %>%
    mutate(
      .imp = i,
      ps = predict(
        #take from the list, the corresponding imputation and visit and calculate propensity score
        fit_list_art_pool[[paste0("imp_", i)]],
        type = "response"
      )
    )
}
