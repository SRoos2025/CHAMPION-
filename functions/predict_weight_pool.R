predict_weight_pool <- function (i) {
  #filter per imputation
  dat_imp <- filter(dat_imputed_both_clones, .imp == i)
  
      dat_imp <- dat_imp %>%
        mutate(
          .imp = i,
          ps = predict(
            #take from the list, the corresponding imputation and visit and calculate propensity score
            fit_list_pool[[paste0("imp_", i)]],
            type = "response"
          )
        )
}
