make_implist <- function(n_imp = 10, path) {
 
  imp_list <- vector("list", n_imp)
   
  for (i in seq_len(n_imp)) {
    
    file_i <- readRDS(paste0(path, "imp_", i, ".rds"))
    
    include_i <- i ==1 #only include unimputed for the first dataset
    #then this line becomes TRUE, and complete has the argument include = TRUE or FALSE
   dat_imputed_all <- complete(file_i, action = "long", include = include_i)
   dat_imputed_all <- dat_imputed_all %>%
     mutate(
       .imp = if_else(.imp == 0, 0, i)
     )
   
   imp_list[[i]] <- dat_imputed_all
  }
  
  bind_rows(imp_list)
  
}
