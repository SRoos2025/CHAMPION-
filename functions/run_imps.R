run_imps <- function(n_imp = 10, maxit = 20, path, 
                     data, method = vec_mtd, predictorMatrix = mat_prd) {
  
  for (i in seq_len(n_imp)) {
    
    fname <- paste0(path, "imp_", i, ".rds")
    
    
    #check if file exists https://www.statology.org/r-check-if-file-exists/
    if (file.exists(fname)) {
      next
    }
    
    imp_i <- mice(
      data = data,
      m = 1,
      maxit = maxit,
      method = method,
      predictorMatrix = predictorMatrix,
      seed = i
    )
    
    saveRDS(imp_i, file = fname) # save inbetween
  
    gc()  #make memory free
  }
  
}
