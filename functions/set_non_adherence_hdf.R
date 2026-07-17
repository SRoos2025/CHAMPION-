#goal of function is to loop over hdf treatments within 90 days and determine non-adherence as censor reason accordingly
#expects data from one id, so when calling this function, make sure to first group by id.
set_non_adherence_hdf <- function(data,
                                  check_period = 14,
                                  max_fu = 90) {
    
    
    #first step, we want to filter the hdf treatments within 90 days
    #note that this function is not grouped per id, so if you use this function you should group by id first.
    #make a vector of the days_from_fdd where hdf_row is not missing within 90 days.
    #These correspond to the days where hdf treatments took place
    hdf_vec = data[["days_from_fdd"]][data[["modality"]] == 1 & data[["days_from_fdd"]] <= 90]
    #determine length of vector
    n <- length(hdf_vec)
    
    #if n == 0, there are no hdf treatments, for this scenario we already set censor reason
    #see code line 142-164
    if (n == 0) {
        return(unique(data[["cens_reason"]]))
    }
    else{
        #set the vector in the right order
        hdf_vec <- sort(hdf_vec)
        #set the start of the first treatment
        j <- 1
        #while it is less then or the last treatment
        while (j <= n) {
            #look 14 days (check_period) ahead from the current treatment
            window_end <- hdf_vec[j] + check_period
            #and determine how many treatments there were within this window
            n_in_window <- sum(
                hdf_vec >= hdf_vec[j] &
                    hdf_vec <= window_end
            )
            #if this was less than 3 there are several options
            if(n_in_window < 3){
                if (window_end <= 90) {
                    if (!is.na(unique(data[["trans_date"]])) && unique(data[["trans_date"]]) <= max_fu && unique(data[["trans_date"]]) <= window_end) {
                        return("transplantation")
                    }else if (!is.na(unique(data[["withdraw_date"]])) && unique(data[["withdraw_date"]]) <= max_fu && unique(data[["withdraw_date"]]) <= window_end) {
                        return("withdrawal")
                    } else if (!is.na(unique(data[["recov_date"]])) && unique(data[["recov_date"]]) <= max_fu && unique(data[["recov_date"]]) <= window_end) {
                        return("kidney function recovery")
                    } else if (!is.na(unique(data[["death_date"]])) && unique(data[["death_date"]]) <= max_fu && unique(data[["death_date"]]) <= window_end) {
                        return("death")
                        #else, if there were less than 3 HDF treatments within the grace period but not due to any causes mentioned above,
                        #it was general non-adherence to the strategy for example due to switchting to HD
                    } else {
                        return("non-adherence")
                    }
                } else if (window_end > 90) {
                    #if windowend is after 90 days, then it may stay the censor reason
                    #note we already filtered out patients that never started hdf, elsewhere.
                    return(unique(data[["cens_reason"]]))
                }
            } else {
                #>=2 treatments within window, continue to j+1
                #the loop should continue until j == n, and then if n is never <2, we define censor reason seperately
                #we repeat this, above it is one of the options for n_in_window <2
                if (j == n) {
                    return(unique(data[["cens_reason"]]))
                }
                
                j <- j + 1
                
            } # end else (n_in_window >= 3 case)
        } # end while-loop
    } # end else (n > 0)
} # end function set_non_adherence_hdf