#goal of function is to loop over hdf treatments within 90 days and determine censor_time accordingly
#expects data from one id, so when calling this function, make sure to first group by id.
set_censor_time_hdf <- function(data,
                                check_period = 14,
                                max_fu = 90) {
    #make vectors of the days with hdf treatments within grace period of 90 days
    hdf_vec <- data[["days_from_fdd"]][data[["modality"]] == 1 & data[["days_from_fdd"]] <= 90]
    #length of this vector is n (total hdf treatments)
    n <- length(hdf_vec)
    #if this is 0 treatments, we return cens_time as we have already defined cens time for those without hdf treatments within the grace period
    if (n == 0) {
        return(unique(data[["cens_time"]]))
    }
    
    else{
        #sort the vector so smallest day first
        hdf_vec <- sort(hdf_vec)
        #define position of vector should start at the beginning
        j <- 1
        
        while (j <= n) {
            #define window end, it is 14 days after each day within the vector
            window_end <- hdf_vec[j] + check_period
            #define amount of hdf treatments within window period, so within the 14 consecutive days
            n_in_window <- sum(hdf_vec >= hdf_vec[j] & hdf_vec <= window_end)
            
            
            #if there are less than 3 hdf treatments within 14 days
            if (n_in_window < 3) {
                #we first check if this window falls entirely within grace period
                if (window_end <= 90) {
                    #first check what the reasons could be and end with death date as other events always occur before death date
                    if (!is.na(unique(data[["trans_date"]])) && unique(data[["trans_date"]]) <= max_fu && unique(data[["trans_date"]]) <= window_end) {
                        return(unique(data[["trans_date"]]))
                    } else if (!is.na(unique(data[["withdraw_date"]])) && unique(data[["withdraw_date"]]) <= max_fu && unique(data[["withdraw_date"]]) <= window_end) {
                        return(unique(data[["withdraw_date"]]))
                    } else if (!is.na(unique(data[["recov_date"]])) && unique(data[["recov_date"]]) <= max_fu && unique(data[["recov_date"]]) <= window_end) {
                        return(unique(data[["recov_date"]]))
                    } else if (!is.na(unique(data[["death_date"]])) && unique(data[["death_date"]]) <= max_fu && unique(data[["death_date"]]) <= window_end) {
                        return(unique(data[["death_date"]]))
                        #if there is no specific reason return window end, so 14 days after current hdf treatment
                    } else {
                        return(window_end)
                    }
                } else {
                    # window_end > 90
                    #if window end falls after 90 days we do not have a complete 14 day period within the grace period to assess adherence
                    #so for example: if someone starts with hdf at day 80, we accept it as we do not have 14 days to assess adherence within the grace period
                    return(unique(data[["last_observed_date"]]))
                }
            } else {
                # n_in_window >= 3: adherence OK for this window, move to next treatment
                if (j == n) {
                    return(unique(data[["last_observed_date"]]))
                }
                j <- j + 1
            }
        } # end while
    } # end else (n != 0)
} # end function