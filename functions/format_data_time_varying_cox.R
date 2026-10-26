format_data_time_varying_cox <- function(data) {
  
  data <- data %>%
    group_by(id, treatment_clone, .imp) %>%
    mutate(
      current_cens_reason = if_else(treatment_clone == "hdf", cens_reason_hdf, cens_reason_hd), #determine current cens reason 
      t_start = days_from_fdd,
      t_stop = lead(days_from_fdd), #end of interval
      last_row = is.na(lead(days_from_fdd)), #true if it is the last row for that id
      t_stop = if_else(is.na(t_stop), current_cens_time, t_stop), #make tstop censor time if next time is missing (last row)
      inf_cens = case_when(
        last_row & current_cens_reason %in% c("withdrawal", "transplantation", "kidney function recovery") ~ 1,
        .default = 0),#for informative censoring during follow-up (so not the artificial censoring caused by our treatment assignment)
      inf_cens_incl_death = case_when( #for outcomes other than death, death is a competing risk
        last_row & current_cens_reason %in% c("withdrawal", "death", "transplantation", "kidney function recovery") ~ 1,
        .default = 0),
      art_cens = case_when( 
        last_row & current_cens_reason %in% c("non-adherence", "never started hd", "never started hdf") ~ 1,
        .default = 0), # for artificial censoring because of our treatment assignment
      death_outcome = case_when(
        last_row & current_cens_reason %in% c("death") ~ 1,
        .default= 0
      ),
      death_cardiovasc_outcome = case_when(
        last_row & current_cens_reason %in% c("death") & death_reason_cardiovasc == 1  ~ 1,
        .default= 0
      ),
      death_cardiac_outcome = case_when(
        last_row & current_cens_reason %in% c("death") & death_reason_cardiac == 1  ~ 1,
        .default= 0
      ),
      death_outcome_infect_incl_covid = case_when(
        last_row & current_cens_reason %in% c("death") & death_reason_infect_incl_covid == 1  ~ 1,
        .default= 0
      ),
      death_outcome_infect_excl_covid = case_when(
        last_row & current_cens_reason %in% c("death") & death_reason_infect_excl_covid == 1  ~ 1,
        .default= 0
      ),
      unique_id = paste0(id, treatment_clone) #create a unique ID combining ID and treatmentclone
    ) %>%
    ungroup()
}
