#7-v2 hd clone
#determine censor time and reason for HDF clone
#go back to observations per two week period and every year afterwards
#last updated 31-08-2026
#0. set up----
##load packages----
pacman::p_load( "rio", #load data
                "purrr", #data untangling
                "magrittr", #untangle data
                "dplyr", #untangle data
                "tidyr", #untangle data
                "stringr", #to detect text patterns with reggex
                "lubridate") #for working with dates


##define path to save any output to, and path import to import data
path <- "..."
path_import <- "..."

#load functions
walk(list.files(paste0(path, "funs/")), \(x)source(paste0(path, "funs/", x)))

#load data with events and measurements of sessions including modality changes and their last event date relevant for mortality outcome
load(paste0(path, "combined_mortality_event.Rdata"))


#hdclone rename
cohort_hd <- combined_mortality_event
rm(combined_mortality_event)

#1.0 set censor time----
#define start time (we actually only know the quartile, but we must define the administrative end date)
#it is common in epidemiology to define time in the middle if it is unknown, 
cohort_hd <- cohort_hd %>%
  mutate(
    start_date = case_when(
      demo_fdd_quarter == 1 ~ ymd(paste0(demo_fdd_year, "-02-15")),
      demo_fdd_quarter == 2 ~ ymd(paste0(demo_fdd_year, "-05-16")),
      demo_fdd_quarter == 3 ~ ymd(paste0(demo_fdd_year, "-08-15")),
      demo_fdd_quarter == 4 ~ ymd(paste0(demo_fdd_year, "-11-15"))
    )
  )

#define end of study end time until end of the study
#also calculate 5 year after start (1826 days), however for Sweden, end of 2023, for Estionia, end of 2022 (see explanation in script 0)
cohort_hd <- cohort_hd %>%
  group_by(id)%>%
  mutate(
    #general end date of study (end of Apollo version 2)
    end_date_study = as.Date("2024-12-31"),
    #end date for sweden (only 1 person in 2024 with one observation)
    end_date_sweden = as.Date("2021-12-31"),
    #no more follow-up in estonia after 2022
    end_date_estonia_hungary = as.Date("2022-12-31"),
    end_date_serbia = as.Date("2023-12-31"),
    #use %--% from lubridate to define time difference beween two dates
    time_till_admin_end = case_when(
      country == "Sweden" ~ time_length(start_date %--% end_date_sweden, unit = "day"),
      country == "Estonia" ~ time_length(start_date %--% end_date_estonia_hungary, unit = "day"),
      country == "Hungary" ~ time_length(start_date %--% end_date_estonia_hungary, unit = "day"),
      country == "Serbia" ~ time_length(start_date %--% end_date_serbia, unit = "day"),
      .default =  time_length(start_date %--% end_date_study, unit = "day")),
    max_follow = as.numeric(1826), #5years = 1826 days
    #take whatever comes first to determine the admin censor time
    #take the smallest, so if time_till_admin_end date is longer than 5 years, take 5 years (max_follow)
    admin_censor_time = pmin(max_follow, time_till_admin_end)
  )%>%
  ungroup()

cohort_hd <- cohort_hd %>%
  arrange(id, days_from_fdd) %>%
  group_by(id) %>%
  mutate(
    #identifier for hdf clone
    treatment_clone = "hd",
    last_treatment = max(days_from_fdd),
    #indicator for when hdf is started
    hd_row = if_else(modality == 0, mut_number, NA),
    last_observed_date = pmax(last_treatment, last_event_date, last_lab, last_comorb, na.rm = TRUE),
    #indicator for when first hd therapy
    first_hd = min(hd_row, na.rm = TRUE),
    #if everything is NA, min () returns infinite so we set this to NA. This solves the popping warning message.
    first_hd = if_else(is.infinite(first_hd), NA, first_hd)
  ) %>%
  ungroup()

#we first define censoring time
#at A we define at for those who never started HD
#at B we define censor time for within grace period for those who did start HD

#A
#hd started ? if not, then censor at 90 days, otherwise NA
cohort_hd <- cohort_hd %>%
  group_by(id)%>%
  mutate(
    #if rownumber corresponds to first row number with hd treament, take that day
    first_hd_day = if_else(mut_number == first_hd, days_from_fdd, NA),
    #fill rows with that first day
    first_hd_day = max(first_hd_day, na.rm=TRUE),
    first_hd_day = if_else(is.infinite(first_hd_day), NA, first_hd_day),
    #create first transplant date, withdrawal date, recovery date
    #note we already took the last event date so this is now filled with only one event
    #note that transplant, recovery and withdrawal are the last events so we can take last_event_date in stead of days_from_fdd here
    #therefore, we take max not min
    #see 274-298 in script 0
    trans_date = if_else(event_discharge_reason == "Transplant-unknown donor", last_event_date, NA),
    trans_date = max(trans_date, na.rm = TRUE),
    trans_date = if_else(is.infinite(trans_date), NA, trans_date),
    recov_date = if_else(event_discharge_reason == "Kidney function recovered", last_event_date, NA),
    recov_date = max(recov_date, na.rm = TRUE),
    recov_date = if_else(is.infinite(recov_date), NA, recov_date),
    withdraw_date = if_else(event_discharge_reason == "Withdrawal from dialysis", last_event_date, NA),
    withdraw_date = max(withdraw_date, na.rm = TRUE),
    withdraw_date = if_else(is.infinite(withdraw_date), NA, withdraw_date),
    #create general censor date, which DOES NOT yet include non-adherence, we defint that time with function set censor time at line 128
    #death date was already defined in script 0
    general_cens_date = pmin(trans_date, recov_date, withdraw_date, death_date, last_observed_date, admin_censor_time, na.rm = TRUE),
    cens_time = case_when( 
      #if the first hd row is missing or first hd day after 90 days, it ends at 90 days
      (is.na(first_hd) | first_hd_day >90) & general_cens_date >=90 ~ 90,
      #of course, if they have transplant date or other censor event before 90 days this should be first (so first censor reason)
      (is.na(first_hd) | first_hd_day >90) & general_cens_date < 90 ~ general_cens_date,
      .default = NA)
  ) %>%
  ungroup() %>%
  suppressWarnings()#we fixed the warnings already by turning infinite to NA for every min function

#save in between
save(cohort_hd, file = paste0(path, "cohort_hd.Rdata"))
#load 
load(paste0(path, "cohort_hd.Rdata"))

#B
#determine if there are no gaps in hd treatments with function set_censor_time_hd
#we use pick everything because we want to apply the function using all data grouped by id
#set_censor_time_hd requires multiple columns input data from id such as last_date, days_from_fdd etc.
cohort_hd <- cohort_hd %>%
  group_by(id) %>%
  mutate(
    cens_time = set_censor_time_hd(pick(everything()))
  ) %>%
  ungroup()

#returns 0, everyone now has a censor time
check <- cohort_hd %>%
  filter(is.na(cens_time))




#3.0 set censor reason----
#at C we set it for those who never started HD
#at D we define it for within the grace period, with non adherence
#at E we define it for those who did start HDF and who were not already non adherent in section D
#of course, if last observed date is before 90 they may still have transplantation BEFORE death and then we take that date
#this is solved in the set non-adherence function after

#C
cohort_hd <- cohort_hd %>%
  group_by(id) %>%
  mutate(
    cens_reason = case_when(
      #first, we define the reasons for patients who did not start hdf within grace period
      #if they never started hdf or after 90 days, but were censored after 90 days the reason is they never started hdf
      (is.na(first_hd) | first_hd_day > 90) & cens_time >= 90 ~ "never started hd",
      #if they were censored before 90 days and the censor time corresponds to tranpslant date than that was the reason
      (is.na(first_hd) | first_hd_day > 90) & cens_time <90 & cens_time == trans_date ~ "transplantation",
      #same goes for withdrawal and kidney recovery, and than for death (but all these events occur before death if they would occur on the same day, therefore this order)
      (is.na(first_hd) | first_hd_day > 90) & cens_time <90 & cens_time == withdraw_date ~ "withdrawal",
      (is.na(first_hd) | first_hd_day > 90) & cens_time <90 & cens_time == recov_date ~ "kidney function recovery",
      (is.na(first_hd) | first_hd_day > 90) & cens_time <90 & cens_time == last_event_date &
        event_discharge_reason == "Left provider" | event_discharge_reason == "Transfer within provider" | event_discharge_reason == "NA" 
      | event_discharge_reason == "" ~ "admin",
      #if they were censored within grace period and censor date is death then cause is death
      (is.na(first_hd) | first_hd_day > 90) & cens_time <90 & cens_time == death_date ~ "death",
      #if it was the last measurement without any of the above then we dont know 
      #and it is administrative loss to follow-up unknown
      (is.na(first_hd) | first_hd_day > 90) & cens_time <90 & (cens_time == last_treatment | 
                                                                 cens_time == last_lab 
                                                               | cens_time == last_comorb | cens_time == admin_censor_time ) ~ "admin",
      .default = NA)
  ) %>%
  ungroup()

#define non adherence
cohort_hd <- cohort_hd %>%
  group_by(id) %>%
  mutate(
    cens_reason = set_non_adherence_hd(pick(everything()))
  )

#E
cohort_hd <- cohort_hd %>%
  group_by(id) %>%
  mutate(
    #now we define the censor reasons for people who did start hd within grace period and who were all the time adherent
    cens_reason = case_when(
      !is.na(first_hd) & first_hd_day <= 90 & 
        (cens_reason != "non-adherence" | is.na(cens_reason)) &
        cens_time == trans_date ~ "transplantation",
      !is.na(first_hd) & first_hd_day <= 90 & 
        (cens_reason != "non-adherence" | is.na(cens_reason)) &
        cens_time == withdraw_date ~ "withdrawal",
      !is.na(first_hd) & first_hd_day <= 90 & 
        (cens_reason != "non-adherence" | is.na(cens_reason)) &
        cens_time == recov_date ~ "kidney function recovery",
      !is.na(first_hd) & first_hd_day <= 90 & 
        (cens_reason != "non-adherence" | is.na(cens_reason)) &
        cens_time == last_event_date & 
        event_discharge_reason %in% c("Left provider", "Transfer within provider", "NA", "") ~ "admin",
      !is.na(first_hd) & first_hd_day <= 90 & 
        (cens_reason != "non-adherence" | is.na(cens_reason)) & 
        cens_time == death_date ~ "death",
      #if it was the last measurement without any of the above then we dont know 
      #and it is administrative loss to follow-up unknown
      !is.na(first_hd) & first_hd_day <= 90 & 
        (cens_reason != "non-adherence" | is.na(cens_reason)) &
        (cens_time == last_treatment | cens_time == last_lab | cens_time == last_comorb | cens_time == admin_censor_time) ~ "admin",
      .default = cens_reason
    )
  ) %>%
  ungroup()

#returns 0, everyone has censor reason      
check <- cohort_hd %>%
  filter(is.na(cens_reason))
#if censor time is longer than admin censor time, censor reason is now administrative
#if censor time is now longer than the admin_censor_time, we set it to admin_censor_time as this is the max
cohort_hd <- cohort_hd %>%
  group_by(id) %>%
  mutate(
    cens_time = if_else(cens_time >= admin_censor_time, admin_censor_time, cens_time),
    cens_reason = if_else(cens_time == admin_censor_time, "study end", cens_reason)
  ) %>%
  ungroup()

summary(cohort_hd$cens_time)#shows there are 38 patients with cens_time of 0
#checking these patients, they have 1 row of data, at day 0. We change this to 1, as we actually have 1 day (1 observation)
#and model may not work with time until event of 0
cohort_hd <- cohort_hd %>%
  group_by(id) %>%
  mutate(
    cens_time = if_else(cens_time == 0, 1, cens_time)
  ) %>%
  ungroup()

#first remove all icd10 text dots because reggex does not work well with .
cohort_hd <- cohort_hd %>%
  mutate(cod_icd10text = str_replace_all(cod_icd10text, "\\.", ""))

#define causes of death for everyone irrespective of censor reason 
cohort_hd <- cohort_hd %>%
  group_by(id) %>%
  mutate(
    death_reason_cardiovasc = case_when(
      max(event_died, na.rm = TRUE) == 1 & 
        str_detect(cod_icd10text[event_died == 1], "^I2[0-5]|^I[10-15]|^I3[0-9]|^I4[0-9]|^I5[0-2]|^I6[0-9]|^I7[0-9]|^G4[5-6]") ~ 1,
      .default = NA
    ),
    death_reason_cardiovasc = max(death_reason_cardiovasc, na.rm = TRUE),
    death_reason_cardiovasc = if_else(is.infinite(death_reason_cardiovasc), NA, death_reason_cardiovasc),
    death_reason_infect_incl_covid = case_when(
      str_detect(cod_icd10text[event_died == 1], "^A[0-9]|^B[0-9]|^U[07]|^J[0-22]") ~ 1,
      .default = NA 
    ),
    death_reason_infect_incl_covid = max(death_reason_infect_incl_covid, na.rm=TRUE),
    death_reason_infect_incl_covid = if_else(is.infinite(death_reason_infect_incl_covid), NA, death_reason_infect_incl_covid),
    death_reason_infect_excl_covid = case_when(
      str_detect(cod_icd10text[event_died == 1], "^A[0-9]|^B[0-9]|^J0[0-9]|^J1[0-9]|^J2[0-2]") ~ 1,
      .default = NA 
    ),
    death_reason_infect_excl_covid = max(death_reason_infect_incl_covid, na.rm = TRUE),
    death_reason_infect_excl_covid = if_else(is.infinite(death_reason_infect_incl_covid), 0, death_reason_infect_incl_covid)
  ) %>%
  ungroup()


#descritpive statistics----
desc <- cohort_hd %>%
  group_by(id) %>%
  mutate(
    #total treatments
    total_treat = n(),
    #total treatments after 90 days
    after = if_else(days_from_fdd >= 90, 1, 0),
    total_treat_after = sum(after == 1),
    #total hdf treatments
    total_hdf = sum(modality == 1, na.rm = TRUE), 
    #total hdf treatments after 90 days
    hdf_after = if_else(days_from_fdd >= 90 & modality == 1, 1, 0),
    total_hdf_after = sum(hdf_after == 1, na.rm = TRUE),
    #total hd treatments 
    total_hd = sum(modality == 0, na.rm = TRUE),
    #total hd treatments after 90 days
    hd_after = if_else(days_from_fdd >= 90 & modality == 0, 1, 0),
    total_hd_after = sum(hd_after == 1, na.rm = TRUE),
    #overall percentage treatments hdf & hd
    perc_hdf = (total_hdf) / total_treat * 100 ,
    perc_hd = (total_hd/total_treat) * 100,
    #overall percentage treatments after 90 days
    perc_hdf_after = (total_hdf_after/total_treat_after)*100,
    perc_hd_after = (total_hd_after/total_treat_after)* 100) %>%
  ungroup()

summary_all <- desc %>%
  group_by(id)%>%
  slice_head(n=1)

hd_adherent <- cohort_hd %>%
  filter(!is.na(first_hd) & first_hd_day <= 90 & cens_reason != "non-adherence") #18,554 patients

summary_adherent <- hd_adherent %>%
  group_by(id) %>%
  mutate(
    #total treatments
    total_treat = n(),
    #total treatments after 90 days
    after = if_else(days_from_fdd >= 90, 1, 0),
    total_treat_after = sum(after == 1),
    #total hdf treatments
    total_hdf = sum(modality == 1, na.rm = TRUE), 
    #total hdf treatments after 90 days
    hdf_after = if_else(days_from_fdd >= 90 & modality == 1, 1, 0),
    total_hdf_after = sum(hdf_after == 1, na.rm = TRUE),
    #total hd treatments 
    total_hd = sum(modality == 0, na.rm = TRUE),
    #total hd treatments after 90 days
    hd_after = if_else(days_from_fdd >= 90 & modality == 0, 1, 0),
    total_hd_after = sum(hd_after == 1, na.rm = TRUE),
    #overall percentage treatments hdf & hd
    perc_hdf = (total_hdf) / total_treat * 100 ,
    perc_hd = (total_hd/total_treat) * 100,
    #overall percentage treatments after 90 days
    perc_hdf_after = (total_hdf_after/total_treat_after)*100,
    perc_hd_after = (total_hd_after/total_treat_after)* 100) %>%
  slice_head(n=1)

hist(desc$first_hd_day)
hist(hd_adherent$first_hd_day)

hist(summary_all$perc_hd_after)
hist(summary_adherent$perc_hd_after)

hist(summary_adherent$perc_hdf_after)

#describe trajectory of hdf clones----

#it says first but they are of course all the same but it is trick to go to 1 row per person
table_censor <- cohort_hd %>%
  group_by(id) %>%
  summarise(
    cens_time =first(cens_time),
    cens_reason =first(cens_reason)
  )%>%
  ungroup()

table_censor_summary <- table_censor %>%
  group_by(cens_reason) %>%
  summarise(
    n = n(),
    pct = round(n() / nrow(table_censor) * 100, 1)
  ) %>%
  ungroup() %>%
  arrange(desc(n))


load(paste0(path, "cohort_hd.Rdata"))

#4.0 bring down to observation row per 2 week period----
grace_cohort <- cohort_hd %>%
  filter(days_from_fdd <= 90 & days_from_fdd <= cens_time) %>%
  mutate(two_week_period = case_when(
    days_from_fdd <= 14 ~ 1,
    days_from_fdd <= 28 ~ 2,
    days_from_fdd <= 42 ~ 3,
    days_from_fdd <= 56 ~ 4,
    days_from_fdd <= 70 ~ 5,
    days_from_fdd <= 84 ~ 6,
    days_from_fdd <= 90 ~ 7,
  ))

#define the variables we are interested in for IPCW and baseline
var_to_fill <- c("txt_dry_weight", 
                 "txt_ktv_ocm", "txt_pre_sbp", "txt_pre_weight", "txt_post_weight", "txt_time_eff", 
                 "txt_pre_dbp", "txt_substitution_volume", "txt_ufv", "txt_idwg_kg", "modality", "txt_per_week",
                 "catheter", "education", "txt_access_flow")


#per period, sort by closest to period end for each 2 weeks
#this way, we can later fill values forward from earlier rows if missing on closest row
period_end <- c( 14,  28,  42, 56, 70, 84,  90)

grace_cohort <- grace_cohort %>%
  #look up to the end day, so if period = 1, period end day becomes 14
  #this is possible because vector period end is sorted so 1 is position 1 in the vector, in this case 14
  mutate(period_end_day = pmin(period_end[two_week_period], cens_time),
         #define distance to period end day
         dist_to_end = period_end_day - days_from_fdd
  ) %>%
  #group by period within the id and then sort the distance to period end
  group_by(id, two_week_period) %>%
  arrange(dist_to_end) %>%
  #fill NA's, take first non NA value from closest row to period end day
  #na.omit removes all NA values from that column within the group
  #first takes the first remaining value — which is from the closest-to-end row that actually has a value
  mutate(across(all_of(var_to_fill), ~first(na.omit(.)))) %>%
  slice_head(n=1) %>%
  select(-period_end_day, -dist_to_end) %>%
  ungroup()

#create dataset with id and rows from 1 till 7 for all 2-week periods
grace_all <- expand.grid(
  id = unique(grace_cohort[["id"]]),
  two_week_period = 1:7
) %>%
  arrange(id, two_week_period)

#combine with grace cohort, so all two week periods that were not there before, now will be added.
grace_cohort <- left_join(grace_all, grace_cohort, by = c("id", "two_week_period"))%>%
  arrange(id, two_week_period)

#some days from fdd are now missing, we dont want them to get imputed
grace_cohort <- grace_cohort %>%
  group_by(id) %>%
  mutate(
    days_from_fdd = case_when(
      is.na(days_from_fdd) ~ two_week_period *14,
      two_week_period == 7 & days_from_fdd >90~ 90, # repeat this in case in was first missing, and now is >90
      .default = days_from_fdd
    ) 
  ) %>%
  ungroup()


year_cohort <- cohort_hd %>%
  filter(days_from_fdd >90 & days_from_fdd <= cens_time) %>%
  mutate(
    year_period = case_when(
      days_from_fdd >= 90 & days_from_fdd <= 360 ~ 1,
      days_from_fdd > 360 & days_from_fdd <= 720 ~ 2,
      days_from_fdd > 720 & days_from_fdd <= 1080 ~ 3,
      days_from_fdd > 1080 & days_from_fdd <= 1440 ~ 4,
      days_from_fdd > 1440 & days_from_fdd <= 1800 ~ 5,
      days_from_fdd > 1800 & days_from_fdd <= 2160 ~ 6,
      days_from_fdd > 2160 & days_from_fdd <= 2520 ~ 7
    )
  )
#define last day of the year for all times
period_year_end <- c(360, 720, 1080, 1440, 1800, 2160, 2520)

year_cohort <- year_cohort %>%
  #look up to the end day, so if period = 1, period end day becomes 14
  #this is possible because vector period end is sorted so 1 is position 1 in the vector, in this case 14
  mutate(period_end_day = pmin(period_year_end[year_period], cens_time),
         #define distance to period end day
         dist_to_end = period_end_day - days_from_fdd
  ) %>%
  #group by period within the id and then sort the distance to period end
  group_by(id, year_period) %>%
  arrange(dist_to_end) %>%
  #fill NA's, take first non NA value from closest row to period end day
  #na.omit removes all NA values from that column within the group
  #first takes the first remaining value — which is from the closest-to-end row that actually has a value
  mutate(across(all_of(var_to_fill), ~first(na.omit(.)))) %>%
  slice_head(n=1) %>%
  select(-period_end_day, -dist_to_end) %>%
  ungroup()

#create database with all two week periods 
year_all <- expand.grid(
  id = unique(year_cohort[["id"]]),
  year_period = 1:7
) %>%
  arrange(id, year_period)

#combine with grace lab
year_cohort <- left_join(year_all, year_cohort, by = c("id", "year_period"))%>%
  arrange(id, year_period)

#some days from fdd are now missing, we dont want them to get imputed
year_cohort <- year_cohort %>%
  group_by(id) %>%
  mutate(
    days_from_fdd = case_when(
      is.na(days_from_fdd) ~ year_period *360,
      year_period == 7 & days_from_fdd >2520~ 2520,
      # and the last should be max 2520
      .default = days_from_fdd
    ) 
  ) %>%
  ungroup()

#rename cohort_hdf so you also keep a database with all treatment mutations for descriptive purposes
#because in this chunk of code, we reduce to yearly and 2 week periods so not all treatment observations remain
cohort_hd_reduced <- bind_rows(grace_cohort, year_cohort) %>%
  arrange(id, two_week_period, year_period)%>%
  group_by(id) %>%
  #set missing periods to 0 instead of NA so they dont get imputed later on
  mutate(
    two_week_period= if_else(is.na(two_week_period), 0, two_week_period),
    year_period = if_else(is.na(year_period), 0, year_period),
    #fill cens_time again
    cens_time = max(cens_time, na.rm = TRUE),
    max_two_week_period = case_when(
      cens_time <= 14 ~ 1,
      cens_time >28 & cens_time < 42 ~ 2,
      cens_time >= 42 & cens_time < 56 ~ 3,
      cens_time >= 56 & cens_time < 70 ~ 4,
      cens_time >= 70 & cens_time < 84 ~5,
      cens_time >= 84 & cens_time < 90 ~6,
      cens_time >= 90 ~7
    ),
    max_year_period = case_when(
      cens_time <= 90 ~0,
      cens_time < 360 & cens_time > 90 ~ 0,
      cens_time >= 360 & cens_time < 720 ~1,
      cens_time >= 720 & cens_time <1080 ~ 2,
      cens_time >= 1080 & cens_time < 1440 ~3,
      cens_time >= 1440 & cens_time < 1800 ~4,
      cens_time >= 1800 ~5)
  ) %>%
  filter((year_period == 0 | year_period <= max_year_period) & (two_week_period == 0  | two_week_period <= max_two_week_period)) %>% #filter out empty rows of year periods after censor time
  #fill cens_reason again
  fill(cens_reason, facility_id, death_date, first_hosp, cardiac_hosp, cardiac_hosp_day, infect_hosp_excl_covid, infect_hosp_incl_covid,
       infect_hosp_excl_covid_day, infect_hosp_incl_covid_day, cardiovasc_hosp, cardiovasc_hosp_day, demo_esrd_cause_icd10text, demo_height,
       demo_male, subgroup_zero, cens_time, subgroup_later,  death_reason_cardiovasc, death_reason_infect_incl_covid, death_reason_infect_excl_covid,  demo_race, country, age_cat, cci, treatment_clone, last_observed_date, education, .direction= "downup")%>%
  ungroup()




# keep relevant column names
#include specifiers for subgroup analyses (as defined in script 0: subgroup_later, subgroup_zero
#for now we do nothing with comorbidity as outcome, but we do include hospitalization and all cause hospitalization
cohort_hd_reduced <- cohort_hd_reduced %>%
  select(id, two_week_period, year_period,  all_of(var_to_fill), facility_id, txt_per_week, days_from_fdd,
         first_hosp, cardiac_hosp, cardiac_hosp_day, infect_hosp_excl_covid, infect_hosp_incl_covid,
         infect_hosp_excl_covid_day, infect_hosp_incl_covid_day, cardiovasc_hosp, cardiovasc_hosp_day,  demo_esrd_cause_icd10text, demo_height,
         country, age_cat, demo_male, days_from_fdd, treatment_clone, cens_time, cens_reason, 
         death_reason_cardiovasc, death_reason_infect_incl_covid, death_reason_infect_excl_covid, cci, subgroup_later, cens_time, subgroup_zero, education)

#save in between
save(cohort_hd_reduced, file = paste0(path, "cohort_hd_reduced.Rdata"))
#load 
load(paste0(path, "cohort_hd_reduced.Rdata"))


#seperately save censor time hd to select labdata in script 1
cens_time_hd <- cohort_hd_reduced %>%
  group_by(id)%>%
  select(id, cens_time) %>%
  slice_head(n=1)

save(cens_time_hd, file = paste0(path, "cens_time_hd.Rdata"))
