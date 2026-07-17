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
#load recent lab data
load(paste0(path, "recent_lab_data.Rdata"))

#hdfclone rename
cohort_hdf <- combined_mortality_event
#rm for workingmemory
rm(combined_mortality_event)

# #try on example
# first_50_ids <- cohort_hdf %>%
#   distinct(id)%>%
#   slice_head(n = 500) %>%
#   pull(id)
#
# cohort_hdf <- cohort_hdf %>%
#   filter(id %in% first_50_ids)

#1 set censor time----
#define start time (we actually only know the quartile, but we must define the administrative end date)
#it is common in epidemiology to define time in the middle if it is unknown,
cohort_hdf <- cohort_hdf %>%
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
cohort_hdf <- cohort_hdf %>%
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

cohort_hdf <- cohort_hdf %>%
    arrange(id, days_from_fdd) %>%
    group_by(id) %>%
    mutate(
        #identifier for hdf clone
        treatment_clone = "hdf",
        #define last observed dialysis treatment
        last_treatment = max(days_from_fdd),
        #last observed date (can either be last treatment date or last event date or last lab measurement)
        last_observed_date = pmax(last_treatment, last_event_date, last_lab, last_comorb, na.rm = TRUE),
        #indicator for when hdf is started
        hdf_row = if_else(modality == 1, mut_number, NA),
        #indicator for when first hdf therapy
        first_hdf = min(hdf_row, na.rm = TRUE),
        #if everything is NA, min () returns infinite so we set this to NA. This solves the popping warning message.
        first_hdf = if_else(is.infinite(first_hdf), NA, first_hdf)
    ) %>%
    ungroup() %>%
    suppressWarnings() #we supress the warning for infinite as we solved this warning at line 92

#we first define censoring time
#at A we define at for those who never started HDF
#at B we define censor time for within grace period for those who did start HDF

#A censor time
#hdf started ? if not, then censor at 90 days
#define some other dates to determine a general censor date
cohort_hdf <- cohort_hdf %>%
    group_by(id)%>%
    mutate(
        #if rownumber corresponds to first row number with hdf treament, take that day
        first_hdf_day = if_else(mut_number == first_hdf, days_from_fdd, NA),
        #fill rows with that first day
        first_hdf_day = max(first_hdf_day, na.rm=TRUE),
        first_hdf_day = if_else(is.infinite(first_hdf_day), NA, first_hdf_day),
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
            #if the first hdf row is missing or first hdf day after 90 days, it ends at 90 days
            (is.na(first_hdf) | first_hdf_day >90) & general_cens_date >=90 ~ 90,
            #of course, if they have transplant date or other censor event before 90 days this should be first (so first censor reason)
            (is.na(first_hdf) | first_hdf_day >90) & general_cens_date < 90 ~ general_cens_date,
            .default = NA)
    ) %>%
    ungroup() %>%
    suppressWarnings()#we fixed the warnings already by turning infinite to NA for every min function

#save in between
save(cohort_hdf, file = paste0(path, "cohort_hdf.Rdata"))
#load
load(paste0(path, "cohort_hdf.Rdata"))

#B
#determine if there are no gaps in hdf treatments within grace period with function set_censor_time_hdf
#we use pick everything because we want to apply the function using all data grouped by id
#set_censor_time_hdf requires multiple columns input data from id such as last_date, days_from_fdd etc.
#`pick()` provides a way to select a subset of your columns using tidyselect. It returns a data frame.
#This is useful for functions that take data frames as inputs.
cohort_hdf <- cohort_hdf %>%
    group_by(id) %>%
    mutate(
        cens_time = set_censor_time_hdf(pick(everything()))
    ) %>%
    ungroup()

#returns 0, everyone now has a censor time
check <- cohort_hdf %>%
    filter(is.na(cens_time))

#2.0 set censor reason----
#at C we set it for those who never started HDF
#at D we define it for within the grace period, with non adherence
#at E we define it for those who did start HDF and who were not already non adherent in section D
#of course, if last observed date is before 90 they may still have transplantation BEFORE death and then we take that date
#this is solved in the set non-adherence function after

#C
cohort_hdf <- cohort_hdf %>%
    group_by(id) %>%
    mutate(
        cens_reason = case_when(
            #first, we define the reasons for patients who did not start hdf within grace period
            #if they never started hdf or after 90 days, but were censored after 90 days the reason is they never started hdf
            (is.na(first_hdf) | first_hdf_day > 90) & cens_time >= 90 ~ "never started hdf",
            #if they were censored before 90 days and the censor time corresponds to tranpslant date than that was the reason
            (is.na(first_hdf) | first_hdf_day > 90) & cens_time <90 & cens_time == trans_date ~ "transplantation",
            #same goes for withdrawal and kidney recovery, and than for death (but all these events occur before death if they would occur on the same day, therefore this order)
            (is.na(first_hdf) | first_hdf_day > 90) & cens_time <90 & cens_time == withdraw_date ~ "withdrawal",
            (is.na(first_hdf) | first_hdf_day > 90) & cens_time <90 & cens_time == recov_date ~ "kidney function recovery",
            (is.na(first_hdf) | first_hdf_day > 90) & cens_time <90 & cens_time == last_event_date &
                event_discharge_reason == "Left provider" | event_discharge_reason == "Transfer within provider" | event_discharge_reason == "NA"
            | event_discharge_reason == "" ~ "admin",
            #if they were censored within grace period and censor date is death then cause is death
            (is.na(first_hdf) | first_hdf_day > 90) & cens_time <90 & cens_time == death_date ~ "death",
            #if it was the last measurement without any of the above then we dont know
            #and it is administrative loss to follow-up unknown
            (is.na(first_hdf) | first_hdf_day > 90) & cens_time <90 & (cens_time == last_treatment |
                                                                           cens_time == last_lab
                                                                       | cens_time == last_comorb) ~ "admin",
            .default = NA)
    ) %>%
    ungroup()

#D
#define non adherence as a reason of censoring
#(so start HDF but not continue with at least 3 treatments every 2 weeks within grace period)
cohort_hdf <- cohort_hdf %>%
    group_by(id) %>%
    mutate(
        cens_reason = set_non_adherence_hdf(pick(everything()))
    )

#E
cohort_hdf <- cohort_hdf %>%
    group_by(id) %>%
    mutate(
        #now we define the censor reasons for people who did start hdf within grace period and who were all the time adherent
        cens_reason = case_when(
            !is.na(first_hdf) & first_hdf_day <= 90 &
                (cens_reason != "non-adherence" | is.na(cens_reason)) &
                cens_time == trans_date ~ "transplantation",
            !is.na(first_hdf) & first_hdf_day <= 90 &
                (cens_reason != "non-adherence" | is.na(cens_reason)) &
                cens_time == withdraw_date ~ "withdrawal",
            !is.na(first_hdf) & first_hdf_day <= 90 &
                (cens_reason != "non-adherence" | is.na(cens_reason)) &
                cens_time == recov_date ~ "kidney function recovery",
            !is.na(first_hdf) & first_hdf_day <= 90 &
                (cens_reason != "non-adherence" | is.na(cens_reason)) &
                cens_time == last_event_date &
                event_discharge_reason %in% c("Left provider", "Transfer within provider", "NA", "") ~ "admin",
            !is.na(first_hdf) & first_hdf_day <= 90 &
                (cens_reason != "non-adherence" | is.na(cens_reason)) &
                cens_time == death_date ~ "death",
            #if it was the last measurement without any of the above then we dont know
            #and it is administrative loss to follow-up unknown
            !is.na(first_hdf) & first_hdf_day <= 90 &
                (cens_reason != "non-adherence" | is.na(cens_reason)) &
                (cens_time == last_treatment | cens_time == last_lab | cens_time == last_comorb) ~ "admin",
            .default = cens_reason
        )
    ) %>%
    ungroup()



#if censor time is admin censor time, censor reason is now specifically study end
#furthermore, censor time can be after admin end which is max follow-up we fix that here
#this is for descriptive purposes
cohort_hdf <- cohort_hdf %>%
    group_by(id) %>%
    mutate(
        cens_time = if_else(cens_time >= admin_censor_time, admin_censor_time, cens_time),
        cens_reason = if_else(cens_time == admin_censor_time, "study end", cens_reason)
    ) %>%
    ungroup()

#returns 0, everyone has censor reason      
check <- cohort_hdf %>%
    filter(is.na(cens_reason))

#save in between
save(cohort_hdf, file = paste0(path, "cohort_hdf.Rdata"))
#load
load(paste0(path, "cohort_hdf.Rdata"))

summary(cohort_hdf$cens_time)#shows there are patients with cens_time of 0
#checking these patients, they have 1 row of data, at day 0. We change this to 1, as we actually have 1 day (1 observation)
#and model may not work with time until event of 0
cohort_hdf <- cohort_hdf %>%
    group_by(id) %>%
    mutate(
        cens_time = if_else(cens_time == 0, 1, cens_time)
    ) %>%
    ungroup()

#define causes of death for everyone irrespective of censor reason
cohort_hdf <- cohort_hdf %>%
    group_by(id) %>%
    mutate(
        death_reason_cardiovasc = case_when(
            max(event_died, na.rm = TRUE) == 1 &
                str_detect(cod_icd10text[event_died == 1], "^I2[0-5]|^I1[0-15]|^I3[0-9]|^I4[0-9]|^I5[0-2]|^I6[0-9]|^I7[0-9]|^G4[5-6]") ~ 1,
            .default = NA
        ),
        death_reason_cardiovasc = max(death_reason_cardiovasc, na.rm = TRUE),
        death_reason_cardiovasc = if_else(is.infinite(death_reason_cardiovasc), NA, death_reason_cardiovasc),
        death_reason_infect_incl_covid = case_when(
            str_detect(cod_icd10text[event_died == 1], "^A[0-9]|^B[0-9]|^U[07]|^J0[0-9]|^J1[0-9]|^J2[0-2]") ~ 1,
            .default = NA
        ),
        death_reason_infect_incl_covid = max(death_reason_infect_incl_covid, na.rm = TRUE),
        death_reason_infect_incl_covid = if_else(is.infinite(death_reason_infect_incl_covid), NA, death_reason_infect_incl_covid)
    ) %>%
    ungroup()

#save in between
save(cohort_hdf, file = paste0(path, "cohort_hdf.Rdata"))
#load
load(paste0(path, "cohort_hdf.Rdata"))

#3.0 descriptive analyses----

cohort_hdf <- cohort_hdf %>%
    group_by(id)%>%
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
        perc_hd_after = (total_hd_after/total_treat_after)* 100
    )%>%
    ungroup()

hdf_starters <- cohort_hdf %>%
    filter(!is.na(first_hdf)) #22,831 patients

hdf_adherent <- cohort_hdf %>%
    filter(!is.na(first_hdf) & first_hdf_day <= 90 & cens_reason != "non-adherence") #14,065 patients

#summarize by bringing down to 1 row per patient
summary <- cohort_hdf %>%
    group_by(id)%>%
    slice_head(n=1)

summary_adherent <- hdf_adherent %>%
    group_by(id) %>%
    slice_head(n=1)
#turn of scientific notation
options(sciphen = 999)
hist(summary$first_hdf_day)
hist(summary_adherent$first_hdf_day)

hist(summary$perc_hdf_after)
hist(summary_adherent$perc_hdf_after)
hist(summary$perc_hd_after)
hist(summary_adherent$perc_hd_after)

#4.0 bring down to observation row per 2 week period for grace period----
#for afterwards, we keep variables for each year
grace_cohort <- cohort_hdf %>%
    filter(days_from_fdd <= 90) %>%
    mutate(two_week_period = case_when(
        days_from_fdd <= 14 ~ 1,
        days_from_fdd <= 28 ~ 2,
        days_from_fdd <= 42 ~ 3,
        days_from_fdd <= 56 ~ 4,
        days_from_fdd <= 70 ~ 5,
        days_from_fdd <= 84 ~ 6,
        days_from_fdd <= 90 ~ 7
    ))

#define the variables we are interested in for IPCW and baseline
var_to_fill <- c("txt_dry_weight",
                 "txt_ktv_ocm", "txt_pre_sbp", "txt_pre_weight", "txt_post_weight", "txt_time_eff",
                 "txt_pre_dbp", "txt_substitution_volume", "txt_ufv", "txt_idwg_kg", "modality", "txt_per_week",
                 "catheter")


#per period, sort by closest to period end for each 2 weeks
#this way, we can later fill values forward from earlier rows if missing on closest row
period_end <- c( 14,  28,  42, 56, 70, 84,  90)

grace_cohort <- grace_cohort %>%
    #look up to the end day, so if period = 1, period end day becomes 14
    #this is possible because vector period end is sorted so 1 is position 1 in the vector, in this case 14
    mutate(period_end_day = period_end[two_week_period],
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

#save in between
save(grace_cohort, file = paste0(path, "grace_cohort.Rdata"))
#load
load(paste0(path, "grace_cohort.Rdata"))


year_cohort <- cohort_hdf %>%
    filter(days_from_fdd >90) %>%
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
    mutate(period_end_day = period_year_end[year_period],
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

#rename cohort_hdf so you also keep a database with all treatment mutations for descriptive purposes
#because in this chunk of code, we reduce to yearly and 2 week periods so not all treatment observations remain
cohort_hdf_reduced <- bind_rows(grace_cohort, year_cohort) %>%
    arrange(id, two_week_period, year_period)%>%
    group_by(id) %>%
    #set missing periods to 0 instead of NA so they dont get imputed later on
    mutate(
        two_week_period= if_else(is.na(two_week_period), 0, two_week_period),
        year_period = if_else(is.na(year_period), 0, year_period),
        #fill cens_time again
        cens_time = max(cens_time, na.rm = TRUE),
        max_year_period = case_when(
            cens_time <= 90   ~ 0,
            cens_time <= 360  ~ 1,
            cens_time <= 720  ~ 2,
            cens_time <= 1080 ~ 3,
            cens_time <= 1440 ~ 4,
            cens_time <= 1800 ~ 5,
            cens_time <= 2160 ~ 6,
            cens_time <= 2520 ~ 7),
        max_two_week_period = case_when(
            cens_time <= 14 ~ 1,
            cens_time <= 28 ~ 2,
            cens_time <= 42 ~ 3,
            cens_time <= 56 ~ 4,
            cens_time <= 70 ~ 5,
            cens_time <= 84 ~ 6,
            cens_time <= 90 ~ 7,
            .default = 7)
    ) %>%
    filter((year_period == 0 | year_period <= max_year_period) & (two_week_period == 0  | two_week_period <= max_two_week_period)) %>% #filter out empty rows of year periods after censor time
    #fill cens_reason again
    fill(cens_reason, facility_id, death_date, first_hosp, total_hosp_days, cardiac_hosp, cardiac_hosp_day, infect_hosp_excl_covid, infect_hosp_incl_covid,
         infect_hosp_excl_covid_day, infect_hosp_incl_covid_day, cardiovasc_hosp, cardiovasc_hosp_day, demo_esrd_cause_icd10text, demo_height,
         demo_male, subgroup_zero, cens_time, subgroup_later, demo_race, country, age_cat, cci, treatment_clone, last_observed_date, first_hdf_day, .direction= "downup")%>%
    ungroup()




# keep relevant column names
#include specifiers for subgroup analyses (as defined in script 0: subgroup_later, subgroup_zero
#for now we do nothing with comorbidity as outcome, but we do include hospitalization and all cause hospitalization
cohort_hdf_reduced <- cohort_hdf_reduced %>%
    select(id, two_week_period, year_period,  all_of(var_to_fill), facility_id, txt_per_week, days_from_fdd,
           first_hosp, total_hosp_days, cardiac_hosp, cardiac_hosp_day, infect_hosp_excl_covid, infect_hosp_incl_covid,
           infect_hosp_excl_covid_day, infect_hosp_incl_covid_day, cardiovasc_hosp, cardiovasc_hosp_day,  demo_esrd_cause_icd10text, demo_height,
           country, age_cat, demo_male, days_from_fdd, treatment_clone, cens_time, cens_reason,
           death_reason_cardiovasc, death_reason_infect_incl_covid, cci, subgroup_later, cens_time, subgroup_zero)

#save in between
save(cohort_hdf_reduced, file = paste0(path, "cohort_hdf_reduced.Rdata"))
#load
load(paste0(path, "cohort_hdf_reduced.Rdata"))

#make seperate database for cens_time
cens_time <- cohort_hdf_reduced %>%
    group_by(id) %>%
    select(cens_time, id) %>%
    slice_head(n =1) %>%
    ungroup()

#if you want to filter the same 500 patients from recent lab data
# recent_lab_data <- recent_lab_data %>%
#   filter(id %in% first_50_ids)

#add this to lab data
recent_lab_data <- left_join(recent_lab_data, cens_time, by = "id")
#we also filter out empty rows of year periods after censor time for the laboratory variables
recent_lab_data <- recent_lab_data %>%
    group_by(id) %>%
    mutate(
        max_year_period = case_when(
            cens_time <= 90   ~ 0,
            cens_time <= 360  ~ 1,
            cens_time <= 720  ~ 2,
            cens_time <= 1080 ~ 3,
            cens_time <= 1440 ~ 4,
            cens_time <= 1800 ~ 5,
            cens_time <= 2160 ~ 6,
            cens_time <= 2520 ~ 7),
        max_two_week_period = case_when(
            cens_time <= 14 ~ 1,
            cens_time <= 28 ~ 2,
            cens_time <= 42 ~ 3,
            cens_time <= 56 ~ 4,
            cens_time <= 70 ~ 5,
            cens_time <= 84 ~ 6,
            cens_time <= 90 ~ 7,
            .default = 7)
    ) %>%
    #year period is all 0 in the two week period, and viceverse
    filter((year_period == 0 | year_period <= max_year_period) & (two_week_period == 0  | two_week_period <= max_two_week_period)) %>%
    select(-cens_time)%>%
    ungroup()


#combine with lab data filtered per 2 week period
#left join will match on id and on two week period or year period if possible,
#if not it will add the row and fill the missing two week period of that missing datafrmae with NA
cohort_hdf_reduced <- left_join(cohort_hdf_reduced, recent_lab_data, by = c("id", "two_week_period", "year_period"))

#save in between
save(cohort_hdf_reduced, file = paste0(path, "cohort_hdf_reduced.Rdata"))
#load
load(paste0(path, "cohort_hdf_reduced.Rdata"))

#5.0 describe trajectory of hdf clones----

#it says first but they are of course all the same but it is a trick to go to 1 row per person
table_censor <- cohort_hdf_reduced %>%
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