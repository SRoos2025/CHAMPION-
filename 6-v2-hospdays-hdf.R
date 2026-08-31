#6.0 add total hospitalization days hdf
#goal is to add total days of hospitalization BEFORE censoring takes place for HDF clone

##load packages----
pacman::p_load( "rio", #load data
                "purrr", #data untangling
                "magrittr", #efficient pipelines
                "dplyr", #untangle data
                "scales",
                "tidyr",
                "here" #to define path to extract and save files
) 

####define path to save any output to, and path import to import data
path <- "..."
path_import <- "..."

#load functions
walk(list.files(paste0(path, "funs_datacleaning/")), \(x)source(paste0(path, "funs_datacleaning/", x)))


#now the only outcome missing from cohort hdf reduced is total hosp days, as we need the cens time to determine this
#we load event select from script one, in which all events are still in there, several rows per person, for the right ID's
load(paste0(path, "event_select.Rdata"))
load(paste0(path, "cens_time_hdf.Rdata"))
load(paste0(path, "cohort_hdf_reduced.Rdata"))

event_select <- event_select %>%
  relocate(id, .before = days_from_fdd)

#add the censor time to event select
#filter for rows within censor time
event_select_hdf <- left_join(event_select, cens_time_hdf, by = "id") %>%
  filter(days_from_fdd <= cens_time) #select only rows before censor time


event_select_hdf <- event_select_hdf %>%
  group_by(id) %>%
  mutate(hosp_days = case_when(
    (days_from_fdd + hosp_days) >= cens_time ~ cens_time - days_from_fdd, #it can occur that a hospitalization starts before censor time bt continues until after so then we take the hospitalization until the censor time
   .default = hosp_days))%>%
  ungroup()

#then calculate total hosp days
event_select_hdf <- event_select_hdf %>%
  group_by(id) %>%
  mutate(
    total_hosp_days = sum(hosp_days, na.rm = TRUE) #calculate sum of days skipping NA rows
  ) %>%
  ungroup()

total_hosp_days_hdf <- event_select_hdf %>%
  group_by(id) %>%
  select(id, total_hosp_days) %>%
  slice_head(n =1)%>% #keep one row per patient
  ungroup()

cohort_hdf_reduced <- left_join(cohort_hdf_reduced, total_hosp_days_hdf, by = "id") #add this variable to the main dataset again

cohort_hdf_reduced <- cohort_hdf_reduced %>%
  group_by(id) %>%
  mutate(
    total_hosp_days = if_else(is.na(total_hosp_days), 0, total_hosp_days) #set missing to zero so it does not get imputed
  )




cohort_hdf_reduced <- cohort_hdf_reduced %>%
  group_by(id) %>%
  mutate(
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
      cens_time >= 1800 ~5
    )
  )%>%
  ungroup()

cohort_hdf_reduced <- cohort_hdf_reduced %>%
     group_by(id) %>%
     mutate(
    remove = if_else(two_week_period > max_two_week_period | year_period > max_year_period, 1, 0), #make identifier for extra rows beyond max two week period or year period that should be removed
    days_from_fdd = days_from_fdd.x #use day identifier from the dialysis data not the lab data
     ) %>%
  filter(remove == 0) %>%
  select(-remove)%>%
  ungroup()

cohort_hdf_reduced <- cohort_hdf_reduced %>%
  mutate(
    mod_mis = if_else(is.na(modality), 1, 0) #add identifier for missing modality (if modality is missing because dialysis session was missing, we do not want convection volume to be imputed)
  )

save(cohort_hdf_reduced, file = paste0(path, "cohort_hdf_reduced.Rdata"))

