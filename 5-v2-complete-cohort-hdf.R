#5-v2-complete-cohort-hdf
#goal is to combine dialysis data from script 3 for hdf clone with lab data as created in script 4 for hdf clone
#last updated 07-09-2026

#0. set up----
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

#load results from script 3 and 4
load(paste0(path, "recent_lab_data_hdf.Rdata"))
load(paste0(path, "cohort_hdf_reduced.Rdata"))

#for visual inspection
cohort_hdf_reduced <- cohort_hdf_reduced %>%
  relocate(days_from_fdd, visit, .before = two_week_period) %>%
  select(-c(two_week_period, year_period))

#remove days from fdd as we have lab_day just to check which day from labdata, but days from fdd from dialysis data is leading
#so it does not matter lab_day is missing for data rows from dialysis data 
#from now on, only visits matter, so two week period and year period may also be removed
recent_lab_data_hdf <- recent_lab_data_hdf %>%
  mutate(lab_day = days_from_fdd) %>%
  relocate(lab_day, .before = lab_albumin) %>%
  select(-c(days_from_fdd, two_week_period, year_period))

#combine with lab data filtered per 2 week period
#left join will match on id and on visit if possible
#if not it will add the row and fill the missing visit with NA
cohort_hdf_reduced <- left_join(cohort_hdf_reduced, recent_lab_data_hdf, by = c("id", "visit"))

#to visually check if everything went well simplify database
check <- cohort_hdf_reduced %>%
  select(id, visit, days_from_fdd, lab_day, lab_alk_phosph, txt_dry_weight)

#save again
save(cohort_hdf_reduced, file = paste0(path, "cohort_hdf_reduced.Rdata"))
#load 
load(paste0(path, "cohort_hdf_reduced.Rdata"))
