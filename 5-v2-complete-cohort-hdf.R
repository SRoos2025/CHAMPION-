#5-v2-complete-cohort-hdf
#goal is to combine dialysis data from script 3 for hdf clone with lab data as created in script 4 for hdf clone

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

#combine with lab data filtered per 2 week period
#left join will match on id and on two week period or year period if possible,
#if not it will add the row and fill the missing two week period of that missing datafrmae with NA
cohort_hdf_reduced <- left_join(cohort_hdf_reduced, recent_lab_data_hdf, by = c("id", "two_week_period", "year_period"))

#save again
save(cohort_hdf_reduced, file = paste0(path, "cohort_hdf_reduced.Rdata"))
#load 
load(paste0(path, "cohort_hdf_reduced.Rdata"))
