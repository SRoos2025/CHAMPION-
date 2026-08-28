#1. labdata preparation for ccw
#goal of script is to select last lab measurement to add this to the created dataset in script 0, called combined mortality event
#last edited on 28-08-2026 by Sanne Roos

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

#load dataset with treatments and events created in script 0
load(paste0(path, "combined_mortality_event.Rdata"))
#load laboratory data
lab_data <- import(paste0(path_import, "lab_db.csv"))

#first extract last lab measurement, which is needed to continue with defining censoring time in script 3-v2-hdf clone
lab_data <-lowercase_and_rename_id(lab_data) %>%
  #relocate to the left
  relocate(id, .before = days_from_fdd)

#first filter relevant ID's
recent_lab_data <- lab_data %>%
  filter(id %in% combined_mortality_event[["id"]])%>%
  group_by(id) %>%
  mutate(last_lab = max(days_from_fdd)) %>%
  ungroup()

#make seperate file for last lab
last_lab <- recent_lab_data %>%
  select(id, last_lab) %>%
  group_by(id) %>%
  slice_head(n=1)

save(last_lab, file = paste0(path, "last_lab.Rdata"))
#load 
load(paste0(path, "last_lab.Rdata"))

#combine for now only the last lab measurement date, later we will fuse all lab measurements in the next script
combined_mortality_event <- left_join(combined_mortality_event, last_lab, by = "id")

#save in between
save(combined_mortality_event, file = paste0(path, "combined_mortality_event.Rdata"))

save(recent_lab_data, file = paste0(path, "recent_lab_data.Rdata"))
