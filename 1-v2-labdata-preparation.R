#1. labdata preparation for ccw
#goal of script is to get laboratory values of interest for the model/weighting for grace period and every year thereafter

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

#1.0 add laboratory to mortality events created in script 0----
#we want to try to apply 2 weights, one for the grace period, with 2 week periods up to 90 days
#the other weight is for informative censoring afterwards due to competing events.
#These weights will be every 3 months up until 40 months(max observed time)

#next, we want to select relevant laboratory data for IPCW within grace period for mortality analysis CCW
#including relevant laboratory data for each year after (between 90 days and a year)
lab_data <-lowercase_and_rename_id(lab_data) %>%
    #relocate to the left
    relocate(id, .before = days_from_fdd)

#first filter relevant ID's
recent_lab_data <- lab_data %>%
    filter(id %in% combined_mortality_event[["id"]])%>%
    group_by(id) %>%
    mutate(last_lab = max(days_from_fdd)) %>%
    ungroup()


#filter for grace period and define 2 week periods within
#because we will perform IPCW for drop out each 2 weeks within grace period
grace_lab <- recent_lab_data %>%
    filter(days_from_fdd <= 90) %>%
    mutate(
        two_week_period = case_when(
            days_from_fdd <= 14 ~ 1,
            days_from_fdd <= 28 ~ 2,
            days_from_fdd <= 42 ~ 3,
            days_from_fdd <= 56 ~ 4,
            days_from_fdd <= 70 ~ 5,
            days_from_fdd <= 84 ~ 6,
            days_from_fdd <= 90 ~ 7
        ))


#define the lab values we are interested in for IPCW and baseline
labs <- c( "lab_crp",  "lab_ferritin",
           "lab_phosph", "lab_prealbumin", "lab_serum_na", "lab_potassium",
           "lab_urine_volume", "lab_hgb", "lab_bicarb", "lab_creatinine", "lab_wktv",
           "lab_pth", "lab_calcium", "lab_hgba1c")

#per period, sort by closest to period end for each 2 weeks
#this way, we can later fill lab values forward from earlier rows if missing on closest row
period_grace_end <- c( 14,  28,  42, 56, 70, 84,  90)

#suggestion for this code idea from stack overflow:
#https://stackoverflow.com/questions/52221044/dplyrfirst-to-choose-first-non-na-value
grace_lab <- grace_lab %>%
    #look up to the end day, so if period = 1, period end day becomes 14
    #this is possible because vector period end is sorted so 1 is position 1 in the vector, in this case 14
    mutate(period_end_day = period_grace_end[two_week_period],
           #define distance to period end day
           dist_to_end = period_end_day - days_from_fdd
    ) %>%
    #group by period within the id and then sort the distance to period end
    group_by(id, two_week_period) %>%
    arrange(dist_to_end) %>%
    #fill NA's, take first non NA value from closest row to period end day
    #na.omit removes all NA values from that column within the group
    #first takes the first remaining value — which is from the closest-to-end row that actually has a value (as we have arranged by dist_end)
    mutate(across(all_of(labs), ~first(na.omit(.)))) %>%
    slice_head(n=1) %>%
    select(-period_end_day, -dist_to_end) %>%
    ungroup()

#create database with all two week periods
#for in between laboratory measurements we want a row filled with NA
grace_all <- expand.grid(
    id = unique(grace_lab[["id"]]),
    two_week_period = 1:7
) %>%
    arrange(id, two_week_period)

#combine with grace lab
grace_lab <- left_join(grace_all, grace_lab, by = c("id", "two_week_period"))%>%
    arrange(id, two_week_period)

#we do the same for the variables for after the grace period
# in which case, we want to weight each year.
year_lab <- recent_lab_data %>%
    filter(days_from_fdd >90) %>%
    mutate(
        year_period = case_when(
            days_from_fdd > 90 & days_from_fdd <= 360 ~ 1,
            days_from_fdd >360 & days_from_fdd <= 720 ~ 2,
            days_from_fdd > 720 & days_from_fdd <= 1080 ~ 3,
            days_from_fdd > 1080 & days_from_fdd <= 1440 ~ 4,
            days_from_fdd > 1440 & days_from_fdd <= 1800 ~ 5,
            days_from_fdd > 1800 & days_from_fdd <= 2160 ~ 6,
            days_from_fdd > 2160 & days_from_fdd <= 2520 ~ 7
        )
    )

period_year_end <- c(360, 720, 1080, 1440, 1800, 2160, 2520)

year_lab <- year_lab %>%
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
    mutate(across(all_of(labs), ~first(na.omit(.)))) %>%
    slice_head(n=1) %>%
    select(-period_end_day, -dist_to_end) %>%
    ungroup()

#create database with all two week periods
year_all <- expand.grid(
    id = unique(grace_lab[["id"]]),
    year_period = 1:7
) %>%
    arrange(id, year_period)

#combine with grace lab
year_lab <- left_join(year_all, year_lab, by = c("id", "year_period"))%>%
    arrange(id, year_period)

recent_lab_data <- bind_rows(grace_lab, year_lab) %>%
    arrange(id, two_week_period, year_period) %>%
    #set missing periods to 0 instead of NA so they dont get imputed later on
    mutate(
        two_week_period= if_else(is.na(two_week_period), 0, two_week_period),
        year_period = if_else(is.na(year_period), 0, year_period)
    )

recent_lab_data <- recent_lab_data %>%
    select(all_of(labs), id, two_week_period, year_period, last_lab)%>%
    relocate(id, .before = lab_prealbumin)

#!!!note that people now also have periods beyond their final or censored date. This will be fixed in the cloning script once we have determined their censoring time.!!!

#make seperate file for last lab
last_lab <- recent_lab_data %>%
    select(id, last_lab) %>%
    group_by(id) %>%
    slice_head(n=1)

#save in between
save(recent_lab_data, file = paste0(path, "recent_lab_data.Rdata"))
#load
load(paste0(path, "recent_lab_data.Rdata"))

save(last_lab, file = paste0(path, "last_lab.Rdata"))
#load
load(paste0(path, "last_lab.Rdata"))

#combine for now only the last lab measurement date, later we will fuse all lab measurements in the next script
combined_mortality_event <- left_join(combined_mortality_event, last_lab, by = "id")

#save in between
save(combined_mortality_event, file = paste0(path, "combined_mortality_event.Rdata"))