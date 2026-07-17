#cohort derivation apollo 2 for target trial emulation
#and covariates
#code by S. Roos
#last updated on 16-07-2026

#0. set up----
##load packages----
pacman::p_load( "rio", #load data
                "purrr", #data untangling
                "magrittr", #efficient pipelines
                "dplyr", #untangle data
                "tidyr", #untangle data
                "here", #to define path to extract and save files
                "stringr", # detect text patterns
                "writexl") #to write to excel

####define path to save any output to, and path import to import data
path <- "..."
path_import <- "..."

#demograph <- import(paste0(path_import,"demographics_db.csv"))
load(paste0(path, "demograph.Rdata"))#demograph but then with everything lower case and GFME_ID = id

clin_data_incenter <- import(paste0(path_import, "txt_ichd.csv"))

event <- import(paste0(path_import, "event_db.csv"))

#if needed this loads home dialysis data
#clin_data_home <- import(paste0(path_import, "txt_hhd.csv"))

#load functions
walk(list.files(paste0(path, "funs_datacleaning/")), \(x)source(paste0(path, "funs_datacleaning/", x)))

#1.0 filter selection for cohort derivation
#1.0 filter----
#clean demograph, set everything to lower case and rename gfme_id to id
demograph <- lowercase_and_rename_id(demograph)
save(demograph, file = paste0(path, "demograph.Rdata"))
#patient ID's that initiated dialysis from 2018 an onward
#demograph contains 108,811 patients, after selecting after 2018, 61,560 remain
recent_demograph <- demograph %>%
    mutate(
        demo_fdd_year = as.numeric(demo_fdd_year)
    ) %>%
    filter(demo_fdd_year >= 2018)

#remove third and fourth quarter of 2024, as they may  have too little follow-up
recent_demograph <- recent_demograph %>%
    mutate(
        remove = if_else(demo_fdd_year == 2024 & demo_fdd_quarter == 2, 1, 0)
    ) %>%
    filter(remove != 1) %>%#59,630
    select(-remove)

#remove demograph to clean working memory
rm(demograph)

#set lowercase and rename id in clin_data_incenter (which contains treatment-related data)
clin_data_incenter <- lowercase_and_rename_id(clin_data_incenter)
recent_data <- clin_data_incenter %>%
    filter(id %in% recent_demograph[["id"]]) #58,607 start with in center treatment

#remove to clean working memory
rm(clin_data_incenter)

#add country to recent data to check if there are countries that only offer one modality in the data.
country_year <- recent_demograph %>%
    select(demo_country, id, demo_fdd_year)

recent_data <- left_join(recent_data, country_year, by = "id")

#check if both treatment options are given in each year
recent_data <- recent_data %>%
    group_by(demo_country, demo_fdd_year) %>%
    mutate(
        hdf_check = if_else(any(txt_modality == "HDF"), 1, 0),
        hd_check = if_else(any(txt_modality == "HD"), 1, 0),
        both = if_else(hdf_check ==1 & hd_check == 1, 1, 0),
        #calculate proportion hdf per country for descriptive purposes
        #total treatments
        total_treat = n(),
        total_hdf = sum(txt_modality == "HDF", na.rm = TRUE),
        perc_hdf = (total_hdf) / total_treat * 100
    ) %>%
    ungroup()

#return excel with proportion HDF treatments per year per country
#this also shows that for Estiona, follow up ends at end of 2022 which is good to know for defining administrative censor time later on
cross <- recent_data %>%
    select(demo_country, demo_fdd_year, perc_hdf) %>%
    distinct() %>%
    pivot_wider(
        names_from  = demo_fdd_year,
        values_from = perc_hdf
    ) %>%
    rename(Country = demo_country)
#write to excel
write_xlsx(cross, file.path(path, "hdf_kruistabel.xlsx"))
#checking this table shows we must remove 2024 from Sweden, the Netherlands, and Czech Republic. In addition, we must remove 2023 and 2024 from Hungary.

#check_hdf <- recent_data %>%
#filter(both == 0)
#unique(check_hdf$demo_country) #Netherlands does not offer HDF in this data
#SWeden does not offer HD in 2024, only HDF so we exclude starting date of 2024 in Sweden

#remove the Netherlands
recent_data <- recent_data %>%
    filter(demo_country != "Netherlands")
n_distinct(recent_data$id)  #58,538 patients left. 69 removed

recent_data <- recent_data %>%
    mutate(
        remove = if_else(demo_country == "Sweden" & (demo_fdd_year == 2024 | demo_fdd_year == 2023 | demo_fdd_year == 2022), 1, 0)
    ) %>%
    filter(remove !=1) %>%
    select(-remove)
n_distinct(recent_data$id)  #58,532 #6 removed

recent_data <- recent_data %>%
    mutate(
        remove = if_else(demo_country == "Serbia" & demo_fdd_year == 2024, 1, 0)
    ) %>%
    filter(remove !=1) %>%
    select(-remove)
n_distinct(recent_data$id)  #58,516 #16 removed

recent_data <- recent_data %>%
    mutate(
        remove = if_else(demo_country == "Hungary" & (demo_fdd_year == 2024 | demo_fdd_year == 2023), 1, 0)
    ) %>%
    filter(remove !=1) %>%
    select(-remove)
n_distinct(recent_data$id) #58,353 #163 removed

recent_data <- recent_data %>%
    mutate(
        remove = if_else(demo_country == "Estonia" & (demo_fdd_year == 2024 | demo_fdd_year == 2023), 1, 0)
    ) %>%
    filter(remove !=1) %>%
    select(-remove)
n_distinct(recent_data$id) #58,353 #0 removed

recent_data <- recent_data %>%
    filter(demo_country != "Czech Republic")
n_distinct(recent_data$id)  #55,309 patients left. 3,207 excluded

#next, we only want to include patients of which we have information close to baseline.
#for now, we want patients with data on dialysis treatment within 14 days
#so first, we want to number mutations
recent_data <- recent_data %>%
    group_by(id) %>%
    #make sure days from first dialysis are arranged correctly within the group(ID)
    arrange(days_from_fdd, .by_group = TRUE) %>%
    mutate(
        mut_number = row_number()
    ) %>%
    ungroup()

#filter if there is any data within 14 days after initiation
#we can do a subgroup analyses for those we have at day 0 and those within 14 days
#also exclude those with frequency of 2x/week at baselinee
recent_data <- recent_data %>%
    group_by(id) %>%
    mutate(
        #total group may have first data within 14 days
        total = if_else(mut_number == 1 & days_from_fdd <= 14, 1, NA),
        #subgroup that starts later than day 0
        subgroup_later = if_else(mut_number == 1 & days_from_fdd <= 14 & days_from_fdd != 0, 1, 0),
        #group that has data exactly at day 0
        subgroup_zero = if_else(mut_number == 1 & days_from_fdd == 0, 1, 0),
        #filter for starting with 3x per week
        keep_three = if_else(mut_number == 1 & txt_per_week == 3, 1, NA)
    ) %>%
    #make sure keep is 1 at each row if it is 1 at the first row
    fill(total, keep_three, subgroup_zero, subgroup_later, .direction = "downup")%>%
    ungroup()

recent_data <- recent_data %>%
    filter(total == 1)
n_distinct(recent_data$id) #34,576 patients #20,733 removed

recent_data <- recent_data %>%
    filter(keep_three == 1)
n_distinct(recent_data$id) #32,653 #1,923 removed

#just to check how many patients, we keep labels subgroup later and subgroup zero to filter these groups later in analysis
#15038 patients start at 0
#
# subgroup_later <- recent_data %>%
#   filter(subgroup_later == 1)
# n_distinct(subgroup_later$id) #17,615 patients


#save inbetween
save(recent_data, file = paste0(path, "recent_data.Rdata"))
load(paste0(path, "recent_data.Rdata"))

#2.0 datacleaning----
recent_data <- recent_data %>%
    mutate(
        #set modality to 1 for HDF and 0 for HD
        modality = if_else(txt_modality== "HDF" | txt_modality == "Mixed HDF/HD Treatment", 1, 0),
        #set catheter to 1 and fistula to 0
        #// indicates double entry for a treatment. The order is random. We choose for catheter only if there were only catheter treatments
        #this means that for the fistula (catheter = 0) there are double entries with catheter. However if a fistula is noted somewhere we take that.
        #note that we will use this as a time varying covariate so the mix-ups will likely have little impact.
        #we also create vascular access which also shows grafts for baseline descriptive purpose
        #for models wwe just use catheter or no catheter
        catheter = if_else(txt_access_type %in% c("permanent catheter (vascular)",
                                                  "temporary catheter (vascular)",
                                                  "Subcutaneous access port",
                                                  "Subcutaneous access port//temporary catheter (vascular)",
                                                  "permanent catheter (vascular)//temporary catheter (vascular)",
                                                  "temporary catheter (vascular)//permanent catheter (vascular)"), 1, 0),
        #note that the // means 2 vascular access types were reported, if fistula was reported we assume fistula is present
        vasc_access = case_when(
            txt_access_type %in% c("fistula",
                                   "shunt",
                                   "temporary catheter (vascular)//fistula",
                                   "fistula//permanent catheter (vascular)",
                                   "Loop AV Fistula",
                                   "permanent catheter (vascular)//fistula"
            ) ~ "AV fistula",
            txt_access_type %in% c("Graft") ~ "AV graft",
            txt_access_type %in% c("permanent catheter (vascular)",
                                   "temporary catheter (vascular)",
                                   "Subcutaneous access port") ~ "Catheter"
        )
    )

#save inbetween
save(recent_data, file = paste0(path, "recent_data.Rdata"))
load(paste0(path, "recent_data.Rdata"))

#3.0 combine events
#make all variables lower case
event <- lowercase_and_rename_id(event)

event_select <- event %>%
    filter(id %in% recent_data[["id"]]) #29,507 patients, not everyone has an event

#first we want to go to 1 row per patient for the final event
event_select <- event_select%>%
    group_by(id) %>%
    mutate(
        last_event_date = max(days_from_fdd),
        death_date = case_when(
            event_died == 1 ~ days_from_fdd,
            .default = NA),
        #if first hospitalization is at 0 days, we set it to NA because time until event analysis does not work well with 0
        event_hosp = if_else(days_from_fdd[event_hosp== 1] == 0, NA, event_hosp),
        first_hosp = min(days_from_fdd[event_hosp ==1], na.rm = TRUE),
        first_hosp = if_else(is.infinite(first_hosp), NA, first_hosp),
        #define first reason, so the reason (coh_icd10text) at the row where there is a hospitalization and this matches to the first hospitalization
        first_hosp_reason = first(coh_icd10text[event_hosp == 1 & !is.na(event_hosp) & days_from_fdd == first_hosp]),
        total_hosp_days = sum(hosp_days, na.rm = TRUE)
    ) %>%
    ungroup()

#first remove all coh_icd10 text dots because reggex does not work well with "." in icd codes
event_select <- event_select %>%
    mutate(coh_icd10text = str_replace_all(coh_icd10text, "\\.", ""))

#define hosp reason for cause specific hospitalization
event_select <- event_select %>%
    group_by(id) %>%
    mutate(
        cardiac_hosp = case_when(
            str_detect(coh_icd10text, "^I2[0-5]|^I3[0-9]|^I4[0-9]|^I5[0-2]") ~ 1,
            .default = 0),
        cardiac_hosp_day = if_else(cardiac_hosp == 1 , days_from_fdd, NA),
        #take the first day
        cardiac_hosp_day = min(cardiac_hosp_day, na.rm=TRUE),
        #if it is missing the code above returns infinite, we turn that back to NA
        cardiac_hosp_day = if_else(is.infinite(cardiac_hosp_day), NA, cardiac_hosp_day),
        #this fills cardiac hosp, if it is 1 somewhere it will now be filled for every row
        cardiac_hosp = max(cardiac_hosp, na.rm=TRUE),
        cardiac_hosp = if_else(is.infinite(cardiac_hosp), 0, cardiac_hosp),
        infect_hosp_excl_covid = case_when(
            str_detect(coh_icd10text, "^J0[0-6]|^J09|^J1[0-8]|^J2[0-2]|^A[0-9]|^B[0-9]") ~ 1,
            .default = 0),
        infect_hosp_excl_covid_day = if_else(infect_hosp_excl_covid == 1 , days_from_fdd, NA),
        infect_hosp_excl_covid_day = min(infect_hosp_excl_covid_day, na.rm=TRUE),
        infect_hosp_excl_covid_day = if_else(is.infinite(infect_hosp_excl_covid_day), NA, infect_hosp_excl_covid_day),
        infect_hosp_excl_covid = max(infect_hosp_excl_covid, na.rm=TRUE),
        infect_hosp_excl_covid = if_else(is.infinite(infect_hosp_excl_covid), 0, infect_hosp_excl_covid),
        #infection related comorbidity including covid
        infect_hosp_incl_covid = case_when(
            str_detect(coh_icd10text, "^J0[0-6]|^J09|^J1[0-8]|^J2[0-2]|^A[0-9]|^B[0-9]|^U071|^U072") ~ 1,
            .default = 0),
        infect_hosp_incl_covid_day = if_else(infect_hosp_incl_covid == 1 , days_from_fdd, NA),
        infect_hosp_incl_covid_day = min(infect_hosp_incl_covid_day, na.rm=TRUE),
        infect_hosp_incl_covid_day = if_else(is.infinite(infect_hosp_incl_covid_day), NA, infect_hosp_incl_covid_day),
        infect_hosp_incl_covid = max(infect_hosp_incl_covid, na.rm=TRUE),
        infect_hosp_incl_covid = if_else(is.infinite(infect_hosp_incl_covid), 0, infect_hosp_incl_covid),
        #non cardiac cardiovascular comorbidity
        cardiovasc_hosp = case_when(
            str_detect(coh_icd10text, "^I708|^I702|^I7[1-4]") ~ 1,
            str_detect(coh_icd10text, "^I(60|61|62|63|64|69)") ~ 1,
            str_detect(coh_icd10text, "^G(45|46)") ~ 1,
            coh_icd10text %in% c("I670", "I678", "I679") ~ 1,
            .default = 0),
        cardiovasc_hosp_day = if_else(cardiovasc_hosp == 1 , days_from_fdd, NA),
        cardiovasc_hosp_day = min(cardiovasc_hosp_day, na.rm=TRUE),
        cardiovasc_hosp_day = if_else(is.infinite(cardiovasc_hosp_day), NA, cardiovasc_hosp_day),
        cardiovasc_hosp = max(cardiovasc_hosp, na.rm=TRUE),
        cardiovasc_hosp = if_else(is.infinite(cardiovasc_hosp), 0, cardiovasc_hosp),
    ) %>%
    ungroup()


#we take only the last event for the mortality analysis
#there can be 2 last event dates, in which cases there is a certain hierarchy to which event we want to keep for this analysis
#discharge reasons are events that will occur before dying/competing events.
events_mortality_analysis <- event_select %>%
    group_by(id) %>%
    #filter for the last event
    filter(days_from_fdd == last_event_date) %>%
    #rename day to time to last event as that is what we will keep
    mutate(last_event_date = days_from_fdd) %>%
    mutate(priority = case_when(
        event_discharge_reason == "Transplant-unknown donor" ~ 1,
        event_discharge_reason == "Kidney function recovered" ~ 2,
        event_discharge_reason == "Withdrawal from dialysis" ~ 3,
        event_discharge_reason == "Modality change" ~ 4,
        event_hosp == 1 ~ 5,
        event_discharge_reason == "Left provider" ~ 6,
        event_discharge_reason == "Transfer within provider" ~ 7,
        event_died == 1 ~ 8,
        event_discharge_reason == "Died" ~ 9, #checked this, in these cases event_died is always 1 as well so actually redundant
        event_discharge_reason == "" ~ 10,
        !is.na(access_removal_reason) ~ 11) #we are not interested in this event and it will not lead to loss to follow-up
    ) %>%
    #take the first priority
    filter(priority == min(priority)) %>%
    slice_head(n = 1) %>% #checked this, there were no duplicates other then event_access_removal so slice_head is safe.
    #we remove days from fdd as this is now last_event_date, and otherwise the left join will find matches on daysfromfdd but we only want to match on ID
    select(-priority, -days_from_fdd) %>%
    ungroup()

combined_mortality_event <- left_join(recent_data, events_mortality_analysis, by = "id")

#save in between
save(combined_mortality_event, file = paste0(path, "combined_mortality_event.Rdata"))
load(paste0(path, "combined_mortality_event.Rdata"))

#3.0 add demographics ----
#next, we want to combine with demographic data for baseline/time fixed confounding
load(paste0(path, "demograph.Rdata"))
demograph_select <- demograph %>%
    filter(id %in% combined_mortality_event[["id"]])

#left join to combined_mortality_event
combined_mortality_event <- left_join(combined_mortality_event, demograph_select, by = "id") %>%
    #apparently, country already existed
    mutate(
        country = demo_country.x
    )%>%
    select(-demo_country.y, -demo_country.x)

#make age category to a factor
combined_mortality_event <- combined_mortality_event %>%
    mutate(
        age_cat = as.factor(demo_age_fdd)
    )%>%
    select(-demo_age_fdd)



#save in between
save(combined_mortality_event, file = paste0(path, "combined_mortality_event.Rdata"))