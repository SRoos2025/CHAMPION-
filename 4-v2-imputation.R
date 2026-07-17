#0. set up----
##load packages----
pacman::p_load( "rio", #load data
                "purrr", #data untangling
                "magrittr", #untangle data
                "dplyr", #untangle data
                "tidyr", #untangle data
                "mice", #for imputation
                "miceadds", #for imputation
                "summarytools", #to inspect variable summary of total dataset
                "fastDummies")#to create dummy variables from categorical ones

##set working directory----
setwd("Z:/data_apollo_v2/")

##define path to save any output to, and path import to import data
path <- "..."
path_import <- "..."

#load data with events and measurements of sessions including modality changes and their last event date relevant for mortality outcome
load(paste0(path, "cohort_hdf_reduced.Rdata"))

impute_hdf <- cohort_hdf_reduced %>%
    #set death reasons to 0 if not applicable
    mutate(
        death_reason_cardiovasc = if_else(is.na(death_reason_cardiovasc), 0, death_reason_cardiovasc),
        death_reason_infect_incl_covid = if_else(is.na(death_reason_infect_incl_covid), 0, death_reason_infect_incl_covid),
        cardiovasc_hosp = if_else(is.na(cardiovasc_hosp), 0, cardiovasc_hosp),
        cardiovasc_hosp_day = if_else(is.na(cardiovasc_hosp_day), 0, cardiovasc_hosp_day),
        infect_hosp_incl_covid = if_else(is.na(infect_hosp_incl_covid), 0, infect_hosp_incl_covid),
        infect_hosp_excl_covid = if_else(is.na(infect_hosp_excl_covid), 0, infect_hosp_excl_covid),
        cardiac_hosp = if_else(is.na(cardiac_hosp), 0, cardiac_hosp),
        cardiac_hosp_day = if_else(is.na(cardiac_hosp_day), 0, cardiac_hosp_day),
        infect_hosp_incl_covid_day = if_else(is.na(infect_hosp_incl_covid_day), 0, infect_incl_covid_day),
        infect_hosp_excl_covid_day = if_else(is.na(infect_hosp_excl_covid_day), 0, infect_excl_covid_day),
        total_hosp_days = if_else(is.na(total_hosp_days), 0, total_hosp_days),
        first_hosp = if_else(is.na(first_hosp), 0, first_hosp),
        #set substitution volume to 0 if modality is 0 (HD)
        txt_substitution_volume = if_else(modality == 0, 0, txt_substitution_volume),
        #scale substition volume to improve numerical stability (from ml to L)
        txt_substitution_volume = txt_substitution_volume / 1000
    )

#make dummy variables from catogorical catheter and age category
impute_hdf <- dummy_cols(impute_hdf, select_columns = c("catheter", "age_cat"))

#rename agecategories as they are currently invalid colnames with tokens like >= 75 which do not work well within colname
impute_hdf <- impute_hdf %>%
    mutate(
        age_cat_seventyfive = `age_cat_>=75` ,
        age_cat_sixtyfive= `age_cat_65-74` ,
        age_cat_fourtyfive  = `age_cat_45-64`,
        age_cat_eighteen = `age_cat_18-44`,
    )

#2.0 define iputation variables
#select relevant variables for imputation so no collinearity will occur
#cens time is zero missing
impute_hdf <- impute_hdf %>%
    select(
        #identifier id
        id,
        #time identifier in days
        days_from_fdd,
        #cluster variables
        facility_id,
        country, #character variable!
        #clinical predictive values
        #systolic blood pressure in mmHg
        txt_pre_sbp, 
        #diastolic blood pressure in mmHg
        txt_pre_dbp,
        #ultrafiltration volume in mL
        txt_ufv, 
        #weekly Kt/V (no unit, dimension less)
        lab_wktv,
        #treatment modality, 1 (HDF) or 0 (HD)
        modality, 
        #treatment frequency / weeks 
        txt_per_week,
        #treatment time in minutes 
        txt_time_eff, 
        #vascular access, 1 = cathether, 0 = shunt
        catheter_1,
        #subsitution volume in mL 
        txt_substitution_volume, 
        #weight in kg
        txt_post_weight,
        #charlson comorbidity
        cci,
        #laboratory values
        #c reactive protein (in mg/L)
        lab_crp, 
        #(ng/mL)
        lab_ferritin,
        #serum phosphate (mg/dL)
        lab_phosph, 
        #serum calcium (mg/dL)
        lab_calcium,
        #serum sodium (mmol/L)
        lab_serum_na, 
        #serum potassium (mmol/L)
        lab_potassium,
        #serum hemoglobin (g/dL)
        lab_hgb, 
        #serum bicarbonate (g/dL)
        lab_bicarb,
        #serum creatinin (mg/dL)
        lab_creatinine, 
        #parathyroid hormone (ng/L)
        lab_pth,
        #glycated hemoglobin (% of total hemoglobin)
        lab_hgba1c, 
        #albumin before dialysis (g/dL)
        lab_prealbumin,
        #residual diuresis (mL) 
        lab_urine_volume,
        #demographic variables
        age_cat_seventyfive, age_cat_sixtyfive, age_cat_fourtyfive, age_cat_eighteen,  
        demo_male, 
        #in cm
        demo_height,
        #outcomes regarding death
        cens_reason, #character variable!
        cens_time,
        #subgroupindicators
        subgroup_later, subgroup_zero,
        #other outcomes
        death_reason_cardiovasc, death_reason_infect_incl_covid, cardiovasc_hosp, cardiovasc_hosp_day,
        infect_hosp_incl_covid, cardiac_hosp, infect_hosp_incl_covid_day, infect_hosp_excl_covid,
        cardiac_hosp_day, total_hosp_days, first_hosp, infect_hosp_excl_covid_day
    )

#modality should be integer in stead of numeric
impute_hdf <- impute_hdf %>%
    mutate(modality = as.integer(modality))

#inspect missingness of some infrequent variables
urine_volume <- impute_hdf %>%
    group_by(id) %>%
    summarise(has_nonmissing = any(!is.na(lab_urine_volume))) %>%
    summarise(n_patients = sum(has_nonmissing))#2604 have any value

hbac <- impute_hdf %>%
    group_by(id) %>%
    summarise(has_nonmissing = any(!is.na(lab_hgba1c))) %>%
    summarise(n_patients = sum(has_nonmissing))#6528 have any value

#set up predition matrix
mat_prd <- mice(impute_hdf, maxit =0)[["predictorMatrix"]] #4 logged Events

mice <- mice(impute_hdf, maxit = 0)
#mice$loggedEvents show event for constant censor reason, country, and 2 subgroups
#but we do not impute this so this should not give problems

#define variables that do not have to be imputed
#they do have to be used as preditors (ie their row should have 1s for preditors)
vec_nimp <- c( #auxilliary variables
    #identifier id
    "id",
    #time variable
    "days_from_fdd",
    #cluster variables
    "facility_id",
    #demographic variables
    "age_cat_seventyfive", "age_cat_sixtyfive", "age_cat_fourtyfive", "age_cat_eighteen",
    "demo_male", "demo_height",
    #outcomes
    "cens_time",
    "death_reason_cardiovasc", "death_reason_infect_incl_covid", "cardiovasc_hosp",
    "infect_hosp_incl_covid", "cardiac_hosp", "infect_hosp_incl_covid_day", "infect_hosp_excl_covid",
    "cardiac_hosp_day", "total_hosp_days", "first_hosp", "infect_hosp_excl_covid_day",
    #clinical values
    "cci",
    #subgroup indicators
    "subgroup_zero", "subgroup_later"
)


#define text variables
text_vars <- c("cens_reason", "country")

#define variables for longitudinal imputation
#idea for next run,
vec_limp <- c(#laboratory values
    "lab_crp", "lab_ferritin",
    "lab_phosph", "lab_calcium",  
    "lab_serum_na", "lab_potassium",
    "lab_hgb", "lab_bicarb",
    "lab_creatinine", "lab_pth",
    "lab_prealbumin", "lab_hgba1c",
    "lab_urine_volume",
    #clinical values
    "txt_pre_sbp", "lab_wktv",
    "txt_time_eff", "txt_post_weight",
    "catheter_1", "txt_ufv",
    "txt_substitution_volume","modality",
    "txt_per_week","txt_pre_dbp")

#adjust predictor matrix, set rows to 0
mat_prd[vec_nimp, ] <- 0

#set cluster variable for the variables we want to impute
mat_prd[vec_limp, "id"] <- -2

# Get methods vector
vec_mtd <- mice(impute_hdf, maxit = 0)[["method"]]

# Change variables to be longitudinally imputed to longitudinal imputation
vec_mtd <- if_else(names(vec_mtd) %in% vec_limp, "2l.pmm", vec_mtd)

vec_mtd[names(vec_mtd) %in% text_vars] <- ""

# Reset names of methods vector
names(vec_mtd) <- colnames(mat_prd)

# Start imputation, 10 datasets(m), 20 iterations (maxit)
#because this takes too long we split it in several so we can save inbetween and combine later
imp_one <- mice(data = impute_hdf,
                m = 1,
                maxit = 20,
                method = vec_mtd,
                predictorMatrix = mat_prd,
                seed = 1) # set seed to make sure it can be repeated

plot(imp_one)
save(imp_one, file = paste0(path, "imp_one"))

#repeat ten times in total
imp_two <- mice(data = impute_hdf,
                m = 1,
                maxit = 20,
                method = vec_mtd,
                predictorMatrix = mat_prd,
                seed = 2) #set different seed per imputation

plot(imp_two)
save(imp_two, file = paste0(path, "imp_two"))

#to try and continu with ipcw in between we combine imp one and two into one dataset
load(paste0(path, "imp_one"))
load(paste0(path, "imp_two"))

dat_imputed_one <- complete(imp_one,
                            action = "long",
                            include = TRUE)

dat_imputed_two <- complete(imp_two,
                            action = "long",
                            include = TRUE) %>%
    mutate(
        #name imp 1 imp 2 because this is the second imputation, remove imp 0 we already have that within imp one
        .imp = if_else(.imp == 1, 2, .imp)
    ) %>%
    filter(.imp == 2)

dat_imputed_first_two <- bind_rows(dat_imputed_one, dat_imputed_two)

#clean working space
rm(dat_imputed_one)
rm(dat_imputed_two)

#save inbetween
save(dat_imputed_first_two, file = paste0(path, "dat_imputed_first_two.Rdata"))

imp_three <- mice(impute_hdf,
                  m = 1,
                  maxit = 20,
                  method = vec_mtd,
                  predictorMatrix = mat_prd,
                  seed = 3)

imp_four <- mice(impute_hdf,
                 m = 1,
                 maxit = 20,
                 method = vec_mtd,
                 predictorMatrix = mat_prd,
                 seed = 4)

imp_five <- mice(impute_hdf,
                 m = 1,
                 maxit = 20,
                 method = vec_mtd,
                 predictorMatrix = mat_prd,
                 seed = 5)

imp_six <- mice(impute_hdf,
                m = 1,
                maxit = 20,
                method = vec_mtd,
                predictorMatrix = mat_prd,
                seed = 6)

imp_seven <- mice(impute_hdf,
                  m = 1,
                  maxit = 20,
                  method = vec_mtd,
                  predictorMatrix = mat_prd,
                  seed = 7)

imp_eight <- mice(impute_hdf,
                  m = 1,
                  maxit = 20,
                  method = vec_mtd,
                  predictorMatrix = mat_prd,
                  seed = 8)

imp_nine <- mice(impute_hdf,
                 m = 1,
                 maxit = 20,
                 method = vec_mtd,
                 predictorMatrix = mat_prd,
                 seed = 9)

imp_ten <- mice(impute_hdf,
                m = 1,
                maxit = 20,
                method = vec_mtd,
                predictorMatrix = mat_prd,
                seed = 10)