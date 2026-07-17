#2. get CCI for comorbidity
#goal of script is to get baseline CCI as time-fixed confounder

#0. set up----
##load packages----
pacman::p_load( "rio", #load data
                "purrr", #data untangling
                "magrittr", #efficient pipelines
                "dplyr", #untangle data
                "scales",
                "tidyr",
                "here",
                "stringr") #to define path to extract and save files


####define path to save any output to, and path import to import data
path <- "..."
path_import <- "..."

#load functions
walk(list.files(paste0(path, "funs_datacleaning/")), \(x)source(paste0(path, "funs_datacleaning/", x)))

#load dataset with treatments and events and last observation of lab measurement, created in script 1
load(paste0(path, "combined_mortality_event.Rdata"))

#load comorbidity data
comorbids <- import(paste0(path_import, "comorbids.csv"))

#7.0 clean comorbid data to present baseline cci
comorbids <- lowercase_and_rename_id(comorbids) %>%
    #relocate to the left
    relocate(id, .before = comorbid_start_days_from_fdd)

#first step, we select relevant ID's
recent_comorbids <- comorbids %>%
    filter(id %in% combined_mortality_event[["id"]])

#first remove all icd10 text dots because reggex does not work well with .
recent_comorbids <- recent_comorbids %>%
    mutate(coh_icd10text = str_replace_all(comorbid_icd10text, "\\.", ""))

#next, we want to transform ICD 10 coding to actual comorbidities for caluclation of charlson comorbidity score(cci)
#source of all ICD 10 codes used: https://icd.who.int/browse10/2019/en#/I21
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        #make the days numeric, so we can filter the negative values out
        comorbid_start_days_from_fdd = as.numeric(comorbid_start_days_from_fdd),
        #if the days are negative, we set them to 0, meaning they are on baseline.
        comorbid_start_days_from_fdd = if_else(comorbid_start_days_from_fdd <0, 0, comorbid_start_days_from_fdd),
        #define last observed day with comorbidity, this contributes later in the code when we determine what is the last observed date ever
        last_comorb = max(comorbid_start_days_from_fdd),
        #history of myocardial infarction yes/no
        #I21 = acute myocardial infarction, I22 subsequent myocardial infarction, I23 certain current compliations following acute myocardial infarction
        myocard_inf = if_else(str_detect(comorbid_icd10text,"^I(21|22|23)"), 1, 0),
        myocard_inf_day = if_else(myocard_inf == 1, comorbid_start_days_from_fdd, NA),
        #if it is twice in the data, we take the first (minimum) time where the complication occured
        myocard_inf_day = min(myocard_inf_day, na.rm=TRUE),
        #if it never occurs, it is infinite, this also gives warning message, we turn that to NA
        myocard_inf_day = if_else(is.infinite(myocard_inf_day), NA, myocard_inf_day),
        #afterwards, we fill myocard_inf to 1 for every row if it is ever 1 (note we can not do that before first defining myocard_inf_day)
        myocard_inf = max(myocard_inf, na.rm=TRUE),
        myocard_inf = if_else(is.infinite(myocard_inf), 0, myocard_inf),
        #history of CVA or TIA
        #we include hemorrage(I60, 61, 62) Infarction, I63, Stroke not specified I64, I69: sequelae (late effects) of cerebrovascular disease,
        #G45: transient cerebral ischaemic attacks
        #regarding I67 (other cerebrovascular disease) we only take dissection, nonruptured, other specified cerebrovascular diseases (acute cerbrovascular inssuficiency or cerbral ischaemia (chronic))
        cva_tia = case_when(
            str_detect(comorbid_icd10text, "^I(60|61|62|63|64|69)") ~ 1,
            str_detect(comorbid_icd10text, "^G45") ~ 1,
            comorbid_icd10text %in% c("I670", "I678", "I679") ~ 1,
            .default = 0),
        cva_tia_day = if_else(cva_tia == 1, comorbid_start_days_from_fdd, NA),
        cva_tia_day = min(cva_tia_day, na.rm=TRUE),
        cva_tia_day = if_else(is.infinite(cva_tia_day), NA, cva_tia_day),
        cva_tia = max(cva_tia, na.rm=TRUE),
        cva_tia = if_else(is.infinite(cva_tia), 0, cva_tia)
    )%>%
    ungroup()

#heart failure & peripheral vascular disease
#I50 is heart failure, I13.0= Hypertensive heart and renal disease with (congestive) heart failure
#I13.2 = Hypertensive heart and renal disease with both (congestive) heart failure and renal failure
#I11.0 = Hypertensive heart disease with (congestive) heart failure
#for peripheral vascular disease
#I70.8 = Atherosclerosis of other arteries, I70.2 = Atherosclerosis of arteries of extremities, I71 = aortic aneurysm, I72 other aneurysm
#I73 = other pripheral vascular diseases
#I74 = Arterial embolism and thrombosis
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        heart_fail = if_else(str_detect(comorbid_icd10text,"^I50|^I130|^I132|^I110"), 1, 0),
        heart_fail_day = if_else(heart_fail == 1, comorbid_start_days_from_fdd, NA),
        heart_fail_day = min(heart_fail_day, na.rm=TRUE),
        heart_fail_day = if_else(is.infinite(heart_fail_day), NA, heart_fail_day),
        heart_fail = max(heart_fail, na.rm=TRUE),
        heart_fail = if_else(is.infinite(heart_fail), 0, heart_fail),
        #peripheral vascular disease
        peri_vas = case_when(
            str_detect(comorbid_icd10text, "^I708|^I702|^I7[1-4]") ~ 1,
            .default = 0),
        peri_vas_day = if_else(peri_vas == 1, comorbid_start_days_from_fdd, NA),
        peri_vas_day = min(peri_vas_day, na.rm=TRUE),
        peri_vas_day = if_else(is.infinite(peri_vas_day), NA, peri_vas_day),
        peri_vas = max(peri_vas, na.rm=TRUE),
        peri_vas = if_else(is.infinite(peri_vas), 0, peri_vas)
    )%>%
    ungroup()

#dementia and chronic pulmonary disease
#G30 = Alzaheimer, G31 = Other degenerative diseases of nervous system, not elsewhere classified (example lewy body)
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        dement = if_else(str_detect(comorbid_icd10text,"^G(30|310|311|318|319)"), 1, 0),
        dement_day = if_else(dement == 1, comorbid_start_days_from_fdd, NA),
        dement_day = min(dement_day, na.rm=TRUE),
        dement_day = if_else(is.infinite(dement_day), NA, dement_day),
        dement = max(dement, na.rm=TRUE),
        dement = if_else(is.infinite(dement), 0, dement),
        #chronic pulmonary disease
        #J40-J47: chronic lower respiratory diseases, 60-70 lung diseases due to external agents, J80-84 Other respiratory diseases principally affecting the interstitium
        #J95-J99 Other diseases of the respiratory system
        chron_pulm = if_else(str_detect(comorbid_icd10text,"^J4[0-7]|^J6[0-9]|J70|^J8[0-4]|^J9[5-9]"), 1, 0),
        chron_pulm_day = if_else(chron_pulm == 1, comorbid_start_days_from_fdd, NA),
        chron_pulm_day = min(chron_pulm_day, na.rm=TRUE),
        chron_pulm_day = if_else(is.infinite(chron_pulm_day), NA, chron_pulm_day),
        chron_pulm = max(chron_pulm, na.rm=TRUE),
        chron_pulm = if_else(is.infinite(chron_pulm), 0, chron_pulm)
    ) %>%
    ungroup()

#connective tissue disease and peptic ulcer disease
# M30-36 systemic connective tissue disorders
#I00-I02 acute rheumatic fever, I05-I09 rheumatic heart diseases, D86 sarcoidosis,
#D89 other disorders of immune mechanism not elsewhere classified (for example cryoglobulinaemia)
#M60 myositis
#K25 gastric ulcer, K26 duodenal ulcer, K27 peptic ulcer site unspecified, K28 gastrojejunal ulcer
#note we did not include K29 which is gastritis and duodenitis
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        conn_tiss = if_else(str_detect(comorbid_icd10text,"^I0[0-2]|^I0[5-9]|^D86|^D89|^M3[0-6]|^M60"), 1, 0),
        conn_tiss_day = if_else(conn_tiss == 1, comorbid_start_days_from_fdd, NA),
        conn_tiss_day = min(conn_tiss_day, na.rm=TRUE),
        conn_tiss_day = if_else(is.infinite(conn_tiss_day), NA, conn_tiss_day),
        conn_tiss = max(conn_tiss, na.rm=TRUE),
        conn_tiss = if_else(is.infinite(conn_tiss), 0, conn_tiss),
        #ulcer disease
        ulcer = if_else(str_detect(comorbid_icd10text,"^K2[5-8]"), 1, 0),
        ulcer_day = if_else(ulcer == 1, comorbid_start_days_from_fdd, NA),
        ulcer_day = min(ulcer_day, na.rm=TRUE),
        ulcer_day = if_else(is.infinite(ulcer_day), NA, ulcer_day),
        ulcer = max(ulcer, na.rm=TRUE),
        ulcer = if_else(is.infinite(ulcer), 0, ulcer)
    )%>%
    ungroup()


#liver disease and hemiplegia
#mild liver disease in context of cci is chronic hepatitis without fibrosis or cirrosis
#K70.1 is alcoholic liver hepatisis, K71.2 Toxic liver disease with acute hepatitis, K 73 Chronic hepatitis, not elsewhere classified, K71.3-8 is toxic liver disease with chronic persistant hepatitis
#K75 Other inflammatory liver diseases, B18 = chronic viral hepatitis
#g80-G83 = Cerebral palsy and other paralytic syndromes
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        mild_liver_dis = if_else(str_detect(comorbid_icd10text,"^K701|^K71[2-8]|^K73|^K75|^B18"), 1, 0),
        mild_liver_dis_day = if_else(mild_liver_dis == 1, comorbid_start_days_from_fdd, NA),
        mild_liver_dis_day = min(mild_liver_dis_day, na.rm=TRUE),
        mild_liver_dis_day = if_else(is.infinite(mild_liver_dis_day), NA, mild_liver_dis_day),
        mild_liver_dis = max(mild_liver_dis, na.rm=TRUE),
        mild_liver_dis = if_else(is.infinite(mild_liver_dis), 0, mild_liver_dis),
        #hemiplegia
        hemi = if_else(str_detect(comorbid_icd10text,"^G8[0-3]"), 1, 0),
        hemi_day = if_else(hemi == 1, comorbid_start_days_from_fdd, NA),
        hemi_day = min(hemi_day, na.rm=TRUE),
        hemi_day = if_else(is.infinite(hemi_day), NA, hemi_day),
        hemi = max(hemi, na.rm=TRUE),
        hemi = if_else(is.infinite(hemi), 0, hemi)
    )%>%
    ungroup()

#severe liver disease
#K70.2 = Alcoholic fibrosis and sclerosis of liver,
#K70.3 = Alcoholic cirrhosis of liver, K 70.4 = alcoholic hepatic failure.
#K72 = Hepatic failure, not elsewhere classified, K 74 = Fibrosis and cirrhosis of liver, K76 = Other diseases of liver
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        sev_liver_dis = if_else(str_detect(comorbid_icd10text,"^K70[2-4]|^K72|^K74|^K76"), 1, 0),
        sev_liver_dis_day = if_else(sev_liver_dis == 1, comorbid_start_days_from_fdd, NA),
        sev_liver_dis_day = min(sev_liver_dis_day, na.rm=TRUE),
        sev_liver_dis_day = if_else(is.infinite(sev_liver_dis_day), NA, sev_liver_dis_day),
        sev_liver_dis = max(sev_liver_dis, na.rm=TRUE),
        sev_liver_dis = if_else(is.infinite(sev_liver_dis), 0, sev_liver_dis)
    )%>%
    ungroup()

#diabetes mellitus
#10.9 is type I Without complications, 11.9 type 2 without. all other within 10 and 11 are subtypes of complications such as neurological, ophtamological, renal
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        dm_uncompl = if_else(str_detect(comorbid_icd10text,"^E109|^E119"), 1, 0),
        dm_uncompl_day = if_else(dm_uncompl == 1, comorbid_start_days_from_fdd, NA),
        dm_uncompl_day = min(dm_uncompl_day, na.rm=TRUE),
        dm_uncompl_day = if_else(is.infinite(dm_uncompl_day), NA, dm_uncompl_day),
        dm_uncompl = max(dm_uncompl, na.rm=TRUE),
        dm_uncompl = if_else(is.infinite(dm_uncompl), 0, dm_uncompl),
        #complicated
        dm_compl = if_else(str_detect(comorbid_icd10text,"^E10[0-8]|^E11[0-8]"), 1, 0),
        dm_compl_day = if_else(dm_compl == 1, comorbid_start_days_from_fdd, NA),
        dm_compl_day = min(dm_compl_day, na.rm=TRUE),
        dm_compl_day = if_else(is.infinite(dm_compl_day), NA, dm_compl_day),
        dm_compl = max(dm_compl, na.rm=TRUE),
        dm_compl = if_else(is.infinite(dm_compl), 0, dm_compl)
    )%>%
    ungroup()



#tumor
#C00-C75 are Malignant neoplasms, stated or presumed to be primary, of specified sites, except of lymphoid, haematopoietic and related tissue (local)
#C76-C80 are Malignant neoplasms of ill-defined, secondary and unspecified sites (metastasized)
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        local_tum = if_else(str_detect(comorbid_icd10text,"^C[0-75]"), 1, 0),
        local_tum_day = if_else(local_tum == 1, comorbid_start_days_from_fdd, NA),
        local_tum_day = min(local_tum_day, na.rm=TRUE),
        local_tum_day = if_else(is.infinite(local_tum_day), NA, local_tum_day),
        local_tum = max(local_tum, na.rm=TRUE),
        local_tum = if_else(is.infinite(local_tum), 0, local_tum),
        #complicated
        meta_tum = if_else(str_detect(comorbid_icd10text,"^C7[6-9]|^C80"), 1, 0),
        meta_tum_day = if_else(meta_tum == 1, comorbid_start_days_from_fdd, NA),
        meta_tum_day = min(meta_tum_day, na.rm=TRUE),
        meta_tum_day = if_else(is.infinite(meta_tum_day), NA, meta_tum_day),
        meta_tum = max(meta_tum, na.rm=TRUE),
        meta_tum = if_else(is.infinite(meta_tum), 0, meta_tum)
    )%>%
    ungroup()

#leukemia and lymphoma
#C91-C96 are different types of leukemia, and C81-C86 different types of lymphoma
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        leuk = if_else(str_detect(comorbid_icd10text,"^C9[1-6]"), 1, 0),
        leuk_day = if_else(leuk == 1, comorbid_start_days_from_fdd, NA),
        leuk_day = min(leuk_day, na.rm=TRUE),
        leuk_day = if_else(is.infinite(leuk_day), NA, leuk_day),
        leuk = max(leuk, na.rm=TRUE),
        leuk = if_else(is.infinite(leuk), 0, leuk),
        #lymphoma
        lymph = if_else(str_detect(comorbid_icd10text,"^C8[1-6]"), 1, 0),
        lymph_day = if_else(lymph == 1, comorbid_start_days_from_fdd, NA),
        lymph_day = min(lymph_day, na.rm=TRUE),
        lymph_day = if_else(is.infinite(lymph_day), NA, lymph_day),
        lymph = max(lymph, na.rm=TRUE),
        lymph = if_else(is.infinite(lymph), 0, lymph)
    )%>%
    ungroup()

#HIV resulting in complications (could be seen as AIDS, is not specifically specified here)
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        hiv = if_else(str_detect(comorbid_icd10text,"^B2[0-4]"), 1, 0),
        hiv_day = if_else(hiv == 1, comorbid_start_days_from_fdd, NA),
        hiv_day = min(hiv_day, na.rm=TRUE),
        hiv_day = if_else(is.infinite(hiv_day), NA, hiv_day),
        hiv = max(hiv, na.rm=TRUE),
        hiv = if_else(is.infinite(hiv), 0, hiv)
    )%>%
    ungroup()

#filter age cateogry for calculation of charlson comorbidity index
age <- combined_mortality_event %>%
    select(id, age_cat)

age <- age %>%
    group_by(id) %>%
    slice_head(n = 1)

recent_comorbids <- left_join(recent_comorbids, age, by = "id")


#compute baseline cci
#if the diagnosis is in the first 5 days we count it for baseline
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    mutate(
        cliver = case_when(
            mild_liver_dis ==1 & mild_liver_dis_day <= 5 ~ 1,
            sev_liver_dis ==1 & sev_liver_dis_day <= 5 ~ 3,
            .default = 0),
        cdm = case_when(
            dm_uncompl == 1 & dm_uncompl_day <= 5 ~ 1,
            dm_compl == 1 & dm_compl_day <= 5 ~ 2,
            .default = 0),
        chemi = case_when(
            hemi == 1 & hemi_day <= 5 ~2,
            .default = 0),
        ctum = case_when(
            local_tum == 1 & local_tum_day <= 5 ~ 2,
            meta_tum == 1 & meta_tum_day <= 5 ~ 6,
            .default = 0),
        cleuk = case_when(
            leuk == 1 & leuk_day <= 5 ~ 2,
            .default = 0),
        clymph = if_else(lymph == 1 & lymph_day <= 5, 2, 0),
        aids = if_else(hiv == 1 & hiv_day <= 5, 6, 0),
        #note that the way age is categorized does not exactly match the way it is for cci calculation, but better than nothing and it is close enough
        #of note CCI is divided in <50, 50-59, 60-69, 70-79 and >80 whereas we have 18-44, 45-64, 65-74 and >= 75
        cage = case_when(
            age_cat == "18-44" ~ 0,
            age_cat == "45-64" ~ 1,
            age_cat == "65-74" ~ 2,
            age_cat == ">=75" ~ 3,
            .default = 0),
        culcer = if_else(ulcer == 1 & ulcer_day <= 5, 1, 0),
        cmyocard_inf = if_else(myocard_inf ==1 & myocard_inf_day <= 5, 1, 0),
        cheart_fail = if_else(heart_fail ==1 & heart_fail_day <= 5, 1, 0),
        cperi_vas = if_else(peri_vas == 1 & peri_vas_day <= 5, 1, 0),
        ccva_tia = if_else(cva_tia ==1 & cva_tia_day <= 5, 1, 0),
        cdement = if_else(dement == 1 & dement_day <= 5, 1, 0),
        cchron_pulm = if_else(chron_pulm == 1 & chron_pulm_day <= 5, 1, 0),
        cconn_tiss = if_else(conn_tiss == 1 & conn_tiss_day <= 5, 1, 0),
        #note everyone starts with 2 because they are on dialysis
        cci = 2 + cliver + cdm + chemi + ctum + cleuk + clymph + aids +cage + culcer + cmyocard_inf +
            cheart_fail + cperi_vas + ccva_tia + cdement + cchron_pulm + cconn_tiss
    )%>%
    ungroup()

#reduce to 1 row per person
recent_comorbids <- recent_comorbids %>%
    group_by(id) %>%
    slice_head(n = 1)

save(recent_comorbids, file = paste0(path, "recent_comorbids.Rdata"))
load(paste0(path, "recent_comorbids.Rdata"))

#select the relevant columns
recent_comorbids <- recent_comorbids %>%
    select(id, last_comorb, cci)

#left join to combined mortality event
combined_mortality_event <- left_join(combined_mortality_event, recent_comorbids, by = "id")

# I left joined twice so rename demo_fdd_year
combined_mortality_event <- combined_mortality_event %>%
    mutate(
        demo_fdd_year = demo_fdd_year.x
    ) %>%
    select(-demo_fdd_year.x, -demo_fdd_year.y)


#add cause of kidney failure, gfr and eduction
combined_mortality_event <- combined_mortality_event %>%
    mutate(
        demo_height_cm = demo_height/100, #transform to m for bmi calculation
        bmi = ((txt_post_weight)/(demo_height_cm^2)),
        cause_kidn = case_when(
            demo_esrd_cause_customtext %in% c("Glomerulonephritis, Nephrotic syndrome",
                                              "Glomerulonephritis, Chronic nephritic syndrome",
                                              "Glomerulonephritis, Unspecified nephritic syndrome",
                                              "Glomerulonephritis, Secondary GN/Vasculitis",
                                              "Glomerulonephritis, Isolated proteinuria with specified morphological lesion",
                                              "Glomerulonephritis, Lupus erythematosus (SLE nephritis)",
                                              "Glomerulonephritis, Recurrent and persistent hematuria",
                                              "Glomerulonephritis, IgA nephropathy, Berger's disease") ~ "Glomerular disease",
            demo_esrd_cause_customtext %in% c("Diabetes, Type 1 diabetes mellitus",
                                              "Diabetes, Type 2 diabetes mellitus",
                                              "Diabetes, Unspecified diabetes mellitus",
                                              "Diabetes, Other specified diabetes mellitus",
                                              "Diabetes, Glomerular disorders in diabetes mellitus",
                                              "Diabetes, Malnutrition-related diabetes mellitus") ~ "Diabetic kidney disease",
            demo_esrd_cause_customtext %in% c("Hypertension, Secondary hypertension",
                                              "Hypertension, Hypertensive chronic kidney disease",
                                              "Hypertension, Hypertensive heart and chronic kidney disease",
                                              "Hypertension, Atherosclerosis of renal artery",
                                              "Hypertension, Pre-existing hypertension complicating pregnancy/childbirth/puerperium") ~ "Hypertension/ Renal vascular disease",
            demo_esrd_cause_customtext %in% c("Neoplasms, Amyloidosis",
                                              "Miscellaneous, Autoimmune diseases",
                                              "Neoplasms, Malignant neoplasm of kidney, except renal pelvis",
                                              "Neoplasms, Multiple myeloma and malignant plasma cell neoplasms",
                                              "Neoplasms, Malignant neoplasms of independent (primary) multiple sites",                                                                       "Neoplasms, Malignant neoplasm of adrenal gland",                                                                                               "Neoplasms, Malignant neoplasms of male genital organs",
                                              "Neoplasms, Malignant neoplasm, unspecified",
                                              "Neoplasms, Myeloid leukaemia") ~ "Other systemic diseases affecting the kidneys",
            demo_esrd_cause_customtext %in% c("Hereditary Kidney & Congenital Disorders, PKD",
                                              "Hereditary Kidney & Congenital Disorders, CATUK",
                                              "Glomerulonephritis, Hereditary nephropathy, not elsewhere classified",
                                              "Hereditary Kidney & Congenital Disorders, Renal agenesis and other reduction defects of kidney",
                                              "Hereditary Kidney & Congenital Disorders, Cystic kidney disease",
                                              "Hereditary Kidney & Congenital Disorders, Other congenital malformations of kidney",
                                              "Hereditary Kidney & Congenital Disorders, Other specified congenital malformation syndromes affecting multiple systems",
                                              "Hereditary Kidney & Congenital Disorders, Fabry disease and other inborn errors of metabolism") ~ "Familial, hereditary nephropathies",
            demo_esrd_cause_customtext %in% c("Renal Tubulo-interstitial, Chronic tubulo-interstitial nephritis",
                                              "Renal Tubulo-interstitial, Drug- and heavy-metal-induced tubulo-interstitial and tubular conditions",
                                              "Renal Tubulo-interstitial, Renal tubulo-interstitial disorders in diseases classified elsewhere",
                                              "Renal Tubulo-interstitial, Other renal tubulo-interstitial diseases",
                                              "Renal Tubulo-interstitial, Acute pyelonephritis") ~ "Tubulointerstitial disease",
            demo_esrd_cause_customtext %in% c("Unknown, Acute kidney failure",
                                              "Unknown, Other Genitourinary or Renal Disease or Renal Failure",
                                              "Renal Tubulo-interstitial, Obstructive and reflux uropathy",
                                              "Other Urological, Calculus of kidney and ureter",
                                              "",
                                              "Other Urological, Other renal disorders",
                                              "Miscellaneous, other",
                                              "Miscellaneous, metabolic disorders excl. Diabetes",
                                              "Other Urological, Neuromuscular dysfunction of bladder, not elsewhere classified",
                                              "Other Urological, Calculus of urinary tract in diseases classified elsewhere",
                                              "Other Urological, Urethral stricture",
                                              "Other Urological, Other disorders of bladder",
                                              "Miscellaneous, Cardiovascular Disease",
                                              "Other Urological, Calculus of lower urinary tract"
            ) ~ "Miscellaneous and unknown renal disorders"
        ),
        education = case_when(
            demo_education %in% c(0, 1) ~ 0, #primary education
            demo_education == 2 ~ 1, #lower secondary education (e.g., middle school)
            demo_education == 3 ~ 2,#upper secondary education (e.g. high school)
            demo_education %in% c(4,5,6,7) ~ 3 #post secondary education
        )
    )



#save in between
save(combined_mortality_event, file = paste0(path, "combined_mortality_event.Rdata"))
#load
load(paste0(path, "combined_mortality_event.Rdata"))