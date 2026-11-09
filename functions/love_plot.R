love_plot <- function (data) {
  
  #assign new names for the plot
  new.names <- c(age_cat_0             = "Age 18-45 years",
                 age_cat_1 =      "Age 45-65 years",
                 age_cat_2 =     "Age 65-75 years",
                 age_cat_3 = "Age  >= 75 years",
                 demo_male           = "Sex",
                 education_cat_base_0  = "Primary education",
                 education_cat_base_1 = "Lower secondary education",
                 education_cat_base_2 = "Upper secondary education",
                 education_cat_base_3 = "Post-secondary education",
                 demo_region = "GDP <= $35,000",
                 txt_pre_sbp         = "Systolic bloodpressure",
                 txt_pre_dbp         = "Diastolic bloodpressure",
                 cci                 = "Charlson Comorbidity Index",
                 bmi                 = "Body Mass Index",
                 ultra_fil_rate      = "Ultrafiltration rate",
                 txt_per_week        = "Treatment frequency",
                 catheter_1          = "Catheter access",
                 lab_wktv            = "Kt/V",
                 txt_qb_mean         = "Blood flow rate",
                 lab_serum_na        = "Serum sodium",
                 lab_potassium       = "Serum potassium",
                 lab_phosph          = "Serum phosphate",
                 lab_albumin         = "Serum albumin",
                 lab_crp             = "Serum CRP",
                 lab_hgba1c          = "Serum hba1c",
                 lab_calcium         = "Serum calcium",
                 lab_creatinine      = "Serum creatinine",
                 lab_hgb             = "Serum hemoglobin",
                 lab_pth             = "Serum PTH",
                 lab_bicarb          = "Serum bicarbonate",
                 lab_ferritin        = "Serum ferritin")
  
  
  plot <- love.plot(
  data,
  var.names = new.names,
  threshold = 0.1, #show threshold line for SMD of 0.1
  abs = TRUE, #show absolute SMD's
  var.order = "unadjusted",
  which.cluster = .all,      # one facet per visit
  stars = "raw",
  cluster.subtitles = FALSE,
  colors = c("Unadjusted" = "#648FFF",
             "Adjusted" = "#FFB000"),
  alpha = 0.5)+
    
    #we use ggh4x to remove y labels for the other plots see: https://teunbrand.github.io/ggh4x/articles/Facets.html
    ggh4x::facet_wrap2(
      ~ factor(cluster, levels = as.character(0:12)),
      ncol = 4,
      scales = "free_x", #let the range of x axis be determined per plot
      axes = "all",
      remove_labels = "y",
      labeller = as_labeller(
        c(
          "0" = "Weighting moment 0 (day 0-13)",
          "1" = "Weighting moment 1 (day 14-27)",
          "2" = "Weighting moment 2 (day 28-41)",
          "3" = "Weighting moment 3 (day 42-55)",
          "4" = "Weighting moment 4 (day 56-69)",
          "5" = "Weighting moment 5 (day 70-83)",
          "6" = "Weighting moment 6 (day 84-90)",
          "7" = "Weighting moment 7 (day 91-359)",
          "8" = "Weighting moment 8 (day 360-719)",
          "9" = "Weighting moment 9 (day 720-1079)",
          "10" = "Weighting moment 10 (day 1080-1439)",
          "11" = "Weighting moment 11 (day 1440-1799)",
          "12"= "Weighting moment 12 (day 1800-1860)"
        )
      )
    ) + 
    labs(x = "Absolute Standardized Mean Differences",
         title = NULL) +
    guides(color = guide_legend(title = NULL)) +
    theme(
      text = element_text(
        family = "Times"
      ),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13),
      strip.text = element_text(size = 13),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.position = "bottom"
    )
  return (plot)
}
