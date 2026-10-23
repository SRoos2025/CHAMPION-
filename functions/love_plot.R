love_plot <- function (data) {
  
  #assign new names for the plot
  new.names <- c(age_cat_0             = "Age 18-45 years",
                 age_cat_1 =      "Age 45-65 years",
                 age_cat_2 =     "Age 65-75 years",
                 age_cat_3 = "Age >= 75 years",
                 demo_male           = "Sex",
                 education_cat_base_0  = "Primary education",
                 education_cat_base_1 = "Lower secondary education",
                 education_cat_base_2 = "Upper secondary education",
                 education_cat_base_3 = "Post-seoncdary education",
                 txt_pre_sbp         = "Systolic bloodpressure",
                 txt_pre_dbp         = "Diastolic bloodpressure",
                 cci                 = "Charlson Comorbidity Index",
                 bmi                 = "BMI",
                 ultra_fil_rate      = "Ultrafiltration rate",
                 txt_per_week        = "Treatments frequency",
                 catheter_1          = "Catheter access",
                 lab_wktv            = "Kt/V",
                 txt_qb_mean         = "Blood flow rate",
                 lab_serum_na        = "Serum sodium",
                 lab_potassium       = "Serum Potassium",
                 lab_phosph          = "Serum Phosphate",
                 lab_albumin         = "Serum Albumin",
                 lab_crp             = "Serum CRP",
                 lab_hgba1c          = "Serum HbA1c",
                 lab_calcium         = "Serum Calcium",
                 lab_creatinine      = "Serum Creatinine",
                 lab_hgb             = "Serum hemoglobin",
                 lab_pth             = "Serum PTH",
                 lab_bicarb          = "Serum Bicarbonate",
                 lab_ferritin        = "Serum Ferritin")
  
  
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
  alpha = 0.8)+
    
    #we use ggh4x to remove y labels for the other plots see: https://teunbrand.github.io/ggh4x/articles/Facets.html
    ggh4x::facet_wrap2(
      ~ factor(cluster, levels = as.character(0:12)),
      ncol = 4,
      scales = "free_x", #let the range of x axis be determined per plot
      axes = "all",
      remove_labels = "y",
      labeller = as_labeller(
        c(
          "0" = "Weighting moment 0",
          "1" = "Weighting moment 1",
          "2" = "Weighting moment 2",
          "3" = "Weighting moment 3",
          "4" = "Weighting moment 4",
          "5" = "Weighting moment 5",
          "6" = "Weighting moment 6",
          "7" = "Weighting moment 7",
          "8" = "Weighting moment 8",
          "9" = "Weighting moment 9",
          "10" = "Weighting moment 10",
          "11" = "Weighting moment 11",
          "12"= "Weighting moment 12"
        )
      )
    ) + 
    labs(x = "Absolute Standardized Mean Differences",
         title = NULL) +
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
