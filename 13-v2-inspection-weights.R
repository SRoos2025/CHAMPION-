#13.0 inspecting_weights
#by S. Roos
#last updated on 09-11-2026
#we use the data with both clones and only imputed data ranging from .imp 1 to .imp 10, as created in 11-v2-imputation
#0. set up----
##load packages----
pacman::p_load( "rio", #load data
                "purrr", #data untangling
                "magrittr", #untangle data
                "dplyr", #untangle data
                "tidyr", #untangle data
                "ggplot2", #for plotting
                "cobalt",
                "ggh4x",
                "patchwork")

##define path to save any output to, and path import to import data
path <- "..."

#load functions
walk(list.files(paste0(path, "funs/")), \(x)source(paste0(path, "funs/", x)))

#for mortality outcomes----
load(paste0(path, "data_ps.Rdata"))

#we checked this after this script
#with and without weights at visit 12, the measurements are equal there so we set to 1 at visit 12
#here it reduces computationally heaviness of model
#for fixed covariates, weighting does have an effect in visit 12 with max weight of 38
# data_ps <- data_ps %>%
#   mutate(
#     ipcw_inf_art_cumprod = if_else(visit == 12, 1, ipcw_inf_art_cumprod),
#     combined_true_weights_cumprod = if_else(visit == 12, 1, combined_true_weights_cumprod),
#     stab_combined_cumprod = if_else(visit == 12, 1, stab_combined_cumprod)
#   )
# save(data_ps, file = paste0(path, "data_ps.Rdata"))

#we select first imputation
covs <- get_covs_imp_1(data = data_ps)

imp_1 <- data_ps %>%
  filter(.imp==1)

first_visit <- data_ps %>%
  filter(visit <= 7)

#define times new roman
windowsFonts(
       Times = windowsFont("Times New Roman")
   )

#1.0 first the true weights---- 
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in informative censoring yes or no
  weights  = imp_1[["combined_true_weights"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_true_cens.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["combined_true_weights"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_true_treat.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps, ps = "ps", inf_cens = "inf_cens")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps, ps = "ps", weight = "ipcw_inf_cens_true")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_true.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial

plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_art", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")

plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_art", weight = "ipcw_art_cens", inf_cens = "art_cens",  text_null = "No artificial censoring", text_one = "Artificial censoring")


figure_1 <- wrap_plots(plot_art, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_true_art.pdf", figure_1, width = 16, height = 16, dpi = 1200)

rm(plot)
rm(plot_art)
gc()

#2.0 now stabilized with marginal probability----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in informative censoring yes or no
  weights  = imp_1[["stab_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_stab_cens.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["stab_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_stab_treat.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps, ps = "ps", inf_cens = "inf_cens")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps, ps = "ps", weight = "stab_ipcw_inf_cens")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_stab.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial
plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_art", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")

plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_art", weight = "stab_ipcw_art_cens", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")

figure_1 <- wrap_plots(plot_art, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_stab_art.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#3.0 fixed---- 
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in informative censoring yes or no
  weights  = imp_1[["fixed_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_fixed_cens.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["fixed_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_fixed_treat.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps, ps = "ps", inf_cens = "inf_cens")


plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps, ps = "ps", weight = "fixed_ipcw_cens")


figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")

ggsave("distr_fixed.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial
plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_art", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring" )


plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_art", weight = "ipcw_art_fixed", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")

figure_1 <- wrap_plots(plot_art, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_fixed_art.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#4.0 pool across visits----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in informative censoring yes or no
  weights  = imp_1[["combine_pool_visits"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)


ggsave("plot_pool_cens.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["combine_pool_visits"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_pool_treat.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps, ps = "ps_pool", inf_cens = "inf_cens")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps, ps = "ps_pool", weight = "combine_pool_visits")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")

ggsave("distr_pool.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial
plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_pool_art", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring" )

plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_pool_art", weight = "combine_pool_visits", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring" )

figure_1 <- wrap_plots(plot_art, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_pool_art.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#5.0combine inf and art cens----
#with differences for combined censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in any censoring yes or no
  weights  = imp_1[["ipcw_inf_art"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_cens.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_inf_art"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_treat.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps, ps = "prop_score_comb", inf_cens = "cens_combine",  text_null = "No censoring", text_one = "Informative or Artificial censoring")


plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps, ps = "prop_score_comb", weight = "ipcw_inf_art", inf_cens = "cens_combine",  text_null = "No censoring", text_one = "Informative or Artificial censoring")


figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_combine.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#6.0combine inf and art cens AND each visit----
#with differences for combined censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in any censoring yes or no
  weights  = imp_1[["ipcw_inf_art_all_visit"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_all_visit_cens.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_inf_art_all_visit"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_all_visits_treat.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps, ps = "prop_score_comb_all", inf_cens = "cens_combine",  text_null = "No censoring", text_one = "Informative or Artificial censoring")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps, ps = "prop_score_comb_all", weight = "ipcw_inf_art_all_visit", inf_cens = "cens_combine",  text_null = "No censoring", text_one = "Informative or Artificial censoring")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_combine_all.pdf", figure_1, width = 16, height = 16, dpi = 1200)



#7.0 capped weights----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_cap"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_cap_cens.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_cap"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_cap_treat.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps, ps = "ps", inf_cens = "inf_cens")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps, ps = "ps", weight = "ipcw_cap")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_cap.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial

plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_art", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring" )

plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_art", weight = "ipcw_cap", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring" )

figure_1 <- wrap_plots(plot_art, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_cap_art.pdf", figure_1, width = 16, height = 16, dpi = 1200)

rm(plot)
rm(plot_art)
gc()






#for hospitalization outcomes----
load(paste0(path, "data_ps_hosp.Rdata"))

#we checked this after this script
#with and without weights at visit 12, the measurements are equal there so we set to 1 at visit 12
#here it reduces computationally heaviness of model
#for fixed covariates, weighting does have an effect in visit 12 with max weight of 38
# data_ps <- data_ps %>%
#   mutate(
#     ipcw_inf_art_cumprod = if_else(visit == 12, 1, ipcw_inf_art_cumprod),
#     combined_true_weights_cumprod = if_else(visit == 12, 1, combined_true_weights_cumprod),
#     stab_combined_cumprod = if_else(visit == 12, 1, stab_combined_cumprod)
#   )
# save(data_ps, file = paste0(path, "data_ps.Rdata"))

#we select first imputation
covs <- get_covs_imp_1(data = data_ps_hosp)

imp_1 <- data_ps_hosp %>%
  filter(.imp==1)

first_visit <- data_ps_hosp %>%
  filter(visit <= 7)

#define times new roman
windowsFonts(
  Times = windowsFont("Times New Roman")
)

#H1.0 first the true weights---- 
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["combined_true_weights"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_true_cens_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["combined_true_weights"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_true_treat_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_hosp, ps = "ps", inf_cens = "inf_cens_incl_death")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_hosp, ps = "ps",inf_cens = "inf_cens_incl_death", weight = "ipcw_inf_cens_true")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_true_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial not needed, the same as for mortality outcome

rm(plot)
gc()

#H2.0 now stabilized with marginal probability----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["stab_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_stab_cens_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["stab_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_stab_treat_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_hosp, ps = "ps", inf_cens = "inf_cens_incl_death")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_hosp, ps = "ps", weight = "stab_ipcw_inf_cens")


figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_stab_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial not needed, the same as mortality outcome

#H3.0 fixed---- 
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["fixed_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_fixed_cens_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["fixed_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_fixed_treat_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_hosp, ps = "ps", inf_cens = "inf_cens_incl_death")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_hosp, ps = "ps", weight = "fixed_ipcw_cens")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_fixed_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial not needed

#4.0 pool across visits----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["combine_pool_visits"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_pool_cens_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["combine_pool_visits"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_pool_treat_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_hosp, ps = "ps_pool", inf_cens = "inf_cens_incl_death")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_hosp, ps = "ps_pool", weight = "combine_pool_visits")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_pool_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial not needed

#5.0combine inf and art cens----
#with differences for combined censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in any censoring yes or no
  weights  = imp_1[["ipcw_inf_art"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_cens_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_inf_art"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_treat_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_hosp, ps = "prop_score_comb", inf_cens = "cens_combine_incl_death",  text_null = "No censoring", text_one = "Informative or Artificial censoring")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_hosp, ps = "prop_score_comb", weight = "ipcw_inf_art", inf_cens = "cens_combine_incl_death",  text_null = "No censoring", text_one = "Informative or Artificial censoring")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_combine_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#6.0combine inf and art cens AND each visit----
#with differences for combined censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in any censoring yes or no
  weights  = imp_1[["ipcw_inf_art_all_visit"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_all_visit_cens_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_inf_art_all_visit"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_all_visits_treat_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_hosp, ps = "prop_score_comb_all", inf_cens = "cens_combine_incl_death", text_null = "No censoring", text_one = "Informative or Artificial censoring")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_hosp, ps = "prop_score_comb_all", weight = "ipcw_inf_art_all_visit", inf_cens = "cens_combine_incl_death", text_null = "No censoring", text_one = "Informative or Artificial censoring")


figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")

ggsave("distr_combine_all_inf_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)


#7.0 capped weights----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_cap"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_cap_cens_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_cap"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_cap_treat_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_hosp, ps = "ps", inf_cens = "inf_cens_incl_death")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_hosp, ps = "ps", weight = "ipcw_cap")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_cap_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial is not needed, the same for mortality
rm(plot)
rm(plot_art)
gc()





#for mortality within high-volume strategy----
load(paste0(path, "data_ps_high_volume.Rdata"))

covs <- get_covs_imp_1(data = data_ps_high_volume)

imp_1 <- data_ps_high_volume %>%
  filter(.imp==1)

first_visit <- data_ps_high_volume %>%
  filter(visit <= 7)

#define times new roman
windowsFonts(
  Times = windowsFont("Times New Roman")
)

#A1.0 first the true weights---- 
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in informative censoring yes or no
  weights  = imp_1[["combined_true_weights"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_true_cens_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["combined_true_weights"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_true_treat_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume, ps = "ps", inf_cens = "inf_cens")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume, ps = "ps", weight = "ipcw_inf_cens_true")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_true_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial

plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_art", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")


plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_art", weight = "ipcw_art_cens", inf_cens = "art_cens",  text_null = "No artificial censoring", text_one = "Artificial censoring")


figure_1 <- wrap_plots(plot_art, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_true_art_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)

rm(plot)
rm(plot_art)
gc()

#A2.0 now stabilized with marginal probability----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in informative censoring yes or no
  weights  = imp_1[["stab_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_stab_cens_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["stab_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_stab_treat_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume, ps = "ps", inf_cens = "inf_cens")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume, ps = "ps", weight = "stab_ipcw_inf_cens")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_stab_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial
plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_art", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")

plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_art", weight = "stab_ipcw_art_cens", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")

figure_1 <- wrap_plots(plot_art, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_stab_art_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)


#A3.0fixed---- 
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in informative censoring yes or no
  weights  = imp_1[["fixed_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_fixed_cens_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["fixed_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_fixed_treat_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume, ps = "ps", inf_cens = "inf_cens")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume, ps = "ps", weight = "fixed_ipcw_cens")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_fixed_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial
plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_art", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring" )

plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_art", weight = "ipcw_art_fixed", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")

figure_1 <- wrap_plots(plot_art, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_fixed_art_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)


#A4.0 pool across visits----
  #with differences for informative censoring
  bal <- bal.tab(
    covs,
    treat    = imp_1[["inf_cens"]], #differences in informative censoring yes or no
    weights  = imp_1[["combine_pool_visits"]], #take the combined pooled weights
    cluster  = imp_1[["visit"]],     # do balance checks per vist
    which.cluster = .all,     # show all visits
    binary = "std", #also show standardized mean differences for categorical variables
    un = TRUE #show unbalanced and balanced
  )

plot <- love_plot(data = bal)

ggsave("plot_pool_cens_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["combine_pool_visits"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_pool_treat_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume, ps = "ps_pool", inf_cens = "inf_cens")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume, ps = "ps_pool", weight = "combine_pool_visits")

figure_1 <- wrap_plots(plot, ncol = 1) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_pool_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial
plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_pool_art", inf_cens = "art_cens")

plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_pool_art", weight = "combine_pool_visits", inf_cens = "art_cens")

figure_1 <- wrap_plots(plot_art, ncol = 1) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_pool_art_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)


#A5.0 combine inf and art cens----
  #with differences for combined censoring
  bal <- bal.tab(
    covs,
    treat    = imp_1[["cens_combine"]], #differences in any censoring yes or no
    weights  = imp_1[["ipcw_inf_art"]], #take the combined pooled weights
    cluster  = imp_1[["visit"]],     # do balance checks per vist
    which.cluster = .all,     # show all visits
    binary = "std", #also show standardized mean differences for categorical variables
    un = TRUE #show unbalanced and balanced
  )

plot <- love_plot(data = bal)

ggsave("plot_combine_cens_inf_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_inf_art"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_treat_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume, ps = "prop_score_comb", inf_cens = "cens_combine",  text_null = "No censoring", text_one = "Informative or Artificial censoring")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume, ps = "prop_score_comb", weight = "ipcw_inf_art", inf_cens = "cens_combine",  text_null = "No censoring", text_one = "Informative or Artificial censoring")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_combine_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#A6.0 combine inf and art cens AND each visit----
#with differences for combined censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in any censoring yes or no
  weights  = imp_1[["ipcw_inf_art_all_visit"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_all_visit_cens_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_inf_art_all_visit"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_all_visits_treat_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume, ps = "prop_score_comb_all", inf_cens = "cens_combine", text_null = "No censoring", text_one = "Informative or Artificial censoring")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume, ps = "prop_score_comb_all", weight = "ipcw_inf_art_all_visit", inf_cens = "cens_combine", text_null = "No censoring", text_one = "Informative or Artificial censoring")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_combine_all_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#A7.0 capped weights----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_cap"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_cap_cens_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_cap"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_cap_treat_high.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume, ps = "ps", inf_cens = "inf_cens")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume, ps = "ps", weight = "ipcw_cap")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_cap_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial

plot_art <- list()
plot_art[[1]]<- plot_ipcw_visit_unweighted(data = first_visit, ps = "ps_art", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")

plot_art[[2]]<- plot_ipcw_visit_weighted(data = first_visit, ps = "ps_art", weight = "ipcw_cap", inf_cens = "art_cens", text_null = "No artificial censoring", text_one = "Artificial censoring")

figure_1 <- wrap_plots(plot_art, ncol = 2) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_cap_art_high.pdf", figure_1, width = 16, height = 16, dpi = 1200)

rm(plot)
rm(plot_art)
gc()



#for hospitalization within high-volume strategy----
load(paste0(path, "data_ps_high_volume_hosp.Rdata"))

#we select first imputation
covs <- get_covs_imp_1(data = data_ps_high_volume_hosp)

imp_1 <- data_ps_high_volume_hosp %>%
  filter(.imp==1)

first_visit <- data_ps_high_volume_hosp %>%
  filter(visit <= 7)

#define times new roman
windowsFonts(
  Times = windowsFont("Times New Roman")
)
#HH1.0 first the true weights---- 
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["combined_true_weights"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_true_cens_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["combined_true_weights"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_true_treat_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume_hosp, ps = "ps", inf_cens = "inf_cens_incl_death")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume_hosp, ps = "ps", weight = "ipcw_inf_cens_true")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_true_high_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial des not have to be repeated, is same as mortality for high volume

rm(plot)
rm(plot_art)
gc()

#HH2.0 now stabilized with marginal probability----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["stab_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_stab_cens_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["stab_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_stab_treat_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume_hosp, ps = "ps", inf_cens = "inf_cens_incl_death")


plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume_hosp, ps = "ps", weight = "stab_ipcw_inf_cens")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_stab_high_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#artificial not needed

#HH3.0fixed---- 
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["fixed_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_fixed_cens_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["fixed_combined"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_fixed_treat_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume_hosp, ps = "ps", inf_cens = "inf_cens_incl_death")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume_hosp, ps = "ps", weight = "fixed_ipcw_cens")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_fixed_high_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)


#HH4.0 pool across visits----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["inf_cens_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["combine_pool_visits"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_pool_cens_high_hosp.pdf", plot, width = 12, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["combine_pool_visits"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_pool_treat_high_hosp.pdf", plot, width = 12, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume_hosp, ps = "ps_pool", inf_cens = "inf_cens_incl_death")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume_hosp, ps = "ps_pool", weight = "combine_pool_visits")

figure_1 <- wrap_plots(plot, ncol = 1) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_pool_high_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#HH5.0 combine inf and art cens----
#with differences for combined censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in any censoring yes or no
  weights  = imp_1[["ipcw_inf_art"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_cens_inf_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_inf_art"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_treat_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume_hosp, ps = "prop_score_comb", inf_cens = "cens_combine_incl_death",  text_null = "No censoring", text_one = "Informative or Artificial censoring")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume_hosp, ps = "prop_score_comb", weight = "ipcw_inf_art", inf_cens = "cens_combine_incl_death",  text_null = "No censoring", text_one = "Informative or Artificial censoring")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_combine_high_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#HH6.0 combine inf and art cens AND each visit----
#with differences for combined censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["cens_combine_incl_death"]], #differences in any censoring yes or no
  weights  = imp_1[["ipcw_inf_art_all_visit"]], #take the combined pooled weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_all_visit_cens_high_hosp.pdf", plot, width = 15, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_inf_art_all_visit"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_combine_all_visits_treat_high_hosp.pdf", plot, width = 12, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume_hosp, ps = "prop_score_comb_all", inf_cens = "cens_combine_incl_death", text_null = "No censoring", text_one = "Informative or Artificial censoring")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume_hosp, ps = "prop_score_comb_all", weight = "ipcw_inf_art_all_visit", inf_cens = "cens_combine_incl_death", text_null = "No censoring", text_one = "Informative or Artificial censoring")

figure_1 <- wrap_plots(plot, ncol = 1) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_combine_all_high_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)

#A7.0 capped weights----
#with differences for informative censoring
bal <- bal.tab(
  covs,
  treat    = imp_1[["inf_cens_incl_death"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_cap"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_cap_cens_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now for Treatment group
bal <- bal.tab(
  covs,
  treat    = imp_1[["treatment_clone"]], #differences in informative censoring yes or no
  weights  = imp_1[["ipcw_cap"]], #take the combined true weights
  cluster  = imp_1[["visit"]],     # do balance checks per vist
  which.cluster = .all,     # show all visits
  binary = "std", #also show standardized mean differences for categorical variables
  un = TRUE #show unbalanced and balanced
)

plot <- love_plot(data = bal)

ggsave("plot_cap_treat_high_hosp.pdf", plot, width = 19, height = 22, dpi = 1200)

#now overall distribution censored & uncensored
plot <- list()
plot[[1]] <- plot_ipcw_visit_unweighted(data = data_ps_high_volume_hosp, ps = "ps", inf_cens = "inf_cens_incl_death")

plot[[2]] <- plot_ipcw_visit_weighted(data = data_ps_high_volume_hosp, ps = "ps", weight = "ipcw_cap")

figure_1 <- wrap_plots(plot, ncol = 2) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & #only take one legend
  theme(legend.position = "bottom")
ggsave("distr_cap_high_hosp.pdf", figure_1, width = 16, height = 16, dpi = 1200)


