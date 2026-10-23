#make distribution plot of weighting for specific study visit
plot_ipcw_visit_weighted <- function(data, ps = "p", weight = "w", inf_cens = "inf_cens",  text_null = "No informative censoring", text_one = "Informative censoring") {
  
  plot <- ggplot(data, aes(x = .data[[ps]], weight =.data[[weight]],  fill = as.factor(.data[[inf_cens]]))) +
    #alpha is transparency, 0 is transparent 1 = opaque
    #color NA we dont want lines around the curve
    geom_density(alpha = 0.3, color = NA) +
    facet_wrap(~ visit, ncol = 2, scales = "free_y",
               labeller = as_labeller(
                 setNames(
                   paste("Weighting moment", 0:12),
                   as.character(0:12)
                 )))+ #max 2 plots per row labeller = labeller(visit = function(x) paste("Visit", x))) + 
    #coord_cartesian(xlim = c(0.8, 1.0)) + # zoom
    labs( x = "Propensity score", y = "Density", title = "Weighted Distribution of propensity scores per study visit", fill = "Censoring status"
    )+
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25)
    )+
    theme_minimal()+
    theme(
      text = element_text(
        family = "Times"
      ),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_fill_manual(
      values = c("0" = "#648FFF", "1" ="#FFB000"),
      labels = c("0" = text_null, "1"= text_one))
  return(plot)
}
