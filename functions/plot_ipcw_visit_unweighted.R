plot_ipcw_visit_unweighted <- function(data, ps = "p", inf_cens = "inf_cens",
                                       text_null = "No informative censoring",
                                       text_one = "Informative censoring") {
  
  visit_labels <- c(
    "0"  = "Weighting moment 0 (day 0-13)",
    "1"  = "Weighting moment 1 (day 14-27)",
    "2"  = "Weighting moment 2 (day 28-41)",
    "3"  = "Weighting moment 3 (day 42-55)",
    "4"  = "Weighting moment 4 (day 56-69)",
    "5"  = "Weighting moment 5 (day 70-83)",
    "6"  = "Weighting moment 6 (day 84-90)",
    "7"  = "Weighting moment 7 (day 91-359)",
    "8"  = "Weighting moment 8 (day 360-719)",
    "9"  = "Weighting moment 9 (day 720-1079)",
    "10" = "Weighting moment 10 (day 1080-1439)",
    "11" = "Weighting moment 11 (day 1440-1799)",
    "12" = "Weighting moment 12 (day 1800-1860)"
  )
  
  plot <- ggplot(data, aes(x = .data[[ps]], fill = as.factor(.data[[inf_cens]]))) +
    geom_density(alpha = 0.3, color = NA) +
    ggh4x::facet_wrap2(
      ~ factor(visit, levels = as.character(0:12)),
      ncol = 2,
      scales = "free_y",
      labeller = as_labeller(visit_labels),
      strip.position = "top"
    ) +
    labs(x = "Propensity score", y = "Density",
         title = "Unweighted", fill = "Censoring status") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(
      values = c("0" = "#648FFF", "1" = "#FFB000"),
      labels = c("0" = text_null, "1" = text_one)
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "Times"),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_rect(fill = "grey85", colour = "black", linewidth = 0.5),
      strip.text = element_text(size = 9, colour = "black", face = "plain"),
      strip.placement = "outside"
    )
  
  return(plot)
}
