make_legend_gu <- function() {
  #' Create Legend for Glucose Uptake Plots
  #'
  #' Creates a standalone legend grob for glucose uptake plots showing:
  #' - Female LF (white solid)
  #' - Female HF (black solid)
  #' - Male LF (white with black dots)
  #' - Male HF (black with white dots)
  #'
  #' @return A grob object containing the legend
  
  library(ggplot2)
  library(ggpattern)
  library(grid)
  
  # Create a minimal dummy plot just to extract the legend
  dummy_data <- data.frame(
    x = rep(1, 4),
    y = 1:4,
    group = factor(
      c("Female LF", "Female HF", "Male LF", "Male HF"),
      levels = c("Female LF", "Female HF", "Male LF", "Male HF")
    )
  )
  
  dummy_plot <- ggplot(dummy_data, aes(x = x, y = y, fill = group, pattern = group, 
                                        pattern_fill = group, pattern_colour = group)) +
    geom_bar_pattern(
      stat = "identity",
      color = "black",
      linewidth = 2.0,
      pattern_density = 0.4,
      pattern_spacing = 0.01,
      pattern_angle = 0
    ) +
    scale_fill_manual(
      values = c(
        "Female LF" = "white",
        "Female HF" = "black",
        "Male LF" = "white",
        "Male HF" = "black"
      )
    ) +
    scale_pattern_manual(
      values = c(
        "Female LF" = "none",
        "Female HF" = "none",
        "Male LF" = "circle",
        "Male HF" = "circle"
      )
    ) +
    scale_pattern_fill_manual(
      values = c(
        "Female LF" = "black",
        "Female HF" = "black",
        "Male LF" = "black",
        "Male HF" = "white"
      )
    ) +
    scale_pattern_colour_manual(
      values = c(
        "Female LF" = NA,
        "Female HF" = NA,
        "Male LF" = NA,
        "Male HF" = NA
      )
    ) +
    theme_classic(base_size = 12, base_family = "Arial") +
    theme(
      legend.position = "right",
      legend.key.spacing.y = unit(0.3, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.6, "cm"),
      legend.title = element_blank(),
      legend.text = element_text(size = 11, face = "bold"),
      legend.key = element_rect(colour = "black", linewidth = 0.8)
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(
          pattern = c("none", "none", "circle", "circle"),
          pattern_fill = c("black", "black", "black", "white"),
          pattern_colour = c(NA, NA, NA, NA),
          size = 4.0
        ),
        keywidth = unit(0.6, "cm"),
        keyheight = unit(0.3, "cm"),
        byrow = TRUE
      ),
      pattern = "none",
      pattern_fill = "none",
      pattern_colour = "none"
    )
  
  # Extract the legend as a grob
  legend_grob <- cowplot::get_legend(dummy_plot)
  
  return(legend_grob)
}
