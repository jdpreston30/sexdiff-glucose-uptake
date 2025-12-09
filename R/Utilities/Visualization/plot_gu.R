plot_gu <- function(data, y_label, y_limits, y_breaks = NULL) {
  #' Plot Glucose Uptake Across Multiple Tissues
  #'
  #' Creates a grouped bar plot showing glucose uptake in multiple tissues with:
  #' - X-axis: Tissue names
  #' - Grouped bars: Female LF, Female HF, Male LF, Male HF (left to right)
  #' - Error bars showing SE
  #' - Spacing between tissue groups
  #' - Patterned fills: Female=solid, Male=polka dots
  #'
  #' @param data Data frame with columns: ID, sex, diet, and tissue columns
  #' @param y_label Y-axis label
  #' @param y_limits Y-axis limits as c(min, max)
  #' @param y_breaks Optional y-axis breaks
  #' @return A ggplot object
  
  library(ggplot2)
  library(ggpattern)
  library(dplyr)
  library(tidyr)
  
  # Pivot data to long format
  tissue_cols <- setdiff(names(data), c("ID", "sex", "diet"))
  
  long_data <- data %>%
    pivot_longer(
      cols = all_of(tissue_cols),
      names_to = "tissue",
      values_to = "glucose_uptake"
    ) %>%
    mutate(
      tissue = factor(tissue, levels = tissue_cols),
      sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
      diet = factor(diet, levels = c("LF", "HF")),
      # Create group variable for ordering
      group = factor(
        paste(sex, diet, sep = "_"),
        levels = c("Female_LF", "Female_HF", "Male_LF", "Male_HF"),
        labels = c("Female LF", "Female HF", "Male LF", "Male HF")
      )
    )
  
  # Calculate summary statistics
  summary_data <- long_data %>%
    group_by(tissue, group) %>%
    summarise(
      mean_val = mean(glucose_uptake, na.rm = TRUE),
      se_val = sd(glucose_uptake, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Create plot with patterned bars
  p <- ggplot(summary_data, aes(x = tissue, y = mean_val, fill = group, pattern = group, pattern_fill = group)) +
    geom_bar_pattern(
      stat = "identity", 
      position = position_dodge(0.8), 
      width = 0.7,
      color = "black", 
      linewidth = 0.6,
      pattern_density = 0.3,
      pattern_spacing = 0.02
    ) +
    geom_errorbar(
      aes(ymin = mean_val, ymax = mean_val + se_val),
      position = position_dodge(0.8), 
      width = 0.25, 
      linewidth = 0.5
    ) +
    scale_fill_manual(
      values = c(
        "Female LF" = "white",
        "Female HF" = "black",
        "Male LF" = "white",
        "Male HF" = "black"
      ),
      labels = c("Female LF", "Female HF", "Male LF", "Male HF")
    ) +
    scale_pattern_manual(
      values = c(
        "Female LF" = "none",      # Solid white
        "Female HF" = "none",      # Solid black
        "Male LF" = "circle",      # White with black dots
        "Male HF" = "circle"       # Black with white dots
      ),
      labels = c("Female LF", "Female HF", "Male LF", "Male HF")
    ) +
    scale_pattern_fill_manual(
      values = c(
        "Female LF" = "black",     # Not used (pattern=none)
        "Female HF" = "black",     # Not used (pattern=none)
        "Male LF" = "black",       # Black dots on white
        "Male HF" = "white"        # White dots on black
      )
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      expand = expansion(mult = c(0, 0.05), add = 0)
    ) +
    labs(
      x = "Tissue",
      y = y_label,
      fill = NULL,
      pattern = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 12, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.position = c(0.044, 1),
      legend.justification = c(0, 1),
      legend.direction = "horizontal",
      legend.text = element_text(size = 8, face = "plain"),
      legend.key.size = unit(0.3, "cm"),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.key.width = unit(0.3, "cm"),
      legend.spacing.x = unit(-0.05, "cm"),
      legend.margin = margin(b = 0),
      axis.text.x = element_text(face = "bold", color = "black"),
      axis.text.y = element_text(face = "bold", color = "black"),
      axis.title.x = element_text(size = 11, face = "bold", color = "black"),
      axis.title.y = element_text(size = 11, face = "bold", color = "black", margin = margin(r = 12)),
      axis.ticks.length = unit(0.15, "cm"),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.8)
    ) +
    guides(
      fill = guide_legend(override.aes = list(pattern = c("none", "none", "circle", "circle"))),
      pattern = "none"
    )
  
  return(p)
}
