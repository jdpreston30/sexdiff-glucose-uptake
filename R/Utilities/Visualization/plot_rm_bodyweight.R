plot_rm_bodyweight <- function(data, lmm_result, 
                               id_col = "ID", 
                               time_col = "time", 
                               value_col = "BW",
                               sex_col = "sex",
                               diet_col = "diet") {
  #' Plot Repeated Measures Body Weight Data
  #'
  #' Creates a publication-ready plot with specific formatting:
  #' - Males: solid lines, Females: dotted lines
  #' - Male HF: filled black squares, Male LF: white squares with black outline
  #' - Female LF: white triangles, Female HF: black triangles
  #' - P-value in top right corner
  #'
  #' @param data Data frame with body weight data in long format
  #' @param lmm_result Result object from run_lmm()
  #' @param id_col Name of ID column
  #' @param time_col Name of time column
  #' @param value_col Name of value column (BW)
  #' @param sex_col Name of sex column
  #' @param diet_col Name of diet column
  #' @return A ggplot object
  
  library(ggplot2)
  library(dplyr)
  
  # Calculate means and SE for each group
  summary_data <- data %>%
    group_by(!!sym(time_col), !!sym(sex_col), !!sym(diet_col)) %>%
    summarise(
      mean_bw = mean(!!sym(value_col), na.rm = TRUE),
      se_bw = sd(!!sym(value_col), na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      group = paste(!!sym(sex_col), !!sym(diet_col), sep = "_")
    )
  
  # Extract p-value from LMM result
  p_value <- lmm_result$anova["time:sex:diet", "Pr(>F)"]
  p_text <- if (p_value < 0.001) {
    "Time \u00D7 Sex \u00D7 Diet: p < 0.001"
  } else {
    paste0("Time \u00D7 Sex \u00D7 Diet: p = ", round(p_value, 4))
  }
  
  # Reorder factor levels for legend order: M_HF, M_LF, F_HF, F_LF
  summary_data$group <- factor(summary_data$group, 
                                levels = c("M_HF", "M_LF", "F_HF", "F_LF"))
  
  # Create the plot
  p <- ggplot(summary_data, aes(x = !!sym(time_col), y = mean_bw, 
                                 group = group,
                                 shape = group,
                                 fill = group,
                                 linetype = !!sym(sex_col))) +
    # Add lines
    geom_line(linewidth = 0.7) +
    # Add points - filled symbols (M_HF, F_HF)
    geom_point(data = filter(summary_data, group %in% c("M_HF", "F_HF")),
               size = 2, stroke = 0.8) +
    # Add points - unfilled symbols (M_LF, F_LF) - slightly larger
    geom_point(data = filter(summary_data, group %in% c("M_LF", "F_LF")),
               size = 2.2, stroke = 0.8) +
    # Manual scales
    scale_shape_manual(
      values = c(
        "M_HF" = 22,   # filled square (male HF)
        "M_LF" = 22,   # square (male LF)
        "F_HF" = 24,   # filled triangle (female HF)
        "F_LF" = 24    # triangle (female LF)
      ),
      labels = c("M_HF" = "Male HF", "M_LF" = "Male LF", 
                 "F_HF" = "Female HF", "F_LF" = "Female LF"),
      breaks = c("M_HF", "M_LF", "F_HF", "F_LF")
    ) +
    scale_fill_manual(
      values = c(
        "M_HF" = "black",     # filled black
        "M_LF" = "white",     # white
        "F_HF" = "black",     # filled black
        "F_LF" = "white"      # white
      ),
      labels = c("M_HF" = "Male HF", "M_LF" = "Male LF", 
                 "F_HF" = "Female HF", "F_LF" = "Female LF"),
      breaks = c("M_HF", "M_LF", "F_HF", "F_LF")
    ) +
    scale_linetype_manual(
      values = c("M" = "solid", "F" = "twodash")
    ) +
    # Scales
    scale_x_continuous(
      breaks = 0:8,
      limits = c(0, 8),
      expand = expansion(mult = c(0.05, 0.05), add = 0)
    ) +
    scale_y_continuous(
      breaks = seq(15, 40, 5),
      limits = c(15, 40),
      expand = expansion(mult = c(0, 0.05), add = 0)
    ) +
    # Labels
    labs(
      x = "Time (weeks)",
      y = "Body Weight (g)",
      shape = NULL,
      fill = NULL
    ) +
    # Add p-value annotation
    annotate("text", x = 6.8, y = 38.8, label = p_text,
             size = 8/.pt, family = "Arial", fontface = "plain",
             hjust = 1, vjust = 1) +
    # Theme
    theme_classic(base_size = 12, base_family = "Arial") +
    theme(
      legend.position = c(0.02, 1),
      legend.justification = c(0, 1),
      legend.text = element_text(size = 8, face = "plain"),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.spacing.y = unit(-0.15, "cm"),
      legend.spacing.x = unit(0.1, "cm"),
      legend.key.height = unit(0.4, "cm"),
      axis.text = element_text(face = "bold", color = "black"),
      axis.title = element_text(face = "bold", color = "black"),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.ticks.length = unit(0.15, "cm"),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.8)
    ) +
    guides(
      linetype = "none",  # Hide linetype legend (redundant with shape/fill)
      shape = guide_legend(override.aes = list(linetype = c("solid", "solid", "twodash", "twodash"))),
      fill = guide_legend(override.aes = list(linetype = c("solid", "solid", "twodash", "twodash")))
    )
  
  return(p)
}
