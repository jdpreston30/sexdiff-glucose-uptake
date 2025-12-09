plot_rm_ogtt <- function(data, rmanova_result, 
                         id_col = "ID", 
                         time_col = "time", 
                         value_col = "OGTT_BG",
                         sex_col = "sex",
                         diet_col = "diet",
                         plot_title = NULL) {
  #' Plot Repeated Measures OGTT Data
  #'
  #' Creates a publication-ready plot with specific formatting:
  #' - Males: solid lines, Females: dotted lines
  #' - Male HF: filled black squares, Male LF: white squares with black outline
  #' - Female LF: white triangles, Female HF: black triangles
  #' - P-values in top right corner
  #'
  #' @param data Data frame with OGTT data in long format
  #' @param rmanova_result Result object from run_rm_anova()
  #' @param id_col Name of ID column
  #' @param time_col Name of time column
  #' @param value_col Name of value column (OGTT_BG)
  #' @param sex_col Name of sex column
  #' @param diet_col Name of diet column
  #' @param plot_title Optional title for the plot (size 14, Arial bold, centered)
  #' @return A ggplot object
  
  library(ggplot2)
  library(dplyr)
  
  # Calculate means and SE for each group
  summary_data <- data %>%
    group_by(!!sym(time_col), !!sym(sex_col), !!sym(diet_col)) %>%
    summarise(
      mean_bg = mean(!!sym(value_col), na.rm = TRUE),
      se_bg = sd(!!sym(value_col), na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      group = paste(!!sym(sex_col), !!sym(diet_col), sep = "_")
    )
  
  # Convert time to numeric for plotting
  summary_data[[time_col]] <- as.numeric(as.character(summary_data[[time_col]]))
  
  # Extract p-values from RM-ANOVA result
  anova_table <- rmanova_result$anova_table
  
  # Sex × Diet p-value
  sex_diet_p <- anova_table$p[anova_table$Effect == "sex:diet"]
  sex_diet_text <- if (sex_diet_p < 0.001) {
    "Sex \u00D7 Diet: p < 0.001"
  } else {
    paste0("Sex \u00D7 Diet: p = ", sprintf("%.3f", sex_diet_p))
  }
  
  # Diet × Time p-value (use corrected if sphericity violated)
  diet_time_row <- anova_table$Effect == "diet:time"
  
  # Check if sphericity corrections exist
  if (!is.null(rmanova_result$sphericity) && "diet:time" %in% rmanova_result$sphericity$Effect) {
    # Use Greenhouse-Geisser corrected p-value
    diet_time_p <- rmanova_result$sphericity$`p[GG]`[rmanova_result$sphericity$Effect == "diet:time"]
  } else {
    diet_time_p <- anova_table$p[diet_time_row]
  }
  
  diet_time_text <- if (diet_time_p < 0.001) {
    "Time \u00D7 Diet: p < 0.001"
  } else {
    paste0("Time \u00D7 Diet: p = ", sprintf("%.3f", diet_time_p))
  }
  
  # Combine p-value text
  p_text <- paste(sex_diet_text, diet_time_text, sep = "\n")
  
  # Reorder factor levels for legend order: M_LF, M_HF, F_LF, F_HF
  summary_data$group <- factor(summary_data$group, 
                                levels = c("M_LF", "M_HF", "F_LF", "F_HF"))
  
  # Create the plot
  p <- ggplot(summary_data, aes(x = !!sym(time_col), y = mean_bg, 
                                 group = group,
                                 shape = group,
                                 fill = group,
                                 linetype = !!sym(sex_col))) +
    # Add lines
    geom_line(linewidth = 0.7) +
    # Add points - male squares (M_HF, M_LF) - scaled up to match triangle height
    geom_point(data = filter(summary_data, group %in% c("M_HF", "M_LF")),
               size = 2.94, stroke = 0.8) +
    # Add points - female triangles (F_HF, F_LF)
    geom_point(data = filter(summary_data, group %in% c("F_HF", "F_LF")),
               size = 2.2, stroke = 0.8) +
    # Manual scales
    scale_shape_manual(
      values = c(
        "M_HF" = 22,   # filled square (male HF)
        "M_LF" = 22,   # square (male LF)
        "F_HF" = 24,   # filled triangle (female HF)
        "F_LF" = 24    # triangle (female LF)
      ),
      labels = c("M_LF" = "Male LF", "F_LF" = "Female LF",
                 "M_HF" = "Male HF", "F_HF" = "Female HF"),
      breaks = c("F_LF", "F_HF", "M_LF", "M_HF")
    ) +
    scale_fill_manual(
      values = c(
        "M_HF" = "black",     # filled black
        "M_LF" = "white",     # white
        "F_HF" = "black",     # filled black
        "F_LF" = "white"      # white
      ),
      labels = c("M_LF" = "Male LF", "F_LF" = "Female LF",
                 "M_HF" = "Male HF", "F_HF" = "Female HF"),
      breaks = c("F_LF", "F_HF", "M_LF", "M_HF")
    ) +
    scale_linetype_manual(
      values = c("M" = "solid", "F" = "twodash")
    ) +
    # Scales
    scale_x_continuous(
      breaks = c(0, 15, 30, 45),
      limits = c(0, 45),
      expand = expansion(mult = c(0.05, 0.05), add = 0)
    ) +
    scale_y_continuous(
      breaks = seq(50, 350, 50),
      limits = c(50, 350),
      expand = expansion(mult = c(0, 0.05), add = 0)
    ) +
    # Labels
    labs(
      x = "Time (minutes)",
      y = "Glucose (mg/dL)",
      title = plot_title,
      shape = NULL,
      fill = NULL
    ) +
    # Add p-value annotation using normalized coordinates
    annotation_custom(
      grob = grid::textGrob(
        label = p_text,
        x = 0.971, y = 1.001,
        hjust = 1, vjust = 1,
        gp = grid::gpar(fontsize = 8, fontfamily = "Arial", fontface = "plain", lineheight = 1.17)
      ),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    # Allow annotations outside plot area
    coord_cartesian(clip = "off") +
    # Theme
    theme_classic(base_size = 12, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.position = c(-0.01, 1.058),
      legend.justification = c(0, 1),
      legend.text = element_text(size = 8, face = "plain"),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.spacing.y = unit(-0.15, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(0.4, "cm"),
      axis.text = element_text(face = "bold", color = "black"),
      axis.title = element_text(size = 11, face = "bold", color = "black"),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.ticks.length = unit(0.15, "cm"),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.8),
      plot.title = element_text(size = 14, face = "bold", family = "Arial", 
                                hjust = 0.5, color = "black")
    ) +
    guides(
      linetype = "none",  # Hide linetype legend (redundant with shape/fill)
      shape = guide_legend(ncol = 1, byrow = TRUE, 
                          override.aes = list(linetype = c("twodash", "twodash", "solid", "solid"))),
      fill = guide_legend(ncol = 1, byrow = TRUE,
                         override.aes = list(linetype = c("twodash", "twodash", "solid", "solid")))
    )
  
  return(p)
}
