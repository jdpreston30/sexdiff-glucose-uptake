plot_anova_barplot <- function(data, anova_result,
                               response,
                               factor1 = "sex",
                               factor2 = "diet",
                               y_label,
                               y_limits,
                               y_breaks = NULL) {
  #' Plot Two-Way ANOVA Results as Bar Plot
  #'
  #' Creates a bar plot with:
  #' - X-axis: Male and Female
  #' - Grouped bars: LF (white) and HF (black)
  #' - Error bars showing SE
  #'
  #' @param data Data frame with raw data
  #' @param anova_result Result object from run_anova()
  #' @param response Name of response variable
  #' @param factor1 Name of first factor (default: "sex")
  #' @param factor2 Name of second factor (default: "diet")
  #' @param y_label Y-axis label
  #' @param y_limits Y-axis limits as c(min, max)
  #' @return A ggplot object
  
  library(ggplot2)
  library(dplyr)
  
  # Calculate means and SE
  summary_data <- data %>%
    group_by(!!sym(factor1), !!sym(factor2)) %>%
    summarise(
      mean_val = mean(!!sym(response), na.rm = TRUE),
      sd_val = sd(!!sym(response), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Create group name before converting factors
      group_name = paste(!!sym(factor1), !!sym(factor2), sep = "_")
    )
  
  # Extract p-values from ANOVA result
  anova_table <- anova_result$anova_table
  p_sex <- anova_table[grep(paste0("^", factor1, "$"), rownames(anova_table), ignore.case = TRUE), "Pr(>F)"][1]
  p_diet <- anova_table[grep(paste0("^", factor2, "$"), rownames(anova_table), ignore.case = TRUE), "Pr(>F)"][1]
  p_interaction <- anova_table[grep(":", rownames(anova_table)), "Pr(>F)"][1]
  
  # Format p-values using journal requirements
  p_text <- paste0(
    "Sex: ", format_p_journal(p_sex), "\n",
    "Diet: ", format_p_journal(p_diet), "\n",
    "Int.: ", format_p_journal(p_interaction)
  )
  
  # Add CLD letters if interaction is significant
  if (!is.null(anova_result$posthoc$cld) && !is.na(p_interaction) && p_interaction < 0.05) {
    # Add CLD letters to summary data (join before converting factor labels)
    cld_df <- anova_result$posthoc$cld
    summary_data <- summary_data %>%
      left_join(
        cld_df %>% select(group, letter),
        by = c("group_name" = "group")
      )
  } else {
    summary_data$letter <- NA
  }
  
  # Now convert factors for plotting
  summary_data <- summary_data %>%
    mutate(
      !!sym(factor1) := factor(!!sym(factor1), 
                               levels = c("F", "M"), 
                               labels = c("Female", "Male")),
      !!sym(factor2) := factor(!!sym(factor2),
                               levels = c("LF", "HF"))
    )
  
  # Create plot
  p <- ggplot(summary_data, aes(x = !!sym(factor1), y = mean_val, fill = !!sym(factor2))) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7,
             color = "black", linewidth = 0.6) +
    geom_errorbar(aes(ymin = mean_val, ymax = mean_val + sd_val),
                  position = position_dodge(0.8), width = 0.25, linewidth = 0.5) +
    scale_fill_manual(
      values = c("LF" = "white", "HF" = "black"),
      labels = c("LF" = "LF", "HF" = "HF")
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      expand = expansion(mult = c(0, 0.05), add = 0)
    ) +
    labs(
      x = "Sex",
      y = y_label,
      fill = NULL
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
    )
  
  # Add CLD letters if present
  if (!all(is.na(summary_data$letter))) {
    # Calculate letter position (above error bar)
    # Use relative offset based on y-axis range
    letter_height <- (y_limits[2] - y_limits[1]) * 0.05
    summary_data <- summary_data %>%
      mutate(letter_y = mean_val + sd_val + letter_height)
    
    p <- p + geom_text(
      data = summary_data,
      aes(x = !!sym(factor1), y = letter_y, label = letter, group = !!sym(factor2)),
      position = position_dodge(0.8),
      size = 10/.pt,  # Same size as axis text
      fontface = "bold",
      family = "Arial"
    )
  }
  
  # Add p-value annotation
  # Position left-aligned to match legend at x=0.044 (converted to data coordinates)
  p <- p + annotate("text", 
                    x = 0.49, 
                    y = y_limits[2] * 0.95, 
                    label = p_text,
                    size = 8/.pt,  # Same size as legend
                    family = "Arial", 
                    fontface = "plain",
                    hjust = 0, 
                    vjust = 1,
                    lineheight = 0.85)
  
  return(p)
}
