plot_gu_ev <- function(data, y_label, y_limits, y_breaks = NULL, bar_width = 0.8, plot_title = NULL, mixed_anova_result = NULL) {
  #' Plot Ex Vivo Glucose Uptake
  #'
  #' Creates a grouped bar plot showing ex vivo glucose uptake with:
  #' - X-axis: Treatment (Basal vs Insulin)
  #' - Grouped bars: Female LF, Female HF, Male LF, Male HF (left to right)
  #' - Error bars showing SE
  #' - Patterned fills: Female=solid, Male=polka dots
  #' - Statistics displayed in top right corner
  #' - Legend on the right side
  #'
  #' @param data Data frame with columns: ID, sex, diet, EV_tx, and response column
  #' @param y_label Y-axis label
  #' @param y_limits Y-axis limits as c(min, max)
  #' @param y_breaks Optional y-axis breaks
  #' @param bar_width Width of bars (default 0.8)
  #' @param plot_title Optional plot title
  #' @param mixed_anova_result Mixed ANOVA result object from run_mixed_anova
  #' @return A ggplot object
  
  library(ggplot2)
  library(ggpattern)
  library(dplyr)
  library(tidyr)
  
  # Get response column (assume it's the last column that's not ID, sex, diet, EV_tx)
  response_col <- setdiff(names(data), c("ID", "sex", "diet", "EV_tx"))[1]
  
  # Prepare data
  plot_data <- data %>%
    filter(!is.na(!!sym(response_col))) %>%
    mutate(
      EV_tx = factor(EV_tx, levels = c("Basal", "Insulin")),
      sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
      diet = factor(diet, levels = c("LF", "HF")),
      # Create group variable for ordering
      group = factor(
        paste(sex, diet, sep = "_"),
        levels = c("Female_LF", "Female_HF", "Male_LF", "Male_HF"),
        labels = c("Female LF", "Female HF", "Male LF", "Male HF")
      )
    ) %>%
    rename(response = !!sym(response_col)) %>%
    droplevels()  # Drop unused factor levels
  
  # Calculate summary statistics
  summary_data <- plot_data %>%
    group_by(EV_tx, group, .drop = TRUE) %>%
    summarise(
      mean_val = mean(response, na.rm = TRUE),
      sd_val = sd(response, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create plot
  p <- ggplot(summary_data, aes(x = EV_tx, y = mean_val, fill = group, pattern = group, pattern_fill = group, pattern_colour = group)) +
    geom_bar_pattern(
      stat = "identity", 
      position = position_dodge(0.75), 
      width = 0.85 * bar_width,
      color = "black", 
      linewidth = 0.6,
      pattern_density = 0.4,
      pattern_spacing = 0.01,
      pattern_angle = 0
    ) +
    geom_errorbar(
      aes(ymin = mean_val, ymax = mean_val + sd_val),
      position = position_dodge(0.75), 
      width = 0.25, 
      linewidth = 0.5
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
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      expand = expansion(mult = c(0, 0.05), add = 0)
    ) +
    labs(
      x = NULL,
      y = y_label,
      title = plot_title,
      fill = NULL,
      pattern = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 12, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13, family = "Arial"),
      plot.margin = margin(t = 5, r = 5, b = 80, l = 5, unit = "pt"),
      axis.text.x = element_text(face = "bold", color = "black", size = 11),
      axis.text.y = element_text(face = "bold", color = "black"),
      axis.title.x = element_text(size = 11, face = "bold", color = "black"),
      axis.title.y = element_text(size = 11, face = "bold", color = "black", margin = margin(r = 12)),
      axis.ticks.length = unit(0.15, "cm"),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.8)
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(
          pattern = c("none", "none", "circle", "circle"),
          pattern_fill = c("black", "black", "black", "white"),
          pattern_colour = c(NA, NA, NA, NA)
        ),
        keyheight = unit(0.8, "cm"),
        keywidth = unit(1.2, "cm"),
        byrow = TRUE
      ),
      pattern = "none",
      pattern_fill = "none",
      pattern_colour = "none"
    )
  
  # Add statistical annotations in top right corner if mixed ANOVA result provided
  if (!is.null(mixed_anova_result)) {
    # Extract p-values from anova_table
    p_sex <- mixed_anova_result$anova_table["sex", "Pr(>F)"]
    p_diet <- mixed_anova_result$anova_table["diet", "Pr(>F)"]
    p_tx <- mixed_anova_result$anova_table["EV_tx", "Pr(>F)"]
    
    # Check all interactions
    interaction_rows <- c("EV_tx:sex", "EV_tx:diet", "sex:diet", "EV_tx:sex:diet")
    interaction_pvals <- mixed_anova_result$anova_table[interaction_rows, "Pr(>F)"]
    
    # Format interaction text
    if (all(interaction_pvals >= 0.05, na.rm = TRUE)) {
      int_text <- "Interactions: n.s."
    } else {
      # Format each interaction p-value using journal requirements
      int_labels <- c("Tx×Sex", "Tx×Diet", "Sex×Diet", "Tx×Sex×Diet")
      int_formatted <- sapply(seq_along(interaction_pvals), function(i) {
        p_val <- interaction_pvals[i]
        if (!is.na(p_val)) {
          paste0(int_labels[i], ": ", format_p_journal(p_val))
        } else {
          NULL
        }
      })
      int_text <- paste(int_formatted[!sapply(int_formatted, is.null)], collapse = "\n")
    }
    
    # Format main effects with lineheight control
    label_text <- paste0(
      "Sex: ", format_p_journal(p_sex), "\n",
      "Diet: ", format_p_journal(p_diet), "\n",
      "Insulin: ", format_p_journal(p_tx), "\n",
      int_text
    )
    
    # Add text annotation in top left corner with left-justified text
    p <- p + annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = label_text,
      size = 8 / .pt,
      hjust = -0.171,
      vjust = 1.08, # down is up
      fontface = "plain",
      lineheight = 0.9
    )
  }
  
  # Add invisible dummy text below x-axis for consistent spacing with other plots
  p <- p + annotate(
    "text",
    x = 1,
    y = -Inf,
    label = "Sex: p=0.000\nDiet: p=0.000\nInt: p=0.000",
    size = 8 / .pt,
    hjust = 0.5,
    vjust = 2.5,
    fontface = "plain",
    color = "transparent"  # Make it invisible
  )
  
  return(p)
}
