plot_gu <- function(data, y_label, y_limits, y_breaks = NULL, bar_width = 0.8, plot_title = NULL, anova_list = NULL, plot_width = NULL, target_bar_width_inches = NULL) {
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
  #' @param bar_width Width of bars (default 0.7), ignored if target_bar_width_inches is specified
  #' @param plot_title Optional plot title
  #' @param anova_list Named list of ANOVA results (from run_anova) for each tissue
  #' @param plot_width Plot width in inches (required if using target_bar_width_inches)
  #' @param target_bar_width_inches Desired absolute bar width in inches (overrides bar_width)
  #' @return A ggplot object
  
  library(ggplot2)
  library(ggpattern)
  library(dplyr)
  library(tidyr)
  
  # Pivot data to long format
  tissue_cols <- setdiff(names(data), c("ID", "sex", "diet"))
  
  # Calculate bar_width if target absolute width is specified
  if (!is.null(target_bar_width_inches)) {
    if (is.null(plot_width)) {
      stop("plot_width must be specified when using target_bar_width_inches")
    }
    n_tissues <- length(tissue_cols)
    # The actual plotting area is approximately 85% of the total plot width
    # (accounting for margins and y-axis labels)
    effective_plot_width <- plot_width * 0.85
    # In ggplot2, bar_width is relative: 1.0 = full width available per tissue
    # Each tissue gets (effective_plot_width / n_tissues) inches
    # We want each bar to be target_bar_width_inches
    # And we apply 0.85 factor in geom_bar_pattern: width = 0.85 * bar_width
    bar_width <- (target_bar_width_inches * n_tissues / effective_plot_width) / 0.85
  }
  
  long_data <- data %>%
    # Remove any rows with all NA values
    filter(!if_all(all_of(tissue_cols), is.na)) %>%
    pivot_longer(
      cols = all_of(tissue_cols),
      names_to = "tissue",
      values_to = "glucose_uptake",
      values_drop_na = TRUE  # Drop NA values during pivot
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
    ) %>%
    # Drop unused factor levels
    droplevels()
  
  # Calculate summary statistics
  summary_data <- long_data %>%
    group_by(tissue, group) %>%
    summarise(
      mean_val = mean(glucose_uptake, na.rm = TRUE),
      sd_val = sd(glucose_uptake, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create plot with simple polka dots
  p <- ggplot(summary_data, aes(x = tissue, y = mean_val, fill = group, pattern = group, pattern_fill = group, pattern_colour = group)) +
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
        keywidth = unit(0.3, "cm"),
        byrow = TRUE
      ),
      pattern = "none",
      pattern_fill = "none",
      pattern_colour = "none"
    )
  
  # Store plot before adding annotations
  base_plot <- p
  
  # Add statistical annotations if ANOVA results provided
  if (!is.null(anova_list)) {
    # Get tissue column names from data
    tissue_cols <- setdiff(names(data), c("ID", "sex", "diet"))
    
    # Extract p-values for each tissue and add as geom_text with negative y
    for (i in seq_along(tissue_cols)) {
      tissue_name <- tissue_cols[i]
      if (tissue_name %in% names(anova_list)) {
        anova_result <- anova_list[[tissue_name]]
        p_sex <- anova_result$anova_table["sex", "Pr(>F)"]
        p_diet <- anova_result$anova_table["diet", "Pr(>F)"]
        p_int <- anova_result$anova_table["sex:diet", "Pr(>F)"]
        
        # Format p-values using journal requirements
        label_text <- paste0(
          "Sex: ", format_p_journal(p_sex), "\n",
          "Diet: ", format_p_journal(p_diet), "\n",
          "Int.: ", format_p_journal(p_int)
        )
        
        # Add text below x-axis (using negative y values in data space)
        p <- p + annotate(
          "text",
          x = i,
          y = -Inf,
          label = label_text,
          size = 8 / .pt,
          hjust = 0.5,
          vjust = 1.75,
          fontface = "plain",
          lineheight = 0.9
        )
      }
    }
  }
  
  return(p)
}
