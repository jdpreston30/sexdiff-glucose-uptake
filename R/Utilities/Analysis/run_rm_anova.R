#' Run Repeated Measures ANOVA with Diagnostics
#'
#' Performs repeated measures ANOVA for data with multiple time points.
#' Appropriate for non-linear temporal patterns (e.g., OGTT curves).
#' Tests sphericity assumption and applies corrections if needed.
#'
#' @param data A data frame in long format
#' @param id_col Name of the subject ID column (as string)
#' @param time_col Name of the time variable column (as string, will be treated as factor)
#' @param value_col Name of the dependent variable column (as string)
#' @param between_factors Vector of between-subject factor column names (as strings)
#' @param alpha Significance threshold (default: 0.05)
#'
#' @return A list containing:
#'   \item{anova_table}{ANOVA results table}
#'   \item{sphericity}{Mauchly's test results and corrections}
#'   \item{descriptives}{Mean and SE by group and time}
#'   \item{model}{The fitted model object}
#'   \item{assumptions_met}{Logical indicating if assumptions satisfied}
#'   \item{warnings}{Character vector of any violations}
#'
#' @examples
#' \dontrun{
#'   result <- run_rm_anova(
#'     data = OGTT,
#'     id_col = "ID",
#'     time_col = "time",
#'     value_col = "OGTT_BG",
#'     between_factors = c("sex", "diet")
#'   )
#' }
#'
#' @importFrom ez ezANOVA
#' @importFrom car Anova leveneTest
#' @importFrom dplyr group_by summarise
#' @export
run_rm_anova <- function(data, id_col, time_col, value_col, between_factors = NULL, 
                         alpha = 0.05) {
  
  library(ez)
  library(car)
  library(dplyr)
  
  cat("\n")
  cat(strrep("=", 72))
  cat("\n")
  cat("REPEATED MEASURES ANOVA WITH DIAGNOSTICS\n")
  cat(strrep("=", 72))
  cat("\n\n")
  
  # Initialize results
  result <- list()
  warnings_list <- character(0)
  assumptions_met <- TRUE
  
  # Data preparation
  cat("DATA PREPARATION\n")
  cat(strrep("-", 72))
  cat("\n")
  
  # Ensure correct data types
  data[[id_col]] <- as.factor(data[[id_col]])
  data[[time_col]] <- as.factor(data[[time_col]])
  
  if (!is.null(between_factors)) {
    for (factor in between_factors) {
      data[[factor]] <- as.factor(data[[factor]])
    }
  }
  
  # Check for complete cases
  n_complete <- sum(complete.cases(data[, c(id_col, time_col, value_col, between_factors)]))
  n_total <- nrow(data)
  cat("Complete cases:", n_complete, "/", n_total, "\n")
  
  if (n_complete < n_total) {
    cat("WARNING: Missing data detected. RM-ANOVA uses listwise deletion.\n")
    warnings_list <- c(warnings_list, "Missing data present")
  }
  
  # Remove missing data
  data <- data[complete.cases(data[, c(id_col, time_col, value_col, between_factors)]), ]
  
  # Check balance
  n_per_subject <- data %>%
    group_by(!!sym(id_col)) %>%
    summarise(n = n(), .groups = "drop")
  
  if (length(unique(n_per_subject$n)) > 1) {
    cat("WARNING: Unbalanced design (different number of observations per subject)\n")
    warnings_list <- c(warnings_list, "Unbalanced design")
    assumptions_met <- FALSE
  } else {
    cat("Balanced design:", unique(n_per_subject$n), "observations per subject\n")
  }
  
  cat("\n")
  
  # Descriptive statistics
  cat(strrep("=", 72))
  cat("\n")
  cat("DESCRIPTIVE STATISTICS\n")
  cat(strrep("=", 72))
  cat("\n\n")
  
  if (!is.null(between_factors)) {
    group_cols <- c(time_col, between_factors)
  } else {
    group_cols <- time_col
  }
  
  descriptives <- data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n = n(),
      mean = mean(!!sym(value_col), na.rm = TRUE),
      sd = sd(!!sym(value_col), na.rm = TRUE),
      se = sd / sqrt(n),
      .groups = "drop"
    )
  
  print(descriptives, n = Inf)
  cat("\n")
  
  result$descriptives <- descriptives
  
  # Assumption testing
  cat(strrep("=", 72))
  cat("\n")
  cat("ASSUMPTION TESTING\n")
  cat(strrep("=", 72))
  cat("\n\n")
  
  # Normality
  cat("TEST 1: NORMALITY OF RESIDUALS\n")
  cat(strrep("-", 72))
  cat("\n")
  
  # Fit simple ANOVA model to get residuals
  if (!is.null(between_factors)) {
    formula_str <- paste(value_col, "~", paste(c(time_col, between_factors), collapse = " * "))
  } else {
    formula_str <- paste(value_col, "~", time_col)
  }
  
  simple_model <- aov(as.formula(formula_str), data = data)
  resids <- residuals(simple_model)
  
  # Sample if too many observations
  if (length(resids) > 5000) {
    resids_sample <- sample(resids, 5000)
    cat("Note: Sampling 5000 residuals for Shapiro-Wilk test\n")
  } else {
    resids_sample <- resids
  }
  
  shapiro_result <- shapiro.test(resids_sample)
  cat("Shapiro-Wilk Test:\n")
  cat("  Test Statistic:", round(shapiro_result$statistic, 4), "\n")
  cat("  p-value:", round(shapiro_result$p.value, 4), "\n")
  
  if (shapiro_result$p.value > alpha) {
    cat("  PASS: Residuals appear normally distributed (p > 0.05)\n\n")
  } else {
    cat("  WARNING: Residuals may not be normally distributed (p < 0.05)\n")
    cat("  RM-ANOVA is fairly robust with n > 30 per group\n\n")
    warnings_list <- c(warnings_list, "Normality violation")
  }
  
  # Homogeneity of variance
  cat("TEST 2: HOMOGENEITY OF VARIANCE\n")
  cat(strrep("-", 72))
  cat("\n")
  
  if (!is.null(between_factors)) {
    interaction_formula <- paste(value_col, "~", paste(c(time_col, between_factors), collapse = " * "))
    levene_result <- leveneTest(as.formula(interaction_formula), data = data)
    
    cat("Levene's Test:\n")
    cat("  F-statistic:", round(levene_result$`F value`[1], 4), "\n")
    cat("  p-value:", round(levene_result$`Pr(>F)`[1], 4), "\n")
    
    if (levene_result$`Pr(>F)`[1] > alpha) {
      cat("  PASS: Variances are homogeneous (p > 0.05)\n\n")
    } else {
      cat("  WARNING: Variances may be unequal (p < 0.05)\n\n")
      warnings_list <- c(warnings_list, "Heterogeneity of variance")
    }
  } else {
    cat("  Skipped (no between-subjects factors)\n\n")
  }
  
  # Run RM-ANOVA
  cat(strrep("=", 72))
  cat("\n")
  cat("REPEATED MEASURES ANOVA RESULTS\n")
  cat(strrep("=", 72))
  cat("\n\n")
  
  # Run ezANOVA using do.call to properly pass column names
  ez_args <- list(
    data = data,
    dv = as.name(value_col),
    wid = as.name(id_col),
    within = as.name(time_col),
    type = 3,
    detailed = TRUE,
    return_aov = TRUE
  )
  
  if (!is.null(between_factors)) {
    ez_args$between <- lapply(between_factors, as.name)
  }
  
  ez_result <- do.call(ezANOVA, ez_args)
  
  # Display ANOVA table
  cat("ANOVA Table:\n")
  print(ez_result$ANOVA)
  cat("\n")
  
  result$anova_table <- ez_result$ANOVA
  result$model <- ez_result$aov
  
  # Test sphericity assumption
  cat(strrep("=", 72))
  cat("\n")
  cat("SPHERICITY ASSUMPTION\n")
  cat(strrep("=", 72))
  cat("\n\n")
  
  if (!is.null(ez_result$`Mauchly's Test for Sphericity`)) {
    cat("Mauchly's Test Results:\n")
    print(ez_result$`Mauchly's Test for Sphericity`)
    cat("\n")
    
    # Check if any violations
    sphericity_violations <- ez_result$`Mauchly's Test for Sphericity`$p < alpha
    
    if (any(sphericity_violations, na.rm = TRUE)) {
      cat("WARNING: Sphericity assumption violated for some effects\n")
      cat("Using corrected p-values (Greenhouse-Geisser or Huynh-Feldt)\n\n")
      
      if (!is.null(ez_result$`Sphericity Corrections`)) {
        cat("Sphericity Corrections:\n")
        print(ez_result$`Sphericity Corrections`)
        cat("\n")
        result$sphericity <- ez_result$`Sphericity Corrections`
      }
      
      warnings_list <- c(warnings_list, "Sphericity violation")
      assumptions_met <- FALSE
    } else {
      cat("PASS: Sphericity assumption met\n\n")
      result$sphericity <- ez_result$`Mauchly's Test for Sphericity`
    }
  } else {
    cat("Note: Sphericity test not applicable\n\n")
  }
  
  # Summary
  cat(strrep("=", 72))
  cat("\n")
  cat("ANALYSIS SUMMARY\n")
  cat(strrep("=", 72))
  cat("\n\n")
  
  result$assumptions_met <- assumptions_met && length(warnings_list) == 0
  result$warnings <- warnings_list
  
  if (length(warnings_list) > 0) {
    cat("WARNINGS:\n")
    for (warning in warnings_list) {
      cat("  -", warning, "\n")
    }
    cat("\n")
  } else {
    cat("All assumptions satisfied\n\n")
  }
  
  # Significant effects
  cat("SIGNIFICANT EFFECTS (p < 0.05):\n")
  sig_effects <- ez_result$ANOVA[ez_result$ANOVA$p < alpha, ]
  if (nrow(sig_effects) > 0) {
    print(sig_effects[, c("Effect", "F", "p")])
  } else {
    cat("  None\n")
  }
  cat("\n")
  
  cat(strrep("=", 72))
  cat("\n\n")
  
  return(result)
}
