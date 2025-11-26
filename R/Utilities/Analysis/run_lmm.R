run_lmm <- function(data, id_col, time_col, value_col, between_factors = NULL, 
                    random_slope = TRUE) {
  #' Run Linear Mixed Model with Diagnostics
  #'
  #' @param data A data frame in long format
  #' @param id_col Name of the subject ID column (as string)
  #' @param time_col Name of the time variable column (as string, should be numeric)
  #' @param value_col Name of the dependent variable column (as string)
  #' @param between_factors Vector of between-subject factor column names (as strings)
  #' @param random_slope Logical, include random slope for time? Default TRUE
  #'
  #' @return A list containing the model, ANOVA table, and diagnostic results
  #' @examples
  #' # result <- run_lmm(data, "ID", "time", "BW", c("sex", "diet"))
  
  library(lme4)
  library(car)
  library(dplyr)
  library(broom.mixed)
  
  cat("\n" %+% strrep("=", 72) %+% "\n")
  cat("LINEAR MIXED MODEL ANALYSIS WITH DIAGNOSTICS\n")
  cat(strrep("=", 72) %+% "\n\n")
  
  # =====================================================================
  # 1. BUILD AND FIT THE MODEL
  # =====================================================================
  cat("📈 BUILDING LINEAR MIXED MODEL\n")
  cat("-" %+% strrep("-", 68) %+% "\n")
  
  # Ensure factors are factors
  data[[id_col]] <- as.factor(data[[id_col]])
  if (!is.null(between_factors)) {
    for (factor in between_factors) {
      data[[factor]] <- as.factor(data[[factor]])
    }
  }
  
  # Build formula
  if (!is.null(between_factors) && length(between_factors) > 0) {
    fixed_effects <- paste(c(time_col, between_factors), collapse = " * ")
  } else {
    fixed_effects <- time_col
  }
  
  # Random effects structure
  if (random_slope) {
    random_effects <- paste0("(1 + ", time_col, " | ", id_col, ")")
    cat("Random effects: Random intercept + random slope for", time_col, "\n")
  } else {
    random_effects <- paste0("(1 | ", id_col, ")")
    cat("Random effects: Random intercept only\n")
  }
  
  # Complete formula
  formula_str <- paste(value_col, "~", fixed_effects, "+", random_effects)
  cat("Model formula:", formula_str, "\n\n")
  
  # Fit the model
  cat("Fitting model...\n")
  model <- lmer(as.formula(formula_str), data = data, REML = TRUE)
  cat("✅ Model fitted successfully\n\n")
  
  # =====================================================================
  # 2. MODEL SUMMARY
  # =====================================================================
  cat(strrep("=", 72) %+% "\n")
  cat("MODEL SUMMARY\n")
  cat(strrep("=", 72) %+% "\n")
  print(summary(model))
  
  # =====================================================================
  # 3. TYPE III ANOVA TABLE
  # =====================================================================
  cat("\n" %+% strrep("=", 72) %+% "\n")
  cat("TYPE III ANOVA TABLE (using car::Anova)\n")
  cat(strrep("=", 72) %+% "\n")
  anova_table <- car::Anova(model, type = 3, test.statistic = "F")
  print(anova_table)
  cat("\n")
  
  # =====================================================================
  # 4. DIAGNOSTIC CHECKS
  # =====================================================================
  cat(strrep("=", 72) %+% "\n")
  cat("DIAGNOSTIC CHECKS\n")
  cat(strrep("=", 72) %+% "\n\n")
  
  # Extract residuals
  residuals <- residuals(model)
  fitted_vals <- fitted(model)
  
  # 4.1 Normality of residuals (Shapiro-Wilk)
  cat("📊 TEST 1: NORMALITY OF RESIDUALS\n")
  cat("-" %+% strrep("-", 68) %+% "\n")
  
  # Sample if too many observations (Shapiro-Wilk limited to 5000)
  if (length(residuals) > 5000) {
    residuals_sample <- sample(residuals, 5000)
    cat("Note: Sampling 5000 residuals for Shapiro-Wilk test\n")
  } else {
    residuals_sample <- residuals
  }
  
  shapiro_result <- shapiro.test(residuals_sample)
  cat("Shapiro-Wilk Test:\n")
  cat("  Test Statistic:", round(shapiro_result$statistic, 4), "\n")
  cat("  p-value:", round(shapiro_result$p.value, 4), "\n")
  
  if (shapiro_result$p.value > 0.05) {
    cat("  ✅ PASS: Residuals appear normally distributed (p > 0.05)\n\n")
  } else {
    cat("  ⚠️  WARNING: Residuals may not be normally distributed (p < 0.05)\n")
    cat("     LMM is robust to mild violations with large samples\n\n")
  }
  
  # 4.2 Homoscedasticity (visual inspection via residual plots)
  cat("📊 TEST 2: HOMOSCEDASTICITY (Residuals vs Fitted)\n")
  cat("-" %+% strrep("-", 68) %+% "\n")
  
  # Calculate correlation between absolute residuals and fitted values
  # as a rough test for heteroscedasticity
  het_cor <- cor.test(abs(residuals), fitted_vals)
  cat("Correlation between |residuals| and fitted values:\n")
  cat("  Correlation:", round(het_cor$estimate, 4), "\n")
  cat("  p-value:", round(het_cor$p.value, 4), "\n")
  
  if (het_cor$p.value > 0.05) {
    cat("  ✅ PASS: No strong evidence of heteroscedasticity (p > 0.05)\n\n")
  } else {
    cat("  ⚠️  WARNING: Possible heteroscedasticity detected (p < 0.05)\n")
    cat("     Consider variance weights or transformation\n\n")
  }
  
  # 4.3 Linearity check (for continuous predictors)
  cat("📊 TEST 3: LINEARITY CHECK\n")
  cat("-" %+% strrep("-", 68) %+% "\n")
  
  # Check if time effect appears linear
  time_values <- data[[time_col]]
  resid_by_time <- tapply(residuals, time_values, mean)
  
  cat("Mean residuals by timepoint:\n")
  print(round(resid_by_time, 4))
  
  # Check if mean residuals are close to zero across time
  max_mean_resid <- max(abs(resid_by_time))
  if (max_mean_resid < 0.5) {
    cat("  ✅ PASS: Mean residuals close to zero across time\n\n")
  } else {
    cat("  ⚠️  WARNING: Some mean residuals deviate from zero\n")
    cat("     Consider non-linear time effects or interactions\n\n")
  }
  
  # 4.4 Random effects check
  cat("📊 TEST 4: RANDOM EFFECTS DISTRIBUTION\n")
  cat("-" %+% strrep("-", 68) %+% "\n")
  
  ranef_vals <- ranef(model)[[1]]
  cat("Random effects summary:\n")
  print(summary(ranef_vals))
  cat("\n")
  
  # Check normality of random intercepts
  if (nrow(ranef_vals) >= 3) {
    ranef_shapiro <- shapiro.test(ranef_vals[, 1])
    cat("Shapiro-Wilk test on random intercepts:\n")
    cat("  p-value:", round(ranef_shapiro$p.value, 4), "\n")
    
    if (ranef_shapiro$p.value > 0.05) {
      cat("  ✅ PASS: Random effects appear normally distributed\n\n")
    } else {
      cat("  ⚠️  WARNING: Random effects may not be normally distributed\n\n")
    }
  }
  
  # =====================================================================
  # 5. MODEL FIT STATISTICS
  # =====================================================================
  cat(strrep("=", 72) %+% "\n")
  cat("MODEL FIT STATISTICS\n")
  cat(strrep("=", 72) %+% "\n")
  
  cat("AIC:", round(AIC(model), 2), "\n")
  cat("BIC:", round(BIC(model), 2), "\n")
  cat("Log-Likelihood:", round(logLik(model), 2), "\n")
  
  # Marginal and conditional R-squared
  cat("\nVariance explained:\n")
  r2_vals <- tryCatch({
    performance::r2(model)
  }, error = function(e) {
    list(R2_conditional = NA, R2_marginal = NA)
  })
  
  if (!is.na(r2_vals$R2_conditional)) {
    cat("  Conditional R² (total):", round(r2_vals$R2_conditional, 4), "\n")
    cat("  Marginal R² (fixed only):", round(r2_vals$R2_marginal, 4), "\n")
  }
  
  # =====================================================================
  # 6. SUMMARY & RECOMMENDATIONS
  # =====================================================================
  cat("\n" %+% strrep("=", 72) %+% "\n")
  cat("SUMMARY & RECOMMENDATIONS\n")
  cat(strrep("=", 72) %+% "\n\n")
  
  cat("✓ Model diagnostics checked:\n")
  cat("  • Normality of residuals (Shapiro-Wilk)\n")
  cat("  • Homoscedasticity (Residuals vs Fitted)\n")
  cat("  • Linearity (Mean residuals by time)\n")
  cat("  • Random effects distribution\n\n")
  
  cat("📝 Interpretation:\n")
  cat("  • Use Type III ANOVA table for hypothesis testing\n")
  cat("  • Check diagnostic plots: plot(model) for visual inspection\n")
  cat("  • Use emmeans package for post-hoc comparisons if needed\n")
  cat("  • Report both fixed effects (from ANOVA) and random effects structure\n")
  
  cat("\n" %+% strrep("=", 72) %+% "\n\n")
  
  # Return results
  return(list(
    model = model,
    anova = anova_table,
    diagnostics = list(
      shapiro = shapiro_result,
      heteroscedasticity = het_cor,
      residuals = residuals,
      fitted = fitted_vals,
      random_effects = ranef_vals
    ),
    fit_stats = list(
      AIC = AIC(model),
      BIC = BIC(model),
      logLik = logLik(model),
      R2 = r2_vals
    )
  ))
}
