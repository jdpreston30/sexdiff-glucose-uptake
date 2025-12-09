#' Run Mixed Model ANOVA for Repeated/Pseudo-Replicated Measures
#'
#' Performs mixed model analysis for designs with repeated or pseudo-replicated 
#' measurements (e.g., multiple explants per animal, repeated measurements on 
#' same subjects). Automatically handles diagnostics, transformations, and 
#' post-hoc comparisons following professional statistical standards.
#'
#' @param data Data frame containing the variables
#' @param response Character string specifying the response variable name
#' @param id_col Character string specifying the subject/animal ID column
#' @param within_factors Character vector of within-subject factor names (e.g., "treatment", "time")
#' @param between_factors Character vector of between-subject factor names (e.g., "sex", "diet")
#' @param alpha Numeric value for significance threshold (default: 0.05)
#' @param auto_transform Logical, automatically try log transformation if normality fails (default: TRUE)
#' @param check_diagnostics Logical, perform assumption checks (default: TRUE)
#' @param posthoc_adjust Method for post-hoc adjustment: "tukey", "bonferroni", "none" (default: "tukey")
#'
#' @return A list containing:
#'   \item{model}{The fitted lmer model object}
#'   \item{anova_table}{Type III ANOVA table with F-statistics and p-values}
#'   \item{diagnostics}{List of assumption test results}
#'   \item{transformation}{Character string indicating transformation used}
#'   \item{posthoc}{Post-hoc test results for significant effects}
#'   \item{assumptions_met}{Logical indicating if assumptions satisfied}
#'   \item{warnings}{Character vector of assumption violations}
#'   \item{method}{Character string describing the analysis method}
#'
#' @examples
#' # Ex vivo explants (multiple explants per animal)
#' result <- run_mixed_anova(
#'   data = ex_vivo_data,
#'   response = "GU_EV_gonadal",
#'   id_col = "animal_ID",
#'   within_factors = "treatment",
#'   between_factors = c("sex", "diet")
#' )
#'
#' # Repeated measures over time
#' result <- run_mixed_anova(
#'   data = longitudinal_data,
#'   response = "body_weight",
#'   id_col = "animal_ID",
#'   within_factors = "week",
#'   between_factors = c("sex", "diet")
#' )

run_mixed_anova <- function(data, response, id_col, within_factors = NULL, 
                           between_factors = NULL, alpha = 0.05, 
                           auto_transform = TRUE, check_diagnostics = TRUE,
                           posthoc_adjust = "tukey") {
  
  # Load required packages
  library(lme4)
  library(car)
  library(emmeans)
  library(dplyr)
  
  cat("\n" %+% strrep("=", 80) %+% "\n")
  cat("MIXED MODEL ANOVA FOR REPEATED/PSEUDO-REPLICATED MEASURES\n")
  cat(strrep("=", 80) %+% "\n\n")
  
  # ========================================================================
  # 1. DATA PREPARATION
  # ========================================================================
  cat("📊 DATA PREPARATION\n")
  cat(strrep("-", 80) %+% "\n")
  
  # Ensure ID is a factor
  data[[id_col]] <- as.factor(data[[id_col]])
  
  # Convert all factors
  all_factors <- c(within_factors, between_factors)
  for (factor in all_factors) {
    if (!is.null(factor)) {
      data[[factor]] <- as.factor(data[[factor]])
    }
  }
  
  cat("Response variable:", response, "\n")
  cat("Subject ID column:", id_col, "\n")
  cat("Within-subject factors:", paste(within_factors, collapse = ", "), "\n")
  cat("Between-subject factors:", paste(between_factors, collapse = ", "), "\n")
  cat("N =", nrow(data), "observations from", nlevels(data[[id_col]]), "subjects\n\n")
  
  # ========================================================================
  # 2. BUILD FORMULA
  # ========================================================================
  
  # Fixed effects: full factorial design
  if (!is.null(all_factors) && length(all_factors) > 0) {
    fixed_effects <- paste(all_factors, collapse = " * ")
  } else {
    stop("Must specify at least one within or between factor")
  }
  
  # Random effects: random intercept for subject
  random_effects <- paste0("(1 | ", id_col, ")")
  
  # ========================================================================
  # 3. FIT MODEL (WITH AUTO-TRANSFORMATION IF NEEDED)
  # ========================================================================
  
  results <- list()
  transformation <- "none"
  assumptions_met <- TRUE
  warnings_list <- character(0)
  
  # Try original scale first
  cat("🔧 FITTING MODEL\n")
  cat(strrep("-", 80) %+% "\n")
  
  formula_str <- paste(response, "~", fixed_effects, "+", random_effects)
  cat("Model formula:", formula_str, "\n")
  
  model <- lmer(as.formula(formula_str), data = data, REML = TRUE)
  cat("✅ Model fitted successfully\n\n")
  
  # ========================================================================
  # 4. DIAGNOSTICS
  # ========================================================================
  
  if (check_diagnostics) {
    cat("🔍 MODEL DIAGNOSTICS\n")
    cat(strrep("-", 80) %+% "\n")
    
    # Extract residuals
    resids <- residuals(model)
    
    # Test residual normality
    cat("Shapiro-Wilk test for residual normality:\n")
    shapiro_test <- shapiro.test(resids)
    cat("  W =", round(shapiro_test$statistic, 4), 
        ", p-value =", format.pval(shapiro_test$p.value, digits = 3), "\n")
    
    normality_ok <- shapiro_test$p.value >= alpha
    
    if (!normality_ok) {
      cat("  ⚠️  Residuals not normally distributed (p < 0.05)\n")
      assumptions_met <- FALSE
      warnings_list <- c(warnings_list, "Non-normal residuals")
      
      # Try log transformation if enabled
      if (auto_transform && min(data[[response]], na.rm = TRUE) > 0) {
        cat("\n🔄 ATTEMPTING LOG TRANSFORMATION\n")
        cat(strrep("-", 80) %+% "\n")
        
        # Add small constant if any zeros
        if (min(data[[response]], na.rm = TRUE) == 0) {
          data$response_transformed <- log(data[[response]] + 1)
          transformation <- "log(x + 1)"
        } else {
          data$response_transformed <- log(data[[response]])
          transformation <- "log(x)"
        }
        
        # Refit model
        formula_str_trans <- paste("response_transformed ~", fixed_effects, "+", random_effects)
        model <- lmer(as.formula(formula_str_trans), data = data, REML = TRUE)
        
        # Re-extract residuals from new model
        resids <- residuals(model)
        
        # Recheck normality
        shapiro_test_trans <- shapiro.test(resids)
        cat("Shapiro-Wilk test after transformation:\n")
        cat("  W =", round(shapiro_test_trans$statistic, 4), 
            ", p-value =", format.pval(shapiro_test_trans$p.value, digits = 3), "\n")
        
        if (shapiro_test_trans$p.value >= alpha) {
          cat("  ✅ Transformation improved normality\n\n")
          assumptions_met <- TRUE
          warnings_list <- warnings_list[warnings_list != "Non-normal residuals"]
        } else {
          cat("  ⚠️  Still non-normal after transformation\n")
          cat("     Consider non-parametric alternatives if severe\n\n")
        }
      }
    } else {
      cat("  ✅ Residuals normally distributed\n")
    }
    
    # Test variance homogeneity
    # Extract fitted data indices to match residuals
    cat("\nLevene's test for homogeneity of variance:\n")
    fitted_indices <- as.numeric(names(resids))
    data_fitted <- data[fitted_indices, ]
    data_fitted$abs_resid <- abs(resids)
    levene_formula <- as.formula(paste("abs_resid ~", paste(all_factors, collapse = " * ")))
    levene_test <- car::leveneTest(levene_formula, data = data_fitted)
    cat("  F =", round(levene_test$`F value`[1], 3), 
        ", p-value =", format.pval(levene_test$`Pr(>F)`[1], digits = 3), "\n")
    
    if (levene_test$`Pr(>F)`[1] < alpha) {
      cat("  ⚠️  Variance heterogeneity detected (p < 0.05)\n")
      assumptions_met <- FALSE
      warnings_list <- c(warnings_list, "Heterogeneous variance")
    } else {
      cat("  ✅ Variance is homogeneous across groups\n")
    }
    
    # Check random effects normality
    cat("\nNormality of random effects:\n")
    random_effects_vals <- ranef(model)[[id_col]][,1]
    random_shapiro <- shapiro.test(random_effects_vals)
    cat("  W =", round(random_shapiro$statistic, 4), 
        ", p-value =", format.pval(random_shapiro$p.value, digits = 3), "\n")
    
    if (random_shapiro$p.value < alpha) {
      cat("  ⚠️  Random effects not normally distributed\n")
      warnings_list <- c(warnings_list, "Non-normal random effects")
    } else {
      cat("  ✅ Random effects normally distributed\n")
    }
    
    cat("\n")
    
    # Store diagnostics
    results$diagnostics <- list(
      shapiro_residuals = shapiro_test,
      levene_variance = levene_test,
      shapiro_random = random_shapiro
    )
  }
  
  # ========================================================================
  # 5. ANOVA TABLE
  # ========================================================================
  
  cat("📈 TYPE III ANOVA TABLE\n")
  cat(strrep("-", 80) %+% "\n")
  
  anova_table <- car::Anova(model, type = "III", test.statistic = "F")
  print(anova_table)
  cat("\n")
  
  # ========================================================================
  # 6. MODEL SUMMARY
  # ========================================================================
  
  cat("📋 MODEL SUMMARY\n")
  cat(strrep("-", 80) %+% "\n")
  print(summary(model))
  cat("\n")
  
  # ========================================================================
  # 7. POST-HOC TESTS
  # ========================================================================
  
  cat("📊 POST-HOC COMPARISONS\n")
  cat(strrep("-", 80) %+% "\n")
  
  # Extract p-values for decision tree
  p_values <- anova_table[, "Pr(>F)"]
  names(p_values) <- rownames(anova_table)
  
  # Identify significant effects
  sig_effects <- names(p_values)[p_values < alpha & !is.na(p_values)]
  sig_effects <- sig_effects[sig_effects != "(Intercept)"]
  
  if (length(sig_effects) == 0) {
    cat("No significant effects detected (all p >= 0.05)\n\n")
  } else {
    cat("Significant effects detected:\n")
    for (effect in sig_effects) {
      cat(" -", effect, "(p =", format.pval(p_values[effect], digits = 3), ")\n")
    }
    cat("\n")
    
    # Post-hoc strategy: Test interactions first, then main effects
    # Check for interactions
    interaction_effects <- sig_effects[grepl(":", sig_effects)]
    main_effects <- sig_effects[!grepl(":", sig_effects)]
    
    results$posthoc <- list()
    
    # Handle interactions
    if (length(interaction_effects) > 0) {
      for (interaction in interaction_effects) {
        cat("--- Post-hoc for:", interaction, "---\n")
        factors_in_int <- strsplit(interaction, ":")[[1]]
        
        # For 2-way interactions, test simple effects
        if (length(factors_in_int) == 2) {
          # Test first factor within levels of second
          emm_formula <- as.formula(paste("pairwise ~", factors_in_int[1], "|", factors_in_int[2]))
          emm_result <- emmeans(model, emm_formula, adjust = posthoc_adjust)
          print(emm_result)
          results$posthoc[[interaction]] <- emm_result
          
        } else if (length(factors_in_int) == 3) {
          # For 3-way, test within combinations
          emm_formula <- as.formula(paste("pairwise ~", factors_in_int[1], "|", 
                                         paste(factors_in_int[-1], collapse = " * ")))
          emm_result <- emmeans(model, emm_formula, adjust = posthoc_adjust)
          print(emm_result)
          results$posthoc[[interaction]] <- emm_result
        }
        cat("\n")
      }
    }
    
    # Handle main effects (only if not involved in significant interactions)
    if (length(main_effects) > 0) {
      for (main_effect in main_effects) {
        # Check if this main effect is in any significant interaction
        in_interaction <- any(sapply(interaction_effects, function(x) grepl(main_effect, x)))
        
        if (!in_interaction) {
          cat("--- Post-hoc for:", main_effect, "(main effect) ---\n")
          emm_formula <- as.formula(paste("pairwise ~", main_effect))
          emm_result <- emmeans(model, emm_formula, adjust = posthoc_adjust)
          print(emm_result)
          results$posthoc[[main_effect]] <- emm_result
          cat("\n")
        } else {
          cat("--- Skipping main effect", main_effect, "(involved in interaction) ---\n\n")
        }
      }
    }
  }
  
  # ========================================================================
  # 8. SUMMARY
  # ========================================================================
  
  cat(strrep("=", 80) %+% "\n")
  cat("ANALYSIS SUMMARY\n")
  cat(strrep("=", 80) %+% "\n")
  cat("Model type: Linear Mixed Model (lmer)\n")
  cat("Transformation:", transformation, "\n")
  cat("Assumptions met:", ifelse(assumptions_met, "✅ YES", "⚠️  NO"), "\n")
  if (length(warnings_list) > 0) {
    cat("Warnings:\n")
    for (w in warnings_list) {
      cat(" -", w, "\n")
    }
  }
  cat(strrep("=", 80) %+% "\n\n")
  
  # Return results
  results$model <- model
  results$anova_table <- anova_table
  results$transformation <- transformation
  results$assumptions_met <- assumptions_met
  results$warnings <- warnings_list
  results$method <- "Linear Mixed Model (REML)"
  
  invisible(results)
}
