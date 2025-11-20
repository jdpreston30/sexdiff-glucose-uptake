shapiro <- function(tb) {
  long <- tb %>%
    pivot_longer(cols = 4:ncol(tb), names_to = "measurement", values_to = "value") %>%
    mutate(log_value = log(value)) %>%
    select(-c(1:3), -measurement)
  normality_p_values <- long %>%
    summarise(
      norm_p = shapiro.test(value)$p.value,
      lognorm_p = shapiro.test(log_value)$p.value
    )
}

perform_shapiro_lognormal_tests <- function(column, threshold = 0.01) {
  shapiro_test_result <- shapiro.test(column)
  shapiro_p_value <- shapiro_test_result$p.value
  normality <- ifelse(shapiro_p_value > threshold, "Normal", "Not Normal")
  log_column <- log(column)
  shapiro_log_test_result <- shapiro.test(log_column)
  shapiro_log_p_value <- shapiro_log_test_result$p.value
  lognormality <- ifelse(shapiro_log_p_value > threshold, "Normal", "Not Normal")
  return(list(Shapiro_p_value = shapiro_p_value, Normality = normality, Shapiro_log_p_value = shapiro_log_p_value, Lognormality = lognormality))
}
