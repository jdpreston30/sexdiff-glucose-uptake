run_aov <- function(data, dependent_var, dataset_name) {
  formula <- as.formula(paste(dependent_var, "~ Sex + Diet + Sex:Diet"))
  aov_model <- aov(formula, data = data)
  anova_result <- summary(aov_model)
  anova_p_values <- c(
    Sex = anova_result[[1]]$"Pr(>F)"[1],
    Diet = anova_result[[1]]$"Pr(>F)"[2],
    Sex_Diet = anova_result[[1]]$"Pr(>F)"[3]
  )
  post_hoc_result <- TukeyHSD(aov_model, which = "Sex:Diet")
  post_hoc_p_values <- post_hoc_result$`Sex:Diet`[, 4] # Extracting p-values
  anova_tibble <- tibble(
    Dataset = dataset_name,
    Factor = names(anova_p_values),
    P_Value = anova_p_values,
    Post_Hoc = NA
  )
  post_hoc_tibble <- tibble(
    Dataset = dataset_name,
    Factor = paste("Post_Hoc", names(post_hoc_p_values)),
    P_Value = post_hoc_p_values,
    Post_Hoc = "Yes"
  )
  return(bind_rows(anova_tibble, post_hoc_tibble))
}