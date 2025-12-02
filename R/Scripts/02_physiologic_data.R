#* 2: Phenotpyic Data Analysis
#+ 2.1: OGTT LMM
#- 2.1.1: Reformat data to long format for LMM
OGTT <- phenotypic_physiologic |>
  select(ID:diet, OGTT_BG_0m:OGTT_BG_45m) |>
  pivot_longer(
    cols = starts_with("OGTT_BG_"),
    names_to = "time",
    names_prefix = "OGTT_BG_",
    names_transform = list(time = ~as.numeric(gsub("m", "", .))),
    values_to = "OGTT_BG"
  )
#- 2.1.2: Run LMM on OGTT Blood Glucose
OGTT_RMANOVA <- run_rm_anova(
  data = OGTT,
  id_col = "ID",
  time_col = "time",
  value_col = "OGTT_BG",
  between_factors = c("sex", "diet")
)
#+ 2.5: Plot all physiologic data
#- 2.5.1: Plot RM OGTT Blood Glucose
p2a <- plot_rm_ogtt(
  data = OGTT,
  rmanova_result = OGTT_RMANOVA
)
