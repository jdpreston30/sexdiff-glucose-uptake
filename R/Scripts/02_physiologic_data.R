#* 2: Phenotpyic Data Analysis
#+ 2.1: OGTT RM ANOVA
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
#- 2.1.2: Run RM ANOVA on OGTT Blood Glucose
OGTT_RMANOVA <- run_rm_anova(
  data = OGTT,
  id_col = "ID",
  time_col = "time",
  value_col = "OGTT_BG",
  between_factors = c("sex", "diet")
)
# + 2.2: Insulin Response Test RM ANOVA
#- 2.2.1: Reformat data to long format for ANOVA
IRT <- phenotypic_physiologic |>
  select(ID:diet, IRT_FBG, IRT_BG_insulin_15m) |>
  filter(!is.na(IRT_BG_insulin_15m)) |>
  pivot_longer(
    cols = c(IRT_FBG, IRT_BG_insulin_15m),
    names_to = "time_tx",
    values_to = "IRT_BG"
  ) |>
  mutate(
    time = case_when(
      time_tx == "IRT_FBG" ~ 0,
      time_tx == "IRT_BG_insulin_15m" ~ 15
    )
  ) |>
  select(ID, sex, diet, time, IRT_BG)
#- 2.2.2: Run ANOVA on IRT Blood Glucose
IRT_RMANOVA <- run_rm_anova(
  data = IRT,
  id_col = "ID",
  time_col = "time",
  value_col = "IRT_BG",
  between_factors = c("sex", "diet")
)
#+ 2.3: FBG ANOVA
FBG_ANOVA <- run_anova(data = phenotypic_physiologic, response = "FBG_3h")
#+ 2.4: HOMA2-IR ANOVA
HOMA2_ANOVA <- run_anova(data = phenotypic_physiologic, response = "HOMA2_IR")
#+ 2.5: 45-min Insulin ANOVA
postprandial_insulin_ANOVA <- run_anova(data = phenotypic_physiologic, response = "OGTT_insulin_45m")
#+ 2.5: Plot all physiologic data
#- 2.5.1: Plot RM OGTT Blood Glucose
p2a <- plot_rm_ogtt(
  data = OGTT,
  rmanova_result = OGTT_RMANOVA
)
#- 2.5.2: Plot IRT
p2b <- plot_IRT(
  data = IRT,
  rmanova_result = IRT_RMANOVA
)
#- 2.5.3: Plot FBG ANOVA
p2c <- plot_anova_barplot(
  data = phenotypic_physiologic,
  anova_result = FBG_ANOVA,
  response = "FBG_3h",
  y_label = "Fasted Glucose (mg/dL)",
  y_limits = c(0, 200),
  y_breaks = c(0, 50, 100, 150, 200)
)
#- 2.5.4: Plot HOMA2-IR ANOVA
p2d <- plot_anova_barplot(
  data = phenotypic_physiologic,
  anova_result = HOMA2_ANOVA,
  response = "HOMA2_IR",
  y_label = "HOMA2-IR",
  y_limits = c(0, 10),
  y_breaks = c(0, 2, 4, 6, 8, 10)
)
#- 2.5.5: Plot 45-min Insulin ANOVA
p2e <- plot_anova_barplot(
  data = phenotypic_physiologic,
  anova_result = postprandial_insulin_ANOVA,
  response = "OGTT_insulin_45m",
  factor1 = "sex",
  factor2 = "diet",
  y_label = "Post-Bolus Insulin (ng/mL)",
  y_limits = c(0, 3),
  y_breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)
)
