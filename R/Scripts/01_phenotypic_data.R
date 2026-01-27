#* 1: Phenotpyic Data Analysis
#+ 1.1: Body weight LMM
#- 1.1.1: Reformat data to long format for LMM
BW <- phenotypic_physiologic |>
  select(ID:BW_8w) |>
  pivot_longer(
    cols = starts_with("BW_"),
    names_to = "time",
    names_prefix = "BW_",
    names_transform = list(time = ~as.numeric(gsub("w", "", .))),
    values_to = "BW"
  )
#- 1.1.2: Run LMM on Body Weight
BW_LMM <- run_lmm(
  data = BW,
  id_col = "ID",
  time_col = "time",
  value_col = "BW",
  between_factors = c("sex", "diet"),
  random_slope = TRUE # includes random slope for time
)
#- 1.1.3: Calculate percent weight gain by group
BW_percent_gain <- BW %>%
  filter(time %in% c(0, 8)) %>%
  pivot_wider(names_from = time, values_from = BW, names_prefix = "week_") %>%
  mutate(
    percent_gain = ((week_8 - week_0) / week_0) * 100
  ) %>%
  group_by(sex, diet) %>%
  summarise(
    mean_percent_gain = mean(percent_gain, na.rm = TRUE),
    sd_percent_gain = sd(percent_gain, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
#+ 1.2: Food Efficiency, LM, FM ANOVA
#- 1.2.1: Food Efficiency ANOVA
FE_ANOVA <- run_anova(data = phenotypic_physiologic, response = "food_efficiency")
#- 1.2.2: Lean Mass ANOVA
LM_ANOVA <- run_anova( data = phenotypic_physiologic, response = "LM")
#- 1.2.3: Fat Mass ANOVA
FM_ANOVA <- run_anova(data = phenotypic_physiologic, response = "FM")
#+ 1.3: Plot all phenotypic data
#- 1.3.1: Plot RM BW
p1A <- plot_rm_bodyweight(
  data = BW,
  lmm_result = BW_LMM,
  id_col = "ID",
  time_col = "time",
  value_col = "BW",
  sex_col = "sex",
  diet_col = "diet"
)
#- 1.3.2: Plot Food Efficiency
p1B <- plot_anova_barplot(
  data = phenotypic_physiologic,
  anova_result = FE_ANOVA,
  response = "food_efficiency",
  y_label = "Food Efficiency (g/kcal)",
  y_limits = c(0, 0.025),
  y_breaks = seq(0, 0.025, 0.005)
)
#- 1.3.3: Plot Lean Mass
p1C <- plot_anova_barplot(
  data = phenotypic_physiologic,
  anova_result = LM_ANOVA,
  response = "LM",
  y_label = "Lean Mass (g)",
  y_limits = c(0, 30),
  y_breaks = seq(0, 30, 5)
)
#- 1.3.4: Plot Fat Mass
p1D <- plot_anova_barplot(
  data = phenotypic_physiologic,
  anova_result = FM_ANOVA,
  response = "FM",
  y_label = "Fat Mass (g)",
  y_limits = c(0, 14),
  y_breaks = seq(0, 14, 2)
)