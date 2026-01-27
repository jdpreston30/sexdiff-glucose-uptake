#* 3: Glucose Uptake
#+ 3.1: In-Vivo Glucose Uptake ANOVA
#- 3.1.1: Gonadal AT
GU_gonadal_ANOVA <- run_anova(data = glucose_uptake, response = "GU_gonadal")
#- 3.1.2: Subcutaneous adipose
GU_SQ_ANOVA <- run_anova(data = glucose_uptake, response = "GU_SQ")
#- 3.1.3: Retroperitoneal adipose
GU_RP_ANOVA <- run_anova(data = glucose_uptake, response = "GU_RP")
#- 3.1.4: Brown adipose
GU_brown_ANOVA <- run_anova(data = glucose_uptake, response = "GU_brown")
#- 3.1.5: EDL muscle
GU_EDL_ANOVA <- run_anova(data = glucose_uptake, response = "GU_EDL")
#- 3.1.6: Gastrocnemius muscle
GU_gastroc_ANOVA <- run_anova(data = glucose_uptake, response = "GU_gastroc")
#- 3.1.7: Soleus muscle
GU_soleus_ANOVA <- run_anova(data = glucose_uptake, response = "GU_soleus")
#- 3.1.8: Heart
GU_heart_ANOVA <- run_anova(data = glucose_uptake, response = "GU_heart")
#+ 3.2: Ex-Vivo Glucose Uptake Mixed Model
#- 3.2.1: Prepare data
# Extract animal ID to account for pseudo-replication
# (Multiple explants from same animal are correlated)
ex_vivo_long <- ex_vivo_glucose_uptake %>%
  mutate(animal_ID = str_remove(ID, "_Ex"))
#- 3.2.2: Run mixed model ANOVA
GU_ex_vivo_ANOVA <- run_mixed_anova(
  data = ex_vivo_long,
  response = "GU_EV_gonadal",
  id_col = "animal_ID",
  within_factors = "EV_tx",
  between_factors = c("sex", "diet"),
  auto_transform = TRUE,
  check_diagnostics = TRUE,
  posthoc_adjust = "tukey"
)

#+ 3.3: Subset GU data
#- 3.3.1: WAT
glucose_uptake_WAT <- glucose_uptake %>%
  select(ID, sex, diet, Gonadal = GU_gonadal, Retroperitoneal = GU_RP, Subcutaneous = GU_SQ)
#- 3.3.2: BAT
glucose_uptake_BAT <- glucose_uptake %>%
  select(ID, sex, diet, Brown = GU_brown)
#- 3.3.3: Skeletal Muscle
glucose_uptake_SM <- glucose_uptake %>%
  select(ID, sex, diet, EDL = GU_EDL, Gastroc = GU_gastroc, Soleus = GU_soleus)
#- 3.3.4: Cardiac
glucose_uptake_Cardiac <- glucose_uptake %>%
  select(ID, sex, diet, Heart = GU_heart)
#+ 3.4: Plot Glucose Uptake
#- 3.4.1: Plot In-Vivo Glucose Uptake (White Adipose Tissue)
p3a <- plot_gu(
  data = glucose_uptake_WAT,
  y_label = "3H-2DG (CPM/mg)",
  y_limits = c(0, 4),
  y_breaks = seq(0, 4, 1),
  plot_title = "White Adipose Tissue",
  bar_width = 0.7,
  anova_list = list(
    Gonadal = GU_gonadal_ANOVA,
    Retroperitoneal = GU_RP_ANOVA,
    Subcutaneous = GU_SQ_ANOVA
  )
)
#- 3.4.2: Plot In-Vivo Glucose Uptake (Brown Adipose Tissue)
p3b <- plot_gu(
  data = glucose_uptake_BAT,
  y_label = "3H-2DG (CPM/mg)",
  y_limits = c(0, 30),
  y_breaks = seq(0, 30, 5),
  plot_title = "Brown Adipose\nTissue",
  bar_width = 0.591,
  #0.596 to get same absolute bar width as p3a.1 with 4 tissues
  anova_list = list(Brown = GU_brown_ANOVA)
)
#- 3.4.3: Plot In-Vivo Glucose Uptake (Skeletal Muscle)
p3c <- plot_gu(
  data = glucose_uptake_SM,
  y_label = "3H-2DG (CPM/mg)",
  y_limits = c(0, 25),
  y_breaks = seq(0, 25, 5),
  plot_title = "Skeletal Muscle",
  bar_width = 0.7,
  anova_list = list(
    EDL = GU_EDL_ANOVA,
    Gastroc = GU_gastroc_ANOVA,
    Soleus = GU_soleus_ANOVA
  )
)
#- 3.4.4: Plot In-Vivo Glucose Uptake (Cardiac)
p3d <- plot_gu(
  data = glucose_uptake_Cardiac,
  y_label = "3H-2DG (CPM/mg)",
  y_limits = c(0, 25),
  y_breaks = seq(0, 25, 5),
  plot_title = "Cardiac Tissue",
  bar_width = 0.591,
  anova_list = list(Heart = GU_heart_ANOVA)
)
#- 3.4.5: Plot Ex-Vivo Glucose Uptake
p3e <- plot_gu_ev(
  data = ex_vivo_long,
  y_label = "3H-2DG (CPM/mg)",
  y_limits = c(0, 25),
  y_breaks = seq(0, 25, 5),
  plot_title = "Ex Vivo Gonadal AT Glucose Uptake",
  bar_width = 0.49,
  mixed_anova_result = GU_ex_vivo_ANOVA
)