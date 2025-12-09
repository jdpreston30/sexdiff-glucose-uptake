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
#- 3.2.1: Prepare data for mixed model analysis
ex_vivo_long <- ex_vivo_glucose_uptake %>%
  mutate(
    # Extract animal ID (remove _Ex suffix to match explants from same animal)
    animal_ID = str_remove(ID, "_Ex"),
    # Convert treatment to factor with proper ordering
    EV_tx = factor(EV_tx, levels = c("Basal", "Insulin"))
  )
#- 3.2.2: Run linear mixed model
# Models: GU ~ sex * diet * treatment + (1|animal_ID)
# Accounts for multiple explants from same animal (pseudo-replication)
GU_ex_vivo_LMM <- run_lmm(
  data = ex_vivo_long,
  id_col = "animal_ID",
  time_col = "EV_tx",
  value_col = "GU_EV_gonadal",
  between_factors = c("sex", "diet"),
  random_slope = FALSE  # Only 2 treatment levels, no need for random slope
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
#+ 3.5
#- 3.3.1: Plot In-Vivo Glucose Uptake (White Fat)
p3a.1 <- plot_gu
#- 3.3.2: Plot In-Vivo Glucose Uptake (Brown Fat)
p3a.2 <- plot_gu
#- 3.3.3: Plot In-Vivo Glucose Uptake (Skeletal Muscle)
p3b <- plot_gu
#- 3.3.4: Plot Ex-Vivo Glucose Uptake
p3b <- plot_gu