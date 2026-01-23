#* 5: Verify Sample Sizes for Figure Legends
#' 
#' This script generates concise sample size summaries for figure legends
#' Outputs saved to: Outputs/Figures/Legends/
#'

# Create output directory if it doesn't exist
legend_dir <- "Outputs/Figures/Legends"
if (!dir.exists(legend_dir)) {
  dir.create(legend_dir, recursive = TRUE)
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("FIGURE LEGEND SAMPLE SIZE VERIFICATION\n")
cat("Generating text files in:", legend_dir, "\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Helper function to get n range
get_n_range <- function(n_counts) {
  n_vals <- n_counts$n
  if (length(unique(n_vals)) == 1) {
    return(paste0("n = ", n_vals[1]))
  } else {
    return(paste0("n = ", min(n_vals), "-", max(n_vals)))
  }
}

#+ 5.1: Figure 1 Sample Sizes
fig1_output <- c()
fig1_output <- c(fig1_output, "FIGURE 1: Body weight, food efficiency, and body composition")
fig1_output <- c(fig1_output, paste(rep("-", 80), collapse = ""))

# Panel A - Body Weight
BW_n <- BW %>% filter(!is.na(BW)) %>% distinct(ID, sex, diet) %>% count(sex, diet)
fig1_output <- c(fig1_output, paste("• Panel A (Body Weight):", get_n_range(BW_n), "per group"))

# Panel B - Food Efficiency
FE_n <- phenotypic_physiologic %>% filter(!is.na(food_efficiency)) %>% count(sex, diet)
fig1_output <- c(fig1_output, paste("• Panel B (Food Efficiency):", get_n_range(FE_n), "per group"))

# Panel C - Lean Mass
LM_n <- phenotypic_physiologic %>% filter(!is.na(LM)) %>% count(sex, diet)
fig1_output <- c(fig1_output, paste("• Panel C (Lean Mass):", get_n_range(LM_n), "per group"))

# Panel D - Fat Mass
FM_n <- phenotypic_physiologic %>% filter(!is.na(FM)) %>% count(sex, diet)
fig1_output <- c(fig1_output, paste("• Panel D (Fat Mass):", get_n_range(FM_n), "per group"))

fig1_output <- c(fig1_output, "")
fig1_output <- c(fig1_output, "Legend states: n = 20 per sex × diet group")

writeLines(fig1_output, file.path(legend_dir, "Fig1_legend.txt"))
cat("✅ Figure 1 legend saved\n")

#+ 5.2: Figure 2 Sample Sizes
fig2_output <- c()
fig2_output <- c(fig2_output, "FIGURE 2: Glucose homeostasis and insulin sensitivity")
fig2_output <- c(fig2_output, paste(rep("-", 80), collapse = ""))

# Panel A - OGTT
OGTT_n <- OGTT %>% filter(!is.na(OGTT_BG)) %>% distinct(ID, sex, diet) %>% count(sex, diet)
fig2_output <- c(fig2_output, paste("• Panel A (OGTT):", get_n_range(OGTT_n), "per group"))

# Panel B - IRT
IRT_n <- IRT %>% filter(!is.na(IRT_BG)) %>% distinct(ID, sex, diet) %>% count(sex, diet)
fig2_output <- c(fig2_output, paste("• Panel B (IRT):", get_n_range(IRT_n), "per group"))

# Panel C - Fasted Glucose
FBG_n <- phenotypic_physiologic %>% filter(!is.na(FBG_3h)) %>% count(sex, diet)
fig2_output <- c(fig2_output, paste("• Panel C (Fasted Glucose):", get_n_range(FBG_n), "per group"))

# Panel D - HOMA2-IR
HOMA_n <- phenotypic_physiologic %>% filter(!is.na(HOMA2_IR)) %>% count(sex, diet)
fig2_output <- c(fig2_output, paste("• Panel D (HOMA2-IR):", get_n_range(HOMA_n), "per group"))

# Panel E - Serum Insulin
insulin_n <- phenotypic_physiologic %>% filter(!is.na(OGTT_insulin_45m)) %>% count(sex, diet)
fig2_output <- c(fig2_output, paste("• Panel E (Serum Insulin):", get_n_range(insulin_n), "per group"))

fig2_output <- c(fig2_output, "")
fig2_output <- c(fig2_output, "Legend states:")
fig2_output <- c(fig2_output, "  Panel A: n = 11-12 per group")
fig2_output <- c(fig2_output, "  Panel B: n = 4 per group")
fig2_output <- c(fig2_output, "  Panel C: n = 20 per group")
fig2_output <- c(fig2_output, "  Panel D: n = 4 per group")
fig2_output <- c(fig2_output, "  Panel E: n = 8-11 per group")

writeLines(fig2_output, file.path(legend_dir, "Fig2_legend.txt"))
cat("✅ Figure 2 legend saved\n")

#+ 5.3: Figure 3 Sample Sizes
fig3_output <- c()
fig3_output <- c(fig3_output, "FIGURE 3: Tissue-specific glucose uptake")
fig3_output <- c(fig3_output, paste(rep("-", 80), collapse = ""))

# Panel A - WAT (all depots combined)
GU_gonadal_n <- glucose_uptake %>% filter(!is.na(GU_gonadal)) %>% count(sex, diet)
GU_RP_n <- glucose_uptake %>% filter(!is.na(GU_RP)) %>% count(sex, diet)
GU_SQ_n <- glucose_uptake %>% filter(!is.na(GU_SQ)) %>% count(sex, diet)
GU_brown_n <- glucose_uptake %>% filter(!is.na(GU_brown)) %>% count(sex, diet)
WAT_range <- get_n_range(bind_rows(GU_gonadal_n, GU_RP_n, GU_SQ_n, GU_brown_n))
fig3_output <- c(fig3_output, paste("• Panel A (WAT & BAT):", WAT_range, "per group"))

# Panel B - Skeletal Muscle
GU_EDL_n <- glucose_uptake %>% filter(!is.na(GU_EDL)) %>% count(sex, diet)
GU_gastroc_n <- glucose_uptake %>% filter(!is.na(GU_gastroc)) %>% count(sex, diet)
GU_soleus_n <- glucose_uptake %>% filter(!is.na(GU_soleus)) %>% count(sex, diet)
SM_range <- get_n_range(bind_rows(GU_EDL_n, GU_gastroc_n, GU_soleus_n))
fig3_output <- c(fig3_output, paste("• Panel B (Skeletal Muscle):", SM_range, "per group"))

# Panel C - Cardiac
GU_heart_n <- glucose_uptake %>% filter(!is.na(GU_heart)) %>% count(sex, diet)
fig3_output <- c(fig3_output, paste("• Panel C (Cardiac):", get_n_range(GU_heart_n), "per group"))

# Panel D - Ex vivo
GU_ex_vivo_n <- ex_vivo_glucose_uptake %>%
  filter(!is.na(GU_EV_gonadal)) %>%
  mutate(animal_ID = str_remove(ID, "_Ex")) %>%
  distinct(animal_ID, sex, diet) %>%
  count(sex, diet)
fig3_output <- c(fig3_output, paste("• Panel D (Ex vivo Gonadal):", get_n_range(GU_ex_vivo_n), "per group"))

fig3_output <- c(fig3_output, "")
fig3_output <- c(fig3_output, "Legend states:")
fig3_output <- c(fig3_output, "  Panels A-C: n = 10-12 per group")
fig3_output <- c(fig3_output, "  Panel D: n = 11-12 per group")

writeLines(fig3_output, file.path(legend_dir, "Fig3_legend.txt"))
cat("✅ Figure 3 legend saved\n")