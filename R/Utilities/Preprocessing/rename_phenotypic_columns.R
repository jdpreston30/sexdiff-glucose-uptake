#* Rename Phenotypic and Physiologic Data Columns
#+ Function to standardize column names for easier coding

rename_phenotypic_columns <- function(data) {
  #' Rename phenotypic_physiologic columns to short, coding-friendly names
  #'
  #' @param data A dataframe with phenotypic_physiologic columns
  #' @return Dataframe with renamed columns
  #'
  #' @examples
  #' phenotypic_physiologic <- rename_phenotypic_columns(phenotypic_physiologic)
  
  rename_map <- c(
    "ID" = "id",
    "Sex" = "sex",
    "Diet" = "diet",
    "BW (g) Week 0" = "bw_w0",
    "BW (g) Week 1" = "bw_w1",
    "BW (g) Week 2" = "bw_w2",
    "BW (g) Week 3" = "bw_w3",
    "BW (g) Week 4" = "bw_w4",
    "BW (g) Week 5" = "bw_w5",
    "BW (g) Week 6" = "bw_w6",
    "BW (g) Week 7" = "bw_w7",
    "BW (g) Week 8" = "bw_w8",
    "kcal/wk 1" = "kcal_w1",
    "kcal/wk 2" = "kcal_w2",
    "kcal/wk 3" = "kcal_w3",
    "kcal/wk 4" = "kcal_w4",
    "kcal/wk 5" = "kcal_w5",
    "kcal/wk 6" = "kcal_w6",
    "kcal/wk 7" = "kcal_w7",
    "kcal/wk 8" = "kcal_w8",

    "Net caloric intake (kcal)" = "net_kcal",
    "Fat Mass (g)" = "FM",
    "Lean Mass (g)" = "LM",
    "0 minute Glucose (mg/dL)" = "OGTT_0m",
    "15 minute Glucose (mg/dL)" = "OGTT_15m",
    "30 minute Glucose (mg/dL)" = "OGTT_30m",
    "45 minute glucose (mg/dL)" = "OGTT_45m",
    "45 minutes insulin (ng/mL)" = "insulin_45min",
    "3 hour Fasted Glucose (mg/dL)" = "gluc_3hr_fasted",
    "Fasted Glucose (mg/dL)" = "fpg",
    "15 Minute post-insulin glucose (mg/dL)" = "gluc_15min_postinsulin",
    "15 Minute post-saline glucose (mg/dL)" = "gluc_15min_postsaline",
    "glucose (mmol/L)" = "gluc_mmol",
    "insulin (pmol/L)" = "insulin_pmol",
    "HOMA2-IR" = "homa2_ir"
  )
  
  # Apply renaming
  data <- data %>%
    rename(any_of(rename_map))
  
  return(data)
}
