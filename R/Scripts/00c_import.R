#* 0d: Importing Clinical Metadata and Preprocess
#+ 0d.1: Import Figure Data from Supplemental Excel File
#- 0d.1.1: Load supplemental data file path from config
raw_data <- config$paths$raw_data
#- 0d.1.2: Phenotypic/Physiologic; calculate derived variables
phenotypic_physiologic <- read_excel(raw_data, sheet = "phenotypic_physiologic") |>
  calc_derived_variables() |>
  mutate(
    ID = as.factor(ID),
    sex = as.factor(sex),
    diet = as.factor(diet)
  )
#- 0d.1.2: Glucose Uptake
glucose_uptake <- read_excel(raw_data, sheet = "GU") |>
  mutate(
    ID = as.factor(ID),
    sex = as.factor(sex),
    diet = as.factor(diet)
  ) |>
  select(-glucose_dose)
#- 0d.1.2: Ex-Vivo Glucose Uptake
ex_vivo_glucose_uptake <- read_excel(raw_data, sheet = "EVGU") |>
  mutate(
    ID = as.factor(ID),
    sex = as.factor(sex),
    diet = as.factor(diet)
  )
