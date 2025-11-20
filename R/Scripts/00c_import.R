#* 0d: Importing Clinical Metadata and Preprocess
#+ 0d.1: Import Figure Data from Supplemental Excel File
#- 0d.1.1: Load supplemental data file path from config
raw_data <- config$paths$raw_data
#- 0d.1.2: Body weight data
phenotypic_physiologic <- read_excel(raw_data, sheet = "phenotypic_physiologic", skip = 1, col_names = TRUE) %>%
  rename_phenotypic_columns()

Fig_1B <- read_excel(raw_data, sheet = "1B")
Fig_1C <- read_excel(raw_data, sheet = "1C")
Fig_1D <- read_excel(raw_data, sheet = "1D")
#- 0d.1.3: Figure 2 sheets
Fig_2A <- read_excel(raw_data, sheet = "2A")
Fig_2B <- read_excel(raw_data, sheet = "2B")
Fig_2C <- read_excel(raw_data, sheet = "2C")
Fig_2D <- read_excel(raw_data, sheet = "2D")
Fig_2E <- read_excel(raw_data, sheet = "2E")
#- 0d.1.4: Figure 3 sheets
Fig_3ABC <- read_excel(raw_data, sheet = "3ABC")
Fig_3D <- read_excel(raw_data, sheet = "3D")

