{
renv::snapshot()
source("R/Utilities/Helpers/restore_renv.R")
source("R/Utilities/Helpers/load_dynamic_config.R")
config <- load_dynamic_config(computer = "auto", config_path = "All_run/config_dynamic.yaml")
source("R/Scripts/00a_environment_setup.R")
source("R/Scripts/00b_setup.R")
source("R/Scripts/00c_import.R")
source("R/Scripts/01_phenotypic_data.R")
source("R/Scripts/02_physiologic_data.R")
source("R/Scripts/03_glucose_uptake.R")
source("R/Scripts/04_render_figures.R")
}
