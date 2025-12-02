{
source("R/Utilities/Helpers/load_dynamic_config.R")
config <- load_dynamic_config(computer = "auto", config_path = "All_run/config_dynamic.yaml")
source("R/Scripts/00a_environment_setup.R")
source("R/Scripts/00b_setup.R")
source("R/Scripts/00c_import.R")
source("R/Scripts/01_phenotypic_data.R")
source("R/Scripts/02_physiologic_data.R")
source("R/Scripts/99_render_figures.R")
}