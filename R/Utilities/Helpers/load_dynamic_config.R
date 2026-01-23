load_dynamic_config <- function(computer = "auto", config_path = "All_Run/config_dynamic.yaml") {
  #' Load Dynamic Configuration
  #'
  #' Simple wrapper to load yaml configuration file
  #'
  #' @param computer Placeholder for compatibility (not used in this repo)
  #' @param config_path Path to yaml config file
  #' @return Config list
  
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }
  
  yaml::read_yaml(config_path)
}
