#' Check System Dependencies
#'
#' Checks for required system-level dependencies and provides installation
#' instructions if any are missing. This function is non-blocking and will
#' only warn users about missing dependencies.
#'
#' @details
#' This function checks for the following system dependencies:
#' - **udunits**: Required for the units and sf R packages
#' - **libxml2**: Required for the xml2 R package
#'
#' @return Invisibly returns a character vector of missing dependency names.
#'   If all dependencies are found, returns an empty character vector.
#'
#' @author Joshua D. Preston
#' @export
check_system_dependencies <- function() {
  cat("\n🔍 Checking system dependencies...\n")
  
  # Define all system dependencies with installation instructions
  system_deps <- list(
    list(
      name = "udunits",
      pkg_config = "udunits2",
      purpose = "Unit conversion library (required for units and sf R packages)",
      install_mac = "brew install udunits",
      install_linux = "sudo apt-get install libudunits2-dev  # or: sudo yum install udunits2-devel",
      install_windows = "Install via Rtools (https://cran.r-project.org/bin/windows/Rtools/)",
      optional = FALSE
    ),
    list(
      name = "libxml2",
      pkg_config = "libxml-2.0",
      purpose = "XML parsing library (required for xml2 R package)",
      install_mac = "brew install libxml2",
      install_linux = "sudo apt-get install libxml2-dev  # or: sudo yum install libxml2-devel",
      install_windows = "Install via Rtools (https://cran.r-project.org/bin/windows/Rtools/)",
      optional = FALSE
    )
  )
  
  missing_deps <- character(0)
  
  # Helper function to check if library exists
  check_library <- function(lib_name, pkg_config_name = lib_name) {
    # Try pkg-config first
    if (Sys.which("pkg-config") != "") {
      result <- suppressWarnings(
        system2("pkg-config", args = c("--exists", pkg_config_name), 
                stdout = FALSE, stderr = FALSE)
      )
      if (result == 0) return(TRUE)
    }
    
    # Fallback: try to find library files
    os <- Sys.info()[["sysname"]]
    if (os == "Darwin") {
      paths <- c("/usr/local/lib", "/opt/homebrew/lib", "/usr/lib")
    } else if (os == "Linux") {
      paths <- c("/usr/lib", "/usr/local/lib", "/lib", "/usr/lib/x86_64-linux-gnu")
    } else {
      return(FALSE)
    }
    
    lib_pattern <- paste0("lib", lib_name, "\\.(so|dylib|a)")
    for (path in paths) {
      if (dir.exists(path)) {
        files <- list.files(path, pattern = lib_pattern, recursive = TRUE)
        if (length(files) > 0) return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Check each dependency
  for (dep in system_deps) {
    is_available <- check_library(dep$name, dep$pkg_config)
    
    if (!is_available) {
      missing_deps <- c(missing_deps, dep$name)
      cat("⚠️  ", dep$name, " not found\n", sep = "")
      cat("   Purpose: ", dep$purpose, "\n", sep = "")
      cat("   Install:\n")
      cat("     macOS:   ", dep$install_mac, "\n", sep = "")
      cat("     Linux:   ", dep$install_linux, "\n", sep = "")
      cat("     Windows: ", dep$install_windows, "\n", sep = "")
      cat("\n")
    } else {
      cat("✓ ", dep$name, " found\n", sep = "")
    }
  }
  
  # Summary message
  if (length(missing_deps) > 0) {
    cat("⚠️  Warning: ", length(missing_deps), " system dependency(ies) missing: ", 
        paste(missing_deps, collapse = ", "), "\n", sep = "")
    cat("   Some R packages may fail to install until these dependencies are available.\n")
    cat("   You can continue, but installation of units/sf/xml2 packages may fail.\n\n")
  } else {
    cat("✅ All system dependencies found!\n\n")
  }
  
  invisible(missing_deps)
}
