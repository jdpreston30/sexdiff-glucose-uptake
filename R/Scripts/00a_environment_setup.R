#* 0a: Environment Setup
#+ 0a.1: Read and load required packages from DESCRIPTION
#- 0a.1.1: Read DESCRIPTION file
cat("📚 Loading required packages...\n")
desc <- read.dcf("DESCRIPTION")
#- 0a.1.2: Extract CRAN packages from Imports
cran_packages <- gsub("\\s+", "", strsplit(desc[, "Imports"], ",")[[1]])
#- 0a.1.3: Extract Bioconductor packages
bioc_text <- desc[, "Bioconductor"]
bioc_packages <- if (!is.na(bioc_text) && bioc_text != "") {
  gsub("\\s+", "", strsplit(bioc_text, ",")[[1]])
} else {
  character(0)
}
#- 0a.1.4: Combine all packages
all_packages <- c(cran_packages, bioc_packages)
#- 0a.1.5: Load all packages
invisible(sapply(all_packages, library, character.only = TRUE))
cat("✅ Environment setup complete! All required packages loaded.\n")
#+ 0a.2: Check system dependencies
source("R/Utilities/Helpers/check_system_dependencies.R")
check_system_dependencies()
