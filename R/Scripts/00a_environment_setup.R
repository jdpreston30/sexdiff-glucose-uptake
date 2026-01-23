#* 0a: Environment Setup
#+ 0a.1: Verify renv is active
cat("📦 Package environment managed by renv\n")
if (!("renv" %in% loadedNamespaces())) {
  warning("⚠️  renv is not active. Attempting to activate...")
  source("renv/activate.R")
}
#+ 0a.2: Check if packages need to be installed
# Read packages from DESCRIPTION file
desc <- read.dcf("DESCRIPTION")
cran_packages <- trimws(strsplit(desc[, "Imports"], ",\\s*")[[1]])
bioc_text <- if ("Bioconductor" %in% colnames(desc)) {
  desc[, "Bioconductor"]
} else {
  NA
}
bioc_packages <- if (!is.na(bioc_text) && bioc_text != "") {
  trimws(strsplit(bioc_text, ",\\s*")[[1]])
} else {
  character(0)
}
core_packages <- c(cran_packages, bioc_packages)

missing_core <- core_packages[!sapply(core_packages, requireNamespace, quietly = TRUE)]
if (length(missing_core) > 0) {
  cat("⚠️  Core packages missing:", paste(missing_core, collapse = ", "), "\n")
  cat("🔄 Running renv::restore() to install packages...\n")
  cat("   (This may take 10-20 minutes on first run)\n\n")
  # Run renv::restore() automatically
  tryCatch({
    renv::restore(prompt = FALSE)  # No prompt, automatic yes
    cat("\n✅ Package installation complete!\n")
  }, error = function(e) {
    stop("❌ Failed to restore packages: ", e$message, 
         "\n   Please run renv::restore() manually and check for errors.")
  })
  # Verify installation succeeded
  still_missing <- core_packages[!sapply(core_packages, requireNamespace, quietly = TRUE)]
  if (length(still_missing) > 0) {
    stop("❌ Packages still missing after restore: ", paste(still_missing, collapse = ", "),
         "\n   Please check renv::status() for details.")
  }
} else {
  cat("✅ renv environment verified. All core packages available.\n")
}

#+ 0a.3: Load all packages from DESCRIPTION
cat("📚 Loading packages...\n")
invisible(lapply(core_packages, function(pkg) {
  library(pkg, character.only = TRUE)
  cat("  ✓", pkg, "\n")
}))
cat("✅ All packages loaded successfully!\n")

#+ 0a.4: Define custom operators
# String concatenation operator (used in run_lmm.R and other utilities)
`%+%` <- function(x, y) paste0(x, y)

#+ 0a.5: Set up package conflict preferences
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(base::setdiff)
cat("✅ Configuration and environment setup complete!\n")