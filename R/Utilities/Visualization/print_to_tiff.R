#' Print Plot to TIFF with Auto-Refresh for macOS Preview
#'
#' Saves ggplot objects as high-resolution TIFF files with automatic Preview.app 
#' integration on macOS. The function handles file path management, automatic 
#' directory creation, and provides seamless workflow integration with Preview's
#' auto-refresh functionality for iterative plot development.
#'
#' @param plot ggplot object or other plot object compatible with ggplot2::ggsave()
#' @param filename Character string for the TIFF filename. The ".tiff" extension
#'   will be automatically added if not present
#' @param width Numeric value for plot width in inches (default: 8.5 for letter size)
#' @param height Numeric value for plot height in inches (default: 11 for letter size)  
#' @param dpi Numeric value for resolution in dots per inch (default: 600 for publication quality)
#' @param output_dir Character string specifying the output directory. If NULL (default),
#'   uses \code{config$paths$figures} or falls back to a default path
#' @param auto_open Logical indicating whether to automatically open the TIFF in
#'   Preview.app on first save (default: TRUE). Subsequent saves will auto-refresh
#'   if Preview is already open
#' @param compression Character string specifying TIFF compression method. Options:
#'   "lzw" (lossless, default), "none", "jpeg", "zip". LZW provides good compression
#'   without quality loss.
#'
#' @return Invisibly returns the full file path to the created TIFF file
#'
#' @details
#' This function provides a streamlined workflow for saving and viewing plots during
#' analysis. Key features include:
#' 
#' - **Auto-refresh workflow**: Opens TIFF in Preview on first save, subsequent saves
#'   automatically refresh the Preview window
#' - **Path management**: Automatically handles file extensions and directory creation
#' - **Config integration**: Uses dynamic configuration paths when available
#' - **High-quality output**: Defaults to 600 DPI for publication-ready figures
#' - **Lossless compression**: Uses LZW compression by default for smaller file sizes
#'   without quality loss
#' - **Background handling**: Ensures white background for clean output
#'
#' @examples
#' \dontrun{
#'   # Basic usage
#'   p <- ggplot(data, aes(x, y)) + geom_point()
#'   print_to_tiff(p, "scatter_plot")
#'   
#'   # Custom dimensions for wide figure
#'   print_to_tiff(p, "wide_plot", width = 12, height = 6)
#'   
#'   # Specify custom output directory
#'   print_to_tiff(p, "analysis_figure", output_dir = "~/Desktop/figures")
#'   
#'   # No compression for maximum compatibility
#'   print_to_tiff(p, "figure", compression = "none")
#' }
#'
#' @importFrom ggplot2 ggsave
#' @export
print_to_tiff <- function(plot, filename, width = 8.5, height = 11, dpi = 600,
                          output_dir = NULL, auto_open = TRUE, compression = "lzw") {
  
  # Use config path if output_dir not specified
  if (is.null(output_dir)) {
    if (exists("config") && !is.null(config$paths$figures)) {
      output_dir <- config$paths$figures
    } else {
      # Fallback if config not loaded
      output_dir <- "/Users/jdp2019/Desktop/PGD_figures"
      warning("Config not found, using fallback path: ", output_dir)
    }
  }
  
  # Ensure filename has .tiff or .tif extension
  if (!grepl("\\.(tiff?|TIF{1,2})$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".tiff")
  }

  # Create full path
  filepath <- file.path(output_dir, filename)

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Check if file already exists (for auto-open logic)
  file_exists <- file.exists(filepath)

  # Save the plot as TIFF
  ggplot2::ggsave(
    filename = filepath,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = "in",
    device = "tiff",
    compression = compression,
    bg = "white"
  )

  # Auto-open in Preview only on first run (or if specified)
  if (auto_open && !file_exists) {
    system(paste("open", shQuote(filepath)))
    cat("TIFF saved and opened in Preview:", filepath, "\n")
    cat("Preview will auto-refresh when you re-run this function!\n")
  } else {
    cat("TIFF updated:", filepath, "\n")
  }

  # Return path invisibly
  invisible(filepath)
}
