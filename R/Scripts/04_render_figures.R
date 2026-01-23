#* 4 Render Figures
#! Journal width req... single column = <=3.5 in
#! Double (side legend) 4-5in
#! 6-7.15 in for full page width
#+ 4.1: Figure 1
#- 4.1.1: Align all plots as a 2x2 grid to ensure both x and y axis alignment
aligned_plots <- align_plots(p1A, p1B, p1C, p1D, align = "hv", axis = "tblr")
#- 4.1.2: Render Figure 1
fig1 <- ggdraw(xlim = c(0, 8.5), ylim = c(0, 11)) +
  draw_plot(aligned_plots[[1]], x = 0.668, y = 7.2525, width = 7.153, height = 2.7) +
  draw_plot(aligned_plots[[2]], x = 0.668, y = 4.2275, width = 3.5, height = 2.7) +
  draw_plot(aligned_plots[[3]], x = 4.46, y = 4.2275, width = 3.361, height = 2.7) +
  draw_plot(aligned_plots[[4]], x = 0.668, y = 1.5025, width = 3.5, height = 2.4) +
  # Labels
  figure_labels(list(
    A = c(0.92, 10.12),
    B = c(0.74, 6.975),
    C = c(4.392, 6.975),
    D = c(0.74, 4.07),
    "Figure 1" = c(0.49, 10.43)
  ))
#+ 4.2: Figure 2
fig2 <- ggdraw(xlim = c(0, 8.5), ylim = c(0, 11)) +
  # 2A
  draw_plot(p2a, x = 0.668, y = 7.2525, width = 3.5, height = 2.7) +
  draw_plot(p2b, x = 4.32, y = 7.2525, width = 3.5, height = 2.7) +
  draw_plot(p2c, x = 0.668, y = 4.2275, width = 3.5, height = 2.7) +
  draw_plot(p2d, x = 4.32+.14, y = 4.2275, width = 3.361, height = 2.7) +
  draw_plot(p2e, x = 0.668, y = 1.5025, width = 3.5, height = 2.4) +
  # Labels
  figure_labels(list(
    A = c(0.74, 10.12),
    B = c(4.392, 10.12),
    C = c(0.74, 7.095),
    D = c(4.392, 7.095),
    E = c(0.74, 4.07),
    "Figure 2" = c(0.49, 10.43)
  ))
#+ 4.3: Figure 3
fig3 <- ggdraw(xlim = c(0, 8.5), ylim = c(0, 11)) +
  #- Row 1
  draw_plot(p3a, x = 0.668, y = 6.2525, width = 4.75, height = 4) +
  draw_plot(p3b, x = 5.32, y = 6.2525, width = 2.5, height = 4) +
  #- Row 2
  draw_plot(p3c, x = 0.668, y = 2.8675, width = 4.75, height = 4) +
  draw_plot(p3d, x = 5.32, y = 2.8675, width = 2.5, height = 4) +
  #- Row 3
  draw_plot(p3e, x = 0.668, y = -0.5175, width = 4.75, height = 4) +
  draw_grob(make_legend_gu(), x = 5.888333, y = 0.9191667, width = 1.8, height = 2.18) +
  # Labels
  figure_labels(list(
    A = c(0.74, 10.12),
    B = c(5.392, 10.12),
    C = c(0.74, 6.735),
    D = c(5.392, 6.735),
    E = c(0.74, 3.35),
    "Figure 3" = c(0.49, 10.43)
  ))
#+ 4.4: Figure 4
#- 4.4.1: Read in existing PNG (BioRender figure)
fig4_img <- png::readPNG("Outputs/Figures/raw/fig4_raw.png")
fig4_grob <- grid::rasterGrob(fig4_img, interpolate = TRUE)
#- 4.4.2: Render Figure 4 with same structure as other figures
fig4 <- ggdraw(xlim = c(0, 8.5), ylim = c(0, 11)) +
  draw_grob(fig4_grob, x = 0.125, y = -0.03, width = 8.5, height = 11) +
  # Labels
  figure_labels(list(
    "Figure 4" = c(0.49, 10.43)
  ))
#+ 4.5: Print All Main Figures
#- 4.5.1: Figure 1
print_to_tiff(fig1, "Fig1", output_dir = "Outputs/Figures")
#- 4.5.2: Figure 2
print_to_tiff(fig2, "Fig2", output_dir = "Outputs/Figures")
#- 4.5.3: Figure 3
print_to_tiff(fig3, "Fig3", output_dir = "Outputs/Figures")
#- 4.5.4: Figure 4
print_to_tiff(fig4, "Fig4", output_dir = "Outputs/Figures")
#+ 4.6: Combine All Figures into Single PDF
#- 4.6.1: Read in all TIFF files
fig1_tiff <- tiff::readTIFF("Outputs/Figures/Fig1.tiff", native = FALSE)
fig2_tiff <- tiff::readTIFF("Outputs/Figures/Fig2.tiff", native = FALSE)
fig3_tiff <- tiff::readTIFF("Outputs/Figures/Fig3.tiff", native = FALSE)
fig4_tiff <- tiff::readTIFF("Outputs/Figures/Fig4.tiff", native = FALSE)
#- 4.6.2: Create PDF with all figures (one per page, no downsizing)
pdf("Outputs/Figures/Figs1-4.pdf", width = 8.5, height = 11)
# Figure 1
grid::grid.newpage()
grid::grid.raster(fig1_tiff, interpolate = TRUE)
# Figure 2
grid::grid.newpage()
grid::grid.raster(fig2_tiff, interpolate = TRUE)
# Figure 3
grid::grid.newpage()
grid::grid.raster(fig3_tiff, interpolate = TRUE)
# Figure 4
grid::grid.newpage()
grid::grid.raster(fig4_tiff, interpolate = TRUE)
dev.off()