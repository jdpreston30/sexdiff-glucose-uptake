#* 5 Render Figures
#+ 5.1: Figure 1
fig1 <- ggdraw(xlim = c(0, 8.5), ylim = c(0, 11)) +
  #- 5.2.1: 1A
  draw_plot(p1A, x = 0.93, y = 7.53, width = 4, height = 2.4) +
  #- 5.2.2: 1B
  draw_plot(p1B, x = 5, y = 7.41, width = 3, height = 2.4) +
  #- 5.2.3: 1C
  draw_plot(p1C, x = 0.93, y = 4.58, width = 3, height = 2.4) +
  #- 5.2.4: 1D
  draw_plot(p1D, x = 5, y = 4.58, width = 3, height = 2.4) +
  #- 5.2.5: Labels
  figure_labels(list(
    A = c(0.83, 10.13),
    B = c(4.5, 10.13),
    C = c(0.83, 7.18),
    D = c(4.5, 7.18),
    "Figure 1" = c(0.49, 10.43)
  ))
#+ 5.6: Print All Main Figures
#- 5.7.1: Print as PNGs
print_to_png(fig1,
  "Fig1.png",
  width = 8.5, height = 11, dpi = 600, output_dir = "Outputs/Figures")
