
# ============================================================
# REUSABLE PLOT FUNCTION
# ============================================================

save_and_show_plot <- function(
    plot,
    filename,
    width = 12,
    height = 6,
    dpi = 300
) {
  
  # Ensure plots directory exists
  if (!dir.exists("plots")) {
    dir.create("plots", recursive = TRUE)
  }
  
  # Save plot as PNG
  ggplot2::ggsave(
    filename = file.path("plots", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  
  # Display plot in RStudio
  print(plot)
  
  cat(
    "\nPlot saved and displayed:",
    file.path("plots", filename),
    "\n"
  )
}
