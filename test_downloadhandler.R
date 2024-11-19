library(gridExtra)
library(ggplot2)
library(cowplot)
library(grid)

  # Function to create a combined plot with a table for download
  load("DataSuitability.RData")
  load("plotting.RData")
  create_combined_plot <- function() {
    #save(DataSuitability, file = "DataSuitability.RData")
    #save(plotting, file = "plotting.RData")
    load("DataSuitability.RData")
    load("plotting.RData")

    # Style the table grob
    table_theme <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=20)), col = NA)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA))
    )
    table_grob <- tableGrob(head(DataSuitability, 20), theme = table_theme, rows = NULL)

    # Create a headline with a sublabel for the current date
    headline <- ggdraw() + 
      draw_label("Report of Tree Suitability by AgroForesTreeAdvice", fontface = 'bold', size = 20, x = 0, hjust = 0) +
      draw_label(paste("Date:", Sys.Date()), fontface = 'italic', size = 12, x = 0, hjust = 0, y = -0.4) +
      theme(plot.margin = margin(0, 10, 20, 0))  # Add space below the headline

    # Combine the elements into a single plot
    combined <- plot_grid(
      headline, NULL, plotting, NULL, table_grob, 
      ncol = 1, 
      rel_heights = c(0.08, 0.01, 1, 0.05, 1)  # Adjust heights to add space between elements
    )

    # Wrap the combined plot in a ggdraw to add a bottom margin
    combined_with_margin <- ggdraw(combined) + 
      theme(plot.margin = margin(5, 5, 5, 5))
    return(combined_with_margin)
    }


  combined <- create_combined_plot()
  svg("output.svg", width = 17, height = 15)
  print(combined)
  dev.off()
