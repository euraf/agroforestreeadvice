library(gridExtra)
library(ggplot2)
library(cowplot)
library(grid)
library(dplyr)

  # Function to create a combined plot with a table for download
load("DataSuitability.RData")
load("plotting.RData")
load("inputsdata.RData")
load("dbfinal.RData")
load("interface.RData")
get_SelectedInputs <- function(ID = inputsdata, IF = interface, lang = language) {
  ID = inputsdata
  IF = interface
  lang = "cz"
  ID <- data.frame(name = names(ID), value = unname(ID))                          # convert named chr to data frame with two columns
  ID$name <- gsub("\\d$", "", ID$name)                                            # remove trailing digits from $name    - eg. height1 -> height         
  ID$objecttype <- IF$objecttype[match(ID$name, IF$criteria)]                     # add $objecttype to ID
  # add $side to ID
  ID$side <- IF$side[match(ID$name, IF$criteria)]

  # find $name in IF where IF$criteria == ID$name and replace it by criteria_cz
  ID$name <- IF[[paste0("criteria_", lang)]][match(ID$name, IF$criteria)]

  # do the same for ID$value and IF$choice
  ID$value <- ifelse(
    is.na(match(ID$value, IF$choice)),
    ID$value,
    IF[[paste0("choice_", lang)]][match(ID$value, IF$choice)]
  )

  # concat "value" of records with same name if record objecttype is "sliderinput"
  ID <- ID %>%
    group_by(name, objecttype, side) %>%
    summarise(value = ifelse(objecttype == "sliderInput", 
      paste(value, collapse = "-"), value)) %>%
    ungroup()

  #drop duplicates
  ID <- ID[!duplicated(ID),]

  # sort by side
  ID <- ID[order(ID$side, decreasing = TRUE),]
  return(ID)
}


create_combined_plot <- function() {
    # Load necessary data
    load("DataSuitability.RData")
    load("plotting.RData")
    load("inputsdata.RData")
    test <- get_SelectedInputs(inputsdata, interface, "cz")

    # Style the table grob for DataSuitability
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

      # Assuming 'test' data frame is defined somewhere in the environment
    table_theme2 <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=nrow(test))), col = NA)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA))
    )
    table_grob2 <- tableGrob(test, theme = table_theme2, rows = NULL)


    # Combine the elements into a single plot
    combined <- plot_grid(
      headline, NULL, plotting, NULL, table_grob, table_grob2,  
      ncol = 1, 
      rel_heights = c(0.08, 0.01, 1, 0.05, 1)  # Adjust heights to add space between elements
    )

    # Wrap the combined plot in a ggdraw to add a bottom margin
    combined_with_margin <- ggdraw(combined) + 
      theme(plot.margin = margin(5, 5, 5, 5))

   

    # Save the combined plot as an SVG file
    svg("test_output.svg", width = 22, height = 17)
    print(combined_with_margin) 
    dev.off()
}

# Assuming 'test' data frame is defined somewhere in the environment
test <- get_SelectedInputs(inputsdata, interface, "cz")
create_combined_plot()

