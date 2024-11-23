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
    reframe(value = ifelse(objecttype == "sliderInput", 
      paste(value, collapse = "-"), value)) %>%
    ungroup()

  #drop duplicates
  ID <- ID[!duplicated(ID),]

  # sort by side
  ID <- ID[order(ID$side, decreasing = TRUE),]

  #drop column objecttype
  ID$objecttype <- NULL
  return(ID)
}


create_combined_plot <- function() {
    # Load necessary data
    load("DataSuitability.RData")
    load("plotting.RData")
    load("inputsdata.RData")
    ChosenInputs <- get_SelectedInputs(inputsdata, interface, "cz")

    # Wrap text in the 'name' and 'value' columns
    ChosenInputs$value <- sapply(ChosenInputs$value, function(x) paste(strwrap(x, width = 50), collapse = "\n"))

    # Style the table grob for DataSuitability
    table_theme <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=20)), col = NA)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA))
    )
    DataSuitability$species <- sapply(DataSuitability$species, function(x) paste(strwrap(x, width = 50), collapse = "\n"))
    table_TreeScoring <- tableGrob(head(DataSuitability, 20), theme = table_theme, rows = NULL)

    # Create a headline with a sublabel for the current date
    headline <- ggdraw() + 
      draw_label("Report of Tree Suitability by AgroForesTreeAdvice", fontface = 'bold', size = 20, x = 0.2, hjust = 0) +
      draw_label(paste("Date:", Sys.Date()), fontface = 'italic', size = 12, x = 0.2, hjust = 0, y = -1) +
      theme(plot.margin = margin(0, 10, 20, 0))  # Add space below the headline

    table_theme2 <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=nrow(ChosenInputs))), col = NA)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA))
    )
    table_SelectedInputs <- tableGrob(ChosenInputs, theme = table_theme2, rows = NULL)

    # Modify the plotting object to truncate y-axis labels
    plotting <- plotting + 
      scale_y_discrete(labels = function(x) sapply(x, function(y) substr(as.character(y), 1, 30)))

    # Combine the elements into a single plot
    combined <- plot_grid(
      headline, NULL, table_SelectedInputs, NULL, plotting, NULL, table_TreeScoring, NULL,  
      ncol = 1, 
      rel_heights = c(0.08, 0.04, 0.80, 0.05, 1, 0.11, 1, 0.1),  # Adjust heights to add space between elements
      align = "v",  # Align the elements vertically
      axis = "l",  # Align the elements to the left
      labels = c("A", "B", "C", "D", "E", "F", "G", "H")  # Add labels to the elements
    )

    # Wrap the combined plot in a ggdraw to add a bottom margin
    combined_with_margin <- ggdraw(combined) + 
      theme(plot.margin = margin(5, 5, 5, 5))

    # Save the combined plot as an SVG file
    svg("test_output.svg", width = 22, height = 20)
    print(combined_with_margin) 
    dev.off()
}

create_combined_plot()
