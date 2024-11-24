library(gridExtra)
library(ggplot2)
library(cowplot)
library(grid)
library(dplyr)
library(gridExtra)

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

    # Split ChosenInputs into two tables based on the 'side' column
    ChosenInputs_responsetrait <- ChosenInputs %>% filter(side == "responsetrait")
    ChosenInputs_responsetrait$side <- NULL
    ChosenInputs_effecttrait <- ChosenInputs %>% filter(side == "effecttrait")
    ChosenInputs_effecttrait$side <- NULL

    # Style the table grob for DataSuitability
    table_theme <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=20)), col = NA)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA))
    )
    DataSuitability$species <- sapply(DataSuitability$species, function(x) paste(strwrap(x, width = 40), collapse = "\n"))
    
    # Create a custom theme to rotate the column names
    table_theme <- ttheme_default(
      colhead = list(
        fg_params = list(rot = 90, just = "right")
      )
    )

    # Create the table with adjusted column widths and rotated column names
    table_TreeScoring <- tableGrob(head(DataSuitability, 20), theme = table_theme, rows = NULL)
    
    # Create a headline with a sublabel for the current date
    headline <- ggdraw() + 
      draw_label("Report of Tree Suitability by AgroForesTreeAdvice", fontface = 'bold', size = 20, x = 0.2, hjust = 0) +
      draw_label(paste("Date:", Sys.Date()), fontface = 'italic', size = 12, x = 0.2, hjust = 0, y = -1) +
      theme(plot.margin = margin(0, 10, 20, 0))  # Add space below the headline

    table_theme2 <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=nrow(ChosenInputs_responsetrait))), col = NA)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA))
    )
    table_SelectedInputs_responsetrait <- tableGrob(ChosenInputs_responsetrait, theme = table_theme2, rows = NULL)

    table_theme3 <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=nrow(ChosenInputs_effecttrait))), col = NA)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA))
    )
    table_SelectedInputs_effecttrait <- tableGrob(ChosenInputs_effecttrait, theme = table_theme3, rows = NULL)

    plotting <- plotting + 
      scale_y_discrete(labels = function(x) sapply(x, function(y) ifelse(nchar(y) > 25, substr(y, 1, 25), y)))


    # Combine the SelectedInputs tables into one row
    selected_inputs_combined <- plot_grid(
      table_SelectedInputs_responsetrait,
      NULL,
      table_SelectedInputs_effecttrait,
      NULL,
      NULL,
      rel_widths = c(1, 1, 1, 0.3, 0.3),  # Adjust widths to add space between tables
      ncol = 5
    )

    # Combine the elements into a single plot
    combined <- plot_grid(
      headline, 
      NULL,
      selected_inputs_combined, 
      NULL,
      plotting, 
      NULL,
      table_TreeScoring, 
      ncol = 1, 
      rel_heights = c(0.08, 0.2, 0.2, 0.2, 1, 0.1, 1, 1),  # Adjust heights to add space between elements
      align = "h",  # Align the elements vertically
      axis = "l"  # Align the elements to the left
      #labels = c("A", "B", "C", "D", "E", "F", "G")  # Add labels to the elements
    )


    # Add upper and left margins
    combined <- combined + theme(plot.margin = margin(t = 20, l = 50, unit = "pt"))



    # Save the combined plot as an SVG file
    svg("test_output.svg", height = 18, width = 14)  # A4 for ref: 8.27 x 11.69 inches - relative: 1,413542
    print(combined) 
    dev.off()
}

create_combined_plot()
