
# Function to get selected inputs and return a translated data frame
GetSelectedInputs <- function(ID = inputsdata, IF = interface, lang = language) {
  tryCatch({
    ID <- data.frame(name = names(ID), value = unname(ID))                          # convert named chr to data frame with two columns
    ID$name <- gsub("\\d$", "", ID$name)                                            # remove trailing digits from $name    - eg. height1 -> height         
    ID$objecttype <- IF$objecttype[match(ID$name, IF$criteria)]                     # add $objecttype to ID
    ID$side <- IF$side[match(ID$name, IF$criteria)]                                 # add $side to ID

    # find $name in IF where IF$criteria == ID$name and replace it by criteria in selected language - eg. "height" -> "Výška"
    ID$name <- IF[[paste0("criteria_", lang)]][match(ID$name, IF$criteria)]

    # do the same for ID$value and IF$choice - eg. "True" -> "Ano"
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

    ID <- ID[!duplicated(ID),]                                                    # remove duplicates
    ID <- ID[order(ID$side, decreasing = TRUE),]                                  # order by side in descending order
    ID$objecttype <- NULL                                                         # remove $objecttype column         
    
    return(ID)
  }, error = function(e) {
    stop("#get_SelectedInputs# - Error processing selected inputs: ", e$message)
  })
}

# Function to create a combined plot with a table for download - takes selected Inputs, plot and both tables and combines them into a single plot
CombinePlotsForDownload <- function(language = "en", interface, DataSuitability, ComputedPlot) {
  ChosenInputs <- GetSelectedInputs(ID = computedInputs, IF = interface, lang = language)

  save(computedInputs, file = "computedInputs.RData")
  save(DataSuitability, file = "DataSuitability.RData")
  save(ComputedPlot, file = "ComputedPlot.RData")
  save(interface, file = "interface.RData")
  # load("computedInputs.RData")
  # load("DataSuitability.RData")
  # load("ComputedPlot.RData")
  # load("interface.RData")
  # lang <- "cz"

  tryCatch({
    # Wrap text in the 'name' and 'value' columns
    ChosenInputs$value <- sapply(ChosenInputs$value, function(x) paste(strwrap(x, width = 50), collapse = "\n"))

    # Split ChosenInputs into two tables based on the 'side' column
    ChosenInputs_responsetrait <- ChosenInputs %>% filter(side == "responsetrait")
    ChosenInputs_responsetrait$side <- NULL
    ChosenInputs_effecttrait <- ChosenInputs %>% filter(side == "effecttrait")
    ChosenInputs_effecttrait$side <- NULL

    # Function to create table theme
    createTable <- function(SetLengthOutput = integer(20)) {
      table_theme <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=SetLengthOutput)), col = NA)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA)))
      
      return(table_theme)
    }

    table_theme <- createTable(20)
    DataSuitability$species <- sapply(DataSuitability$species, function(x) paste(strwrap(x, width = 40), collapse = "\n"))

    # Convert float to int and NA to 0 in DataSuitability (ignore the 'species' column)
    DataSuitability <- DataSuitability %>% 
      mutate(across(-species, ~ ifelse(is.na(.), 0, as.integer(.))))

    # Create the table with adjusted column widths and rotated column names
    table_TreeScoring <- tableGrob(head(DataSuitability, 20), 
                                  theme = ttheme_default(colhead = list(fg_params = list(rot = 90, just = "right"))), 
                                  rows = NULL)
    
    # Create a headline with a sublabel for the current date
    headline <- ggdraw() + 
      draw_label("Report of Tree Suitability by AgroForesTreeAdvice", fontface = 'bold', size = 20, x = 0.2, hjust = 0) +
      draw_label(paste("Date:", Sys.Date()), fontface = 'italic', size = 12, x = 0.2, hjust = 0, y = -1) +
      theme(plot.margin = margin(0, 10, 20, 0))  # Add space below the headline

    table_SelectedInputs_responsetrait <- tableGrob(ChosenInputs_responsetrait,
      theme = createTable(nrow(ChosenInputs_responsetrait)), rows = NULL)

    table_SelectedInputs_effecttrait <- tableGrob(ChosenInputs_effecttrait,
      theme = createTable(nrow(ChosenInputs_effecttrait)), rows = NULL)

    ComputedPlot <- ComputedPlot + 
      scale_y_discrete(labels = function(x) sapply(x, function(y) ifelse(nchar(y) > 25, substr(y, 1, 25), y)))

    # Combine the SelectedInputs tables into one row
    selected_inputs_combined <- plot_grid(
      NULL,
      table_SelectedInputs_responsetrait,
      NULL,
      table_SelectedInputs_effecttrait,
      NULL,
      align = "hv", 
      rel_widths = c(0.5, 0.9, 2, 0.9, 0.5),  # Adjust widths to add space between tables
      ncol = 5
    )

    # Combine all elements into a single plot
    combined <- plot_grid(
      headline, 
      NULL,
      selected_inputs_combined, 
      NULL,
      ComputedPlot, 
      NULL,
      table_TreeScoring, 
      ncol = 1, 
      rel_heights = c(0.08, 0.1, 0.2, 0.2, 1, 0.1, 1, 1),  # Adjust heights to add space between elements
      align = "h", 
      axis = "l"  
    )

    # Add top, left and bottom margins
    combined <- combined + theme(plot.margin = margin(t = 20, l = 100, r = 100, b = 70, unit = "pt"))

  }, error = function(e) {
    stop("#CombinePlotsForDownload# - Error creating combined plot: ", e$message)
  })
  return(combined)
}

create_dataINFO_plot <- function() {
  tryCatch({
    
    #get average lenght of each column
    avg_length <- sapply(datainfo, function(column) {
      mean(nchar(as.character(column)), na.rm = TRUE)
    })

    wrapCoef <- 0.65                                                                                    # Coefficient to adjust the width of the columns
    coreTextSize <- 0.85                                                                                # Font size for the table cells

    # dynamically adjust the width of the columns based on the average length of the data
    datainfo <- as.data.frame(mapply(function(column, width) {
      sapply(column, function(x) paste(strwrap(as.character(x), width = width), collapse = "\n"))
    }, datainfo, avg_length*wrapCoef, SIMPLIFY = FALSE))

    datainfo <- replace(datainfo, datainfo == "NA", "")                                                 # replace "NA" with empty string

    # Customize the table theme to have smaller text
    table_theme <- ttheme_default(
      core = list(fg_params = list(cex = coreTextSize)),
      colhead = list(fg_params = list(cex = 1.2)),
      rowhead = list(fg_params = list(cex = 1.2))
    )

    dataINFO_table <- tableGrob(head(datainfo, 20), theme = table_theme, rows = NULL)
    
    # Create a headline with a sublabel for the current date
    headline <- ggdraw() + 
      draw_label("Additional informations about the trees by AgroForesTreeAdvice", fontface = 'bold', size = 20, x = 0, hjust = 0) +
      draw_label(paste("Date:", Sys.Date()), fontface = 'italic', size = 12, x = 0, hjust = 0, y = 0) 

    # Combine all elements into a single plot
    combined <- plot_grid(
      headline, 
      NULL,
      dataINFO_table, 
      ncol = 1, 
      align = "h",
      axis = "lt",
      rel_heights = c(0.05, 0.001, 1)
    )

    # Add top, left and bottom margins
    combined <- combined + theme(plot.margin = margin(t = 20, l = 50, r = 50, b = 70, unit = "pt"))

    # Save the combined plot as an SVG file
    svg("test_output_datainfo.svg", height = 19, width = 14)  # A4 for ref: 8.27 x 11.69 inches - relative: 1,413542
    print(combined) 
    dev.off()
  }, error = function(e) {
    stop("#CombinePlotsForDownload# - Error creating combined plot: ", e$message)
  })
}


# CombinePlotsForDownload()

