
# Function to get inputs selected by the user and return a translated data frame
GetSelectedInputs <- function(ID = inputsdata, IF = interface, lang = language) {
  tryCatch({
    ID <- data.frame(name = names(ID), value = unname(ID))                          # convert named chr to data frame with two columns
    ID$name <- gsub("\\d$", "", ID$name)                                            # remove trailing digits from $name    - eg. height1 -> height         
    ID$objecttype <- IF$objecttype[match(ID$name, IF$criteria)]                     # add $objecttype to ID
    ID$side <- IF$side[match(ID$name, IF$criteria)]                                 # add $side to ID
    
    # sometimes the side is not found - registered as "NA" - we will try to find it by IF$BigCriteria
    ID$side <- ifelse(is.na(ID$side), IF$side[match(ID$name, IF$BigCriteria)], ID$side)

    #sometimes the objecttype is not found - registered as "NA" - we will try to find it by IF$BigCriteria
    ID$objecttype <- ifelse(is.na(ID$objecttype), IF$objecttype[match(ID$name, IF$BigCriteria)], ID$objecttype)

    # find $name in IF where IF$criteria == ID$name and replace it by criteria in selected language - eg. "height" -> "Výška"
    translated_names <- IF[[paste0("criteria_", lang)]][match(ID$name, IF$criteria)]
    ID$name <- ifelse(is.na(translated_names), ID$name, translated_names)

    #Sometimes the correct translation can be in IF$BigCriteria
    translated_names <- IF[[paste0("criteria_", lang)]][match(ID$name, IF$BigCriteria)]
    ID$name <- ifelse(is.na(translated_names), ID$name, translated_names)

    # concat "value" of records with same name if record objecttype is "sliderinput"
    ID <- ID %>%
      group_by(name, objecttype, side) %>%
      reframe(value = ifelse(objecttype == "sliderInput", 
      paste(value, collapse = "-"), value)) %>%
      ungroup()

    # do the same for ID$value and IF$choice - eg. "True" -> "Ano"
    ID$value <- ifelse(
      is.na(match(ID$value, IF$choice)),
      ID$value,
      IF[[paste0("choice_", lang)]][match(ID$value, IF$choice)]
    )

    # We want to simplify - when objecttype is "checkboxInput" - set value to "Selected"
    ID$value <- ifelse(ID$objecttype == "checkboxInput", "Selected", ID$value)

    ID <- ID[!duplicated(ID),]                                                    # remove duplicates
    ID <- ID[order(ID$side, decreasing = TRUE),]                                  # order by side in descending order
    ID$objecttype <- NULL                                                         # remove $objecttype column         
    
    return(ID)
  }, error = function(e) {
    stop("#get_SelectedInputs# - Error processing selected inputs: ", e$message)
  })
}
DownloadHeadline_translate <- function(HeadlineToTranslate, language = "en") {
  # Translate the headline of the download page - takes data from translate_plot_categories
  # where type == "Download page headline" and tries to match inputted text to it

  if (language == "en") {
    return(HeadlineToTranslate)
  }
  if (!is.character(HeadlineToTranslate) || nchar(HeadlineToTranslate) == 0) {
    return("DownloadHeadline_translate unable to translate headline.")
  }
  if (!is.character(language) || nchar(language) == 0) {
    language <- "en"
  }

  language <- paste0("choice_",language)
  vocabulary <- read.csv("R/translate_plot_categories.csv", sep = ";", header = TRUE)

  # We only need relevant rows - eg. "Download page headline"
  vocabulary <- vocabulary[vocabulary$type == "Download page headline", ]

  # to make the translator more tolerant to changes
  TranslatedHeadline <- vocabulary[vocabulary$choice_en == HeadlineToTranslate, language]

  # Handle case where translation is not found
  if (is.na(TranslatedHeadline) || nchar(TranslatedHeadline) == 0) {
    warning("Translation not found for the given headline.")
    return(HeadlineToTranslate)
  }

  return(TranslatedHeadline)
}

# Function to create a combined plot with a table for download - takes selected Inputs, plot and both tables and combines them into a single plot
CombinePlotsForDownload <- function(language = "en", interface = "", DataSuitability = "", plotSuitability = "", inputsdata = "") {
  ChosenInputs <- GetSelectedInputs(ID = inputsdata, IF = interface, lang = language)
  TranslatedHeadline <- DownloadHeadline_translate("Report of Tree Suitability by AgroForesTreeAdvice", language = language)

  tryCatch({
    # Wrap text in the 'name' and 'value' columns
    ChosenInputs$value <- sapply(ChosenInputs$value, function(x) paste(strwrap(x, width = 45), collapse = "\n"))
    ChosenInputs$name <- sapply(ChosenInputs$name, function(x) paste(strwrap(x, width = 45), collapse = "\n"))

    # Count the number of rows in the ChosenInputs table - we will adjust size of text if too much rows
    ChosenInputs_rows <- nrow(ChosenInputs)
    graph_height <- 1  # Default height of the graph

    if (ChosenInputs_rows > 22) {
      ChosenInputs_TextSize <- 0.6
      graph_height <- 0.8
      }
    if (ChosenInputs_rows > 17) {
      ChosenInputs_TextSize <- 0.8
    } else {
      ChosenInputs_TextSize <- 1
    }

    # Split ChosenInputs into two tables based on the 'side' column
    ChosenInputs_responsetrait <- ChosenInputs %>% filter(side == "responsetrait")
    ChosenInputs_responsetrait$side <- NULL
    ChosenInputs_effecttrait <- ChosenInputs %>% filter(side == "effecttrait")
    ChosenInputs_effecttrait$side <- NULL

    # count and compare rows - append empty to the smaller table
    response_rows <- nrow(ChosenInputs_responsetrait)
    effect_rows <- nrow(ChosenInputs_effecttrait)

    # Usually the tables have diff number of rows, so we need to adjust them - we calculate the diff, then append empty rows to the smaller table
    if (response_rows > effect_rows) {
      rows_to_add <- response_rows - effect_rows
      empty_rows <- data.frame(matrix("", nrow = rows_to_add, ncol = ncol(ChosenInputs_effecttrait)))
      colnames(empty_rows) <- colnames(ChosenInputs_effecttrait) # This is to ensure that the empty rows have the same column names as the original table
      ChosenInputs_effecttrait <- rbind(ChosenInputs_effecttrait, empty_rows)
    } else if (effect_rows > response_rows) {
      rows_to_add <- effect_rows - response_rows
      empty_rows <- data.frame(matrix("", nrow = rows_to_add, ncol = ncol(ChosenInputs_responsetrait)))
      colnames(empty_rows) <- colnames(ChosenInputs_responsetrait) # This is to ensure that the empty rows have the same column names as the original table
      ChosenInputs_responsetrait <- rbind(ChosenInputs_responsetrait, empty_rows)
    }


    # Function to create table theme
    createTable <- function(SetLengthOutput = integer(20), text_size = integer(1)) {
      table_theme <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=SetLengthOutput)), col = NA),
                  fg_params = list(cex = text_size)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA)))
      
      return(table_theme)
    }

    table_theme <- createTable(20)

    # Convert float to int and NA to 0 in DataSuitability (ignore the 'species' column)
    DataSuitability <- DataSuitability %>% 
      mutate(across(-species, ~ ifelse(is.na(.), 0, as.integer(.))))

    # delete too long lines in the table
    colnames(DataSuitability) <- sapply(colnames(DataSuitability), function(x) {
      if (nchar(x) > 30) {x <- substr(x, 1, 30)} 
      return(x)
    })

    # Wrap column headers and delete the too long ones
    colnames(DataSuitability) <- sapply(colnames(DataSuitability), function(x) {
      if (nchar(x) > 20) {
        paste0(substr(x, 1, 15), "\n", substr(x, 16, nchar(x)))
      } else {
        x
      }
    })

    # Create the table with adjusted column widths and rotated column names
    table_TreeScoring <- tableGrob(head(DataSuitability, 20), 
                    theme = ttheme_default(colhead = list(fg_params = list(rot = 90, just = "right"))), 
                    rows = NULL)
    
    # Create a headline with a sublabel for the current date
    headline <- ggdraw() + 
      draw_label(TranslatedHeadline , fontface = 'bold', size = 20, x = 0.2, hjust = 0) +
      draw_label(paste("Date:", Sys.Date()), fontface = 'italic', size = 12, x = 0.2, hjust = 0, y = -1) +
      theme(plot.margin = margin(0, 10, 20, 0))  # Add space below the headline

    table_SelectedInputs_responsetrait <- tableGrob(ChosenInputs_responsetrait,
      theme = createTable(SetLengthOutput = nrow(ChosenInputs_responsetrait), text_size = ChosenInputs_TextSize), 
      rows = NULL)

    table_SelectedInputs_effecttrait <- tableGrob(ChosenInputs_effecttrait,
      theme = createTable(SetLengthOutput = nrow(ChosenInputs_effecttrait), text_size = ChosenInputs_TextSize), 
      rows = NULL)

    # Combine the SelectedInputs tables into one row
    selected_inputs_combined <- plot_grid(
      NULL,
      table_SelectedInputs_responsetrait,
      NULL,
      table_SelectedInputs_effecttrait,
      NULL,
      align = "hv", 
      rel_widths = c(0.4, 1, 1.8, 1, 0.4),  # Adjust widths to add space between tables
      ncol = 5
    )

    # Combine all elements into a single plot
    combined <- plot_grid(
      headline, 
      NULL,
      selected_inputs_combined, 
      NULL,
      plotSuitability + theme(plot.margin = margin(t = 0, b = 0, r = -80, l = -80, unit = "pt")),
      NULL,
      table_TreeScoring, 
      ncol = 1, 
      rel_heights = c(0.07, 0.15, 0.2, 0.2, 1*graph_height, 0.1, 1, 1),  # Adjust heights to add space between elements
      align = "h", 
      axis = "l"  
    )

    # Add top, left and bottom margins
    combined <- combined + theme(plot.margin = margin(t = 10, l = 110, r = 110, b = 50, unit = "pt"))

  }, error = function(e) {
    stop("#CombinePlotsForDownload# - Error creating combined plot: ", e$message)
  })
  return(combined)
}

# Function to create a table with additional information about the trees
create_dataINFO_plot <- function(datainfo = datainfo, language = "en") {
  tryCatch({
    
    #get average lenght of each column
    avg_length <- sapply(datainfo, function(column) {
      mean(nchar(as.character(column)), na.rm = TRUE)
    })
    TranslatedHeadline <- DownloadHeadline_translate("Additional informations about the trees by AgroForesTreeAdvice", language = language)

    wrapCoef <- 0.6                                                                                  # When to wrap the text in the table cells
    coreTextSize <- 0.9                                                                              # Font size for the table cells

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
      draw_label(TranslatedHeadline , fontface = 'bold', size = 20, x = 0, hjust = 0) +
      draw_label(paste("Date:", Sys.Date()), fontface = 'italic', size = 12, x = 0, hjust = 0, y = 0) 

    # Combine all elements into a single plot
    combined <- plot_grid(
      headline, 
      NULL,
      dataINFO_table, 
      ncol = 1, 
      align = "h",
      axis = "lt",
      rel_heights = c(0.05, 0, 1.2),
      rel_widths = c(1, 1, 0.8) 
    )

    # Add top, left and bottom margins
    combined <- combined + theme(plot.margin = margin(t = 10, l = 80, r = 80, b = 0, unit = "pt"))
    return(combined)
 
  }, error = function(e) {
      message("###create_dataINFO_plot - error:", e$message)
      return("Error creating dataINFO plot")
    })
}
