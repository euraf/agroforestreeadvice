
# Function to get inputs selected by the user and return a translated data frame
GetSelectedInputs <- function(ID = inputsdata, IF = interface, lang = language) {
  df <- data.frame() # empty temp dataframe for conversion of list to dataframe
  tryCatch({
    # Convert the list to a dataframe - if the value is an atomic vector, create multiple rows
    for (name in names(ID)) {
      value <- ID[[name]]
      if (is.null(value)) {
        next # Skip NULL values
      } 
      if (is.atomic(value) && length(value) > 1) {
        temp_df <- data.frame(name = name, value = value, stringsAsFactors = FALSE)
      } else {
        temp_df <- data.frame(name = name, value = as.character(value), stringsAsFactors = FALSE)
      }
      df <- rbind(df, temp_df)
    }
    ID <- df # we assign the converted dataframe back to ID

    # we will drop false rows - as they were not selected
    ID <- ID[ID$value != "FALSE",]

    ID$objecttype <- IF$objecttype[match(ID$name, IF$criteria)]                            # convert named chr to data frame with two columns
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
    print(ID)
    # translate the column names
    if (lang == "cz") {
      colnames(ID) <- c("Vlastnost", "side", "Hodnota")
    } else if (lang == "de") {
      colnames(ID) <- c("Eigenschaft", "side", "Wert")
    } else if (lang == "fr") {
      colnames(ID) <- c("Caractéristique", "side", "Valeur")
    } else if (lang == "es") {
      colnames(ID) <- c("Característica", "side", "Valor")
    } else if (lang == "it") {
      colnames(ID) <- c("Caratteristica", "side", "Valore")
    } else if (lang == "pl") {
      colnames(ID) <- c("Cecha", "side", "Wartość")
    } else if (lang == "pt") {
      colnames(ID) <- c("Característica", "side", "Valor")
    } else if (lang == "nl") {
      colnames(ID) <- c("Kenmerk", "side", "Waarde")
    } else {
      colnames(ID) <- c("name", "side", "value")
    }
  
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
    ChosenInputs[[3]] <- sapply(ChosenInputs[[3]], function(x) paste(strwrap(x, width = 45), collapse = "\n"))
    ChosenInputs[[1]] <- sapply(ChosenInputs[[1]], function(x) paste(strwrap(x, width = 45), collapse = "\n"))

    # Count the number of rows in the ChosenInputs table - we will adjust size of text if too much rows
    ChosenInputs_rows <- nrow(ChosenInputs)
    graph_height <- 1  # Default height of the graph

    if (ChosenInputs_rows > 18) {
      ChosenInputs_TextSize <- 0.8
      graph_height <- 0.45
      }
    if (ChosenInputs_rows > 14) {
      ChosenInputs_TextSize <- 0.9
      graph_height <- 0.55
    } else {
      ChosenInputs_TextSize <- 1
      graph_height <- 0.75
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
        core = list(
          bg_params = list(fill = c(rep(c("white", "grey95"), length.out = SetLengthOutput)), col = "black"),
          fg_params = list(cex = text_size, col = "black")
        ),
        colhead = list(
          bg_params = list(fill = "white", col = "black"),
          fg_params = list(col = "black")
        ),
        rowhead = list(
          bg_params = list(fill = "white", col = "black"),
          fg_params = list(col = "black")
        )
      )
      
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
    table_TreeScoring <- tableGrob(
      head(DataSuitability, 20), 
      theme = ttheme_default(
        core = list(
          fg_params = list(col = "black"),
          bg_params = list(fill = "white", col = "black")
        ),
        colhead = list(
          fg_params = list(rot = 90, just = "right", col = "black"),
          bg_params = list(fill = "white", col = "black")
        ),
        rowhead = list(
          fg_params = list(col = "black"),
          bg_params = list(fill = "white", col = "black")
        )
      ), 
      rows = NULL
    )
    
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
      rel_heights = c(0.07, 0.3, 0.2, 0.3, 1*graph_height, 0.2 ,1),  # Adjust heights to add space between elements
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