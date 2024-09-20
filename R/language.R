# this file is weak point, the most of translations are handled by i18n library, but that is not suitable for
# translation of dynamic tables, hence the need for these custom functions here, which are hard-coded and heavy-handed


Plot_legend_lang <- function(lang) {
  # Load the translation of the graph legend
  BigCriteria_translates <- read.csv("R/translation/BigCriteria_translates.csv", stringsAsFactors = FALSE)

  #return as character vector use en as name and cz as value
  final <- BigCriteria_translates[[as.character(lang)]]
  final <- setNames(final, BigCriteria_translates[["en"]])

  return(final)
}
# test = Plot_legend_lang("cz")


translator <- function(data, interface, language) {
  # Tries to translate all data in Information table - search in the interface and vocabulary
  vocabulary <- read.csv("R/translate_plot_categories.csv", sep = ";", header = TRUE)

  # Ensure 'info' side exists in 'interface'
  if (!"info" %in% interface$side) {
    warning("No 'info' side found in the 'interface' dataframe.")
    return(data)  # Exit early if no 'info' side to avoid further errors
  }
  valid_values <- c("info", "info_do_not_modify")
  interface <- interface[interface$side %in% valid_values, ]

  # Create a translation map from vocabulary
  translations <- c(setNames(vocabulary[[language]], vocabulary$type),setNames(interface[[language]], interface$choice))
  # Function to translate cell values, handling both simple "furniture" and comma-separated cases "furniture, sports"
    translate_cell <- function(cell, translations) {
    print(cell)
    if (cell %in% names(translations)) { return(translations[cell] )
      } else { return(cell) }
    
    if (str_detect(cell, ", ")) {
      # Handle comma-separated values
      translated_values <- sapply(str_split(cell, ",\\s*")[[1]], function(x) {
        if (x %in% names(translations)) { return(translations[x] )
        } else { return(x) }
      })
      str_c(translated_values, collapse = ", ") # puts cells back together
    }
  }
  # Apply translations to all cells in the data
  
  data <- data %>% 
    mutate(across(everything(), ~sapply(., function(cell) {
      if (is.na(cell)) {
        return(NA)
      } else {
        translate_cell(cell, translations)
        }
      }
      )
    ))
  # Rename columns based on vocabulary
  rename_cols <- filter(vocabulary, object == "DFinfo_headline")
  rename_cols <- setNames(str_to_sentence(rename_cols[[language]]), rename_cols$type)
  names(data) <- sapply(names(data), function(x) ifelse(x %in% names(rename_cols), rename_cols[x], x))

  return(data)
}
#translated_info <- translator(x, interfaceCzech, "cz")


help_text <- function(id, lang) {
  # Load help information - very primitive, returns as list of strings, then is viewed in modal window "Information"
  help_text <- read.csv("R/help_information.csv", stringsAsFactors = FALSE)

  # Gets only "tool" rows containg only actual ID - eg. Czech, DENTRO...
  help_text <- help_text[grepl(id, help_text$tool), ]

  help_text <- help_text[[lang]]
  if (length(help_text) == 0)      {return("No help information found. Add your ID to tool column in help_information.csv or create custom help row.
                                          Please follow the used formating. Tool - add ID of tools which want to view this help. 
                                          Filter and type are only for describtion.")}

  if (length(help_text) %% 2 != 0) {return("Texts are not divisible by two! Check the help_information.csv file. Tool is probably incorrect.
                                            To use filter you need to add your tool ID to both - header and text of wanted Filter.")}

  return(help_text)
}
#help_text("Czech", "cz")