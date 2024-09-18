
library(dplyr)
library(stringr)
#library(tidyverse)
library(purrr)          
library(shiny.i18n)     # for translations in the app
library(cowplot)        # for ggplot2 plots in download
library(gridExtra)
library(rsvg)           # convert svg to png in downloads
##global----

dataCzech<-read.table("models/dataCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceCzech<-read.table("models/interfaceCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
load("datainfo.RData")

translator <- function(data, interface, language) {
  data <- datainfo
  interface <- interfaceCzech
  language <- "choice_cz"
  vocabulary <- read.csv("R/translate_plot_categories.csv", sep = ",", header = TRUE)

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
translated_info <- translator(dataCzech, interfaceCzech, "choice_cz")
