
#library(plotly)
library(shinydashboard) #for Dashboard appearance
library(DT) #for Data Table
library(bslib) #for tooltip
library(dplyr)
library(stringr)
#library(tidyverse)
library(purrr)          
library(shiny.i18n)     # for translations in the app
library(cowplot)        # for ggplot2 plots in download
library(gridExtra)
library(rsvg)           # convert svg to png in downloads
##global----
dataCzech<-read.table("models/dataCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep=";", skipNul =TRUE, header=TRUE)
interfaceCzech<-read.table("models/interfaceCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep=";", header=TRUE)
dataCzech <- data.frame(lapply(dataCzech, function(x) {if (is.character(x)) {return(trimws(x))} else {return(x)}}))
interfaceCzech <- data.frame(lapply(interfaceCzech, function(x) {if (is.character(x)) {return(trimws(x))} else {return(x)}}))

load("dfSuitability.RData")
load("allinputs.RData")

Hard_criteria_filter <- function(db, inputsdata, interface) {
  # Filter trees - if 999 in weightwithincriteria, then it is a hard criteria and algorithm will drop trees which do not meet it 
  # it will check if values of these criteria is 1 - if not - drop them
  # In esence, it checks what is hard criterium and then drops trees which do not have it as 1
  db <- dfSuitability
  inputsdata <- allinputs
  interface <- interfaceCzech

  
  # Consolidate criteria from interface based on inputsdata and slider inputs
  used_criteria <<- rbind(interface[interface$criteria %in% names(inputsdata) &
                                    interface$weightwithincriteria == 999 & !is.na(interface$weightwithincriteria),],
                         interface[interface$objecttype == "sliderInput",])

  #drop info side
  used_criteria <<- used_criteria[used_criteria$side != "info",]

  # drop criteria which weightwithincriteria is not 999
  used_criteria <- used_criteria[used_criteria$weightwithincriteria == 999,]
  used_criteria_uniq <- unique(used_criteria$criteria)

  unique_db <- db[FALSE, ]
  unique_species <- unique(db$species)

  # loop species, find values for each relevant criteria of the species, and if there is a zero, then add "yes" to delete column
  for (species in unique_species) {
      species_data <- db[db$species == species, ]
      
      # Get only the criteria that are relevant
      criteria_data <- species_data[species_data$criteria %in% used_criteria_uniq, ]
      
      # Get unique values of the criteria
      unique_values <- unique(criteria_data$value)
      unique_criteria <- unique(criteria_data$criteria)
      
      # Determine if there are any zeros in the values
      delete_flag <- if("0" %in% unique_values) "yes" else "no"
      
      # Create a new row to be added to unique_db
      new_row <- data.frame(
          species = species,
          value = paste(unique_values, collapse = ", "),
          criteria = paste(unique_criteria, collapse = ", "),
          delete = delete_flag)
      
      # Append new row to unique_db
      unique_db <- rbind(unique_db, new_row)
  }

  # Select species where 'delete' is "yes"
  delete_species <- unique_db$species[unique_db$delete == "yes"]

  # drop these species
  db <- db[!db$species %in% delete_species, ]

  return(db)
}
xxx2 <-Hard_criteria_filter()







# Filter the DataFrame where 'weightwithincriteria' is 999
df <- interfaceCzech
filtered_df = df[df['weightwithincriteria'] == 999]

# Extract the 'criteria' column
criteria_data = filtered_df['criteria']

# Print the extracted data
print(criteria_data)
