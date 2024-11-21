library(dplyr)

load("inputsdata.RData")
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
  return(ID)
}


test <- get_SelectedInputs(inputsdata, interface, "cz")
