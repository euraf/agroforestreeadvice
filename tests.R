library(dplyr)

load("inputsdata.RData")
load("interface.RData")

language <- "choice_cz"
language <- gsub("choice", "", language)

# convert named chr to data frame with two columns
inputsdata2 <- data.frame(name = names(inputsdata), value = unname(inputsdata))

#in name column drop last char if it is number
inputsdata2$name <- gsub("\\d$", "", inputsdata2$name)

# find the $objecttype in interface where interface$criteria == inputsdata2$name
inputsdata2$objecttype <- interface$objecttype[match(inputsdata2$name, interface$criteria)] 

# find $name in interface where interface$criteria == inputsdata2$name and replace it by criteria_cz
inputsdata2$name <- interface[[paste0("criteria", language)]][match(inputsdata2$name, interface$criteria)]

# do the same for inputsdata2$value and interface$choice
inputsdata2$value <- ifelse(
  is.na(match(inputsdata2$value, interface$choice)),
  inputsdata2$value,
  interface[[paste0("choice", language)]][match(inputsdata2$value, interface$choice)]
)

