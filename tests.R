library(dplyr)

load("inputsdata.RData")
load("interface.RData")

language <- "choice_en"
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

# concat "value" of records with same name if record objecttype is "sliderinput"
inputsdata3 <- inputsdata2 %>%
  group_by(name, objecttype) %>%
  summarise(value = ifelse(objecttype == "sliderInput", paste(value, collapse = "-"), value)) %>%
  ungroup()

#drop duplicates
inputsdata3 <- inputsdata3[!duplicated(inputsdata3),]
