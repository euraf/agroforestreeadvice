#rsconnect::showLogs(appPath=#"D:\\Mes_documents\\a_ABSys\\autreschercheurs\\BertReubens\\DigitAFtreeAdvice\\rsconnect\\shinyapps.io\\gosme\\DigitAFtreeAdvice.dcf", 
#                    "https://gosme.shinyapps.io/DigitAFtreeAdvice/", streaming=TRUE)
#"D:\\Mes_documents\\a_ABSys\\autreschercheurs\\BertReubens\\DigitAFtreeAdvice\\rsconnect\\shinyapps.io\\gosme"

require (shiny)
require (ggplot2) #for the barplot graph
#library(plotly)
require (shinydashboard) #for Dashboard appearance
require (DT) #for Data Table
require (bslib) #for tooltip
#library(reactlog) #to display reactive graph
require (leaflet)#for the map
require (sf) #for the map
require (maps) #for the world map centroids
#options(shiny.reactlog = TRUE)
require (svglite)        # for svg download
require (shinyjs)
require (openxlsx)       # for writing xlsx files in download
require (dplyr)
require (stringr)
require (purrr)          
require (shiny.i18n)     # for translations in the app
require (cowplot)        # for ggplot2 plots in download
require (gridExtra)
require (rsvg)           # convert svg to pdf in downloads
##global----

#load data and interface of all models
source("load_interface_data.R")

#include downloadhandler functions
source("DownloadHandler.R")

# Initialize the translator
i18n <- Translator$new(translation_csvs_path = "R/translation/")
i18n$set_translation_language("en")  # Default language

available_interfaces = ls(pattern = "^interface")
columnnames<-unlist(sapply(available_interfaces, function(x) names(get(x))))
toto<-strsplit(columnnames, split="_")
languages<-unique(sapply(toto[lapply(toto, length)==2],"[[", 2))

reshapecontrols<-function(controls, language, compactconditions=FALSE, compactobjectives){
  print("reshapecontrols")
  #print(str(controls))
  #print(paste("language=", language))
  toto<-strsplit(c(names(controls)), split="_")
  languages<-unique(sapply(toto[lapply(toto, length)==2],"[[", 2))
  #print(paste("languages=", paste(languages, collapse=",")))
  if(is.null(language) ) language<-"en"
  if(! language %in% languages) {print(paste(language, "is not in the languages available for this interface, so defaulting to english"))
    language<-"en"
  }
  #we select the desired language
  toto<-controls[!is.na(controls$criteria)& !is.na(controls$objecttype),c("side", "order", "BigCriteria", "criteria", "choice", "objecttype", paste(c("BigCriteria", "criteria", "choice"),language, sep="_"))]
  names(toto)<-c("side", "order", "BigCriteria", "criteria", "choice", "objecttype", "labelBigCriteria", "labelcriteria", "labelchoice")
  #we first reshape the choices (in the case of multichoices controls: selectInput, checkBoxGroupInput)
  
  compact<-data.frame()
  for (crit in unique(toto$criteria)){
    #print(crit)
    dat<-toto[toto$criteria==crit,]
    if(nrow(dat)>1){
      ligne1<-dat[1,]
      ligne1$choice<-paste(dat$choice, collapse=",")
      ligne1$labelchoice<-paste(dat$labelchoice, collapse=",")
    } else ligne1<-dat
    #print(dim(ligne1))
    compact<-rbind(compact, ligne1)
  }
  #I don t know why compact$order became character
  compact$order<-as.numeric(compact$order)
  #print(str(compact))
  if (compactconditions) {message("compact conditions not yet coded")}
  if(compactobjectives){ #we keep only the bigCriteria for the objectives, not the detailed objectives
    bigeffects<-unique(compact[compact$side=="effecttrait", c("side", "BigCriteria", "order", "labelBigCriteria")])
    if(length(bigeffects$side) > 0) {
      bigeffects$criteria<-bigeffects$BigCriteria
      bigeffects$labelcriteria<-bigeffects$labelBigCriteria
      bigeffects$choice<-""
      bigeffects$labelchoice<-""
      bigeffects$objecttype<-"checkboxInput"
    } 
    #message(paste(c(names(compact), names(bigeffects)), collapse=" "))
    compact<-rbind(compact[compact$side=="responsetrait",],bigeffects)
  }
  compact<-compact[order(compact$side, compact$order),]
  #print(head(compact))
  return(compact)
}

#source default functions to comput the suitability
source("default_functions.R")

# read data for databases page ---
toolsdata<-read.table("models/allModels.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
#"project"         "countries"       "Info"            "reference"       "link_reference"  "Link_standalone"
#countries are comma-delimited, warning about the spelling: it must be the spelling used by the "world" map of maps package
# # Sample data.table with projects and countries
# sample_data <- data.frame(
#   project = c('Czech', 'DECIDUOUS', 'GoÖko', 'DENTRO', 'JBOJP', 'SCSM', 'STA', 'SUOMI', 'UK Guide'),
#   countries = c('Czech Republic', 'France', 'Germany', 'Belgium', 'Netherlands', 'Netherlands',
#                 'Cameroon, China, Colombia, Ghana, Laos, Nicaragua, Tanzania, Uganda, Vietnam', 
#                 'Finland', 'UK'
#   )
# )


# Function to get country centroid coordinates
get_country_coords <- function() {
  # Get world map data
  world_map <- maps::map("world", exact = FALSE, plot = FALSE, fill = TRUE)
  world_map<-st_as_sf(world_map)
  world_centroids <- st_make_valid(st_transform(world_map, crs=4326))
  world_centroids$longitude<-sf::st_coordinates(sf::st_centroid(world_centroids))[,1]
  world_centroids$latitude<-sf::st_coordinates(sf::st_centroid(world_centroids))[,2]
  world_centroids<-as.data.frame(world_centroids)[,c("ID", "longitude", "latitude")]
  return(world_centroids)
}

# Create project colors and icon indices
# Create a list of different colored icons
combinaisons<-expand.grid(markerColor=c("red", "darkred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "pink", "cadetblue", "white", "gray", "black"),
                          icon=c("flag", "star", "check", "circle", "certificate", "tag", "bookmark", "globe", "map-marker"))
iconespossibles<-mapply(makeAwesomeIcon, as.character(combinaisons$icon), rep("fa", nrow(combinaisons)), as.character(combinaisons$markerColor),MoreArgs =list( iconColor = "white"),SIMPLIFY =FALSE)


# Function to prepare data for mapping
prepare_map_data <- function(data, country_coords=country_coords) {
  country_coords <- get_country_coords()
  
  # Split the comma-separated countries and create a row for each country-project pair
  projects <- data$project
  countries_list <- strsplit(data$countries, ",\\s*")
  
  # Create a data frame with one row per country-project pair
  map_data <- data.frame(
    project = rep(projects, sapply(countries_list, length)),
    country = unlist(countries_list)
  )
  toto<-setdiff(map_data$country, country_coords$ID)
  if(length(toto)>0) warning("Warning: the following countries are not spelled as in the 'world' map in package maps: ", paste(toto, collapse=", "))
  # Merge with country coordinates
  map_data <- merge(map_data, country_coords, 
                    by.x = "country", by.y = "ID", 
                    all.x = TRUE)
  
  # Calculate slight offsets for countries with multiple projects to prevent exact overlap
  # Group by country, count projects
  country_counts <- aggregate(map_data[, "country", drop=FALSE], by = map_data[, "country", drop=FALSE], "length")
  names(country_counts)[2]<-"N"
  map_data <- merge(map_data, country_counts, by = "country")
  
  # Assign an index to each project within a country
  map_data<-map_data[order(map_data$country),]
  map_data$project_index<-1
  for(i in 2:nrow(map_data)) if(map_data$country[i]==map_data$country[i-1]) map_data$project_index[i]<-map_data$project_index[i-1]+1
  
  
  # Create offsets based on the index and total number of projects
  map_data$offset_lon <- ifelse(map_data$N > 1, 
                                (map_data$project_index - (map_data$N + 1) / 2) * 2, 
                                0)
  map_data$offset_lat <- ifelse(map_data$N > 1, 
                                (map_data$project_index - (map_data$N + 1) / 2) * 2, 
                                0)
  
  
  # Apply offsets to coordinates
  map_data$longitude <- map_data$longitude + map_data$offset_lon / 111  # Roughly 111 km per degree
  map_data$latitude <- map_data$latitude + map_data$offset_lat / 111
  
  
  colors<-unname(sapply(iconespossibles,"[[", "markerColor"))
  # project_icons <- list(
  #   makeAwesomeIcon(icon = "flag", markerColor = "red", iconColor = "white", library = "fa"),
  #   makeAwesomeIcon(icon = "star", markerColor = "darkblue", iconColor = "white", library = "fa"),
  #   makeAwesomeIcon(icon = "check", markerColor = "green", iconColor = "white", library = "fa"),
  #   makeAwesomeIcon(icon = "circle", markerColor = "purple", iconColor = "white", library = "fa"),
  #   makeAwesomeIcon(icon = "certificate", markerColor = "orange", iconColor = "white", library = "fa"),
  #   makeAwesomeIcon(icon = "tag", markerColor = "yellow", iconColor = "black", library = "fa"),
  #   makeAwesomeIcon(icon = "bookmark", markerColor = "darkred", iconColor = "white", library = "fa"),
  #   makeAwesomeIcon(icon = "globe", markerColor = "darkgreen", iconColor = "white", library = "fa"),
  #   makeAwesomeIcon(icon = "map-marker", markerColor = "cadetblue", iconColor = "white", library = "fa")
  # )
  # Assign each project an icon index
  projects <- unique(map_data$project)
  project_attrs <- data.frame(
    project = projects,
    color = colors[1:length(projects)],
    icon_index = 1:length(projects)
  )
  #put back the other project information
  project_attrs<-merge(project_attrs, data[,c("project", "countries", "Info", "reference", "link_reference",   "Link_standalone")])
  
  map_data <- merge(map_data, project_attrs, by = "project")
  
  return(map_data)
}


#load the suitability functions
source("load_suitability_functions.R")

#colorscontrols<-c("")
# Brown: #A52A2A
#   Peach: #FFDAB9
#   Salmon: #FA8072
#   Light Slate Grey: #778899
#   Olive Green: #808000
#   Dark Green: #006400
#   Orange: #FFA500
#   LightSteelBlue4: #6E7B8B
#   Green4: #008B00
#   Midnight Blue: #191970
#   Honeydew3: #C1CDC1
#   Orange2: #EE9A00
