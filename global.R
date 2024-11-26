#rsconnect::showLogs(appPath=#"D:\\Mes_documents\\a_ABSys\\autreschercheurs\\BertReubens\\DigitAFtreeAdvice\\rsconnect\\shinyapps.io\\gosme\\DigitAFtreeAdvice.dcf", 
#                    "https://gosme.shinyapps.io/DigitAFtreeAdvice/", streaming=TRUE)
#"D:\\Mes_documents\\a_ABSys\\autreschercheurs\\BertReubens\\DigitAFtreeAdvice\\rsconnect\\shinyapps.io\\gosme"

library(shiny)
library(svglite)        # for svg download
library(shinyjs)
library(openxlsx)       # for writing xlsx files in download
library(ggplot2) #for the barplot graph
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
library(rsvg)           # convert svg to pdf in downloads
##global----

#load("dataSTA.Rdata")
#load("dataFlanders.Rdata")
#load("dataDeciduous.Rdata")
#load("dataSCSM.Rdata")
# dataDENTRO<-read.xlsx("models/DENTRO.xlsx", sheet="data")
# interfaceDENTRO<-read.xlsx("models/DENTRO.xlsx", sheet="interface")
# dataSTA<-read.xlsx("models/STA.xlsx", sheet="data")
# interfaceSTA<-read.xlsx("models/STA.xlsx", sheet="interface")
# dataDECIDUOUS<-read.xlsx("models/DECIDUOUS.xlsx", sheet="data")
# interfaceDECIDUOUS<-read.xlsx("models/DECIDUOUS.xlsx", sheet="interface")
# dataSCSM<-read.xlsx("models/SCSM.xlsx", sheet="data")
# interfaceSCSM<-read.xlsx("models/SCSM.xlsx", sheet="interface")
# dataCzech<-read.xlsx("models/Czech.xlsx", sheet="data")
# interfaceCzech<-read.xlsx("models/Czech.xlsx", sheet="interface")

#don't forget to save files as tab-delimited, with utf-8 encoding because of Czech special characters 
dataDENTRO<-read.table("models/dataDENTRO.txt", fileEncoding = "UTF-8", encoding = "UTF-8",fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceDENTRO<-read.table("models/interfaceDENTRO.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
dataSTA<-read.table("models/dataSTA.txt",  fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceSTA<-read.table("models/interfaceSTA.txt",  fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
dataDECIDUOUS<-read.table("models/dataDECIDUOUS.txt",  fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceDECIDUOUS<-read.table("models/interfaceDECIDUOUS.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
dataSCSM<-read.table("models/dataSCSM.txt", fileEncoding = "UTF-8", encoding = "UTF-8", quote="", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceSCSM<-read.table("models/interfaceSCSM.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
dataCzech<-read.table("models/dataCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep=";", skipNul =TRUE, header=TRUE)
interfaceCzech<-read.table("models/interfaceCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep=";", header=TRUE)
dataJBOJP<-read.table("models/dataJBOJP.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceJBOJP<-read.table("models/interfaceJBOJP.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
dataDEHM<-read.table("models/dataDEHM.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceDEHM<-read.table("models/interfaceDEHM.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
dataSUOMI<-read.table("models/dataSUOMI.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceSUOMI<-read.table("models/interfaceSUOMI.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)

# In czech, there are empty spaces around words in some cells 
dataCzech <- data.frame(lapply(dataCzech, function(x) {if (is.character(x)) {return(trimws(x))} else {return(x)}}))
interfaceCzech <- data.frame(lapply(interfaceCzech, function(x) {if (is.character(x)) {return(trimws(x))} else {return(x)}}))

#include downloadhandler functions
source("DownloadHandler.R")

# Initialize the translator
i18n <- Translator$new(translation_csvs_path = "R/translation/")
i18n$set_translation_language("en")  # Default language

#remove commas in the interface because commas are used for separating values  
interfaceSTA<-interfaceSTA[!is.na(interfaceSTA$side),]
interfaceSTA[1:length(interfaceSTA)]<-lapply(interfaceSTA[1:length(interfaceSTA)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceDENTRO<-interfaceDENTRO[!is.na(interfaceDENTRO$side),]
interfaceDENTRO[1:length(interfaceDENTRO)]<-lapply(interfaceDENTRO[1:length(interfaceDENTRO)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceDECIDUOUS<-interfaceDECIDUOUS[!is.na(interfaceDECIDUOUS$side),]
interfaceDECIDUOUS[1:length(interfaceDECIDUOUS)]<-lapply(interfaceDECIDUOUS[1:length(interfaceDECIDUOUS)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceSCSM<-interfaceSCSM[!is.na(interfaceSCSM$side),]
interfaceSCSM[1:length(interfaceSCSM)]<-lapply(interfaceSCSM[1:length(interfaceSCSM)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceCzech<-interfaceCzech[!is.na(interfaceCzech$side),]
interfaceCzech[1:length(interfaceCzech)]<-lapply(interfaceCzech[1:length(interfaceCzech)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceJBOJP<-interfaceJBOJP[!is.na(interfaceJBOJP$side),]
interfaceJBOJP[1:length(interfaceJBOJP)]<-lapply(interfaceJBOJP[1:length(interfaceJBOJP)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceDEHM<-interfaceDEHM[!is.na(interfaceDEHM$side),]
interfaceDEHM[1:length(interfaceDEHM)]<-lapply(interfaceDEHM[1:length(interfaceDEHM)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceSUOMI<-interfaceSUOMI[!is.na(interfaceSUOMI$side),]
interfaceSUOMI[1:length(interfaceSUOMI)]<-lapply(interfaceSUOMI[1:length(interfaceSUOMI)], function(x) gsub(pattern=",", replacement=".", x=x))

toto<-strsplit(c(names(interfaceSTA), 
                 names(interfaceDENTRO), 
                 names(interfaceDECIDUOUS), 
                 names(interfaceSCSM), 
                 names(interfaceCzech),
                 names(interfaceJBOJP),
                 names(interfaceDEHM),
                 names(interfaceSUOMI)), split="_")
languages<-unique(sapply(toto[lapply(toto, length)==2],"[[", 2))

reshapecontrols<-function(controls, language, compactconditions=FALSE, compactobjectives){
  # print("reshapecontrols")     ## very useful for debugging
  #print(str(controls))
  #print(paste("language=", language))
  toto<-strsplit(c(names(controls)), split="_")
  languages<-unique(sapply(toto[lapply(toto, length)==2],"[[", 2))
  #print(paste("languages=", paste(languages, collapse=",")))
  if(is.null(language) ) language<-"en"
  if(!language %in% languages) {print(paste(language, "is not in the languages available for this interface, so defaulting to english"))
    language<-"en"
  }
  #we select the desired language
  toto<-controls[!is.na(controls$criteria)& !is.na(controls$objecttype),c("side", "order", "BigCriteria", "criteria", "choice", "objecttype", paste(c("BigCriteria", "criteria", "choice"),language, sep="_"))]
  names(toto)<-c("side", "order", "BigCriteria", "criteria", "choice", "objecttype", "labelBigCriteria", "labelcriteria", "labelchoice")
  #we first reshape the choices (in the case of multichoices controls: selectInput, checkBoxGroupInput)
  
  compact<-data.frame()
  # print("Uniq criteria:")                ## very useful for debugging
  # print(unique(toto$criteria))

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


#' orders a df of species scores by a given side (or, in the future, a weight between effecttraits and responsetraits)
#'
#' @param df a data frame of scores with columns "side", "BigCriteria", "English.name", "Scientific.name", "value"
#' @param orderby currently= either "effecttrait or responsetrait, in the future: weight of effecttrait (0= order by responsetrait, 1 = order by effecttrait, in between = weighted mean of both)
#' @param idvariable variable to use as ordered factor, it should give unique id to each row
#' @param interface data.frame describing the interface of the app for this database, with columns "initialorder" (not used), "side" (either reponsetrait or effecttrait), "order" (not used), "BigCriteria", "criteria", "choice", "objecttype" (checkbox, Selectinput etc...), "weightwithincriteria" (not used for now), "BigCriteria_en", "criteria_en", "choice_en" and other colums for translations in other languages
#' #'
#' @return a data frame of scores with columns "side", "BigCriteria", "English.name", "Scientific.name", "value", "species" (the idvariable as an ordered factor), ordered by orderby
#' @export
#'
#' @examples
orderdf<-function(df, orderby, idvariable, interface){
  # Calculate the sum of the variables in orderby for each species to find the correct order
  #assign("df_from_orderdf", df, envir = .GlobalEnv) # debugging, this is to be able to see the result in the console
  #assign("orderby_from_orderdf", orderby, envir = .GlobalEnv) # debugging, this is to be able to see the result in the console
  #assign("idvariable_from_orderdf", idvariable, envir = .GlobalEnv) # debugging, this is to be able to see the result in the console
  #assign("interface_from_orderdf", interface, envir = .GlobalEnv) # debugging, this is to be able to see the result in the console
  
  linestokeep<-df$BigCriteria %in% unique(interface[!is.na(interface$side) & interface$side==orderby, c("BigCriteria")])
  if(sum(linestokeep)==0) linestokeep=TRUE
  species_order <- df[linestokeep,]
  species_order<-aggregate(species_order[,"value", drop=FALSE], by=species_order[,idvariable, drop=FALSE], sum, na.rm=TRUE)
  species_order<-species_order[order(species_order$value, decreasing=TRUE),]
  species_order<-species_order[,idvariable] 
  species_order<-species_order[!is.na(species_order)]
  
  # Reorder the levels of the species variable based on the sum
  df$species <- factor(df[,idvariable], levels = species_order)
  #reorder rows
  df<-df[order(df$species, decreasing=TRUE), c("species", setdiff(names(df), "species"))] 
  #decreasing = TRUE so that the best are on top in the dataframe (best = first in the levels of the factor)

  # Update reactive interface - so other functions know which interface was used (eg. download handler)
  reactive_Interface(interface)

  return(df)
}

#' Title
#'
#' @details 
#' |User input|Database|Score|
#' |-----------|---------|-------|
#' |1 item from a drop-down list (or in radio buttons)|Yes/no columns for each possible item|1 if the tree has this feature, 0 otherwise|
#' |1 item from a drop-down list (or in radio buttons)|1 column containing an item|1 if the tree has this feature, 0 otherwise|
#' |1 or more items in a set of checkboxes|Yes/no columns for each possible item|(number of items present in tree features, among selected items)/(number of selected items)|
#' |1 or more items in a set of checkboxes|1 column containing one or more items, or several columns each containing 1 item|(number of items present in tree features, among selected items)/(number of selected items)|
#' |1 or more items in a set of checkboxes|several columns (with names corresponding to items) containing scores|sum of scores of chosen columns|
#' |1 single numerical value|1 column containing a single value|1-abs(feature-value)/(max(features)-min(features))|
#' |1 single numerical value|1 column containing a range (x-y) |1 if value is within range, 0 if value is outside range|
#' |range of values|1 column containing a single value|1 if the characteristic is within the input range, 0 if the characteristic is outside it|
#' @param criteria single name of a criteria for which to compute the score
#' @param type #type of widget (one of "checkboxGroupInput", "selectInput", "sliderInput", "checkboxInput", "numericInput")
#' @param inputs #character vector of reformatted inputs (until I update everything to accept lists)
#' @param db #database of species characteristics
#' @param BigCriteria #big criteria to which the criteria belongs
#' @param side #side to which the criteria belongs (one of "responsetrait", "effecttrait")
#' @param yesindicator #value used in the database to indicate that the species fits this criteria (by default, "yes", "oui", "x", "T", "TRUE")
#'
#' @return a (long) data.frame of the initial database (I know it is not most efficient) with added columns "value" (with the value of the score for the criteria), "BigCriteria" and "side" ; rbinded for each criteria
#' @export
#'
#' @examples
default_computecrit<-function(criteria,type,inputs, db, BigCriteria, side, weight = as.integer(1), yesindicator=c("yes", "oui", "x", "X", "T", "TRUE", "VRAI")){
  message("computing value for criteria ", criteria , " of type ", type, " based on iputs ", paste(inputs, collapse=","))
  #print("####### get inputs[criteria]")
  #print(inputs[criteria][1])

  # We need access to used inputs - download handler needs to access them for table creation
  computedInputs <<- inputs
  
  if (type=="checkboxGroupInput"){ #for checkboxgroups, criteria is the title of the group
    #extract the relevant inputs to see which were chosen
    chosen<-unlist(inputs[gsub(pattern="[0-9]+", replacement="", x=names(inputs))==criteria])
    services<-strsplit( #services is a list (one for each species) of vectors of keywords (or numbers but not used in this case)
      gsub(pattern="(", replacement=", ", fixed=TRUE, x=gsub(pattern=")", replacement="", fixed=TRUE, 
                                                             x=db[,intersect(names(db), c(criteria, chosen))])) #replace first ( by comma and remove )
      , split="\\s*[,;]\\s*") #commas or semicolon followed by 0 or more whitespaces (and also remove trailing blanks)
   
    if(criteria %in% names(db)){ #one column criteria, with content equal to possible choices
      db$value<-as.numeric(db[,criteria] %in% chosen) 
    } else { # several columns, one for each possible choice
      if (all(sapply(make.names(chosen), function(ch) ch %in% names(db)))) { #all the chosen are among the column names
        db$value<-0
        for (ch in chosen) {
          if(class(db[,ch])=="numeric") {#the database contains scores
            scores<-db[,ch]
            scores[is.na(scores)]<-0
            db$value<-db$value+scores
          } else { #the database contains keywords
            #count the number of characteristics %in% inputs to get the score
            # Function to count the number of matching keywords
            count_matching_keywords <- function(keyword_list) {
              sum(keyword_list %in% chosen)
            }
            # Apply the function to each species
            nbmatches <- sapply(services, count_matching_keywords)
            #then divide by the number of possibilities to obtain score between 0 and 1
            db$value<-nbmatches/length(chosen)
          }
        } #end for each chosen
      } else {
        print(paste("could not guess which variable to use for", chosen)) ; db$value<-NA
      }}
   } else if (type=="selectInput") {
    
    chosen<-inputs[criteria]
    if(substr(chosen[1],start=1, stop=4) == "not ") # if the user selected "not " in the selectInput, then we select all species
    {
      print(paste0(chosen[0],": selected all"))
      db$value<-0
    }

    else if (criteria %in% names(db)){ #one column criteria, with content equal to possible choices
      db$value<-as.numeric(db[,criteria]==chosen) 
    } else {
      if (sum(grepl(pattern=make.names(chosen), x=names(db), fixed=TRUE))==1) { #the chosen is among the column names
        db$value<-as.numeric(db[,grepl(pattern=chosen, x=names(db))]) 
      } else {
        print(paste("could not guess which variable to use for", chosen)) ; db$value<-NA
      }}
    
  } else if (type=="checkboxInput") {
    db$value<- as.numeric(db[,criteria] %in% yesindicator)
  } else if (type=="sliderInput") {
    chosen<-as.numeric(inputs[gsub(pattern="[0-9]+", replacement="", x=names(inputs))==criteria])
    
    #chosen<-as.numeric(inputs[grepl(pattern=criteria, x=names(inputs))])
    #I don't know why, sometimes inputs are duplicated...
    chosen<-unique(as.numeric(chosen))
    chosen<-chosen[!is.na(chosen)]
    
    
    if(any(grepl(pattern=")-(", fixed=TRUE, x=db[,criteria]))) { #db gives a range of values
      splits<-strsplit(db[,criteria], split=")-(", fixed=TRUE)
      mini<-numeric(length(splits))
      mini[sapply(splits, length)>0]<-as.numeric(gsub(pattern="(", fixed=TRUE, replacement="", x=sapply(splits[sapply(splits, length)>0], "[[", 1)))
      maxi<-mini
      maxi[sapply(splits, length)>1]<-as.numeric(gsub(pattern=")", fixed=TRUE, replacement="", x=sapply(splits[sapply(splits, length)>1], "[[", 2)))
      if(length(chosen)==2) { #sliderinput with a range and db with a range: percentage of desired within treetrait
        overlap <- function(A, B) {
          shared <- pmax(0, min(A[2], B[2]) - max(A[1], B[1]))
          max(shared / c(diff(A), diff(B)))
        }
        db$value<-0
        for(i in 1:nrow(db)) db$value[i]<-overlap(chosen, c(mini[i], maxi[i]))
      } else { #sliderinput with just one chosen value: 1 if within treerange, 0 otherwise
        db$value<-as.numeric(mini<=chosen & maxi>=chosen)
      }
    } else { #db gives only one value
      treetraits<-as.numeric(db[,criteria])
      if(length(chosen)==2) { #sliderinput with a range: 0 if the species is outside, 1 if it is inside
        db$value<-as.numeric(treetraits>=min(chosen) & treetraits<=max(chosen))
      } else { #sliderinput with just one value: 1 when criteria = chosen, 0 when it is the farthest away among all species
        rangevalues<-range(treetraits, na.rm=TRUE)
        db$value<-pmax(0, 1-abs((treetraits-chosen)/(rangevalues[2]-rangevalues[1])))
      }
    }
    
    #chosen<-as.numeric(chosen[!duplicated(names(chosen))])
    
    
  } else if (type=="numericInput") {
    chosen<-inputs[criteria]
    if(any(grepl(pattern=")-(", fixed=TRUE, x=db[,criteria]))) { #db gives a range of values
      splits<-strsplit(db[,criteria], split=")-(", fixed=TRUE)
      mini<-as.numeric(gsub(pattern="(", fixed=TRUE, replacement="", x=sapply(splits, "[[", 1)))
      maxi<-mini
      maxi[sapply(splits, length)>1]<-as.numeric(gsub(pattern=")", fixed=TRUE, replacement="", x=sapply(splits[sapply(splits, length)>1], "[[", 2)))
      db$value<-as.numeric(mini<=chosen & maxi>=chosen)
    } else { #unique value
      rangevalues<-range(as.numeric(db[,criteria]))
      db$value<-1-abs((as.numeric(db[,criteria])-as.numeric(inputs[criteria]))/(rangevalues[2]-rangevalues[1]))
    }
    
  }

  if (!is.na(weight) && weight == as.integer(999)) {
    weight <- as.integer(1)
    print(paste("Weight is INFINITE for criteria:", criteria))
  }
  else {
      # Ensure weight is numeric
      weight <- as.numeric(weight)
      if (is.na(weight))  {
          weight <- as.integer(1)
          warning("Weight is not numeric and cannot be converted. Weight set to 1.")
        }
    }
  #message("values= ", paste(db$value, collapse=","))
  db$criteria<-criteria
  db$BigCriteria<-BigCriteria
  db$side<-side
  db$value<-db$value*weight
  return(db)
}

# if you need to further reformat the data, you can do it within the suitability_MODELNAME.txt file
source("R/suitability_DENTRO.R")
source("R/suitability_DECIDUOUS.R")
source("R/suitability_STA.R")
source("R/suitability_SCSM.R")
source("R/suitability_Czech.R")
source("R/suitability_JBOJP.R")
source("R/suitability_DEHM.R")
source("R/suitability_SUOMI.R") 





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
