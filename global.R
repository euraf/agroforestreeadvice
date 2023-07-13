#rsconnect::showLogs(appPath=#"D:\\Mes_documents\\a_ABSys\\autreschercheurs\\BertReubens\\DigitAFtreeAdvice\\rsconnect\\shinyapps.io\\gosme\\DigitAFtreeAdvice.dcf", 
#                    "https://gosme.shinyapps.io/DigitAFtreeAdvice/", streaming=TRUE)
#"D:\\Mes_documents\\a_ABSys\\autreschercheurs\\BertReubens\\DigitAFtreeAdvice\\rsconnect\\shinyapps.io\\gosme"

library(shiny)
library(ggplot2)
#library(plotly)
library(shinydashboard)
library(DT)
##global----

#load("~/a_ABSYS/autreschercheurs/BertReubens/FlandersTreeAdvice/dataDENTRO.Rdata")
load("dataDENTRO.Rdata")
interface<-interface[!is.na(interface$side),]
interface[1:length(interface)]<-lapply(interface[1:length(interface)], function(x) gsub(pattern=",", replacement=".", x=x))
toto<-strsplit(names(interface), split="_")
languages<-unique(sapply(toto[lapply(toto, length)==2],"[[", 2))
reshapecontrols<-function(controls, language, compactconditions=FALSE, compactobjectives=TRUE){
  #we select the desired language
  toto<-controls[!is.na(controls$criteria),c("side", "BigCriteria", "criteria", "choice", "objecttype", paste(c("BigCriteria", "criteria", "choice"),language, sep="_"))]
  names(toto)<-c("side", "BigCriteria", "criteria", "choice", "objecttype", "labelBigCriteria", "labelcriteria", "labelchoice")
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
  #print(str(compact))
  if (compactconditions) {message("compact conditions not yet coded")}
  if(compactobjectives){
    #print("to do !")
    bigeffects<-unique(compact[compact$side=="effecttrait", c("side", "BigCriteria", "labelBigCriteria")])
    bigeffects$criteria<-bigeffects$BigCriteria
    bigeffects$labelcriteria<-bigeffects$labelBigCriteria
    bigeffects$choice<-""
    bigeffects$labelchoice<-""
    bigeffects$objecttype<-"checkboxInput"
    #message(paste(c(names(compact), names(bigeffects)), collapse=" "))
    compact<-rbind(compact[compact$side=="responsetrait",],bigeffects)
  }
  return(compact)
}


compute_suitability<-function(inputsdata=NULL,
                              dataDENTRO, 
                              interface,
                              orderby="responsetrait"){
  #print("suitability")
  #print(str(inputsdata))
  interface<-interface[!is.na(interface$side),]
  #selects data values for each criteria/subcriteria for response traits according to local conditions
  #and rescales answers that are on a scale with less than 7 levels
  #print("responsetraits")
  #browser()
  if(!is.null(inputsdata)) {
    choicetokeep<-unlist(interface[!is.na(interface$side) 
                                   & interface$side=="responsetrait" 
                                   & interface$choice %in% unlist(inputsdata), c("choice", "criteria")])
  } else {
    choicetokeep<-unlist(interface[!is.na(interface$side) 
                                   & interface$side=="responsetrait", c("choice", "criteria")])
  }
  toto<-dataDENTRO[dataDENTRO$choice %in% choicetokeep,]
  if(nrow(toto)<1) {
    toto<-dataDENTRO[!is.na(interface$side) & interface$side=="responsetrait" ]
    toto$value<-1} #if no choices match data, all trees become equal
  #todo icicicici rescale values that are not on a 1-7 scale
  toto<-merge(toto, interface)[,c("English.name","Scientific.name", "value", "BigCriteria", "side" )]
  toto<-aggregate(toto[,"value", drop=FALSE], by=toto[,c("side", "BigCriteria", "English.name","Scientific.name"), drop=FALSE], mean, na.rm=TRUE)
  
  
  #print("effecttraits")
  #find the choices that belong to the bigcriteria selected
  if(!is.null(inputsdata)) {
    choicetokeep<-interface[!is.na(interface$side) 
                            & interface$side=="effecttrait" 
                            & interface$BigCriteria %in% unlist(inputsdata), "choice"]
  } else {
    choicetokeep<-interface[!is.na(interface$side) 
                            & interface$side=="effecttrait", "choice"]
  }
  #browser()
  titi<-dataDENTRO[dataDENTRO$choice %in% choicetokeep,]
  if(nrow(titi)==0) {
    print("no effect available for selected objectives, so keep all objectives")
    titi<-unique(dataDENTRO[, c("English.name","Scientific.name")])
    titi$value<-1
  } #if no choices match data, all choices are kept
  #todo icicicici rescale values that are not on a 1-7 scale
  titi<-merge(titi, interface)
  titi<-titi[,c("English.name","Scientific.name", "value", "side","BigCriteria" )]#merge by choice, except if titi was created just to have list of trees, in which case we take the cardinal
  titi<-aggregate(titi[,"value", drop=FALSE], by=titi[,c("side", "BigCriteria", "English.name","Scientific.name"), drop=FALSE], mean, na.rm=TRUE)
  
  df<-rbind(toto, titi)
  
  # Calculate the sum of the variables in orderby for each species to find the correct order
  species_order <- df[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side==orderby, c("BigCriteria")],]
  species_order<-aggregate(species_order[,"value", drop=FALSE], by=species_order[,c("English.name","Scientific.name")], sum, na.rm=TRUE)
  species_order<-species_order[order(species_order$value, decreasing=FALSE),]
  species_order<-species_order$English.name #I have to do it by english name because latin names are not unique
  species_order[!is.na(species_order)]
  
  # Reorder the levels of the species variable based on the sum
  df$species <- factor(df$English.name, levels = species_order)
  # give negative values for response traits so that they appear on the left
  df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]<- -  df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  return(df)
  
}


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
