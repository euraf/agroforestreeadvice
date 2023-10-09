#rsconnect::showLogs(appPath=#"D:\\Mes_documents\\a_ABSys\\autreschercheurs\\BertReubens\\DigitAFtreeAdvice\\rsconnect\\shinyapps.io\\gosme\\DigitAFtreeAdvice.dcf", 
#                    "https://gosme.shinyapps.io/DigitAFtreeAdvice/", streaming=TRUE)
#"D:\\Mes_documents\\a_ABSys\\autreschercheurs\\BertReubens\\DigitAFtreeAdvice\\rsconnect\\shinyapps.io\\gosme"

library(shiny)
library(ggplot2)
#library(plotly)
library(shinydashboard)
library(DT)
#library(dplyr)
##global----

#load("~/a_ABSYS/autreschercheurs/BertReubens/FlandersTreeAdvice/dataDENTRO.Rdata")
#load("dataDENTRO.Rdata")
#load("~\\a_ABSys\\DIGITAF\\WP2farmers\\task2.2_treecropperformance\\DigitAFtreeAdvice\\Rdatapreparation\\Flanders\\dataFlanders.Rdata")
load("dataSTA.Rdata")
load("dataFlanders.Rdata")
load("dataDeciduous.Rdata")
load("dataSCSM.Rdata")



  

interfaceSTA<-interfaceSTA[!is.na(interfaceSTA$side),]
interfaceSTA[1:length(interfaceSTA)]<-lapply(interfaceSTA[1:length(interfaceSTA)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceDENTRO<-interfaceDENTRO[!is.na(interfaceDENTRO$side),]
interfaceDENTRO[1:length(interfaceDENTRO)]<-lapply(interfaceDENTRO[1:length(interfaceDENTRO)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceDECIDUOUS<-interfaceDECIDUOUS[!is.na(interfaceDECIDUOUS$side),]
interfaceDECIDUOUS[1:length(interfaceDECIDUOUS)]<-lapply(interfaceDECIDUOUS[1:length(interfaceDECIDUOUS)], function(x) gsub(pattern=",", replacement=".", x=x))
interfaceSCSM<-interfaceSCSM[!is.na(interfaceSCSM$side),]
interfaceSCSM[1:length(interfaceSCSM)]<-lapply(interfaceSCSM[1:length(interfaceSCSM)], function(x) gsub(pattern=",", replacement=".", x=x))

toto<-strsplit(c(names(interfaceSTA), names(interfaceDENTRO), names(interfaceDECIDUOUS), names(interfaceSCSM)), split="_")
languages<-unique(sapply(toto[lapply(toto, length)==2],"[[", 2))

reshapecontrols<-function(controls, language, compactconditions=FALSE, compactobjectives=TRUE){
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
  linestokeep<-df$BigCriteria %in% unique(interface[!is.na(interface$side) & interface$side==orderby, c("BigCriteria")])
  if(sum(linestokeep)==0) linestokeep=TRUE
  species_order <- df[linestokeep,]
  species_order<-aggregate(species_order[,"value", drop=FALSE], by=species_order[,idvariable, drop=FALSE], sum, na.rm=TRUE)
  species_order<-species_order[order(species_order$value, decreasing=TRUE),]
  species_order<-species_order[,idvariable] 
  species_order[!is.na(species_order)]
  
  # Reorder the levels of the species variable based on the sum
  df$species <- factor(df[,idvariable], levels = species_order)
  #reorder rows
  df<-df[order(df$species, decreasing=TRUE), c("species", setdiff(names(df), "species"))] 
  #decreasing = TRUE so that the best are on top in the dataframe (best = first in the levels of the factor)
  return(df)
}

#' compute_suitability for Flanders data
#'
#' @param inputsdata named character vector of choices (for response traits) or Big criteria (for effect traits) made by the user, warning: these are the values internal to the interface, not the labels seen on the screen (which depend on language) ; the names are 
#' @param database data.frame of tree characteristics, with columns "English.name" (unique values), "Scientific.name" (returned as is), "sheet" (not used), "choice" (), "cellcontent" (not used), "value" (value on which to perform computations) )
#' @param interface data.frame describing the interface of the app for this database, with columns "initialorder" (not used), "side" (either reponsetrait or effecttrait), "order" (not used), "BigCriteria", "criteria", "choice", "objecttype" (checkbox, Selectinput etc...), "weightwithincriteria" (not used for now), "BigCriteria_en", "criteria_en", "choice_en" and other colums for translations in other languages
#' @param orderby either effecttrait or reponsetrait, for final ordering of the data.frame rows
#'
#' @return A data.frame with names "side", "BigCriteria", "English.name", "Scientific.name", "value", "species", where side, big criteria (but only those relevant to inputdata choices, except for the effecttraits side (if no big criteria chosen, then keep all))"English.name" and "Scientific.name" are the same as in database, and value is computed to be the score for each tree for each retained big criteria, and species is an ordered factor (ordered by sum of effecttraits or responsetraits depending on argument orderby)
#' @export
#'
#' @examples compute_suitability(inputsdata = c("(Heavy).clay", "Moist", "biodiversity"), database=dataDENTRO, interface=interface)
compute_suitability_DENTRO<-function(inputsdata=NULL,
                                     database, 
                                     interface,
                                     orderby="responsetrait"){
  print("suitability")
  
  message(str(inputsdata))
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
  toto<-database[database$choice %in% choicetokeep,]
  if(nrow(toto)<1) {
    toto<-database[!is.na(interface$side) & interface$side=="responsetrait" ]
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
  titi<-database[database$choice %in% choicetokeep,]
  if(nrow(titi)==0) {
    print("no effect available for selected objectives, so keep all objectives")
    titi<-unique(database[, c("English.name","Scientific.name")])
    titi$value<-1
  } #if no choices match data, all choices are kept
  #todo icicicici rescale values that are not on a 1-7 scale
  titi<-merge(titi, interface)
  titi<-titi[,c("English.name","Scientific.name", "value", "side","BigCriteria" )]#merge by choice, except if titi was created just to have list of trees, in which case we take the cardinal
  titi<-aggregate(titi[,"value", drop=FALSE], by=titi[,c("side", "BigCriteria", "English.name","Scientific.name"), drop=FALSE], mean, na.rm=TRUE)
  
  df<-rbind(toto, titi)
  
  df<-orderdf(df=df, orderby=orderby, idvariable="English.name", interface=interface) #I have to do it by english name because latin name is not unique (several cultivars of poplar)
  
  # give negative values for response traits so that they appear on the left
  df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]<- -  df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  return(df)
  
}

#' compute_suitability for ShadeTreeAdvice data
#'
#' @param inputsdata named character vector of choices (for response traits) or Big criteria (for effect traits) made by the user, warning: these are the values internal to the interface, not the labels seen on the screen (which depend on language)
#' @param database data.frame of tree characteristics, with columns  "Country" (not used)   "Region" (not used), "countryregion", "Crop", "Subgroup" (for response traits)  "ES"    (for effect traits)     "Tree_latin" (id) "Estimate" (value on which to perform computations: score from 0 to 5)  "qSE" (not used) )
#' @param interface data.frame describing the interface of the app for this database, with columns "initialorder" (not used), "side" (either reponsetrait or effecttrait), "order" (not used), "BigCriteria", "criteria", "choice", "objecttype" (checkbox, Selectinput etc...), "weightwithincriteria" (not used for now), "BigCriteria_en", "criteria_en", "choice_en" and other colums for translations in other languages
#' @param orderby either effecttrait or reponsetrait, for final ordering of the data.frame rows
#'
#' @return A data.frame with names "side", "BigCriteria", "English.name", "Scientific.name", "value", "species", where side, big criteria (but only those relevant to inputdata choices, except for the effecttraits side (if no big criteria chosen, then keep all))"English.name" and "Scientific.name" are the same as in database, and value is computed to be the score for each tree for each retained big criteria, and species is an ordered factor (ordered by sum of effecttraits or responsetraits depending on argument orderby)
#' @export
#'
#' @examples compute_suitability(inputsdata = c(countryregion="Vietnam (North-West Vietnam)", crop="Arabica coffee",	altitude="all altitudes", precipitation="Medium precipitation",biodiversity="biodiversity"), database=database, interface=interface)
#' @examples compute_suitability_STA(
#' 
#inputsdata = c(countryregion="Uganda (Mount Elgon)", crop="Arabica coffee",	altitude="High altitude", precipitation="All precipitation zones","pestdiseasecontrol"="pestdiseasecontrol")
#database=dataSTA
#interface=interfaceSTA
#' 



compute_suitability_STA<-function(inputsdata=NULL,
                                  database, 
                                  interface,
                                  orderby="responsetrait"){
  
  #print(str(inputsdata))
  interface<-interface[!is.na(interface$side),]
  database$countryregion<-paste(database$Country, " (", database$Region,")", sep="")
  database$crop<-database$Crop
  names(database)[names(database)=="ES"]<-"choice" #warning: choices have many synonyms, they are grouped through criteria column in interface
  names(database)[names(database)=="Estimate"]<-"value"
  criteriaresponse<-unique(unname(unlist(interface[!is.na(interface$side) 
                                                   & interface$side=="responsetrait", c("criteria")])))
  # BCeffect<-unique(unname(unlist(interface[!is.na(interface$side) 
  #                                          & interface$side=="effecttrait", c("BigCriteria")])))
  # 
  print("adaptation")
  #selects the subset of database corresponding to the selected country, region, crop, location
  subsetDB<-database
  #remove the criteria that the user did not select specifically (e.g. still the default all...), and filter database based on the other criteria
  for(crit in criteriaresponse) {
    if(substr(inputsdata[crit],start=1, stop=4) == "all ") {
      inputsdata<-inputsdata[setdiff(names(inputsdata), crit)]
    } else { # if the user chose a specific value for the crit crtieria then filter the data according to the user choice
      if(crit %in% names(subsetDB)) { #crop or countryregion
        subsetDB<-subsetDB[ subsetDB[[crit]] == inputsdata[crit] ,]
      } else { #altitude or precicpitation
        subsetDB<-subsetDB[ subsetDB$Subgroup == inputsdata[crit] 
                            | tolower(substr(subsetDB[["Subgroup"]], start=1, stop=4))=="all ",]   
      }
    }
  }
  
  if(nrow(subsetDB)==0) {
    print("this combination of country, crop, precipitation altitude does not exist")
    showModal(modalDialog(
      title = "this combination of country, crop, precipitation, altitude does not exist",
      "So we give you species cited in other countries/regions/precipitations/altitudes",
      "please consider this list as indicative only as the results might not be relevant for your conditions"
    ))
    #keep all data
    subsetDB<-database
    #compute adaptation score
    dataadaptation<-unique(database[,c("Country", "Region", "countryregion", "Crop", "crop", "Subgroup", "Tree_latin")])
    dataadaptation$countryregion<-paste(dataadaptation$Country, " (", dataadaptation$Region,")", sep="")
    dataadaptation$crop<-dataadaptation$Crop
    
    if(!is.null(inputsdata)) {
      choicetokeep<-unlist(interface[!is.na(interface$side) 
                                     & interface$side=="responsetrait" 
                                     & interface$choice %in% unlist(inputsdata), c("choice")])
    } else {
      choicetokeep<-unique(unname(unlist(interface[!is.na(interface$side) 
                                                   & interface$side=="responsetrait", c("choice")])))
    }
    resultadapt<-data.frame()
    for(ctk in choicetokeep){
      criterion<-interface[interface$choice==ctk, "criteria"]
      toto<-dataadaptation
      toto$criteria<-criterion
      toto$side<-"responsetrait"
      if(criterion %in% names(dataadaptation)) variabletolookat<-criterion else variabletolookat<-"Subgroup"
      toto$value<-ifelse(toto[variabletolookat]==ctk,1,0)
      resultadapt<-rbind(resultadapt, toto)
    }
    resultadapt<-aggregate(resultadapt[,"value", drop=FALSE], by=resultadapt[,c("Tree_latin", "criteria")], max)
    
    #add side and big criteria info
    resultadapt<-merge(resultadapt, unique(interface[,c("side", "BigCriteria", "criteria", "choice" )]), all.x=TRUE)[,c("side", "Tree_latin", "value", "BigCriteria", "criteria", "choice" )]
    #resultadapt<-aggregate(resultadapt[,"value", drop=FALSE], by=resultadapt[,c("side", "BigCriteria", "Tree_latin"), drop=FALSE], mean, na.rm=TRUE)
  } else { #there was data fitting all the users criteria
    #we give the same adaptation score to all trees
    resultadapt<-merge(unique(database[,c("Tree_latin"), drop=FALSE]),
                       unique(interface[interface$side=="responsetrait",c("side", "BigCriteria", "criteria", "choice" )]))#,
                       #all.x=TRUE)
    resultadapt$value<-1
  }
  
  print("effectiveness")  
  #keep the ecosystem services that were chosen by user
  #browser()
  BCtokeep<-interface[ interface$side=="effecttrait" & interface$BigCriteria %in% inputsdata,]
  if (nrow(BCtokeep)==0){print("no objective selected, so keep them all")
    BCtokeep<-interface[interface$side=="effecttrait", ]
  }
  #browser()
  toto<-subsetDB[subsetDB$choice %in% BCtokeep$choice,]
  if(nrow(toto)==0) {
    print("no tree provides the selected ES in your conditions, so we keep all ES")
    showModal(modalDialog(
      title = "No tree provides the selected Ecosystem Services in your conditions",
      "So we give you all the ecosystem services"
    ))
  } else {subsetDB<-subsetDB[subsetDB$choice %in% BCtokeep$choice,]}
  
  #add the big Criteria and side info 
  subsetDB<-merge(subsetDB, BCtokeep, all.x=TRUE)
  #add the adaptation values
  subsetDB<-rbind(
    subsetDB[,c("Tree_latin", "side", "BigCriteria", "criteria", "choice", "value")], 
    resultadapt[,c("Tree_latin", "side", "BigCriteria", "criteria", "choice", "value")]
  )
  
  #just in case we end up wit hseveral lines for the same combination of tree and BigCriteria
  df<-aggregate(subsetDB[,"value", drop=FALSE], by=subsetDB[,c("Tree_latin", "side", "BigCriteria", "criteria"), drop=FALSE], mean, na.rm=TRUE)
  
  #order the df by orderby, using latin name as id
  df<-orderdf(df=df, orderby=orderby, idvariable="Tree_latin", interface=interface) 
  
  # give negative values for response traits so that they appear on the left
  df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]<- -  df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  return(df)
  
}



#' compute_suitability for Deciduous data
#'
#' @param inputsdata named character vector of choices (for response traits) or Big criteria (for effect traits) made by the user, warning: these are the values internal to the interface, not the labels seen on the screen (which depend on language)
#' @param database data.frame of tree characteristics, with columns  , "Crop", "Subgroup" (for response traits)  "ES"    (for effect traits)     "Tree_latin" (id) "Estimate" (value on which to perform computations: score from 0 to 5)  "qSE" (not used) )
#' @param interface data.frame describing the interface of the app for this database, with columns "initialorder" (not used), "side" (either reponsetrait or effecttrait), "order" (not used), "BigCriteria", "criteria", "choice", "objecttype" (checkbox, Selectinput etc...), "weightwithincriteria" (not used for now), "BigCriteria_en", "criteria_en", "choice_en" and other colums for translations in other languages
#' @param orderby either effecttrait or reponsetrait, for final ordering of the data.frame rows
#'
#' @return A data.frame with names "side", "BigCriteria", "English.name", "Scientific.name", "value", "species", where side, big criteria (but only those relevant to inputdata choices, except for the effecttraits side (if no big criteria chosen, then keep all))"English.name" and "Scientific.name" are the same as in database, and value is computed to be the score for each tree for each retained big criteria, and species is an ordered factor (ordered by sum of effecttraits or responsetraits depending on argument orderby)
#' @export
#'
#' @examples compute_suitability(inputsdata = c(countryregion="Vietnam (North-West Vietnam)", crop="Arabica coffee",	precipitation="Medium precipitation","biodiversity"), database=database, interface=interface)

compute_suitability_DECIDUOUS<-function(inputsdata=NULL,
                                        database, 
                                        interface,
                                        orderby="responsetrait"){
  # on stocke toutes les entrées utilisateur
  user_risque_gel_tardif <- inputsdata["user_gel_tardif"]
  user_risk_def_hyd <- inputsdata["user_risk_def_hyd"]
  user_irrigation <- inputsdata["user_irrigation"]
  user_prof_sol_ara <- inputsdata["user_prof_sol_ara"]
  user_hyd_sol <- inputsdata["user_hyd_sol"]
  user_ca_actif <- inputsdata["user_ca_actif"]
  user_dispo <- inputsdata["user_dispo"]
  user_hauteur <- inputsdata["user_hauteur"]
  user_especes <- gsub(pattern="especeuser_", replacement="", x=inputsdata[grepl(x=names(inputsdata), pattern= "especeuser_")])
  
  # Criteres d'importance des experts (Cf. google doc) de  1 (plus important) a 5 (moins important)
  # a terme il faudra reflechir a mettre des sliders pour laisser le choix a l utilisaeur
  # criteres PG :
  crit_PG_prof_sol <- 3.2
  crit_PG_hydromorphie <- 2.3
  crit_PG_sens_deficit_hydrique <- 2.4
  crit_PG_taux_ca <- 2.4
  crit_PG_pH <- 3.0
  # criteres especes :
  crit_esp_sensibilite_gel_hiver <- 3.1
  crit_esp_sensibilite_gel_tardif<- 2.0
  crit_esp_sensibilite_deficit_hydrique <- 2.7
  crit_esp_saisonalite_prod <- 3.0
  crit_esp_hauteur_arbre <- 5.0
  crit_esp_dispo <- 2.3
  crit_esp_periode_entretien <- 4.2
  crit_esp_besoin_conservation <- 2.6
  crit_esp_besoin_filet <- 3.0
  
  # stocke le tableau de données dans "result"
  result <- database

  # # on met un 1, 0.5 ou 0 dans les cases qui respectent les critères d'entrée de l'utilisateur, et 0 si ce n'est pas le cas
  result$note_gel_tardif<-NA
  result$note_gel_tardif[user_risque_gel_tardif=="usergeltardif_low"]<-1
  result$note_gel_tardif[user_risque_gel_tardif=="usergeltardif_medium" & result$sens_gel_tar=="faible"]<-1
  result$note_gel_tardif[user_risque_gel_tardif=="usergeltardif_medium" & result$sens_gel_tar=="moyenne"]<-0.5
  result$note_gel_tardif[user_risque_gel_tardif=="usergeltardif_medium" & result$sens_gel_tar=="forte"]<-0
  result$note_gel_tardif[user_risque_gel_tardif=="usergeltardif_high" & result$sens_gel_tar=="faible"]<-1
  result$note_gel_tardif[user_risque_gel_tardif=="usergeltardif_high" & result$sens_gel_tar=="moyenne"]<-0.5
  result$note_gel_tardif[user_risque_gel_tardif=="usergeltardif_high" & result$sens_gel_tar=="forte"]<-0
  
  result$note_risk_def_hyd<-NA
  result$note_risk_def_hyd[user_risk_def_hyd=="userriskdefhyd_low"]<-1
  result$note_risk_def_hyd[user_risk_def_hyd=="userriskdefhyd_medium" & result$sens_def_hyd_e=="faible"]<-1
  result$note_risk_def_hyd[user_risk_def_hyd=="userriskdefhyd_medium" & result$sens_def_hyd_e=="moyenne"]<-0.5
  result$note_risk_def_hyd[user_risk_def_hyd=="userriskdefhyd_medium" & result$sens_def_hyd_e=="forte"]<-0
  result$note_risk_def_hyd[user_risk_def_hyd=="userriskdefhyd_high" & result$sens_def_hyd_e=="faible"]<-1
  result$note_risk_def_hyd[user_risk_def_hyd=="userriskdefhyd_high" & result$sens_def_hyd_e=="moyenne"]<-0.5
  result$note_risk_def_hyd[user_risk_def_hyd=="userriskdefhyd_high" & result$sens_def_hyd_e=="forte"]<-0
  
  
  result$note_irrigation<-NA
  result$note_irrigation[user_irrigation=="userirrigation_regular"]<-1
  result$note_irrigation[user_irrigation=="userirrigation_possible" & result$sens_def_hyd_e=="faible"]<-1
  result$note_irrigation[user_irrigation=="userirrigation_possible" & result$sens_def_hyd_e=="moyenne"]<-0.5
  result$note_irrigation[user_irrigation=="userirrigation_possible" & result$sens_def_hyd_e=="forte"]<-0
  result$note_irrigation[user_irrigation=="userirrigation_no" & result$sens_def_hyd_e=="faible"]<-1
  result$note_irrigation[user_irrigation=="userirrigation_no" & result$sens_def_hyd_e=="moyenne"]<-0.5
  result$note_irrigation[user_irrigation=="userirrigation_no" & result$sens_def_hyd_e=="forte"]<-0
  
 
  result$note_prof_sol_ara<- as.numeric(result[,user_prof_sol_ara]=="oui")
  
  result$note_hyd_sol <- as.numeric((result[,user_hyd_sol]=="oui"))
  
  result$note_ca_actif<-NA
  result$note_ca_actif[user_ca_actif=="usercaactif_low"]<-1
  result$note_ca_actif[user_ca_actif=="usercaactif_high" & result$tol_ca_actif=="1 a 10%"]<-0
  result$note_ca_actif[user_ca_actif=="usercaactif_high" & result$tol_ca_actif==">10%"]<-1
  
  result$note_dispo<-NA
  result$note_dispo[user_dispo=="userdispo_high"]<-1
  result$note_dispo[user_dispo=="userdispo_medium" & result$bes_ent=="faible"]<-1
  result$note_dispo[user_dispo=="userdispo_medium" & result$bes_ent=="moyen"]<-0.5
  result$note_dispo[user_dispo=="userdispo_medium" & result$bes_ent=="fort"]<-0
  result$note_dispo[user_dispo=="userdispo_low" & result$bes_ent=="faible"]<-1
  result$note_dispo[user_dispo=="userdispo_low" & result$bes_ent=="moyen"]<-0.5
  result$note_dispo[user_dispo=="userdispo_low" & result$bes_ent=="fort"]<-0
 
  result$note_hauteur <- as.numeric((result[gsub(pattern=" ", replacement=".", fixed=TRUE, x= user_hauteur)]=="oui"))
  
  # on crée les colonnes scores pour chaque critère et la colonne score global
  result$score_gel_tardif = result$note_gel_tardif * (6 - crit_esp_sensibilite_gel_tardif) # inverstion de score car l'echelle etait inversee sur google doc
  result$score_risk_def_hyd = result$note_risk_def_hyd * (6 - crit_PG_sens_deficit_hydrique)
  result$score_irrigation = result$note_irrigation * (6 - crit_esp_sensibilite_deficit_hydrique)
  result$score_prof_sol_ara = result$note_prof_sol_ara * (6 - crit_PG_prof_sol)
  result$score_hyd_sol = result$note_hyd_sol * (6 - crit_PG_hydromorphie)
  result$score_ca_actif = result$note_ca_actif * (6 - crit_PG_taux_ca)
  result$score_dispo = result$note_dispo * (6 - crit_esp_dispo)
  result$score_hauteur = result$note_hauteur * (6 - crit_esp_hauteur_arbre)
  
  # note deficit hydrique = r?sultatnt de def_hyd et irrigation
  #result$score_def_hyd_combine <- (result$score_def_hyd + result$score_irrigation)/2
  
  # on somme tous les scores pour avoir la note globale
  # result$global_score <- result$score_gel_tardif+
  #   result$score_def_hyd_combine+
  #   result$score_prof_sol_ara+
  #   result$score_hyd_sol+
  #   result$score_ca_actif+
  #   result$score_dispo+
  #   result$score_hauteur
  # 
  
  # # on classe les r?sultats par note la plus haute
  #result <- result[order(-result$global_score),]
   #agroforesTreeAdvice: add big criteria for the adaptation to species
  responses<-reshape(result[,c("couple_espece_PG",
                                     "score_gel_tardif",
                                     "score_risk_def_hyd", "score_irrigation",
                                     "score_prof_sol_ara",
                                     "score_hyd_sol",
                                     "score_ca_actif",
                                     "score_dispo",
                                     "score_hauteur"#,
                                     #"global_score"
  )], direction="long",
  idvar="couple_espece_PG",
  varying=c("score_gel_tardif",
            "score_risk_def_hyd", "score_irrigation",
            "score_prof_sol_ara",
            "score_hyd_sol",
            "score_ca_actif",
            "score_dispo",
            "score_hauteur"),
  v.names="score_",
  times=c("gel_tardif",
          "risk_def_hyd", "irrigation",
          "prof_sol_ara",
          "hyd_sol",
          "ca_actif",
          "dispo",
          "hauteur")
  )
  responses$side<-"responsetrait"
  responses$side[responses$time=="hauteur"]<-"effecttrait"
  responses$value<- responses$"score_" 
  BC<-unique(interface[,c("BigCriteria", "criteria")]) ; rownames(BC)<-BC$criteria
  responses$BigCriteria<-BC[paste("user_", responses$time, sep=""),"BigCriteria"]
  responses$criteria<-responses$time
  
  effects<-result[,c("couple_espece_PG", "Espece")]
  effects$side<-"effecttrait"
  effects$value <- as.numeric(effects$Espece %in% user_especes)
  effects$BigCriteria<-"species"
  effects$criteria<-effects$Espece
  
  df <- rbind(responses[,c("couple_espece_PG", "side", "value", "BigCriteria", "criteria")],
                        effects[,c("couple_espece_PG", "side", "value", "BigCriteria", "criteria")])
  
  
  #just in case we end up wit hseveral lines for the same combination of tree and BigCriteria
  df<-aggregate(df[,"value", drop=FALSE], by=df[,c("couple_espece_PG", "side", "BigCriteria", "criteria"), drop=FALSE], mean, na.rm=TRUE)
  
  #order the df by orderby, using latin name as id
  df<-orderdf(df=df, orderby=orderby, idvariable="couple_espece_PG", interface=interface) 
  
  #give negative values to response criteria
  df$value[df$side=="responsetrait"]<- -df$value[df$side=="responsetrait"]
  
  # final_result <- result[order(-result$global_score),] %>%
  #   # selectionne uniquement les especes selectionnees
  #   filter(Espece %in% user_especes) %>%
  #   # selectionne uniquement les variables interessantes
  #   select(couple_espece_PG,
  #          score_gel_tardif,
  #          score_def_hyd_combine,
  #          score_prof_sol_ara,
  #          score_hyd_sol,
  #          score_ca_actif,
  #          score_dispo,
  #          score_hauteur,
  #          global_score) %>%
  #   
  #   rename('Espèce & Porte greffe' = couple_espece_PG,
  #          'Gel' = score_gel_tardif,
  #          'Deficit hydrique' = score_def_hyd_combine,
  #          'Profondeur sol' = score_prof_sol_ara,
  #          'Hydromorphie' = score_hyd_sol,
  #          'CaCO3' = score_ca_actif,
  #          'Entretien' = score_dispo,
  #          'Hauteur' = score_hauteur)
  
  return(df)
  
}

#' compute_suitability for SCSM data
#'
#' @param inputsdata named character vector of choices (for response traits) or Big criteria (for effect traits) made by the user, warning: these are the values internal to the interface, not the labels seen on the screen (which depend on language)
#' @param database data.frame of tree characteristics, with columns  , 
#' @param interface data.frame describing the interface of the app for this database, with columns "initialorder" (not used), "side" (either reponsetrait or effecttrait), "order" (not used), "BigCriteria", "criteria", "choice", "objecttype" (checkbox, Selectinput etc...), "weightwithincriteria" (not used for now), "BigCriteria_en", "criteria_en", "choice_en" and other colums for translations in other languages
#' @param orderby either effecttrait or reponsetrait, for final ordering of the data.frame rows
#'
#' @return A data.frame with names "side", "BigCriteria", "English.name", "Scientific.name", "value", where side, big criteria (but only those relevant to inputdata choices, except for the effecttraits side (if no big criteria chosen, then keep all))"English.name" and "Scientific.name" are the same as in database, and value is computed to be the score for each tree for each retained big criteria, and species is an ordered factor (ordered by sum of effecttraits or responsetraits depending on argument orderby)
#' @export
#'
#' @examples compute_suitability(inputsdata = c(countryregion="Vietnam (North-West Vietnam)", crop="Arabica coffee",	precipitation="Medium precipitation","biodiversity"), database=database, interface=interface)

compute_suitability_SCSM<-function(inputsdata=NULL,
                                        database, 
                                        interface,
                                        orderby="responsetrait"){
  
  database['BigCriteria']='climate'
  database['side']='responsetrait'
  
  database['species_phylogenetic']=database['species']
  #icici we use as id the combination genus species
  database['idspecies']=paste(database$genus,database$species, sep=" ")
  
  
  # Calulate value for temperature
  database_temp = database
  database_temp['criteria'] = 'temperature'
  temperature = as.numeric(inputsdata["temperature"])
  #database_temp = mutate(database_temp, value = ifelse(temperature_min<=temperature & temperature<=temperature_max, 1, -1))  
  #icicic from Marie: since I had a problem when deploying the app to shinyappsio (often due to hidden package dependencies), I try without dplyr
  #also, I give the value 0 if it is outside the range because the app expects a score from 0 to Inf
  database_temp$value<-ifelse(database_temp$temperature_min<=temperature & temperature<=database_temp$temperature_max, 1, 0)
  
  # Calulate value for precipitation
  database_precipitation = database
  database_precipitation['criteria'] = 'precipitation'
  precipitation = as.numeric(inputsdata["precipitation"])
  #database_precipitation = mutate(database_precipitation, value = ifelse(precipitation_min<=precipitation & precipitation<=precipitation_max, 1, -1))  
  #icicic from Marie: since I had a problem when deploying the app to shinyappsio (often due to hidden package dependencies), I try without dplyr
  #also, I give the value 0 if it is outside the range because the app expects a score from 0 to Inf
  database_precipitation$value<-ifelse(database_precipitation$temperature_min<=temperature & temperature<=database_precipitation$temperature_max, 1, 0)
  
  dbfinal<-rbind(database_temp, database_precipitation)
  
  #todo, compute effectiveness based on effect traits and inputdata of objectives
  database['BigCriteria']='climate'
  database['side']='responsetrait'
  computecrit<-function(characteristics,type=c("checkboxgroup", "selectinrange"),inputs){
    if (type=="checkboxgroup"){
      #strsplit the inputs to see which were chosen, strsplit the characteristics to see all that is provided by the species, 
      #count the number of characteristics %in% inputs to get the score
      #then divide by the number of possibilities to obtain score between 0 and 1
    } else if (type=="selectinrange") {
      #strsplit the inputs to find min and max, give score 1 if the species is within the chosen range
    }
  }
  for (crit in c("utilities", "form", "height","lifespan")){
    #if (!grepl("any", inputsdata[[crit]])) {
    #  database[,paste("score_", crit)]<-computecrit(database$crit, type=ifelse(crit=="utilities", ))
    #}
  }
  
  
  
  #order the df by orderby, using latin name as id (ads an id variable, which is a factor with levels ordered by the orderby side)
  #icicicic I know it is not logical to do that here, it would be more logical to reorder the factor outside of the computation of the score
  # to do: separate computation of score and ordering of the species
  dbfinal<-orderdf(df=dbfinal, orderby=orderby, idvariable='idspecies', interface=interface) 
  
  
  
  
  
  
  
  
  
  # give negative values for response traits so that they appear on the left
  dbfinal$value<- -dbfinal$value
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  
  return(dbfinal)

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
