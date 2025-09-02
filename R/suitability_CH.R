#add an IDAFTA column to the data so that reference to this ID column can be automated
#dataCH$IDAFTA<-paste(dataCH$Lateinischer.Name, dataCH$Kategorie) #no: use only latin name as IDAFTA, because the data is filtered by Kategorie right away in suitability_CH
dataCH$IDAFTA<-dataCH$Lateinischer.Name
#add a tooltipspecies column so that information is displayed when hovering on the barplot
removeNA<-function(x) {
  x[is.na(x)]<-""
  x[x=="NA"]<-""
  return(x)
}
dataCH$tooltipspecies<-paste(dataCH$Name,
                             removeNA(dataCH$"Boden"), 
                             removeNA(dataCH$"Standortanspruch"), 
                             removeNA(dataCH$"Bemerkung"), 
                             removeNA(dataCH$"Empfohlene.Sorte"),
                             removeNA(dataCH$"Befruchtung..bspw..mehrere.Sorten."),
                             removeNA(dataCH$"Pflanzabstand.und.Position.in.der.Hecke"), 
                             removeNA(dataCH$"Bemerkungen"), 
                             removeNA(dataCH$Comments))
#actually, the tooltipspecies is not displayed because I removed the Kategorie in the IDAFTA column of data, to simplify yaxis labels in the graph...
#solution: use only latin name as IDAFTA, because the data is filtered by Kategorie right away in suitability_CH

#' compute score (not adapted:-Inf, OK:0, preferred:>1) for soil adaptation: if any descriptor of soil has a minus sign, not adapted, then compute the number of positive signs, and divide by ??? to standardise between ?? and ??
#'
#' @param inputsdata named character vector of choices (for response traits) or Big criteria (for effect traits) made by the user, warning: these are the values internal to the interface, not the labels seen on the screen (which depend on language)
#' @param database data.frame of tree characteristics (only the species filtered by Kategorie) 
#' @param interface data.frame describing the interface of the app for this database, with columns "initialorder" (not used), "side" (either reponsetrait or effecttrait), "order" (not used), "BigCriteria", "criteria", "choice", "objecttype" (checkbox, Selectinput etc...), "weightwithincriteria" (not used for now), "BigCriteria_en", "criteria_en", "choice_en" and other colums for translations in other languages
#'
#' @returns a (long) data.frame of the initial database (I know it is not most efficient) with added columns "value" (with the value of the score for the criteria), "BigCriteria" and "side" 
#' @export
#'
#' @examples
computescore_soil_CH<-function(interface,
                               inputsdata, 
                               database){
  #message("computing value for soil based on iputs ", paste(inputsdata, collapse=","))
  
  #we narrow down the interface info to soil criteria for which the user provided information
  subinterface<-interface[interface$BigCriteria=="soil",] #soil related criteria
  subinterface$pasted<-paste(make.names(subinterface$criteria), make.names(subinterface$choice), sep="_")
  subinputs<-inputsdata[gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)) %in% subinterface$criteria] #inputs related to soil
  pasted<-paste(make.names(names(subinputs)), make.names(subinputs), sep="_")
  #subinterface<-subinterface[subinterface$criteria %in% gsub(pattern="[0-9]+", replacement="", x=names(subinputs)),]
  subinterface<-subinterface[subinterface$objecttype == "checkboxInput" | (subinterface$objecttype == "selectInput" & subinterface$pasted %in% pasted),] #selected by user
   # numericInput
    #we find the corresponding column names in data (criteria_choice for dropdown menus, criteria for checkboxes)
  subdata<-database[,names(database) %in% subinterface$pasted]
  #if any descriptor of soil has a minus sign, not adapted, 
  subdata$scorenegatif<-apply(subdata, 1, function(x) ifelse(any(x %in% c("-", "--")), -Inf, 0))
  #then compute the number of positive signs, and divide by ??? to standardise between ?? and ??
  subdata$scorepositif<-apply(subdata, 1, function(x) sum(ifelse(x=="+", 1, ifelse(x=="++", 2, 0))))
  #then add the score for pH
  splits<-strsplit(database[,"pH"], split=")–(", fixed=TRUE)
  mini<-numeric(length(splits))
  mini[sapply(splits, length)>0]<-as.numeric(gsub(pattern="(", fixed=TRUE, replacement="", x=sapply(splits[sapply(splits, length)>0], "[[", 1)))
  maxi<-mini
  maxi[sapply(splits, length)>1]<-as.numeric(gsub(pattern=")", fixed=TRUE, replacement="", x=sapply(splits[sapply(splits, length)>1], "[[", 2)))
  subdata$scorepositif<-subdata$scorepositif+as.numeric(mini<=as.numeric(subinputs[["pH"]]) & maxi>=as.numeric(subinputs[["pH"]]))
  database$value<-subdata$scorenegatif+subdata$scorepositif
  database$side<-"responsetrait"
  database$BigCriteria<-"soil"
  database$criteria<-"soil"
  return(database)
}


#' compute_suitability for Swiss database
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

compute_suitability_CH<-function(inputsdata=NULL,
                                 database, 
                                 interface,
                                 orderby="responsetrait"){
  
  print(str(inputsdata))
  #we filter by Kategorie ("hard filter" because else tree species are not unique IDs)
  if(!"Kategorie" %in% names(inputsdata)) {
    dbfinal<-data.frame(IDAFTA="For this model, it is mandatory to provide at least parameter Kategorie",
                        "side"="reponsetrait", "BigCriteria"="", 
                        "English.name"="", "Scientific.name"="", "value"=0)
    print("end suitability CH with missing Kategorie parameter")
    return(dbfinal)
  }
  
  dbfinal<-data.frame()
  database<-database[database$Kategorie==inputsdata["Kategorie"],]
  if(nrow(database)==0) {
    dbfinal<-data.frame(IDAFTA=paste("The swiss dataset contains no trees for Kategotie", inputsdata["Kategorie"]),
                        "side"="reponsetrait", "BigCriteria"="", 
                        "English.name"="", "Scientific.name"="", "value"=0)
    print("end suitability CH with empty tree list for the chosen Kategorie")
    return(dbfinal)
  }
  database$IDAFTA<-database$Lateinischer.Name #because we removed Kategorie, so tree altin name becomes the ID
  
  toto<-unique(interface[interface$side %in% c("responsetrait", "effecttrait"),c("criteria", "objecttype", "side", "BigCriteria")])
  rownames(toto)<-toto$criteria
  standardformcriteria<-intersect(c("climate.change", "orientation", "cold.winter",
                                    "spring.frost", "drought", 
                                    "hot.summer","Blütezeit", "Erntezeit",                                  
                                    "Height", "Width", "Mykorrhiza",  "Ökologie",  "Verwendung" ),                                
                                  gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)) 
  ) #we intersect to cover the case when parameters are sent through url=> not all parameters might be present
  for(crit in standardformcriteria){
    print(paste("compute score for", crit))
    currentcritscore<-default_computecrit(criteria=crit,
                                          type= toto[crit, "objecttype"],
                                          BigCriteria=toto[crit, "BigCriteria"],
                                          side=toto[crit, "side"],
                                          inputs=inputsdata, 
                                          db=database)
    if(nrow(dbfinal)>0) if(!identical(sort(names(dbfinal)), sort(names(currentcritscore)))){
      message(paste(setdiff(names(dbfinal), names(currentcritscore)), collapse=" "), " ne sont pas dans final et ", 
                    paste(setdiff(names(currentcritscore), names(dbfinal)), collapse=" "), " ne sont pas dans current")
    }
    dbfinal<-rbind(dbfinal, currentcritscore)
  }
  
  #here, you can rbind dbfinal with the scores of the criteria that are not scored in the standard way
  currentcritscore<-computescore_soil_CH(interface,
                                         inputsdata, 
                                         database)
  if(!identical(sort(names(dbfinal)), sort(names(currentcritscore)))){
    message(paste(setdiff(names(dbfinal), names(currentcritscore)), collapse=" "), " ne sont pas dans final et ", 
            paste(setdiff(names(currentcritscore), names(dbfinal)), collapse=" "), " ne sont pas dans current")
  }
  dbfinal<-rbind(dbfinal,currentcritscore)
  
  #remove trees with negative values for soil or climate
  dbfinal<-dbfinal[!(dbfinal$IDAFTA %in% dbfinal$IDAFTA[dbfinal$value<0]),]
  
  #order the df by orderby, using latin name as id (this adds an id variable, which is a factor with levels ordered by the orderby side)
  #icicicic I know it is not logical to do that here, it would be more logical to reorder the factor outside of the computation of the score
  # to do: separate computation of score and ordering of the species
  dbfinal<-orderdf(df=dbfinal, orderby=orderby, idvariable="IDAFTA", interface=interface) 
  
  # give negative values for response traits so that they appear on the left
  dbfinal$value[dbfinal$side=="responsetrait"]<- -dbfinal$value[dbfinal$side=="responsetrait"]
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  
  return(dbfinal)
  
}
