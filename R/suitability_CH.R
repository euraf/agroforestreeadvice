#add an IDAFTA column to the data so that reference to this ID column can be automated
#dataCH$IDAFTA<-paste(dataCH$Lateinischer.Name, dataCH$Kategorie)
dataCH$IDAFTA<-dataCH$Lateinischer.Name
#add a tooltipspecies column so that information is displayed when hovering on the barplot
removeNA<-function(x) {
  x[is.na(x)]<-""
  x[x=="NA"]<-""
  return(x)
}
dataCH$tooltipspecies<-paste(dataCH$Name,
                             removeNA(dataCH$"Bemerkung"), 
                             removeNA(dataCH$"Empfohlene.Sorte"),
                             removeNA(dataCH$"Befruchtung..bspw..mehrere.Sorten."),
                             removeNA(dataCH$"Pflanzabstand.und.Position.in.der.Hecke"), 
                             removeNA(dataCH$"Bemerkungen"), 
                             removeNA(dataCH$Comments))
#actually, the tooltipspecies is not displayed because I removed the Kategorie in the IDAFTA column of data, to simplify yaxis labels in the graph...
#solution: use only latin name as IDAFTA, because the data is filtered by Kategorie right away in suitability_CH



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
    dbfinal<-data.frame(IDAFTA="For this model, it is mandatory to provide at leaset parameter Kategorie",
                        "side"="reponsetrait", "BigCriteria"="", 
                        "English.name"="", "Scientific.name"="", "value"=0)
    print("end suitability CH with missing Kategorie parameter")
    return(dbfinal)
  }
  
  dbfinal<-data.frame()
  database<-database[database$Kategorie==inputsdata["Kategorie"],]
  database$IDAFTA<-database$Lateinischer.Name #because we removed Kategorie, so tree altin name becomes the ID
  
  toto<-unique(interface[,c("criteria", "objecttype", "side", "BigCriteria")])
  rownames(toto)<-toto$criteria
  standardformcriteria<-intersect(c("Boden", "Standortanspruch", "Blütezeit", "Erntezeit",                                  
                                    "Height", "Width", "Mykorrhiza",  "Ökologie",  "Verwendung" ),                                
                                  gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)) 
  ) #we intersect to cover the case when parameters are sent through url=> not all parameters might be present
  for(crit in standardformcriteria){
    #print(paste("compute score for", crit))
    dbfinal<-rbind(dbfinal, default_computecrit(criteria=crit,
                                                type= toto[crit, "objecttype"],
                                                BigCriteria=toto[crit, "BigCriteria"],
                                                side=toto[crit, "side"],
                                                inputs=inputsdata, 
                                                db=database))
  }
  
  #here, you can rbind dbfinal with the scores of the criteria that are not scored in the standard way
  
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
