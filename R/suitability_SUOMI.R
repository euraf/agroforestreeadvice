#add an IDAFTA column to the data so that reference to this ID column can be automated
dataSUOMI$IDAFTA<-dataSUOMI$latin

#' compute_suitability for [MODELNAME]
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

compute_suitability_SUOMI<-function(inputsdata=NULL,
                                   database, 
                                   interface,
                                   orderby="responsetrait"){
  
  dbfinal<-data.frame()
  toto<-unique(interface[,c("criteria", "objecttype", "side", "BigCriteria")])
  rownames(toto)<-toto$criteria
  standardformcriteria<-intersect(gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)), 
                                  c("yield", "use", "soilhumidity", "soilrichness", "soilcalcareous", "soiltexture", "crusting",
                                    "size",  "sun", "shelter", "zone")) #we intersect to cover the case when parameters are sent through url=> not all parameters might be present
    
  
  #"description"
  #warning: the data has not been corrected to separate "good" conditions and "not so good" conditions (in parenthesis => trees with parentheses are not counted)
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
  notsogood<-data.frame()
  for(crit in c("notsogood_yield", "notsogood_use","notsogood_zone")){
  #  message("################## not so good ######################")
  critbis<-gsub(pattern="notsogood_", replacement="", fixed=TRUE, x=crit)
  inputsbis<-inputsdata[gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)) == critbis]
  notsogood<-rbind(notsogood, default_computecrit(criteria=critbis,
                      type= toto[critbis, "objecttype"],
                      BigCriteria=toto[critbis, "BigCriteria"],
                      side=toto[critbis, "side"],
                      inputs=inputsbis,
                      db=database[!is.na(database[,crit]) & database[,crit]!="",]))
  }
  notsogood<-notsogood[notsogood$value>0,]
  notsogood$value<-notsogood$value/2
  #browser()
  dbfinal<-rbind(dbfinal, notsogood)
  dbfinal<-aggregate(dbfinal[,"value", drop=FALSE], 
                     by=dbfinal[,setdiff(names(dbfinal), "value")], sum, na.rm=TRUE)


   #order the df by orderby, using latin name as id (this adds an id variable, which is a factor with levels ordered by the orderby side)
  #icicicic I know it is not logical to do that here, it would be more logical to reorder the factor outside of the computation of the score
  # to do: separate computation of score and ordering of the species
  dbfinal<-orderdf(df=dbfinal, orderby=orderby, idvariable="latin", interface=interface) 
  
  # give negative values for response traits so that they appear on the left
  dbfinal$value[dbfinal$side=="responsetrait"]<- -dbfinal$value[dbfinal$side=="responsetrait"]
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  
  return(dbfinal)
  
}
