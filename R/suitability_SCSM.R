#add an IDAFTA column to the data so that reference to this ID column can be automated
dataSCSM$IDAFTA<-paste(dataSCSM$genus,dataSCSM$species, sep=" ")

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
  
  database['species_phylogenetic']=database['species']
  #icici we use as id the combination genus species
  database['idspecies']=paste(database$genus,database$species, sep=" ")
  #warning: some latin species are duplicated (solanum	betaceum, trifolium	pratense), with different common names
  
  # 
  # # Calulate value for temperature
  # database_temp = database
  # database_temp['criteria'] = 'temperature'
  # database_temp$BigCriteria<-"climate"
  # database_temp$side<-"responsetrait"
  # temperature = as.numeric(inputsdata["temperature"])
  # #database_temp = mutate(database_temp, value = ifelse(temperature_min<=temperature & temperature<=temperature_max, 1, -1))  
  # #icicic from Marie: since I had a problem when deploying the app to shinyappsio (often due to hidden package dependencies), I try without dplyr
  # #also, I give the value 0 if it is outside the range because the app expects a score from 0 to Inf
  # database_temp$value<-ifelse(database_temp$temperature_min<=temperature & temperature<=database_temp$temperature_max, 1, 0)
  # 
  # # Calulate value for precipitation
  # database_precipitation = database
  # database_precipitation['criteria'] = 'precipitation'
  # database_precipitation$BigCriteria<-"climate"
  # database_precipitation$side<-"responsetrait"
  # precipitation = as.numeric(inputsdata["precipitation"])
  # #database_precipitation = mutate(database_precipitation, value = ifelse(precipitation_min<=precipitation & precipitation<=precipitation_max, 1, -1))  
  # #icicic from Marie: since I had a problem when deploying the app to shinyappsio (often due to hidden package dependencies), I try without dplyr
  # #also, I give the value 0 if it is outside the range because the app expects a score from 0 to Inf
  # database_precipitation$value<-ifelse(database_precipitation$temperature_min<=temperature & temperature<=database_precipitation$temperature_max, 1, 0)
  # 
  # dbfinal<-rbind(database_temp, database_precipitation)
  # 
  dbfinal<-data.frame()
  toto<-unique(interface[,c("criteria", "objecttype", "side", "BigCriteria")])
  rownames(toto)<-toto$criteria #in SCSM, utilities were a mix of several Big criteria, but it breaks the code if not all choices of the same criterion are in the same Bigcriteria

  print("compute adaptation from responsetraits")
  standardformcriteria<-c("temperature", "precipitation")
  for(crit in standardformcriteria){
    dbfinal<-rbind(dbfinal, default_computecrit(criteria=crit,
                                                type= toto[crit, "objecttype"],
                                                BigCriteria=toto[crit, "BigCriteria"],
                                                side=toto[crit, "side"],
                                                inputs=inputsdata, 
                                                db=database))
  }
  
  print("compute efficiency from effecttraits")
  standardformcriteria<-c("utilities", "form", "height","lifespan")
  for(crit in standardformcriteria){
    dbfinal<-rbind(dbfinal, default_computecrit(criteria=crit,
                                                type= toto[crit, "objecttype"],
                                                BigCriteria=toto[crit, "BigCriteria"],
                                                side=toto[crit, "side"],
                                                inputs=inputsdata, 
                                                db=database))
  }
  
  
  #for (crit in c("utilities", "form", "height","lifespan")){
  #if (!grepl("any", inputsdata[[crit]])) {
  #  database[,paste("score_", crit)]<-computecrit(database$crit, type=ifelse(crit=="utilities", ))
  #}
  
  #}
  #browser()
  
  #order the df by orderby, using latin name as id (ads an id variable, which is a factor with levels ordered by the orderby side)
  #icicicic I know it is not logical to do that here, it would be more logical to reorder the factor outside of the computation of the score
  # to do: separate computation of score and ordering of the species
  dbfinal<-orderdf(df=dbfinal, orderby=orderby, idvariable='idspecies', interface=interface) 
  
  # give negative values for response traits so that they appear on the left
  dbfinal$value[dbfinal$side=="responsetrait"]<- -dbfinal$value[dbfinal$side=="responsetrait"]
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  
  return(dbfinal)
  
}
