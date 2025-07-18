#add an IDAFTA column to the data so that reference to this ID column can be automated
dataCzech$IDAFTA<-dataCzech$Scientific_name
#add a tooltip column if it does not exist
if (is.null(dataCzech$tooltipspecies)) dataCzech$tooltipspecies<-dataCzech$Latin_name

#' compute_suitability for Czech data
#'
#' @param inputsdata named character vector of choices (for response traits) or Big criteria (for effect traits) made by the user, warning: these are the values internal to the interface, not the labels seen on the screen (which depend on language)
#' @param database data.frame of tree characteristics, with columns  , 
#' @param interface data.frame describing the interface of the app for this database, with columns "initialorder" (not used), "side" (either reponsetrait or effecttrait), "order" (not used), "BigCriteria", "criteria", "choice", "objecttype" (checkbox, Selectinput etc...), "weightwithincriteria" (not used for now), "BigCriteria_en", "criteria_en", "choice_en" and other colums for translations in other languages
#' @param orderby either effecttrait or reponsetrait, for final ordering of the data.frame rows
#'
#' @return A data.frame with names "side", "BigCriteria", "English.name", "Scientific.name", "value", where side, big criteria (but only those relevant to inputdata choices, except for the effecttraits side (if no big criteria chosen, then keep all))"English.name" and "Scientific.name" are the same as in database, and value is computed to be the score for each tree for each retained big criteria, and species is an ordered factor (ordered by sum of effecttraits or responsetraits depending on argument orderby)
#' @export
#'
#' @examples compute_suitability_Czech(inputsdata = c(climateclass=3, legislation="approval"), database=dataCzech, interface=interfaceCzech)

compute_suitability_Czech<-function(inputsdata=NULL,
                                   database, 
                                   interface,
                                   orderby="responsetrait",
                                   use_weights=FALSE){
  dbfinal<-data.frame()
  #save(inputsdata, file = "inputsdata.RData")
  toto<-unique(interface[,c("criteria", "objecttype", "side", "BigCriteria")])
  weight <- interface[, c("criteria", "weightwithincriteria")]
  print(weight)
  if (use_weights==FALSE) weight$weightwithincriteria<-1
  
  rownames(toto)<-toto$criteria
  standardformcriteria<-intersect(gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)), 
                                c("legislation", "fruit", "forage", "forest", "ornamental",
                                    "height", "coppice", "habitus", "growthspeed", 
                                    "earlinessleafing", "floweringdate","subsidy",
                                    "climateclass", "altitude", "soil_fertility",
                                    "soil_water", "light", "wood", "food", "Undergrowth",
                                    "approval", "endengeredG", "endengeredU", "endengeredY")) #we intersect to cover the case when parameters are sent through url=> not all parameters might be present
  for(crit in standardformcriteria){
    #print(paste("compute score for", crit))
    dbfinal<-rbind(dbfinal, default_computecrit(criteria=crit,
                                                type= toto[crit, "objecttype"],
                                                BigCriteria=toto[crit, "BigCriteria"],
                                                side=toto[crit, "side"],
                                                weight=weight[weight$criteria==crit,"weightwithincriteria"][1],
                                                inputs=inputsdata, 
                                                db=database))                                             
  }

  #order the df by orderby, using latin name as id (ads an id variable, which is a factor with levels ordered by the orderby side)
  #icicicic I know it is not logical to do that here, it would be more logical to reorder the factor outside of the computation of the score
  # to do: separate computation of score and ordering of the species
  dbfinal<-orderdf(df=dbfinal, orderby=orderby, idvariable='Scientific_name', interface=interface) 
  
  # give negative values for response traits so that they appear on the left
  dbfinal$value[dbfinal$side=="responsetrait"]<- -dbfinal$value[dbfinal$side=="responsetrait"]
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  


  #assign("dbfinal", dbfinal, envir = .GlobalEnv) # debugging, this is to be able to see the result in the console
  return(dbfinal)
  
}