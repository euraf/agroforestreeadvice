#add an IDAFTA column to the data so that reference to this ID column can be automated
dataDENTRO$IDAFTA<-dataDENTRO$English.name


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