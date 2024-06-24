#' compute_suitability for JBOJP
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

compute_suitability_JBOJP<-function(inputsdata=NULL,
                                   database, 
                                   interface,
                                   orderby="responsetrait"){
  print("computing compute_suitability_JBOJP")
  
  dbfinal<-data.frame()
  toto<-unique(interface[,c("criteria", "objecttype", "side", "BigCriteria")])
  rownames(toto)<-toto$criteria
  standardformcriteria<-intersect(gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)), 
                                  c("inJBOJP none of the columns follow standard rules")) #we intersect to cover the case when parameters are sent through url=> not all parameters might be present
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
  #in JBOJP, the criteria as computed as follows: 
  #- if the content of the column is the same as the usser input, score is 2 (i.e. works without adaptation)
  #- if the content is NEE, score is 0
  #- if the content is a number, it requires that the user selected the corresponding capacity (in which case the score is 1), otherwise 0
  
  #database<-read.table("models/dataJBOJP.txt", sep="\t", header=TRUE)
  #interface<-read.table("models/interfaceJBOJP.txt", sep="\t", header=TRUE, fill=TRUE)
  #actually the BigCriteria GeneralUse is used to filter the rows (necessary to avoid duplicated species)
  #=> make it into radioButtons
  userobjective<-inputsdata["Objective"]
  database<-database[database[,make.names(userobjective)]==userobjective,] #select the lines corresponding to the user objective
  database$species<-ifelse(database$Onderstam=="", database$Botanisch, paste(database$Botanisch, database$Onderstam, sep="/"))
  if(length(unique(database$specie))!=nrow(database)) {warning("there are duplicated species"); print(database[,c("Botanisch", "Onderstam", "Toepassingen", "species")])}
  
  for( crit in setdiff(toto$criteria, c("", "Objective"))){
    print(crit)
    #browser()
    intercrit<-interface[interface$criteria==crit,]
    correspondingcolumns<-intersect(names(database), make.names(intercrit$choice))
    if(length(correspondingcolumns)>0){ #this is one of the columns in green/red
      datacrit<-database[,c("species", "Soort", "Botanisch", "Onderstam", correspondingcolumns)]
      datacrit$BigCriteria<-unique(intercrit$BigCriteria)
      datacrit$side<-unique(intercrit$side)
      datacrit$value<-0
      for(cc in correspondingcolumns) { #get all the conditions of the user that belong to this criteria
        datacrit$value<-datacrit$value+2*(datacrit[,cc] %in% inputsdata) #if the user conditions are OK for the tree, add 2
        numbers<-suppressWarnings(as.numeric(datacrit[,cc]))
        MITStoget<-ifelse(is.na(numbers), NA, paste("MITS", numbers, sep=""))
        userMITS<-MITStoget
        userMITS[!userMITS %in% inputsdata]<-NA #remove the MITS that the user did not declare having
        datacrit$value[!is.na(userMITS)]<-datacrit$value[!is.na(userMITS)] + 1 #add 1 to the score when the user has the necessary MITS
      } 
    }else if (crit=="Toepassingen"){
      datacrit<-database[,c("species", "Soort", "Botanisch", "Onderstam", "Toepassingen")]
      datacrit$BigCriteria<-unique(intercrit$BigCriteria)
      datacrit$side<-unique(intercrit$side)
      datacrit$value<-ifelse(datacrit$Toepassingen %in% inputsdata,2,0)
    }
    
    #print("dbfinal:")
    #print(names(dbfinal))
    #print("datacrit:")
    #print(names(datacrit))
    dbfinal<-rbind(dbfinal, datacrit[,c("species", "Soort", "Botanisch", "Onderstam",
                                        "BigCriteria", "side", "value")])
  }
  
   #order the df by orderby, using latin name as id (this adds an id variable, which is a factor with levels ordered by the orderby side)
  #icicicic I know it is not logical to do that here, it would be more logical to reorder the factor outside of the computation of the score
  # to do: separate computation of score and ordering of the species
  dbfinal<-orderdf(df=dbfinal, orderby=orderby, idvariable="species", interface=interface) 
  
  # give negative values for response traits so that they appear on the left
  dbfinal$value[dbfinal$side=="responsetrait"]<- -dbfinal$value[dbfinal$side=="responsetrait"]
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  
  return(dbfinal)
  
}
