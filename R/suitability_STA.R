#add an IDAFTA column to the data so that reference to this ID column can be automated
dataSTA$IDAFTA<-paste(dataSTA$Tree_latin, dataSTA$Region, dataSTA$precipitation, dataSTA$altitude)
#add a tooltip column if it does not exist
#if (is.null(dataSTA$tooltipspecies)) dataSTA$tooltipspecies<-dataSTA$Tree_latin #not possible: STA is not organized with one line per species


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
  #browser()
  # showModal(modalDialog(
  #   title = "I haven't finished recoding the model after cleaning of the data"
  # ))
  # return(data.frame(species="no data yet", side="responsetrait", value=1, BigCriteria="please describe your site and objectives"))
  
  
  dbfinal<-data.frame()
  interfcrit<-unique(interface[,c("criteria", "objecttype", "side", "BigCriteria")])
  rownames(interfcrit)<-interfcrit$criteria
  standardformcriteria<-intersect(gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)), 
                                  c("Provision", "RegulationBiotic", "RegulationMicroclimate", "RegulationSoil", "RegulationInteractions", "RegulationOther")) #we intersect to cover the case when parameters are sent through url=> not all parameters might be present
  criteriaresponse<-c("Region", "precipitation", "altitude" )
  print("adaptation")
  #selects the subset of database corresponding to the selected country, region, crop, location
  subsetDB<-database
  #remove the criteria that the user did not select specifically (e.g. still the default all...), and filter database based on the other criteria
  for(crit in criteriaresponse) {
    if(substr(inputsdata[crit],start=1, stop=4) == "All ") {
      inputsdata<-inputsdata[setdiff(names(inputsdata), crit)]
    } else { # if the user chose a specific value for the crit crtieria then filter the data according to the user choice
      subsetDB<-subsetDB[ subsetDB[[crit]] == inputsdata[crit] ,]
    }
  }
  
  if(nrow(subsetDB)==0) {
    combin1<-unique(database[,c("Region"), drop=FALSE])
    combin2<-unique(database[,c("precipitation", "altitude")])
    combin2<-combin2[! (combin2$precipitation=="All precipitation zones" & combin2$altitude=="All altitudes"),]
    combin3<-merge(combin1, combin2)
    combin3<-combin3[apply(combin3, 1, paste, collapse="_") %in% apply(database[,names(combin3)], 1, paste, collapse="_"),]
    combin3[,c("precipitation", "altitude")]<-lapply(combin3[,c("precipitation", "altitude")], function(x) {x[substr(x,start=1, stop=4) == "All "]<-""; return(x)})
    print("this combination of region, precipitation altitude does not exist")
    showModal(modalDialog(
      title = "this combination of region, precipitation, altitude does not exist",
      "So we give you species cited in other countries/regions/precipitations/altitudes",
      "please consider this list as indicative only as the results might not be relevant for your conditions",
      "the available conditions are:",
      paste(apply(combin3,1, paste, collapse=" "), collapse="; "), 
    ))
    #keep all data
    subsetDB<-database
    #compute adaptation score: number of conditions (among the items in "Country", "Region", "Crop", "altitude", "precipitation" that the user did not leave at "All ...") that the line validates (NB this might be used in the future to weight the scores??)
    dataadaptation<-unique(database[,c("Country", "Region", "altitude", "precipitation", "IDAFTA")])
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
      toto$value<-ifelse(toto[criterion]==ctk,1,0)
      resultadapt<-rbind(resultadapt, toto)
    }
    resultadapt<-aggregate(resultadapt[,"value", drop=FALSE], by=resultadapt[,c("IDAFTA", "criteria")], mean)
    #add side and big criteria info
    resultadapt<-merge(resultadapt, unique(interface[,c("side", "BigCriteria", "criteria", "choice" )]), all.x=TRUE)[,c("side", "IDAFTA", "value", "BigCriteria", "criteria", "choice" )]
    #resultadapt<-aggregate(resultadapt[,"value", drop=FALSE], by=resultadapt[,c("side", "BigCriteria", "IDAFTA"), drop=FALSE], mean, na.rm=TRUE)
  } else { #there was data fitting all the users criteria
    #we give the same adaptation score to all trees
    resultadapt<-merge(unique(database[,c("IDAFTA"), drop=FALSE]),
                       unique(interface[interface$side=="responsetrait",c("side", "BigCriteria", "criteria", "choice" )]))#,
    #all.x=TRUE)
    resultadapt$value<-1
  }
  
  print("efficiency")
  for(crit in standardformcriteria){
    print(paste("compute score for", crit))
    dbfinal<-rbind(dbfinal, default_computecrit(criteria=crit,
                                                type= interfcrit[crit, "objecttype"],
                                                BigCriteria=interfcrit[crit, "BigCriteria"],
                                                side=interfcrit[crit, "side"],
                                                inputs=inputsdata, 
                                                db=subsetDB))
  }
  
  #here, you can rbind dbfinal with the scores of the criteria that are not scored in the standard way
  
  # #just in case we end up wit hseveral lines for the same combination of tree and criteria... actually we do, I dont know why
  if (nrow(dbfinal)>1) dbfinal<-aggregate(dbfinal[,"value", drop=FALSE], by=dbfinal[,c("IDAFTA", "side", "BigCriteria", "criteria"), drop=FALSE], mean, na.rm=TRUE)
  resultadapt<-aggregate(resultadapt[,"value", drop=FALSE], by=resultadapt[,c("IDAFTA", "side", "BigCriteria", "criteria"), drop=FALSE], mean, na.rm=TRUE)
  dbfinal<-rbind(resultadapt, dbfinal)
  
  # 
  
  #order the df by orderby, using IDAFTA as id (this adds an id variable, which is a factor with levels ordered by the orderby side)
  #icicicic I know it is not logical to do that here, it would be more logical to reorder the factor outside of the computation of the score
  # to do: separate computation of score and ordering of the species
  dbfinal<-orderdf(df=dbfinal, orderby=orderby, idvariable="IDAFTA", interface=interface) 
  
  # give negative values for response traits so that they appear on the left
  dbfinal$value[dbfinal$side=="responsetrait"]<- -dbfinal$value[dbfinal$side=="responsetrait"]
  
  #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
  print("fin suitability")
  return(dbfinal)
}



# 
# #print(str(inputsdata))
# interface<-interface[!is.na(interface$side),]
# database$Countryregion<-paste(database$Country, database$Region, sep=".")
# database$crop<-database$Crop
# names(database)[names(database)=="criteria"]<-"choice" 
# names(database)[names(database)=="Estimate"]<-"score"
# criteriaresponse<-unique(unname(unlist(interface[!is.na(interface$side) 
#                                                  & interface$side=="responsetrait", c("criteria")])))
# # BCeffect<-unique(unname(unlist(interface[!is.na(interface$side) 
# #                                          & interface$side=="effecttrait", c("BigCriteria")])))
# # 
# print("adaptation")
# #selects the subset of database corresponding to the selected country, region, crop, location
# subsetDB<-database
# #remove the criteria that the user did not select specifically (e.g. still the default all...), and filter database based on the other criteria
# for(crit in criteriaresponse) {
#   if(substr(inputsdata[crit],start=1, stop=4) == "All ") {
#     inputsdata<-inputsdata[setdiff(names(inputsdata), crit)]
#   } else { # if the user chose a specific value for the crit crtieria then filter the data according to the user choice
#     if(crit %in% names(subsetDB)) { #crop or countryregion
#       subsetDB<-subsetDB[ subsetDB[[crit]] == inputsdata[crit] ,]
#     } else { #altitude or precicpitation
#       subsetDB<-subsetDB[ subsetDB$Subgroup == inputsdata[crit] 
#                           | tolower(substr(subsetDB[["Subgroup"]], start=1, stop=4))=="all ",]   
#     }
#   }
# }
#
# if(nrow(subsetDB)==0) {
#   print("this combination of country, crop, precipitation altitude does not exist")
#   showModal(modalDialog(
#     title = "this combination of country, crop, precipitation, altitude does not exist",
#     "So we give you species cited in other countries/regions/precipitations/altitudes",
#     "please consider this list as indicative only as the results might not be relevant for your conditions"
#   ))
#   #keep all data
#   subsetDB<-database
#   #compute adaptation score
#   dataadaptation<-unique(database[,c("Country", "Region", "countryregion", "Crop", "crop", "Subgroup", "New_Tree_Latin")])
#   dataadaptation$countryregion<-paste(dataadaptation$Country, " (", dataadaptation$Region,")", sep="")
#   dataadaptation$crop<-dataadaptation$Crop
#   
#   if(!is.null(inputsdata)) {
#     choicetokeep<-unlist(interface[!is.na(interface$side) 
#                                    & interface$side=="responsetrait" 
#                                    & interface$choice %in% unlist(inputsdata), c("choice")])
#   } else {
#     choicetokeep<-unique(unname(unlist(interface[!is.na(interface$side) 
#                                                  & interface$side=="responsetrait", c("choice")])))
#   }
#   resultadapt<-data.frame()
#   for(ctk in choicetokeep){
#     criterion<-interface[interface$choice==ctk, "criteria"]
#     toto<-dataadaptation
#     toto$criteria<-criterion
#     toto$side<-"responsetrait"
#     if(criterion %in% names(dataadaptation)) variabletolookat<-criterion else variabletolookat<-"Subgroup"
#     toto$value<-ifelse(toto[variabletolookat]==ctk,1,0)
#     resultadapt<-rbind(resultadapt, toto)
#   }
#   resultadapt<-aggregate(resultadapt[,"value", drop=FALSE], by=resultadapt[,c("New_Tree_Latin", "criteria")], max)
#   
#   #add side and big criteria info
#   resultadapt<-merge(resultadapt, unique(interface[,c("side", "BigCriteria", "criteria", "choice" )]), all.x=TRUE)[,c("side", "New_Tree_Latin", "value", "BigCriteria", "criteria", "choice" )]
#   #resultadapt<-aggregate(resultadapt[,"value", drop=FALSE], by=resultadapt[,c("side", "BigCriteria", "Tree_latin"), drop=FALSE], mean, na.rm=TRUE)
# } else { #there was data fitting all the users criteria
#   #we give the same adaptation score to all trees
#   resultadapt<-merge(unique(database[,c("New_Tree_Latin"), drop=FALSE]),
#                      unique(interface[interface$side=="responsetrait",c("side", "BigCriteria", "criteria", "choice" )]))#,
#   #all.x=TRUE)
#   resultadapt$value<-1
# }
# 
# print("effectiveness")  
# #keep the ecosystem services that were chosen by user
# #browser()
# BCtokeep<-interface[ interface$side=="effecttrait" & interface$BigCriteria %in% inputsdata,]
# if (nrow(BCtokeep)==0){print("no objective selected, so keep them all")
#   BCtokeep<-interface[interface$side=="effecttrait", ]
# }
# #browser()
# toto<-subsetDB[subsetDB$choice %in% BCtokeep$choice,]
# if(nrow(toto)==0) {
#   print("no tree provides the selected ES in your conditions, so we keep all ES")
#   showModal(modalDialog(
#     title = "No tree provides the selected Ecosystem Services in your conditions",
#     "So we give you all the ecosystem services"
#   ))
# } else {subsetDB<-subsetDB[subsetDB$choice %in% BCtokeep$choice,]}
# 
# #add the big Criteria and side info 
# subsetDB<-merge(subsetDB, BCtokeep, all.x=TRUE)
# #add the adaptation values
# subsetDB<-rbind(
#   subsetDB[,c("New_Tree_Latin", "side", "BigCriteria", "criteria", "choice", "value")], 
#   resultadapt[,c("New_Tree_Latin", "side", "BigCriteria", "criteria", "choice", "value")]
# )
# 
# #just in case we end up wit hseveral lines for the same combination of tree and BigCriteria
# df<-aggregate(subsetDB[,"value", drop=FALSE], by=subsetDB[,c("New_Tree_Latin", "side", "BigCriteria", "criteria"), drop=FALSE], mean, na.rm=TRUE)
# 
# #order the df by orderby, using latin name as id
# df<-orderdf(df=df, orderby=orderby, idvariable="New_Tree_Latin", interface=interface) 
# 
# # give negative values for response traits so that they appear on the left
# df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]<- -  df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]
# 
# #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
# print("fin suitability")
# 


















# 
# compute_suitability_STAold<-function(inputsdata=NULL,
#                                   database, 
#                                   interface,
#                                   orderby="responsetrait"){
#   
#   #print(str(inputsdata))
#   interface<-interface[!is.na(interface$side),]
#   database$countryregion<-paste(database$Country, " (", database$Region,")", sep="")
#   database$crop<-database$Crop
#   names(database)[names(database)=="ES"]<-"choice" #warning: choices have many synonyms, they are grouped through criteria column in interface
#   names(database)[names(database)=="Estimate"]<-"value"
#   criteriaresponse<-unique(unname(unlist(interface[!is.na(interface$side) 
#                                                    & interface$side=="responsetrait", c("criteria")])))
#   # BCeffect<-unique(unname(unlist(interface[!is.na(interface$side) 
#   #                                          & interface$side=="effecttrait", c("BigCriteria")])))
#   # 
#   print("adaptation")
#   #selects the subset of database corresponding to the selected country, region, crop, location
#   subsetDB<-database
#   #remove the criteria that the user did not select specifically (e.g. still the default all...), and filter database based on the other criteria
#   for(crit in criteriaresponse) {
#     if(substr(inputsdata[crit],start=1, stop=4) == "all ") {
#       inputsdata<-inputsdata[setdiff(names(inputsdata), crit)]
#     } else { # if the user chose a specific value for the crit crtieria then filter the data according to the user choice
#       if(crit %in% names(subsetDB)) { #crop or countryregion
#         subsetDB<-subsetDB[ subsetDB[[crit]] == inputsdata[crit] ,]
#       } else { #altitude or precicpitation
#         subsetDB<-subsetDB[ subsetDB$Subgroup == inputsdata[crit] 
#                             | tolower(substr(subsetDB[["Subgroup"]], start=1, stop=4))=="all ",]   
#       }
#     }
#   }
#   
#   if(nrow(subsetDB)==0) {
#     print("this combination of country, crop, precipitation altitude does not exist")
#     showModal(modalDialog(
#       title = "this combination of country, crop, precipitation, altitude does not exist",
#       "So we give you species cited in other countries/regions/precipitations/altitudes",
#       "please consider this list as indicative only as the results might not be relevant for your conditions"
#     ))
#     #keep all data
#     subsetDB<-database
#     #compute adaptation score
#     dataadaptation<-unique(database[,c("Country", "Region", "countryregion", "Crop", "crop", "Subgroup", "Tree_latin")])
#     dataadaptation$countryregion<-paste(dataadaptation$Country, " (", dataadaptation$Region,")", sep="")
#     dataadaptation$crop<-dataadaptation$Crop
#     
#     if(!is.null(inputsdata)) {
#       choicetokeep<-unlist(interface[!is.na(interface$side) 
#                                      & interface$side=="responsetrait" 
#                                      & interface$choice %in% unlist(inputsdata), c("choice")])
#     } else {
#       choicetokeep<-unique(unname(unlist(interface[!is.na(interface$side) 
#                                                    & interface$side=="responsetrait", c("choice")])))
#     }
#     resultadapt<-data.frame()
#     for(ctk in choicetokeep){
#       criterion<-interface[interface$choice==ctk, "criteria"]
#       toto<-dataadaptation
#       toto$criteria<-criterion
#       toto$side<-"responsetrait"
#       if(criterion %in% names(dataadaptation)) variabletolookat<-criterion else variabletolookat<-"Subgroup"
#       toto$value<-ifelse(toto[variabletolookat]==ctk,1,0)
#       resultadapt<-rbind(resultadapt, toto)
#     }
#     resultadapt<-aggregate(resultadapt[,"value", drop=FALSE], by=resultadapt[,c("Tree_latin", "criteria")], max)
#     
#     #add side and big criteria info
#     resultadapt<-merge(resultadapt, unique(interface[,c("side", "BigCriteria", "criteria", "choice" )]), all.x=TRUE)[,c("side", "Tree_latin", "value", "BigCriteria", "criteria", "choice" )]
#     #resultadapt<-aggregate(resultadapt[,"value", drop=FALSE], by=resultadapt[,c("side", "BigCriteria", "Tree_latin"), drop=FALSE], mean, na.rm=TRUE)
#   } else { #there was data fitting all the users criteria
#     #we give the same adaptation score to all trees
#     resultadapt<-merge(unique(database[,c("Tree_latin"), drop=FALSE]),
#                        unique(interface[interface$side=="responsetrait",c("side", "BigCriteria", "criteria", "choice" )]))#,
#     #all.x=TRUE)
#     resultadapt$value<-1
#   }
#   
#   print("effectiveness")  
#   #keep the ecosystem services that were chosen by user
#   #browser()
#   BCtokeep<-interface[ interface$side=="effecttrait" & interface$BigCriteria %in% inputsdata,]
#   if (nrow(BCtokeep)==0){print("no objective selected, so keep them all")
#     BCtokeep<-interface[interface$side=="effecttrait", ]
#   }
#   #browser()
#   toto<-subsetDB[subsetDB$choice %in% BCtokeep$choice,]
#   if(nrow(toto)==0) {
#     print("no tree provides the selected ES in your conditions, so we keep all ES")
#     showModal(modalDialog(
#       title = "No tree provides the selected Ecosystem Services in your conditions",
#       "So we give you all the ecosystem services"
#     ))
#   } else {subsetDB<-subsetDB[subsetDB$choice %in% BCtokeep$choice,]}
#   
#   #add the big Criteria and side info 
#   subsetDB<-merge(subsetDB, BCtokeep, all.x=TRUE)
#   #add the adaptation values
#   subsetDB<-rbind(
#     subsetDB[,c("Tree_latin", "side", "BigCriteria", "criteria", "choice", "value")], 
#     resultadapt[,c("Tree_latin", "side", "BigCriteria", "criteria", "choice", "value")]
#   )
#   
#   #just in case we end up wit hseveral lines for the same combination of tree and BigCriteria
#   df<-aggregate(subsetDB[,"value", drop=FALSE], by=subsetDB[,c("Tree_latin", "side", "BigCriteria", "criteria"), drop=FALSE], mean, na.rm=TRUE)
#   
#   #order the df by orderby, using latin name as id
#   df<-orderdf(df=df, orderby=orderby, idvariable="Tree_latin", interface=interface) 
#   
#   # give negative values for response traits so that they appear on the left
#   df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]<- -  df$value[df$BigCriteria %in% interface[!is.na(interface$side) & interface$side=="responsetrait", c("BigCriteria")]]
#   
#   #df10best<-df[df$English.name %in% species_order[(length(species_order)-10):length(species_order)],]
#   print("fin suitability")
#   return(df)
#   
# }
