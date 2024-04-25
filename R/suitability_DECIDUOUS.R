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
  
  result$note_hauteur <- as.numeric((result[gsub(pattern="<", replacement=".", fixed=TRUE, x= user_hauteur)]=="oui"))
  
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
