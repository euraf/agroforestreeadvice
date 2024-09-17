#add an IDAFTA column to the data so that reference to this ID column can be automated
dataCzech$IDAFTA<-dataCzech$Scientific_name
#add a tooltip column if it does not exist
if (is.null(dataCzech$tooltipspecies)) dataCzech$tooltipspecies<-dataCzech$Czech_name

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
                                   use_weights=TRUE){
  dbfinal<-data.frame()
  toto<-unique(interface[,c("criteria", "objecttype", "side", "BigCriteria")])
  weight <- interface[, c("criteria", "weightwithincriteria")]
  print(weight)
  if (use_weights==FALSE) weight$weightwithincriteria<-1
  
  rownames(toto)<-toto$criteria
  standardformcriteria<-intersect(gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)), 
                                c("legislation", "fruit", "forage", "forest", "ornamental",
                                    "height", "understory_tree", "habitus", "growthspeed", 
                                    "earlinessleafing", "floweringdate", 
                                    "climateclass", "altitude", "soil_fertility",
                                    "soil_water", "light", "wood", "food",
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

Hard_criteria_filter <- function(db, inputsdata, interface) {
  # Filter trees - if 999 in weightwithincriteria, then it is a hard criteria and algorithm will drop trees which do not meet it 
  # it will check if values of these criteria is 1 - if not - drop them
  # In esence, it checks what is hard criterium and then drops trees which do not have it as 1
  
  if (999 %in% interface$weightwithincriteria) {
    print("Filtered by hard criteria") }
  else {
    print("NO hard criteria (999) found in interface")
        return(db) }
  
  # Consolidate criteria from interface based on inputsdata and slider inputs
  used_criteria <<- rbind(interface[interface$criteria %in% names(inputsdata) &
                                    interface$weightwithincriteria == 999 & !is.na(interface$weightwithincriteria),],
                         interface[interface$objecttype == "sliderInput",])

  # drop criteria which weightwithincriteria is not 999
  used_criteria <- used_criteria[used_criteria$weightwithincriteria == 999,]
  used_criteria_uniq <- unique(used_criteria$criteria)

  unique_db <- db[FALSE, ]
  unique_species <- unique(db$species)

  # loop species, find values for each relevant criteria of the species, and if there is a zero, then add "yes" to delete column
  for (species in unique_species) {
      species_data <- db[db$species == species, ]
      
      # Get only the criteria that are relevant
      criteria_data <- species_data[species_data$criteria %in% used_criteria_uniq, ]
      
      # Get unique values of the criteria
      unique_values <- unique(criteria_data$value)
      unique_criteria <- unique(criteria_data$criteria)
      
      # Determine if there are any zeros in the values
      delete_flag <- if("0" %in% unique_values) "yes" else "no"
      
      # Create a new row to be added to unique_db
      new_row <- data.frame(
          species = species,
          value = paste(unique_values, collapse = ", "),
          criteria = paste(unique_criteria, collapse = ", "),
          delete = delete_flag)
      
      # Append new row to unique_db
      unique_db <- rbind(unique_db, new_row)
  }

  # Select species where 'delete' is "yes"
  delete_species <- unique_db$species[unique_db$delete == "yes"]

  # drop these species
  db <- db[!db$species %in% delete_species, ]

  return(db)
}

dfczechinfo <- function(interface = interface, data = data) {
    #loads all "info" side rows of interface, find their values for each tree in data and 
    #then agregate them into one row if they share "criteria" value

    #from interface - load rows where column side == "info"
    datainfo2 <- data.frame(interface[interface$side == "info", c("criteria", "choice")])

    sorted_col_names <<- unique(datainfo2$criteria) #get unique values of "criteria" column

    #combine "choice" values of duplicated "criteria" values
    datainfo2 <- aggregate(choice ~ criteria, data = datainfo2, paste, collapse = ", ")
    # Check if the column "Scientific_name" exists
    if ("Scientific_name" %in% names(data)) {
      datainfo3 <- data.frame(data["Scientific_name"])
    } else {
      # If "Scientific_name" does not exist, try another column, e.g., "nameCommon"
      if ("nameCommon" %in% names(data)) {
        datainfo3 <- data.frame(data["nameCommon"])
      } else {
        print("Neither 'Scientific_name' nor 'nameCommon' exist in the data frame.")
      }
    }
    #try each criteria from datainfo2$criteria as a column, if it exists, then assign the value from datainfo2 to the new column
    for (i in 1:length(datainfo2$criteria)) {
      if (datainfo2$criteria[i] %in% colnames(data)) {
        datainfo3[datainfo2$criteria[i]] <- data[, datainfo2$criteria[i]]
      }
    }
    
    # loads all possible choices for each criteria
    choices <- lapply(datainfo2$choice, function(x) strsplit(as.character(x), ", ")[[1]])

    # iterate the choices, if the values is data column, append the column to datainfo3
    for (i in 1:length(choices)) {
      for (j in 1:length(choices[[i]])) {
        if (choices[[i]][j] %in% colnames(data)) {
          datainfo3[choices[[i]][j]] <- data[, choices[[i]][j]]
        }
      }
    }

    # turn list to vector
    choices <- unlist(choices)
    returned_text <- list()
    for(i in seq_along(choices)) {  # finds answers in choice_en, choice_cz, etc. for each informative_criteria                                              
      row <- interface[interface$choice == choices[i], ]
      info <- row[, "choice"]
      returned_text[[length(returned_text) + 1]] <- list(column = choices[i], info = info)
    }

    # try to match the column name with the column in datainfo3 and replace the value VRAI with returned_text
    for(i in seq_along(returned_text)) {
      if (returned_text[[i]]$column %in% colnames(datainfo3)) {
        datainfo3[[returned_text[[i]]$column]][datainfo3[[returned_text[[i]]$column]] == "VRAI"] <- returned_text[[i]]$info
      }
    }

    for (col in datainfo2$criteria) {
      choice <- lapply(datainfo2$choice[datainfo2$criteria == col], function(x) strsplit(as.character(x), ", ")[[1]])
      choice <- unlist(choice)
      choice <- c(choice, col)
      datainfo3[[col]] <- do.call(paste, c(lapply(choice, function(x) datainfo3[[x]]), sep = ", "))
    }

    #drop all columns other then (col in datainfo2$criteria)
    datainfo3 <- datainfo3[, c("Scientific_name" ,datainfo2$criteria)]

    datainfo3 <- data.frame(lapply(datainfo3, function(x) {
      x <- gsub("FAUX", "", x)          # Replace "FAUX" with ""
      x <- gsub(" , ", "", x)           # Replace " , " with ""
      x <- gsub("^\\,+|^\\ +", "", x)   # Remove commas and spaces at the start
      x <- gsub("\\,+$|\\ +$", "", x)   # Remove commas and spaces at the end
      x <- trimws(x, which = c("both")) # Remove leading and trailing whitespaces
      return(x)
    }))

    # sort the columns by the order of sorted_col_names - this is the order of original interface file
    datainfo3 <- datainfo3[, c("Scientific_name", sorted_col_names)]

    return(datainfo3)
  
}
