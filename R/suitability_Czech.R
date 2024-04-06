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
                                   orderby="responsetrait"){
  
  dbfinal<-data.frame()
  toto<-unique(interface[,c("criteria", "objecttype", "side", "BigCriteria")])
  rownames(toto)<-toto$criteria
  standardformcriteria<-intersect(gsub(pattern="[0-9]+", replacement="", x=names(inputsdata)), 
                                  c("legislation", "fruit", "forage", "forest", "ornamental",
                                    "height", "understory_tree", "habitus", "growthspeed", 
                                    "earlinessleafing", "floweringdate", 
                                    "climateclass", "altitude", "soil_fertility",
                                    "soil_water", "light", "wood", "food",
                                    "approval", "endengeredG", "endengeredU", "endengeredY")) #we intersect to cover the case when parameters are sent through url=> not all parameters might be present
  for(crit in standardformcriteria){
    print(paste("compute score for", crit))
    dbfinal<-rbind(dbfinal, default_computecrit(criteria=crit,
                                                type= toto[crit, "objecttype"],
                                                BigCriteria=toto[crit, "BigCriteria"],
                                                side=toto[crit, "side"],
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

#' @param db The database of trees.
#' @param inputsdata A list of criteria for filtering the trees.
#'
#' @return The filtered database of trees.
#'
#' @examples
#' db <- data.frame(growthspeed = c("fast", "medium", "slow"),
#'                  habitus = c("upright", "spreading", "columnar"),
#'                  earlynessleafing = c("early", "medium", "late"),
#'                  understory_tree = c(TRUE, FALSE, TRUE),
#'                  height = c(5, 10, 15))
#' inputsdata <- list(growthspeed = "fast",
#'                    habitus = "upright",
#'                    earlynessleafing = "early",
#'                    understory_tree = TRUE,
#'                    height1 = 5,
#'                    height2 = 10)
#' filtered_db <- Hard_criteria_filter(db, inputsdata)
#' print(filtered_db)
#'
#' @export
Hard_criteria_filter <- function(db, inputsdata) {
  # Filter trees based on criteria in inputsdata - if the tree do not meet these criteria, it is removed
  # print(inputsdata)

  #if ("growthspeed" %in% names(inputsdata))              {db <- db[db$growthspeed == inputsdata[["growthspeed"]],]}
  #if ("habitus" %in% names(inputsdata))                  {db <- db[db$habitus == inputsdata[["habitus"]],]}
  #if ("earlinessleafing" %in% names(inputsdata))         {db <- db[db$earlinessleafing == inputsdata[["earlinessleafing"]],]}
  #if ("understory_tree" %in% names(inputsdata))          {db <- db[db$understory_tree == inputsdata[["understory_tree"]],]}
  #if ("height1" %in% names(inputsdata))                  {db <- db[db$height >= inputsdata[["height1"]],]}
  #if ("height2" %in% names(inputsdata))                  {db <- db[db$height <= inputsdata[["height2"]],]}
  if ("fruit" %in% names(inputsdata))                    {db <- db[db$fruit == "VRAI", ]}
  if ("forage" %in% names(inputsdata))                   {db <- db[db$forage == "VRAI", ]}
  if ("forest" %in% names(inputsdata))                   {db <- db[db$forest == "VRAI", ]}

  # write_xlsx(db, "01_dbfinal_filtered.xlsx") #for debugging
  return(db)
}

dfczechinfo <- function(interface = interfaceCzech, data = dataCzech) {
    #loads all "info" side rows of interface, find their values for each tree in data and 
    #then agregate them into one row if they share "criteria" value

    #from interface - load rows where column side == "info"
    datainfo2 <- data.frame(interface[interface$side == "info", c("criteria", "choice")])

    #combine "choice" values of duplicated "criteria" values
    datainfo2 <- aggregate(choice ~ criteria, data = datainfo2, paste, collapse = ", ")
    datainfo3 <- data.frame(dataCzech["Scientific_name"])

    #try each criteria from datainfo2$criteria as a column, if it exists, then assign the value from datainfo2 to the new column
    for (i in 1:length(datainfo2$criteria)) {
      if (datainfo2$criteria[i] %in% colnames(dataCzech)) {
        datainfo3[datainfo2$criteria[i]] <- dataCzech[, datainfo2$criteria[i]]
      }
    }
    
    # loads all possible choices for each criteria
    choices <- lapply(datainfo2$choice, function(x) strsplit(as.character(x), ", ")[[1]])

    # iterate the choices, if the values is data column, append the column to datainfo3
    for (i in 1:length(choices)) {
      for (j in 1:length(choices[[i]])) {
        if (choices[[i]][j] %in% colnames(dataCzech)) {
          datainfo3[choices[[i]][j]] <- dataCzech[, choices[[i]][j]]
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

    return(datainfo3)
}

translator <- function(data, interface, vocabulary, language) {
  # Tries to translate all data - based on the interface and vocabulary

  # Ensure 'info' side exists in 'interface'
  if (!"info" %in% interface$side) {
    warning("No 'info' side found in the 'interface' dataframe.")
    return(data)  # Exit early if no 'info' side to avoid further errors
  }
  interface <- interface[interface$side == "info", ]

  # Create a translation map from vocabulary
  translations <- c(setNames(vocabulary[[language]], vocabulary$type),setNames(interface[[language]], interface$choice))

  # Function to translate cell values, handling both simple "furniture" and comma-separated cases "furniture, sports"
    translate_cell <- function(cell, translations) {
    if (str_detect(cell, ", ")) {
      # Handle comma-separated values
      translated_values <- sapply(str_split(cell, ",\\s*")[[1]], function(x) {
        if (x %in% names(translations)) {
          return(translations[x])
        } else {
          return(x)
        }
      })
      str_c(translated_values, collapse = ", ") # puts cells back together
    } else {
      # Handle simple values
      if (cell %in% names(translations)) {
        return(translations[cell])
      } else {
        return(cell)
      }
    }
  }
  # Apply translations to all cells in the data
  data <- data %>% 
    mutate(across(everything(), ~sapply(., translate_cell, translations = translations)))

  # Rename columns based on vocabulary
  rename_cols <- filter(vocabulary, object == "DFinfo_headline")
  rename_cols <- setNames(str_to_title(rename_cols[[language]]), rename_cols$type)
  names(data) <- sapply(names(data), function(x) ifelse(x %in% names(rename_cols), rename_cols[x], x))

  return(data)
}

#translated_info <- translator(x, interfaceCzech, vocabulary, "cz")
