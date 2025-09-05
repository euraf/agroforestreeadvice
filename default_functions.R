#' orders a df of species scores by a given side (or, in the future, a weight between effecttraits and responsetraits)
#'
#' @param df a data frame of scores with columns "side", "BigCriteria", "English.name", "Scientific.name", "value"
#' @param orderby currently= either "effecttrait or responsetrait, in the future: weight of effecttrait (0= order by responsetrait, 1 = order by effecttrait, in between = weighted mean of both)
#' @param idvariable variable to use as ordered factor, it should give unique id to each row
#' @param interface data.frame describing the interface of the app for this database, with columns "initialorder" (not used), "side" (either reponsetrait or effecttrait), "order" (not used), "BigCriteria", "criteria", "choice", "objecttype" (checkbox, Selectinput etc...), "weightwithincriteria" (not used for now), "BigCriteria_en", "criteria_en", "choice_en" and other colums for translations in other languages
#' #'
#' @return a data frame of scores with columns "side", "BigCriteria", "English.name", "Scientific.name", "value", "species" (the idvariable as an ordered factor), ordered by orderby
#' @export
#'
#' @examples
orderdf<-function(df, orderby, idvariable, interface){
  # Calculate the sum of the variables in orderby for each species to find the correct order
  linestokeep<-df$BigCriteria %in% unique(interface[!is.na(interface$side) & interface$side==orderby, c("BigCriteria")])
  if(sum(linestokeep)==0) linestokeep=TRUE
  species_order <- df[linestokeep,]
  species_order<-aggregate(species_order[,"value", drop=FALSE], by=species_order[,idvariable, drop=FALSE], sum, na.rm=TRUE)
  species_order<-species_order[order(species_order$value, decreasing=TRUE),]
  species_order<-species_order[,idvariable] 
  species_order<-species_order[!is.na(species_order)]
  
  # Reorder the levels of the species variable based on the sum
  df$species <- factor(df[,idvariable], levels = species_order)
  #reorder rows
  df<-df[order(df$species, decreasing=TRUE), c("species", setdiff(names(df), "species"))] 
  #decreasing = TRUE so that the best are on top in the dataframe (best = first in the levels of the factor)
  
  # Update reactive interface - so other functions know which interface was used (eg. download handler)
  reactive_Interface(interface)
  
  return(df)
}

#' Title
#'
#' @details 
#' |User input|Database|Score|
#' |-----------|---------|-------|
#' |1 item from a drop-down list (or in radio buttons)|Yes/no columns for each possible item|1 if the tree has this feature, 0 otherwise|
#' |1 item from a drop-down list (or in radio buttons)|1 column containing an item|1 if the tree has this feature, 0 otherwise|
#' |1 item from a checkbox|1 column containing a value|the value if the checkbox is checked, 0 otherwise|
#' |1 or more items in a set of checkboxes|Yes/no columns for each possible item|(number of items present in tree features, among selected items)/(number of selected items)|
#' |1 or more items in a set of checkboxes|1 column containing one or more items, or several columns each containing 1 item|(number of items present in tree features, among selected items)/(number of selected items)|
#' |1 or more items in a set of checkboxes|several columns (with names corresponding to items) containing scores|sum of scores of chosen columns|
#' |1 single numerical value|1 column containing a single value|1-abs(feature-value)/(max(features)-min(features))|
#' |1 single numerical value|1 column containing a range (x-y) |1 if value is within range, 0 if value is outside range|
#' |range of values|1 column containing a single value|1 if the characteristic is within the input range, 0 if the characteristic is outside it|
#' @param criteria single name of a criteria for which to compute the score
#' @param type #type of widget (one of "checkboxGroupInput", "selectInput", "sliderInput", "checkboxInput", "numericInput")
#' @param inputs #character vector of reformatted inputs (until I update everything to accept lists)
#' @param db #database of species characteristics
#' @param BigCriteria #big criteria to which the criteria belongs
#' @param side #side to which the criteria belongs (one of "responsetrait", "effecttrait")
#' @param yesindicator #value used in the database to indicate that the species fits this criteria (by default, "yes", "oui", "x", "T", "TRUE")
#'
#' @return a (long) data.frame of the initial database (I know it is not most efficient) with added columns "value" (with the value of the score for the criteria), "BigCriteria" and "side" ; rbinded for each criteria
#' @export
#'
#' @examples
default_computecrit<-function(criteria,type,inputs, db, BigCriteria, side, weight = as.integer(1), yesindicator=c("yes", "oui", "x", "X", "T", "TRUE", "VRAI", "1")){
  message("computing value for criteria ", criteria , " of type ", type, " based on iputs ", paste(inputs, collapse=","))
  if (type %in% c("checkboxGroupInput", "radioButtons")){ #for checkboxgroups and radiobuttons, criteria is the title of the group
    #extract the relevant inputs to see which were chosen
    chosen<-unlist(inputs[gsub(pattern="[0-9]+", replacement="", x=names(inputs))==criteria])
    services<-strsplit( #services is a list (one for each species) of vectors of keywords (or numbers but not used in this case)
      gsub(pattern="(", replacement=", ", fixed=TRUE, x=gsub(pattern=")", replacement="", fixed=TRUE, 
                                                             x=db[,intersect(names(db), c(criteria, chosen))])) #replace first ( by comma and remove )
      , split="\\s*[,;]\\s*") #commas or semicolon followed by 0 or more whitespaces (and also remove trailing blanks)
    
    if(criteria %in% names(db)){ #one column criteria, with content equal to possible choices, or comma or semicolon separated keywords
      db$value<-sapply(services, function(x) length(intersect(x, chosen)))
      db$value<-db$value/length(chosen)
    } else { # several columns, one for each possible choice
      if (all(sapply(make.names(chosen), function(ch) ch %in% names(db)))) { #all the chosen are among the column names
        db$value<-0
        for (ch in chosen) {
          if(class(db[,ch])=="numeric") {#the database contains scores
            scores<-db[,ch]
            scores[is.na(scores)]<-0
            db$value<-db$value+scores
          } else { #the database contains keywords
            #count the number of characteristics %in% inputs to get the score
            # Function to count the number of matching keywords
            count_matching_keywords <- function(keyword_list) {
              sum(keyword_list %in% chosen)
            }
            # Apply the function to each species
            nbmatches <- sapply(services, count_matching_keywords)
            #then divide by the number of possibilities to obtain score between 0 and 1
            db$value<-nbmatches/length(chosen)
          }
        } #end for each chosen
      } else {
        print(paste("could not guess which variable to use for", chosen)) ; db$value<-NA
      }}
  } else 
    if (type=="selectInput") {
      
      chosen<-inputs[criteria]
      if(criteria %in% names(db)){ #one column criteria, with content equal to possible choices
        db$value<-as.numeric(db[,criteria]==chosen) 
      } else {
        if (sum(grepl(pattern=make.names(chosen), x=names(db), fixed=TRUE))==1) { #the chosen is among the column names
          db$value<-as.numeric(db[,grepl(pattern=chosen, x=names(db))]) 
        } else {
          print(paste(chosen, "potentially corresponds to severalcolumns:", paste(names(db)[grepl(pattern=make.names(chosen), x=names(db), fixed=TRUE)], collapse=","))) ; db$value<-NA
        }}
      
    } else 
      if (type=="checkboxInput") {
        if(class(db[,criteria])=="numeric") { #the database already contains scores
          db$value<- db[,criteria]
        } else db$value<- as.numeric(db[,criteria] %in% yesindicator)
      } else 
        if (type=="sliderInput") {
          chosen<-as.numeric(inputs[gsub(pattern="[0-9]+", replacement="", x=names(inputs))==criteria])
          
          #chosen<-as.numeric(inputs[grepl(pattern=criteria, x=names(inputs))])
          #I don't know why, sometimes inputs are duplicated...
          chosen<-unique(as.numeric(chosen))
          chosen<-chosen[!is.na(chosen)]
          
          
          if(any(grepl(pattern=")-(", fixed=TRUE, x=db[,criteria]))) { #db gives a range of values
            splits<-strsplit(db[,criteria], split=")-(", fixed=TRUE)
            mini<-numeric(length(splits))
            mini[sapply(splits, length)>0]<-as.numeric(gsub(pattern="(", fixed=TRUE, replacement="", x=sapply(splits[sapply(splits, length)>0], "[[", 1)))
            maxi<-mini
            maxi[sapply(splits, length)>1]<-as.numeric(gsub(pattern=")", fixed=TRUE, replacement="", x=sapply(splits[sapply(splits, length)>1], "[[", 2)))
            if(length(chosen)==2) { #sliderinput with a range and db with a range: percentage of desired within treetrait
              overlap <- function(A, B) {
                shared <- pmax(0, min(A[2], B[2]) - max(A[1], B[1]))
                max(shared / c(diff(A), diff(B)))
              }
              db$value<-0
              for(i in 1:nrow(db)) db$value[i]<-overlap(chosen, c(mini[i], maxi[i]))
            } else { #sliderinput with just one chosen value: 1 if within treerange, 0 otherwise
              db$value<-as.numeric(mini<=chosen & maxi>=chosen)
            }
          } else { #db gives only one value
            treetraits<-as.numeric(db[,criteria])
            if(length(chosen)==2) { #sliderinput with a range: 0 if the species is outside, 1 if it is inside
              db$value<-as.numeric(treetraits>=min(chosen) & treetraits<=max(chosen))
            } else { #sliderinput with just one value: 1 when criteria = chosen, 0 when it is the farthest away among all species
              rangevalues<-range(treetraits, na.rm=TRUE)
              db$value<-pmax(0, 1-abs((treetraits-chosen)/(rangevalues[2]-rangevalues[1])))
            }
          }
          
          #chosen<-as.numeric(chosen[!duplicated(names(chosen))])
          
          
        } else 
          if (type=="numericInput") {
            chosen<-inputs[criteria]
            if(any(grepl(pattern=")-(", fixed=TRUE, x=db[,criteria]))) { #db gives a range of values
              splits<-strsplit(db[,criteria], split=")-(", fixed=TRUE)
              mini<-as.numeric(gsub(pattern="(", fixed=TRUE, replacement="", x=sapply(splits, "[[", 1)))
              maxi<-mini
              maxi[sapply(splits, length)>1]<-as.numeric(gsub(pattern=")", fixed=TRUE, replacement="", x=sapply(splits[sapply(splits, length)>1], "[[", 2)))
              db$value<-as.numeric(mini<=chosen & maxi>=chosen)
            } else { #unique value
              rangevalues<-range(as.numeric(db[,criteria]))
              db$value<-1-abs((as.numeric(db[,criteria])-as.numeric(inputs[criteria]))/(rangevalues[2]-rangevalues[1]))
            }
            
          }
  #message("values= ", paste(db$value, collapse=","))
  db$criteria<-criteria
  db$BigCriteria<-BigCriteria
  db$side<-side
  return(db)
}
