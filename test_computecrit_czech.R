
compute_suitability_Czech<-function(inputsdata=NULL,database,interface,orderby="responsetrait",use_weights=TRUE){
  dbfinal<<-data.frame()
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

default_computecrit<-function(criteria,type,inputs, db, BigCriteria, side, weight = as.integer(1), yesindicator=c("yes", "oui", "x", "X", "T", "TRUE", "VRAI")){
  
  
  
  message("computing value for criteria ", criteria , " of type ", type, " based on iputs ", paste(inputs, collapse=","))
  print("####### get inputs[criteria]")
  print(inputs[criteria][1])
  if (type=="checkboxGroupInput"){ #for checkboxgroups, criteria is the title of the group
    #extract the relevant inputs to see which were chosen
    chosen<-unlist(inputs[gsub(pattern="[0-9]+", replacement="", x=names(inputs))==criteria])
    print("####### chosen")
    print(chosen)
    services<-strsplit( #services is a list (one for each species) of vectors of keywords (or numbers but not used in this case)
      gsub(pattern="(", replacement=", ", fixed=TRUE, x=gsub(pattern=")", replacement="", fixed=TRUE, 
                                                             x=db[,intersect(names(db), c(criteria, chosen))])) #replace first ( by comma and remove )
      , split="\\s*[,;]\\s*") #commas or semicolon followed by 0 or more whitespaces (and also remove trailing blanks)
   
    if(criteria %in% names(db)){ #one column criteria, with content equal to possible choices
      db$value<-as.numeric(db[,criteria] %in% chosen) 
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
   } else if (type=="selectInput") {
    
    chosen<-inputs[criteria]
    if(substr(chosen[1],start=1, stop=4) == "not ") # if the user selected "not " in the selectInput, then we select all species
    {
      print(paste0(chosen[0],": selected all"))
      db$value<-0
    }

    else if (criteria %in% names(db)){ #one column criteria, with content equal to possible choices
      db$value<-as.numeric(db[,criteria]==chosen) 
    } else {
      if (sum(grepl(pattern=make.names(chosen), x=names(db), fixed=TRUE))==1) { #the chosen is among the column names
        db$value<-as.numeric(db[,grepl(pattern=chosen, x=names(db))]) 
      } else {
        print(paste("could not guess which variable to use for", chosen)) ; db$value<-NA
      }}
    
  } else if (type=="checkboxInput") {
    db$value<- as.numeric(db[,criteria] %in% yesindicator)
  } else if (type=="sliderInput") {
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
    
    
  } else if (type=="numericInput") {
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

  if (!is.na(weight) && weight == as.integer(999)) {
    weight <- as.integer(1)
    print(paste("Weight is INFINITE for criteria:", criteria))
  }
  else {
      # Ensure weight is numeric
      weight <- as.numeric(weight)
      if (is.na(weight))  {
          weight <- as.integer(1)
          warning("Weight is not numeric and cannot be converted. Weight set to 1.")
        }
    }
  #message("values= ", paste(db$value, collapse=","))
  db$criteria<-criteria
  db$BigCriteria<-BigCriteria
  db$side<-side
  db$value<-db$value*weight
  return(db)
}


compute_suitability_Czech(inputsdata=inputsdata, database=database, interface=interface, orderby="responsetrait", use_weights=TRUE)

default_computecrit(criteria="subsidy", type="checkboxGroupInput", inputs=inputsdata, db=database, BigCriteria="subsidy", side="responsetrait", weight=1, yesindicator=c("yes", "oui", "x", "X", "T", "TRUE", "VRAI"))


interfaceCzech<-read.table("models/interfaceCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep=";", header=TRUE)
interfaceCzech<-interfaceCzech[!is.na(interfaceCzech$side),]
interfaceCzech[1:length(interfaceCzech)]<-lapply(interfaceCzech[1:length(interfaceCzech)], function(x) gsub(pattern=",", replacement=".", x=x))
interface <- interfaceCzech
database<-read.table("models/dataCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep=";", skipNul =TRUE, header=TRUE)
load("inputsdata.RData")
load("allinputs.RData")

default_computecrit<-function(criteria,type,inputs, db, BigCriteria, side, weight = as.integer(1), yesindicator=c("yes", "oui", "x", "X", "T", "TRUE", "VRAI")){
  criteria <- "subsidy"
    type <- "checkboxGroupInput"
    inputs <- allinputs
    db <- database
    BigCriteria <- "subsidy"
    side <- "responsetrait"
    weight <- 1
    yesindicator <- c("yes", "oui", "x", "X", "T", "TRUE", "VRAI")
  
  
  message("computing value for criteria ", criteria , " of type ", type, " based on iputs ", paste(inputs, collapse=","))
  print("####### get inputs[criteria]")
  print(inputs[criteria][1])
  if (type=="checkboxGroupInput"){ #for checkboxgroups, criteria is the title of the group
    #extract the relevant inputs to see which were chosen
    chosen<-unlist(inputs[gsub(pattern="[0-9]+", replacement="", x=names(inputs))==criteria])
    print("####### chosen")
    print(chosen)
    services<-strsplit( #services is a list (one for each species) of vectors of keywords (or numbers but not used in this case)
      gsub(pattern="(", replacement=", ", fixed=TRUE, x=gsub(pattern=")", replacement="", fixed=TRUE, 
                                                             x=db[,intersect(names(db), c(criteria, chosen))])) #replace first ( by comma and remove )
      , split="\\s*[,;]\\s*") #commas or semicolon followed by 0 or more whitespaces (and also remove trailing blanks)
   
    if(criteria %in% names(db)){ #one column criteria, with content equal to possible choices
      db$value<-as.numeric(db[,criteria] %in% chosen) 
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
   } else if (type=="selectInput") {
    
    chosen<-inputs[criteria]
    if(substr(chosen[1],start=1, stop=4) == "not ") # if the user selected "not " in the selectInput, then we select all species
    {
      print(paste0(chosen[0],": selected all"))
      db$value<-0
    }

    else if (criteria %in% names(db)){ #one column criteria, with content equal to possible choices
      db$value<-as.numeric(db[,criteria]==chosen) 
    } else {
      if (sum(grepl(pattern=make.names(chosen), x=names(db), fixed=TRUE))==1) { #the chosen is among the column names
        db$value<-as.numeric(db[,grepl(pattern=chosen, x=names(db))]) 
      } else {
        print(paste("could not guess which variable to use for", chosen)) ; db$value<-NA
      }}
    
  } else if (type=="checkboxInput") {
    db$value<- as.numeric(db[,criteria] %in% yesindicator)
  } else if (type=="sliderInput") {
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
    
    
  } else if (type=="numericInput") {
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

  if (!is.na(weight) && weight == as.integer(999)) {
    weight <- as.integer(1)
    print(paste("Weight is INFINITE for criteria:", criteria))
  }
  else {
      # Ensure weight is numeric
      weight <- as.numeric(weight)
      if (is.na(weight))  {
          weight <- as.integer(1)
          warning("Weight is not numeric and cannot be converted. Weight set to 1.")
        }
    }
  #message("values= ", paste(db$value, collapse=","))
  db$criteria<-criteria
  db$BigCriteria<-BigCriteria
  db$side<-side
  db$value<-db$value*weight
  return(db)
}