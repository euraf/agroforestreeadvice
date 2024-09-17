
dataCzech<-read.table("models/dataCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceCzech<-read.table("models/interfaceCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)


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

fin = dfczechinfo(interface = interfaceCzech, data = dataCzech)
