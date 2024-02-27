#do not launch with the Run App button, but run the last line (  runApp(shinyApp(ui, server, uiPattern = ".*"))  )

library(shiny)
library(jsonlite)
library(callr)
library(datasets)

#setwd("E:\\Mes_documents\\a_ABSys\\DIGITAF\\WP2farmers\\task2.2_treecropperformance\\DigitAFtreeAdvice\\agroforestreeadvice avecHTTP")
#source("E:\\Mes_documents\\a_ABSys\\DIGITAF\\WP2farmers\\task2.2_treecropperformance\\DigitAFtreeAdvice\\agroforestreeadvice avecHTTP\\global.R")
source("global.R")

#icicicci todo: in global, instead of reading the models from local file, read them directly from github:
#readr::read_csv("url_obtained_from_clicking_raw_view_in_github")


ui <- function(req) {
  # The `req` object is a Rook environment
  # See https://github.com/jeffreyhorner/Rook#the-environment
  if (identical(req$REQUEST_METHOD, "GET")) {

    source(file.path("R", "Tabinterface.R")) #not local because we want the function
    source("ui_visual.R", local = TRUE)

  } else if (identical(req$REQUEST_METHOD, "POST")) {
    # Handle the POST
    query_params <- parseQueryString(req$QUERY_STRING)
    print(query_params)
    body_bytes <- req$rook.input$read(-1) #not used for now, maybe when there are more parameters to pass (or complex structured parameters)
    if(req$PATH_INFO == "/API"){ #
      json_recu <- jsonlite::fromJSON(rawToChar(body_bytes))
      desiredmodel <- query_params$model
      allotherparameters<-setdiff(names(query_params), c("in_language", "model"))
      queryinputs<-unlist(query_params[allotherparameters]) #icicicic todo : check that all necessary inputs are provided before sending to the suitability function
      print(paste("computing suitability of model", desiredmodel))
      resultdf<-do.call(paste("compute_suitability_", desiredmodel, sep=""), list(
        inputsdata=queryinputs,
        database=get(paste("data", desiredmodel, sep="")),
        interface=get(paste("interface", desiredmodel, sep="")))
      )
      #resultdf <- as.data.frame(query_params)
      httpResponse(
        status = 200L,
        content_type = "application/json",
        content = jsonlite::toJSON(resultdf, dataframe="rows")
      )
    } else {
      httpResponse(
        status = 200L,
        content_type = "application/json",
        content = '{"status": "error: POST should only be sent to the API (agroforestryadvice/API)"}'
      )
    }
  }
}
attr(ui, "http_methods_supported") <- c("GET", "POST")

#server <- function(input, output, session) {}
source("server_logic.R", local = TRUE)


app<-shinyApp(ui, server, uiPattern = ".*")

