# api.R
library(plumber)
library(jsonlite)

# Source your existing code
source("global.R")

#* @apiTitle AgroforestrTreeAdvice API
#* @apiDescription API for computing tree suitability with dynamic parameters

#* Get tree suitability with dynamic parameters
#* @param req The request object containing all query parameters
#* @get /suitability
function(req) {
  # Parse all query parameters dynamically
  query_params <- req$argsQuery
  
  if (is.null(query_params$model)) {
    return(list(status = "error", message = "model parameter is required"))
  }
  
  tryCatch({
    desiredmodel <- query_params$model
    allotherparameters <- setdiff(names(query_params), c("in_language", "model"))
    queryinputs <- unlist(query_params[allotherparameters])
    
    resultdf <- do.call(paste("compute_suitability_", desiredmodel, sep=""), list(
      inputsdata = queryinputs,
      database = get(paste("data", desiredmodel, sep="")),
      interface = get(paste("interface", desiredmodel, sep=""))
    ))
    
    list(status = "success", data = resultdf)
  }, error = function(e) {
    list(status = "error", message = as.character(e))
  })
}

#* Get available models and their parameters
#* @get /models
function() {
  # Return information about available models and their parameters
  tryCatch({
    models <- ls(pattern = "^interface", envir = .GlobalEnv)
    model_names <- gsub("^interface", "", models)
    
    model_info <- list()
    for (model in model_names) {
      interface_obj <- get(paste("interface", model, sep=""))
      # Extract parameter names from your interface object
      # This depends on how your interface objects are structured
      model_info[[model]] <- list(
        name = model,
        parameters = unique(interface_obj[interface_obj$side%in% c("responsetrait", "effecttrait"),"criteria"]) # Adjust based on your structure
      )
    }
    
    list(status = "success", models = model_info)
  }, error = function(e) {
    list(status = "error", message = as.character(e))
  })
}

#* Health check endpoint
#* @get /health
function() {
  list(status = "ok", timestamp = Sys.time())
}
