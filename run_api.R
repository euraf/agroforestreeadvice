# run_api.R
library(plumber)

# Set port from environment variable (Render provides this)
port <- as.numeric(Sys.getenv("PORT", 8000))
host <- "0.0.0.0"

cat("Starting API server on", host, ":", port, "\n")

# Create and run the API
pr <- plumb("api.R")
pr$run(host = host, port = port)
