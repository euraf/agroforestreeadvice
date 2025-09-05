FROM r-base:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
libsodium-dev \
&& rm -rf /var/lib/apt/lists/*
  
  WORKDIR /app

# Copy all files
COPY . .

# Install R packages
RUN Rscript -e "install.packages(c('plumber', 'jsonlite', 'callr', 'datasets', 'httr', 'dplyr'), repos='https://cran.rstudio.com/')"

# Expose port
EXPOSE 8000

# Start the application
CMD ["Rscript", "run_api.R"]
