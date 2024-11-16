# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# Maintainer information
LABEL maintainer="Mwangi George <mwangigeorge648@gmail.com>"

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Create and set working directory
RUN mkdir -p /fp_quantification
WORKDIR fp_quantification

# Copy R dependencies first to cache them
COPY requirements.R ./requirements.R

# Install R packages
RUN Rscript /fp_quantification/requirements.R

# Copy the rest of the needed files
COPY global.R ./global.R
COPY app.R ./app.R
COPY data/ ./data/
COPY R/ ./R/
COPY utils/ ./utils/

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/fp_quantification', host = '0.0.0.0', port = 3838)"]
