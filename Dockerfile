# Base image
FROM rocker/shiny:4.4.1

# System libraries
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && apt-get clean

# R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'bs4Dash', \
    'highcharter', \
    'dplyr', \
    'tidyr', \
    'readr', \
    'purrr', \
    'stringr', \
    'lubridate', \
    'DT', \
    'visNetwork', \
    'fresh', \
    'RColorBrewer', \
    'shinyjs', \
    'reactable', \
    'depmixS4', \
    'sparkline', \
    'aws.s3' \
    ), repos='https://cran.rstudio.com/')"

# App directory
COPY . /srv/shiny-server/

# Port exposure
EXPOSE 3838

# Launch command
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838)"]