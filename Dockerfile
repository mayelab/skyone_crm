FROM rocker/geospatial

MAINTAINER MayeLab "gmayeregger@gmail.com"

# system libraries of general use 
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    && rm -rf /var/lib/apt/lists/*

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'xfun', 'remotes'), repos='https://cloud.r-project.org/')"

# install dependencies of the euler app  
RUN R -e "remotes::install_github('johndharrison/slickR')"
RUN R -e "install.packages(c('shinydashboard', 'tidyverse', 'readr', 'rvest', 'leaflet', 'stringr', 'DT', 'data.table', 'leaflet.extras', 'sp', 'sf', 'editData', 'shinyalert', 'shinyWidgets', 'hereR', 'shinybusy', 'RMySQL', 'lwgeom', 'DBI', 'shinyjs', 'digest', 'lubridate', 'stringi', 'blastula', 'tcltk', 'httr', 'jsonlite', 'rlist', 'vembedr', 'rgeos', 'aws.s3', 'base64enc', 'magick', 'colorspace', 'callr', 'fontawesome', 'toastui', 'tinytex', 'glue', 'sortable', 'metathis', 'qrcode', 'rclipboard', 'zip', 'htmltools', 'textutils'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/crm
COPY crm /root/crm

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/crm', host='0.0.0.0', port=3838)"]
