FROM rocker/geospatial
LABEL version="1.0"
#R version: 4.2.0
RUN apt-get update && apt-get install -y libsecret-1-0
RUN R -e "install.packages('remotes')"
RUN R -e 'remotes::install_github("prioritizr/aoh", auth_token="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")'
RUN R -e "install.packages(c('plumber', 'config', 'tictoc', 'logger','glue', 'urltools', 'Rook', 'ggplot2' , 'plyr', 'dplyr', 'ggalluvial', 'rredlist', 'tools', 'rgbif', 'rCAT', 'maps', 'countrycode', 'rnaturalearthdata', 'exactextractr', 'rasterVis', 'readr', 'R.utils', 'CoordinateCleaner', 'terra', 'plotly'))"

#FIX: https://github.com/ropensci/CoordinateCleaner/issues/63
#RUN R -e "devtools::install_github('ropensci/CoordinateCleaner')"

# copy everything from the current directory into the container
#COPY sredlist-server-develop /
COPY / /

# open port 8000 to traffic
EXPOSE 8000

ENTRYPOINT ["Rscript", "server.R"]