FROM rocker/geospatial
LABEL version="1.0"
#R version: 4.1.1
RUN R -e "install.packages(c('plumber', 'config', 'tictoc', 'logger','glue', 'urltools', 'Rook', 'ggplot2' , 'plyr', 'dplyr', 'ggalluvial', 'rredlist', 'tools', 'rgbif', 'rCAT', 'maps', 'countrycode', 'rnaturalearthdata', 'exactextractr', 'rasterVis', 'readr'))"

#FIX: https://github.com/ropensci/CoordinateCleaner/issues/63
RUN R -e "devtools::install_github('ropensci/CoordinateCleaner')"

# copy everything from the current directory into the container
#COPY sredlist-server-develop /
COPY / /
#COPY / /sredlist/

# copy data full raster into the container
#COPY data /data/

# open port 8000 to traffic
EXPOSE 8000

ENTRYPOINT ["Rscript", "server.R"]

#CMD ["/server.R"]
