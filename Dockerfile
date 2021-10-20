FROM rocker/geospatial
LABEL version="1.0"

RUN R -e "install.packages(c('plumber', 'config', 'tictoc', 'logger','glue', 'urltools', 'Rook', 'ggplot2' , 'plyr', 'dplyr', 'ggalluvial', 'rredlist', 'tools', 'rgbif', 'CoordinateCleaner', 'rCAT', 'maps', 'countrycode', 'rnaturalearthdata', 'exactextractr', 'rasterVis'))"

# copy everything from the current directory into the container
COPY / /

# open port 8000 to traffic
EXPOSE 8000

ENTRYPOINT ["Rscript", "server.R"]

#CMD ["/server.R"]