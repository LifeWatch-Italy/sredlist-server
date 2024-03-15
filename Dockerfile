FROM rocker/geospatial
LABEL version="1.0"
#R version: 4.2.0
RUN apt-get update && apt-get install -y libsecret-1-0
RUN R -e "install.packages('remotes')"
RUN R -e 'remotes::install_github("prioritizr/aoh")'
RUN R -e 'remotes::install_github("jeffreyevans/spatialEco")'
RUN R -e "install.packages(c('plumber', 'config', 'tictoc', 'logger','glue', 'urltools', 'Rook', 'ggplot2', 'gridExtra' , 'plyr', 'dplyr', 'tidyr', 'ggalluvial', 'ncdf4', 'rredlist', 'tools', 'rgbif', 'maps', 'countrycode', 'sf', 'rnaturalearthdata', 'raster', 'exactextractr', 'rasterVis', 'readr', 'R.utils', 'CoordinateCleaner', 'terra', 'plotly', 'gdalUtilities', 'smoothr', 'adehabitatHR', 'robis', 'future', 'future.callr', 'promises', 'scales', 'plotly', 'maptools', 'mapview', 'leaflet', 'leaflet.extras', 'htmltools', 'leafem', 'protolite', 'cowplot', 'alphahull', 'taxize', 'TAF', 'knitr', 'rmarkdown', 'flextable', 'png', 'grid', 'magick', 'leaflet.esri'))"

#FIX: https://github.com/ropensci/CoordinateCleaner/issues/63
#RUN R -e "devtools::install_github('ropensci/CoordinateCleaner')"

# copy everything from the current directory into the container
#COPY sredlist-server-develop /
COPY / /

# open port 8000 to traffic
EXPOSE 8000

ENTRYPOINT ["Rscript", "server.R"]
