# sRedList-API
Plumber R application:
To run the server you must:
1)Install the following libraries in R:
library(plumber)
library(logger)
library(glue)
library(urltools)
library(Rook) ; library(ggplot2) ; library(raster) ; library(dplyr) ; library(sf) ; library(rgdal) ; library(plyr) ; library(ncdf4) ; library(ggalluvial) ; library(rredlist)
2)run the script inside the R env:
source(".server.R")

Run the plumber's API on http://127.0.0.1:8000
Running swagger Docs on http://127.0.0.1:8000/__docs__/
