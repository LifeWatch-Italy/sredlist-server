### Only for Victor
# setwd("C:/Victor_local/Platform/InProgress/sredlist-server-develop")




### Set the asynchronous coding
library(promises) ; library(future)
future::plan("multisession")


######################
### LOAD LIBRARIES ###
######################

library(plumber)

### logging
library(logger)

### Ensure glue is a specific dependency so it's avaible for logger
library(glue)
library(urltools)
library(R.utils)
library(Rook) ; library(ggplot2) ; library(gridExtra) ; library(raster) ; library(plyr) ;library(dplyr) ; library(sf) ; library(rgdal) ;  library(ncdf4) ; library(ggalluvial) ; library(rredlist) ;  library(tools) # nolint

### GBIF mapping
library(rgbif) ; library(CoordinateCleaner) ; library(rCAT) ; library(maps) ; library(countrycode); library(rnaturalearthdata); library(robis) # nolint
library(plotly)
library(adehabitatHR) ; library(smoothr)

### AOH analyses
library(exactextractr)
library(rasterVis)
library(readr)
library(aoh)
library(terra)
library(scales)


#####################################
### CHARGE CODE FOR THE FUNCTIONS ###
#####################################

### Config
config <- config::get()

# The CRS we use in all the platform
CRSMOLL<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Extensions File Distribution
extensions <- list("shp", "shx", "prj", "dbf", "cpg")

# Avoid problems with spherical geometries
sf::sf_use_s2(FALSE)


### Script to deal with errors on the platform
source("error-management.R")

### Load R code functions 
source("sRLfun_FirstPage.R")
source("sRLfun_Mapping.R")
source("sRLfun_AOH.R")
source("sRLfun_Outputs.R")



### Specify how logs are written
if (!fs::dir_exists(config$log_dir)) fs::dir_create(config$log_dir)
log_appender(appender_tee(tempfile("plumber_", config$log_dir, ".log")))


convert_empty <- function(string) {
  if (string == "") {
    "-"
  } else {
    string
  }
}



### Charge the files that will remain unaltered

# Charge species information from RL - updated in 27-09-2021
speciesRL <- read.csv("Species/species-all-page.csv") # nolint

# Load Map countries
distCountries_notsimplif <- st_read("Species/Map countries/Admin_dissolved_by_country_MOLLWEIDE.shp")  ; st_crs(distCountries_notsimplif)<-CRSMOLL
distCountries_WGS <- st_read("Species/Map countries/Admin_dissolved_by_country_Simplif_0.005.shp")
distCountries<-st_read("Species/Map countries/Admin_dissolved_by_country_Simplif_0.005MOLLWEIDE.shp") ; st_crs(distCountries)<-CRSMOLL

# Load ESA land cover data from 2020 and altitude data
alt_raw <- rast(config$alt_raster_path) ; crs(alt_raw)<-CRSMOLL  # I ensure the CRS is correctly assigned (it was saved as a CRSMOLL raster)
cci2 <- rast(config$cci2_raster_path); crs(cci2)<-CRSMOLL
grid22<-rast("resources/EmptyGrid2x2/Empty.grid.2x2.Mollweide.tif")
CCI_large<-stackOpen(paste0(config$cciStack2_path, "/2020/CCI_Stack_Agg30_Year2020.stk"))
alt_large<-raster(paste0(config$cciStack2_path, "/ElevationAgg30.tif"))

# Load Elevation at 10x10km scale to extract elevation preference for large range species, one aggregated with min value, one aggregated with max value (to be sure to be conservative)
Alt1010_min<-raster(paste0(config$cciStack2_path, "/ElevationAgg30_MINIMUM.tif"))
Alt1010_max<-raster(paste0(config$cciStack2_path, "/ElevationAgg30_MAXIMUM.tif"))


# Load Crosswalk CSV and Density CSV
density<-read.csv("Species/Density.table.csv", sep=",")
crosswalk <- read.table("Species/Crosswalk_CCI_IUCN.txt", header=T)
crosswalkLARGE<-read.csv("Species/Lumbierres_large_crosswalk.csv")
GL_file<-read.csv("Species/Generation_length_sRedList.csv", sep=",")
if(config$crosswalk == "Santini"){crosswalk_to_use<- crosswalk[is.na(crosswalk$esa_code)==F, c("iucn_code", "esa_code")] ; names(crosswalk_to_use)<-c("code", "value")}
if(config$crosswalk == "Lumbierres"){crosswalk_to_use<- read.table("Species/Crosswalk_CCI_IUCN_Lumbierres.txt", dec="/", header=T) ; crosswalk_to_use$code<-as.character(crosswalk_to_use$code)}

# Load the empty output log
output<-read.csv("Species/Output log empty.csv")

# Load Hydrobasins map
#hydro_raw<-st_read(config$hydrobasins_path) %>% st_transform(., crs=CRSMOLL)



###########################
### PLUMBER APPLICATION ###
###########################

pr <- pr("API.R")
#pr <- pr()

redlist <- Plumber$new("redlist.R")
pr$mount("api/redlist", redlist)$setErrorHandler(error_handler)

sredlist <- Plumber$new("sredlist.R")
pr$mount("api/sredlist", sredlist)$setErrorHandler(error_handler)

pr$registerHooks(
  list(
    preroute = function() {
      # Start timer for log info
      tictoc::tic()
    },
    postroute = function(req, res) {
      end <- tictoc::toc(quiet = TRUE)
      # Log details about the request and the response
      # TODO: Sanitize log details - perhaps in convert_empty
      log_info('{convert_empty(req$REMOTE_ADDR)} "{convert_empty(req$HTTP_USER_AGENT)}" {convert_empty(req$HTTP_HOST)} {convert_empty(req$REQUEST_METHOD)} {convert_empty(req$PATH_INFO)} {convert_empty(res$status)} {round(end$toc - end$tic, digits = getOption("digits", 5))}') # nolint
    }
  )
)

#bytes = 10MB
options_plumber(maxRequestSize = 10000000)
pr %>% pr_hook("exit", function() {
  print("Bye bye from sRedList!")
}) %>% pr_run(port = 8000, host = "0.0.0.0") # nolint


