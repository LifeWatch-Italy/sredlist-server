
##################################################
### TEMP: PARAMETERS TO INCLUDE AS INTERACTIVE ###
##################################################

year_GBIF<-1900
keepyearNA_GBIF<-T
uncertainty_GBIF<-100 # In KILOmeters
cleaningpar_GBIF<-c("capitals", "centroids", "equal", "gbif", "institutions", "zeros", "seas")
GBIF_xmin<- -180 ; GBIF_xmax<-180 ; GBIF_ymin<- -90 ; GBIF_ymax<-90

GBIF_BUFF_km2<-10
GBIF_crop<-"Land" # Could be Land or Sea or could be empty

GL_species<-2 # This is the generation length parameters




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
library(Rook) ; library(ggplot2) ; library(raster) ; library(plyr) ;library(dplyr) ; library(sf) ; library(rgdal) ;  library(ncdf4) ; library(ggalluvial) ; library(rredlist) ;  library(tools) # nolint

### GBIF mapping
library(rgbif) ; library(CoordinateCleaner) ; library(rCAT) ; library(maps) ; library(countrycode); library(rnaturalearthdata); # nolint
library(plotly)

### AOH analyses
library(exactextractr)
library(rasterVis)
library(readr)
library(aoh)
library(terra)


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

# Load Crosswalk CSV and Density CSV
density<-read.csv("Species/Density.table.csv", sep=",") ; density<-select(density, c("Species", "Density"))
crosswalk <- read.csv("Species/Crosswalk_CCI_IUCN.csv")






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
