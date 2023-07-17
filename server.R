### Only for Victor
setwd("C:/Users/Victor/Documents/sRedList/Platform/InProgress/sredlist-server-develop")


### Set the asynchronous coding
options(future.globals.maxSize= 2000*1024^2) # Max 2 GB of RAM per session
library(promises) ; library(future) ; library(future.callr)
future::plan(future.callr::callr) # This plan uses transient parallel workers (better to have spread tasks among workers and release memory)
#future::plan("multisession") # This plan uses persistent parallel workers (quicker but some RAM accumulation)


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
library(Rook) ; library(ggplot2) ; library(gridExtra) ; library(raster) ; library(plyr) ;library(dplyr) ; library(tidyr) ; library(sf) ; library(rgdal) ;  library(ncdf4) ; library(ggalluvial) ; library(rredlist) ;  library(tools) # nolint
library(TAF)

### GBIF mapping
library(rgbif) ; library(CoordinateCleaner) ; library(rCAT) ; library(maps) ; library(countrycode); library(rnaturalearthdata); library(robis) # nolint
library(plotly) ; library(mapview) ; library(leaflet) ; library(leaflet.extras) ; library(htmltools) ; library(leafem)
library(adehabitatHR) ; library(smoothr) ; library(spatialEco) ; library(alphahull)

### AOH analyses
library(exactextractr)
library(rasterVis)
library(readr)
library(aoh)
library(terra)
library(scales)
library(taxize)

### RMarkDown
library(knitr) ; library(rmarkdown) ; library(flextable) ; library(png) ; library(grid) ; library(magick)


#####################################
### CHARGE CODE FOR THE FUNCTIONS ###
#####################################

### Config
config <- config::get()
#config <- config::get(file="C:/Users/Victor/Documents/sRedList/Platform/InProgress/config_Victor.yml")

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
source("sRLfun_OptionalAnalyses.R")


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
speciesRL <- readRDS("Species/species-all-page.rds") # nolint

# Load Map countries
distCountries_mapping <- st_read("Species/Map countries/Red_List_countries_msSimplif0.05_MOLL.shp")  ; st_crs(distCountries_mapping)<-CRSMOLL # Use to create maps in GBIF step 3
distCountries_WGS <- st_read("Species/Map countries/Red_List_countries_msSimplif0.05_WGS_combined.shp") # Used to filter GBIF data in GBIF step 2
distCountries<-st_read("Species/Map countries/Red_List_countries_msSimplif0.005_MOLL.shp") ; st_crs(distCountries)<-CRSMOLL # Used to map countries in the background
distCountries_light <- distCountries %>% st_simplify(., dTolerance=0.01) %>% st_transform(., crs=st_crs(distCountries_WGS))
coo_raw<-read_sf("Species/Map countries/Red_List_countries_msSimplif_coo_0.001.shp") # Used to map COO
eez_raw<-read_sf("Species/Map countries/Red_List_EEZ_Simplif_coo_0.001.shp") # Used to map COO


# Load Crosswalk CSV and Density CSV
density_data<-read.csv("Species/Density.table.csv", sep=",") %>% subset(., .$Density>0)
crosswalk <- read.table("Species/Crosswalk_CCI_IUCN.txt", header=T)
crosswalkLARGE<-read.csv("Species/Lumbierres_large_crosswalk.csv")
GL_file<-read.csv("Species/Generation_length_sRedList.csv", sep=",")
if(config$crosswalk == "Santini"){crosswalk_to_use<- crosswalk[is.na(crosswalk$esa_code)==F, c("iucn_code", "esa_code")] ; names(crosswalk_to_use)<-c("code", "value")}
if(config$crosswalk == "Lumbierres"){crosswalk_to_use<- read.table("Species/Crosswalk_CCI_IUCN_Lumbierres.txt", dec="/", header=T) ; crosswalk_to_use$code<-as.character(crosswalk_to_use$code)}

# Load the empty output log
output<-read.csv("Species/Output log empty.csv")

# Load Hydrobasins map
hydro_raw<-st_read(config$hydrobasins_path) %>% st_transform(., crs=CRSMOLL)



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

#bytes = 40MB
options_plumber(maxRequestSize = 40000000)
pr %>% pr_hook("exit", function() {
  print("Bye bye from sRedList!")
}) %>% pr_run(port = 8000, host = "0.0.0.0") # nolint


