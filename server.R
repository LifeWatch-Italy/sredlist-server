# Set working directory (Victor path if we are on his laptop, LifeWatch path otherwise)
setwd(dir=ifelse(file.exists("C:/Users/Victor"),"C:/Users/Victor/Documents/sRedList/Platform/InProgress/sredlist-server-develop", "/media/docker/sRedList/sredlist-server"))


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

### Ensure glue is a specific dependency so it's available for logger
library(glue)
library(urltools)
library(R.utils)
library(Rook) ; library(ggplot2) ; library(gridExtra) ; library(raster) ; library(plyr) ;library(dplyr) ; library(tidyr) ; library(sf) ; library(ncdf4) ; library(ggalluvial) ; library(rredlist) ;  library(tools) # nolint
library(TAF)

### GBIF mapping
library(rgbif) ; library(CoordinateCleaner) ; library(maps) ; library(countrycode); library(rnaturalearthdata); library(robis) # nolint
library(plotly) ; library(mapview) ; library(leaflet) ; library(leaflet.extras) ; library(htmltools) ; library(leafem) ; library(leaflet.esri)
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

### Config (if on Victor's computer, load config_Victor, otherwise load config.yml)
config <- config::get(file="../config.yml")

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
coo_raw<-read_sf("Species/Map countries/Red_List_countries_msSimplif_coo_0.001.shp") ; names(coo_raw)<-c("SIS_name0", "SIS_name1", "lookup", "lookup_SIS0", "geometry") ; coo_raw$lookup_SIS0[coo_raw$SIS_name0=="Namibia"]<-"NA" # Used to map COO; Namibia should be "NA" and not NA
eez_raw<-read_sf("Species/Map countries/Red_List_EEZ_Simplif_coo_0.001.shp") ; names(eez_raw)<-c("SIS_name0", "SIS_name1", "lookup", "lookup_SIS0", "geometry") ; eez_raw$lookup_SIS0[eez_raw$SIS_name0=="Namibia"]<-"NA"  ; eez_raw$lookup[eez_raw$SIS_name0=="Namibia"]<-"NA" # Used to map COO
distCountries_NRL <- st_read("Species/Map countries/Countries_NRL_landmarCombined_msSimplif_0.05.shp") ;  names(distCountries_NRL)<-c("SIS_name0", "SIS_name1", "N", "geometry") # Use to crop countries in 1a and 1b for NRL
distCountries_mapping <- st_read("Species/Map countries/Red_List_countries_msSimplif0.05_MOLL.shp")  ; st_crs(distCountries_mapping)<-CRSMOLL # Use to create maps in GBIF step 3
distCountries_WGS <- st_read("Species/Map countries/Red_List_countries_msSimplif0.05_WGS.shp") # Used to filter GBIF data in GBIF step 2
distCountries<-st_read("Species/Map countries/Red_List_countries_msSimplif0.005_MOLL.shp") ; st_crs(distCountries)<-CRSMOLL # Used to map countries in the background
distCountries_light <- distCountries %>% st_simplify(., dTolerance=0.01) %>% st_transform(., crs=st_crs(distCountries_WGS))
realms_raw<-realms_raw<-st_read("Species/Map countries/RL_Realms.shp")
realms_mcp<-st_union(realms_raw) %>% st_as_sf(.) %>% st_transform(., CRSMOLL) %>% st_convex_hull(.) %>% st_buffer(., 1000)

# Load Crosswalk CSV and Density CSV
density_data<-read.csv("Species/Density.table.csv", sep=",") %>% subset(., .$Density>0)
crosswalk <- read.table("Species/Crosswalk_CCI_IUCN.txt", header=T)
crosswalkLARGE<-read.csv("Species/Lumbierres_large_crosswalk.csv")
GL_file<-read.csv("Species/Generation_length_sRedList.csv", sep=",")
if(config$crosswalk == "Santini"){crosswalk_to_use<- crosswalk[is.na(crosswalk$esa_code)==F, c("iucn_code", "esa_code")] ; names(crosswalk_to_use)<-c("code", "value")}
if(config$crosswalk == "Lumbierres"){crosswalk_to_use<- read.table("Species/Crosswalk_CCI_IUCN_Lumbierres.txt", dec="/", header=T) ; crosswalk_to_use$code<-as.character(crosswalk_to_use$code)}

# Load the empty output log
output<-read.csv("Species/Output log empty.csv")

# If output tracker folder does not exist, I create it
if(file.exists("Species/Stored_outputs") == FALSE){dir.create("Species/Stored_outputs")}



# Load Hydrobasins map
hydro_raw<-st_read(config$hydrobasins_path) ; st_crs(hydro_raw)<-CRSMOLL



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
options_plumber(maxRequestSize = 100*1000000)
pr %>% pr_hook("exit", function() {
  print("Bye bye from sRedList!")
}) %>% pr_run(port = 8000, host = "0.0.0.0") # nolint


