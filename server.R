library(plumber)
# logging
library(logger)
# Ensure glue is a specific dependency so it's avaible for logger
library(glue)

library(urltools)

library(Rook) ; library(ggplot2) ; library(raster) ; library(plyr) ;library(dplyr) ; library(sf) ; library(rgdal) ;  library(ncdf4) ; library(ggalluvial) ; library(rredlist) ;  library(tools) # nolint

#GBIF
library(rgbif) ; library(CoordinateCleaner) ; library(rCAT) ; library(maps) ; library(countrycode); library(rnaturalearthdata); # nolint

#AOH
library(exactextractr)

# Config
config <- config::get()

# Errors
source("error-management.R")

# Utils
source("utils.R")


# Specify how logs are written
if (!fs::dir_exists(config$log_dir)) fs::dir_create(config$log_dir)
log_appender(appender_tee(tempfile("plumber_", config$log_dir, ".log")))


convert_empty <- function(string) {
  if (string == "") {
    "-"
  } else {
    string
  }
}


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
pr %>% pr_run(port = 8000, host = "0.0.0.0")