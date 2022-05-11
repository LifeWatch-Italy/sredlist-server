
# Generated distribution from File by scientific_name of species and folder path
read_distribution <- function(scientific_name, path) {
  speciesPath <- paste0(config$distribution_path, scientific_name, "/", path) # nolint
  files <- base::list.files(path = speciesPath, pattern = "\\.shp$")

  if (length(files) == 0) {
    not_found("Shapefile of the species does not exist!") # nolint
  } else {
    distributionPath <- paste0(speciesPath, "/", files[1]) # nolint
  }

  #distributions<-st_read("Distributions/Chameleons/CHAMELEONS.shp") # nolint
  print(distributionPath)
  distributions <- sf::st_read(distributionPath)
  names(distributions)[which(names(distributions) %in% c("SCINAME", "binomial", "BINOMIAL"))] <- "binomial" # nolint
  names(distributions)[which(names(distributions) %in% c("PRESENC", "PRESENCE", "presence"))] <- "presence" # nolint
  names(distributions)[which(names(distributions) %in% c("ORIGIN", "origin"))] <- "origin" # nolint
  names(distributions)[which(names(distributions) %in% c("SEASONA", "SEASONAL", "seasonal"))] <- "seasonal" # nolint

  return(distributions)
}

# read_map_countries <- function() {
#   countries <- st_read("Species/Map countries/Admin_dissolved_by.country.shp") # nolint
#   return(countries)
# }




