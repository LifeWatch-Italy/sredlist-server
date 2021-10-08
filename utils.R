firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) # nolint

# Generated distribution from File by scientific_name of species
read_distribution <- function(scientific_name) {
  speciesPath <- paste0("Distributions/", scientific_name) # nolint
  files <- base::list.files(path = speciesPath, pattern = "\\.shp$")

  if (length(files) == 0) {
    not_found("Shapefile of the species does not exist!") # nolint
  } else {
    distributionPath <- paste0(speciesPath, "/", files[1]) # nolint
  }

  #distributions<-st_read("Distributions/Chameleons/CHAMELEONS.shp") # nolint
  print(distributionPath)
  distributions <- st_read(distributionPath)
  names(distributions)[which(names(distributions) %in% c("SCINAME", "binomial", "BINOMIAL"))] <- "binomial" # nolint
  names(distributions)[which(names(distributions) %in% c("PRESENC", "PRESENCE", "presence"))] <- "presence" # nolint
  names(distributions)[which(names(distributions) %in% c("ORIGIN", "origin"))] <- "origin" # nolint
  names(distributions)[which(names(distributions) %in% c("SEASONA", "SEASONAL", "seasonal"))] <- "seasonal" # nolint

  return(distributions)
}