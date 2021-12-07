firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) # nolint

# Generated distribution from File by scientific_name of species and folder path
read_distribution <- function(scientific_name, path) {
  speciesPath <- paste0("Distributions/", scientific_name, "/", path) # nolint
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

read_map_countries <- function() {
  countries <- st_read("Species/Map countries/Admin_dissolved_by.country.shp") # nolint
  return(countries)
}

createDataGBIF <- function(scientific_name) { # nolint
    #Download data
    dat <- occ_search(scientificName = scientific_name, hasCoordinate = T)$data
    #print(dat)
    if (is.null(dat)) {
      not_found("GBIF occurrences of the species could not be found! Check whether the scientific name of the species has been typed correctly!") # nolint
    }
    #Select columns of interest
    dat <- dat %>%
    dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount, # nolint
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, datasetName)

    #print(head(as.data.frame(dat)))

    #Remove records with no spatial coordinates
    dat <- dat %>% filter(!is.na(decimalLongitude)) %>% filter(!is.na(decimalLatitude)) # nolint

    return(dat)
}

cleanDataGBIF <- function(data) { # nolint
  flags <- clean_coordinates(x = data,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions", # nolint
                                     "zeros", "countries"))
  return(flags)

}

# Save EOO distribution from GBIF procedure
saveEooDistribution <- function(scientific_name, EOO, gbif_data_number) {
  # Create a file path E.g: Distributions/Nile tilapia/Nile_tilapia_GBIF_20211207/ # nolint
  upload_folder_scientific_name <- R.utils::capitalize(paste0(stringr::str_replace(scientific_name, " ", "_"), format(Sys.time(), "_GBIF_%Y%m%d"))) # nolint
  # Create a file path
  filePath <- paste0("Distributions/", scientific_name, "/", upload_folder_scientific_name, "/") # nolint
  if (dir.exists(filePath)) {
    print("The directory exists")
  } else {
  # create the "my_new_folder
    dir.create(filePath)
  }
  path <- paste0(filePath, upload_folder_scientific_name, ".shp")
  st_write(EOO, path, append = FALSE)

  print("Write metadata file")
  text <- paste0(
  "A distribution has been stored for the species: ",
  scientific_name,
  ".\nIt was created with the GBIF procedure from the sRedList platform, based on ", # nolint
  gbif_data_number,
  " occurrence data downloaded from GBIF on the ",
  Sys.time(),
  " CET."
  )
  writeLines(text, paste0(filePath, upload_folder_scientific_name, ".txt"))
}