#Extensions File Distribution
extensions <- list("shp", "shx", "prj", "dbf", "cpg")

#* Upload Distribution species
#* @post species/<scientific_name>/distribution
#* @param req:file Distribution file (shp,.shx,.prj,.dbf,.cpg)
#* @param scientific_name:str Insert Species Name
#* @serializer unboxedJSON
#* @parser multi 
#* @tag sRedList
function(scientific_name, req) {
  scientific_name <- url_decode(scientific_name)

  # Required for multiple file uploads
  names(req)

  # Parses into a Rook multipart file type;needed for API conversions
  fileInfo <- list(formContents = Rook::Multipart$parse(req)) # nolint
  # This is where the file name is stored
  # print(fileInfo$formContents$file$filename) # nolint
  file_name <- fileInfo$formContents$req$filename

  print(file_ext(file_name))
  if(file_ext(file_name) %not in% extensions) {
    invalid_extension(file_name)
  }

  # The file is downloaded in a temporary folder
  tmpfile <- fileInfo$formContents$req$tempfile
  #print(fileInfo) # nolint

  # Create a file path
  filePath <- paste0("Distributions/", scientific_name, "/") # nolint
  if (dir.exists(filePath)) {
    print("The direcoty exists")
  } else {
  # create the "my_new_folder
    dir.create(filePath)
  }
  fn <- paste0(filePath, file_name, sepp = "")
  print(fn)

  #Copies the file into the designated folder
  file.copy(tmpfile, fn)


  print(paste0("Your file is now stored in ", fn))
  return(list(path = fn))
}

#* Plot the distributions plot from sRedList platform
#* @get species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param presences:[int] presences
#* @param seasons:[int] seasons
#* @param origins:[int] origins
#* @serializer png list(width = 800, height = 600)
#* @tag sRedList
function(scientific_name, presences = list(1, 2, 3), seasons = list(1, 2), origins = list(1, 2)) { # nolint
    presences <- as.integer(presences);
    seasons <- as.integer(seasons);
    origins <- as.integer(origins);

    scientific_name <- url_decode(scientific_name)
    distributions <- read_distribution(scientific_name)
    dist_species <- subset(distributions, distributions$binomial == scientific_name) # nolint
    #TODO: added parameter API
    #choice_presence <- c(1, 2, 3)
    #choice_season <- c(1, 2)
    #choice_origin <- c(1, 2)

    choice_presence <- c(presences)
    choice_season <- c(seasons)
    choice_origin <- c(origins)

    dist_species <- subset(dist_species,
                dist_species$presence %in% choice_presence &
                dist_species$seasonal %in% choice_season &
                dist_species$origin %in% choice_origin)

    return(plot(ggplot(dist_species) +
        geom_sf() +
        theme_void() +
        ggtitle("Distribution")))

}


#* Global Biodiversity Information Facility
#* @get species/<scientific_name>/gbif/distribution
#* @param scientific_name:string Scientific Name
#* @serializer png list(width = 800, height = 600)
#* @tag sRedList
function(scientific_name) {
    scientific_name <- url_decode(scientific_name)
    #Download data
    dat <- occ_search(scientificName = scientific_name, hasCoordinate = T)$data
    #Select columns of interest
    dat <- dat %>%
    dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount, # nolint
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, datasetName)

    print(head(as.data.frame(dat)))

    #Remove records with no spatial coordinates
    dat <- dat %>% filter(!is.na(decimalLongitude)) %>% filter(!is.na(decimalLatitude)) # nolint

    #Plot data: We would like this map to be interactive, so that users can remove some data points they don’t trust #nolint
    wm <- borders("world", colour="gray50", fill="gray50")
    return(plot(ggplot() + coord_fixed() + wm + geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 0.5) + theme_bw())) # nolint

}