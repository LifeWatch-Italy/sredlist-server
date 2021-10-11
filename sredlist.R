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

#* Info distribution species from sRedList platform
#* @get species/<scientific_name>/distribution/info
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  scientific_name <- url_decode(scientific_name)
  distributions <- read_distribution(scientific_name)
  distSP <- subset(distributions, distributions$binomial == scientific_name) # nolint
  return(list(
    presences = union(c(1, 2, 3), unique(distSP$presence)),
    seasons = union(c(1, 2), unique(distSP$seasonal)),
    origins = union(c(1, 2), unique(distSP$origin))
  ))
}

#* Plot the distributions plot from sRedList platform
#* @get species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param presences:[int] presences (1, 2, 3)
#* @param seasons:[int] seasons (1, 2)
#* @param origins:[int] origins (1, 2)
#* @serializer png list(width = 800, height = 600)
#* @tag sRedList
function(scientific_name, presences = list(), seasons = list() , origins = list()) { # nolint
    #Filter param
    scientific_name <- url_decode(scientific_name)
    if (length(presences) != 0) presences <- as.character(presences);
    if (length(seasons) != 0) seasons <- as.character(seasons);
    if (length(origins) != 0) origins <- as.character(origins);
    print(presences)
    print(seasons)
    print(origins)

    #Load Map countries
    distCountries <- read_map_countries()
    #Load Distribution Species
    distributions <- read_distribution(scientific_name)

    distSP <- subset(distributions, distributions$binomial == scientific_name) # nolint

    choice_presence <- c(presences)
    choice_season <- c(seasons)
    choice_origin <- c(origins)

    distSP <- subset(distSP, # nolint
                 distSP$presence %in% choice_presence &
                 distSP$seasonal %in% choice_season &
                 distSP$origin %in% choice_origin)

    distSP$cols <- NA
    distSP$cols <- revalue(as.character(distSP$presence), c("1"=NA, "2"=NA, "3"=NA, "4"="brown2", "5"="brown4", "6"="gray70")) # nolint

    if (nrow(distSP) > 0) {
      for (i in which(is.na(distSP$cols))) {
        print(distSP$origin[i])
        if (distSP$origin[i] == "1") {
            if(distSP$seasonal[i] == "1"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#d95f02", "2"="#fc8d62", "3"="#fc8d62"))} # nolint
            if(distSP$seasonal[i] == "2"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#1b9e77", "2"="#66c2a5", "3"="#66c2a5"))} # nolint
            if(distSP$seasonal[i] == "3"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#7570b3", "2"="#8da0cb", "3"="#8da0cb"))} # nolint
        } else{
          distSP$cols[i]<-revalue(as.character(distSP$origin[i]), c("2"="darkorchid1", "3"="darkorchid4", "4"="darkseagreen3", "5"="gray70", "6"="darkorchid1")) # nolint
        }   # nolint
      }
    }
    sf::sf_use_s2(FALSE)
    print(plot(ggplot() +
      geom_sf(data = st_buffer(st_crop(distCountries, extent(distSP)),0), fill="white", col="gray50") + # nolint
      geom_sf(data = distSP, fill = distSP$cols) +
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