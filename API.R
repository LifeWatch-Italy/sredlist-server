# API.R

#* @apiTitle LifeWach-RedList-Platform API
#* @apiDescription This is a sample server for a Red List workflow.
#* @apiTOS http://example.com/terms/
#* @apiContact list(name = "API Support", url = "http://www.example.com/support", email = "support@example.com") # nolint
#* @apiLicense list(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html") # nolint
#* @apiVersion 1.0.0
#* @apiTag RedList-API API from RedList
#* @apiTag sRedList Plaftorm API

#Load species information from RL - updated in 27-09-2021
speciesRL <- read.csv("Species/species-all-page.csv") # nolint # nolint

#Extensions File Distribution
extensions <- list("shp", "shx", "prj", "dbf", "cpg")

#* @filter cors
function(res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    plumber::forward()
}


#* Find Species
#* @get api/species/search
#* @serializer json
#* @param scientific_name:str Digit Scientific Name (min. 3 characters)
#* @tag RedList-API
function(scientific_name) {
  if (nchar(scientific_name) < 3) {
    invalid_params("You must enter at least 3 characters.")
  }
  indices <- grep(tolower(scientific_name), tolower(speciesRL$scientific_name))
  return(speciesRL[indices, ])
}

#* Species Object
#* @get api/species/<scientific_name>
#* @param scientific_name:str Scientific Name
#* @serializer json
#* @tag RedList-API
function(scientific_name) {
  species <- rl_search(scientific_name, key = config$red_list_token)#$result
  return(species)
}


#* Citation Link (link to assessment)
#* @get api/species/<scientific_name>/citation
#* @serializer unboxedJSON
#* @param scientific_name:str Scientific Name
#* @tag RedList-API
function(scientific_name) {
  # Get link to current assessment (this link could go where I wrote "link to assessment" in the PNG file) # nolint
  cite <- unlist(strsplit(as.character(rl_sp_citation(name = scientific_name, key=config$red_list_token)$result), " ")) # nolint
  return(list(link = cite[substr(cite, 1, 4) == "http"]))
}

#* Plot the historic plot
#* @get api/species/<scientific_name>/historic
#* @param scientific_name:str Scientific Name
#* @serializer png list(width = 800, height = 600)
#* @tag RedList-API
function(scientific_name) {
  historic <- rl_history(scientific_name, key = config$red_list_token)$result

  if(length(historic) == 0){ # nolint
    not_found("Scientific name not found.")
  }
    historic$Cat<-revalue(historic$category, c("Least Concern"="LC", "Near Threatened"="NT", "Vulnerable"="VU", "Endangered"="EN", "Critically Endangered"="CR", "Extinct in the Wild"="EW", "Extinct"="EX", "Data Deficient"="DD")) # nolint
    historic$Cat[historic$Cat %not in% c("LC", "NT", "DD", "VU", "EN", "CR", "EW", "EX")]<-"old" # nolint
    historic$Cat<-factor(historic$Cat, c("LC", "NT", "DD", "old", "VU", "EN", "CR", "EW", "EX")) # nolint
    historic$year<-as.numeric(historic$year)

  return(plot(ggplot(historic)+
    geom_line(aes(x = year, y = as.numeric(Cat)), size = 0.5) +
    geom_point(aes(x = year, col = Cat, y = Cat), shape = 15, size = 5, show.legend = F) + # nolint
    scale_colour_manual(values=rev(c("#000000ff", "#542344ff", "#d81e05ff", "#fc7f3fff", "#f9e814ff", "#bcbddc", "#d1d1c6ff", "#cce226ff", "#60c659ff")), name="", drop=FALSE) + # nolint
    scale_y_discrete(rev(levels(historic$Cat)), drop=FALSE, name="Extinction risk") + # nolint
    theme_minimal()+
    xlab("")+
    ggtitle("Assessments historic")))
}

#* Plot the distributions plot from RedList API
#* @get api/species/<scientific_name>/redlist/distribution
#* @param scientific_name:string Scientific Name
#* @serializer png list(width = 800, height = 600)
#* @tag RedList-API
function(scientific_name) {
  scientific_name <- url_decode(scientific_name)

  speciesPath <- paste0("Distributions/", scientific_name) # nolint
  files <- base::list.files(path = speciesPath, pattern = "\\.shp$")

  if (length(files) == 0) {
    not_found("Shapefile of the species does not exist!")
  } else {
    distributionPath <- paste0(speciesPath, "/", files[1]) # nolint
  }

  print(distributionPath)
  distributions <- st_read(distributionPath)

  names(distributions)[which(names(distributions) %in% c("SCINAME", "binomial", "BINOMIIAL"))] <- "binomial" # nolint
  names(distributions)[which(names(distributions) %in% c("PRESENC", "PRESENCE", "presence"))] <- "presence" # nolint
  names(distributions)[which(names(distributions) %in% c("ORIGIN", "origin"))] <- "origin" # nolint
  names(distributions)[which(names(distributions) %in% c("SEASONA", "SEASONAL", "seasonal"))] <- "seasonal" # nolint

  #TODO: call RL API for get distribution
  print(names(distributions))
  if (scientific_name == "Chamaeleo chamaeleon") {
    dist_sp<-distributions[distributions$binomial== scientific_name, ] # nolint
    return(plot(ggplot(dist_sp) +
    geom_sf() +
    theme_void() +
    ggtitle("Distribution")))
  }
  return(plot(ggplot(distributions) +
    geom_sf() +
    theme_void() +
    ggtitle("Distribution")))

}

#* Upload Distribution species
#* @post api/species/<scientific_name>/distribution
#* @param req:file Distribution file (shp,.shx,.prj,.dbf,.cpg)
#* @param scientific_name:str Insert Species Name
#* @serializer unboxedJSON
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
#* @get api/species/<scientific_name>/sredlist/distribution
#* @param scientific_name:string Scientific Name
#* @serializer png list(width = 800, height = 600)
#* @tag sRedList
function(scientific_name) {
  scientific_name <- url_decode(scientific_name)
  print(scientific_name)

  speciesPath <- paste0("Distributions/", scientific_name) # nolint
  files <- base::list.files(path = speciesPath, pattern = "\\.shp$")

  if (length(files) == 0) {
    not_found("Shapefile of the species does not exist!")
  } else {
    distributionPath <- paste0(speciesPath, "/", files[1]) # nolint
  }

  #distributions<-st_read("Distributions/Chameleons/CHAMELEONS.shp") # nolint
  distributions <- st_read(distributionPath)

  names(distributions)[which(names(distributions) %in% c("SCINAME", "binomial", "BINOMIAL"))] <- "binomial" # nolint
  names(distributions)[which(names(distributions) %in% c("PRESENC", "PRESENCE", "presence"))] <- "presence" # nolint
  names(distributions)[which(names(distributions) %in% c("ORIGIN", "origin"))] <- "origin" # nolint
  names(distributions)[which(names(distributions) %in% c("SEASONA", "SEASONAL", "seasonal"))] <- "seasonal" # nolint

  #TODO: call RL API for get distribution
  print(distributions)
  dist_species <- subset(distributions, distributions$binomial == scientific_name) # nolint

  choice_presence <- c(1, 2, 3)
  choice_season <- c(1, 2)
  choice_origin <- c(1, 2)

  dist_species <- subset(dist_species,
               dist_species$presence %in% choice_presence &
               dist_species$seasonal %in% choice_season &
               dist_species$origin %in% choice_origin)

  return(plot(ggplot(dist_species) +
    geom_sf() +
    theme_void() +
    ggtitle("Distribution")))

}