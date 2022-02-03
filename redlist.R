#Load species information from RL - updated in 27-09-2021
speciesRL <- read.csv("Species/species-all-page.csv") # nolint

#* Find Species
#* @get species/search
#* @serializer json
#* @param scientific_name:str Digit Scientific Name (min. 3 characters)
#* @tag RedList
function(scientific_name) {
  if (nchar(scientific_name) < 3) {
    invalid_params("You must enter at least 3 characters.")
  }
  indices <- grep(tolower(scientific_name), tolower(speciesRL$scientific_name))
  return(speciesRL[indices, ])
}

#* Species Object
#* @get species/<scientific_name>
#* @param scientific_name:str Scientific Name
#* @serializer json
#* @tag RedList
function(scientific_name) {
  species <- rl_search(scientific_name, key = config$red_list_token)#$result
  return(species)
}


#* Citation Link (link to assessment)
#* @get species/<scientific_name>/citation
#* @serializer unboxedJSON
#* @param scientific_name:str Scientific Name
#* @tag RedList
function(scientific_name) {
  # Get link to current assessment (this link could go where I wrote "link to assessment" in the PNG file) # nolint
  cite <- unlist(strsplit(as.character(rl_sp_citation(name = scientific_name, key=config$red_list_token)$result), " ")) # nolint
  return(list(link = cite[substr(cite, 1, 4) == "http"]))
}

#* Plot the historic plot
#* @get species/<scientific_name>/historic
#* @param scientific_name:str Scientific Name
#* @serializer png list(width = 800, height = 600)
#* @tag RedList
function(scientific_name) {
  historic <- rl_history(scientific_name, key = config$red_list_token)$result

  if(length(historic) == 0){ # nolint
    not_found("Scientific name not found.")
  }
    historic$Cat<-revalue(historic$category, c("Least Concern"="LC", "Near Threatened"="NT", "Vulnerable"="VU", "Endangered"="EN", "Critically Endangered"="CR", "Extinct in the Wild"="EW", "Extinct"="EX", "Data Deficient"="DD")) # nolint
    historic$Cat[historic$Cat %not in% c("LC", "NT", "DD", "VU", "EN", "CR", "EW", "EX")]<-"old" # nolint
    historic$Cat <- factor(historic$Cat, c("LC", "NT", "DD", "old", "VU", "EN", "CR", "EW", "EX")) # nolint
    historic$year <- as.numeric(historic$year)

  return(plot(ggplot(historic)+
    geom_line(aes(x = year, y = as.numeric(Cat)), size = 0.5) +
    geom_point(aes(x = year, col = Cat, y = Cat), shape = 15, size = 5, show.legend = F) + # nolint
    scale_colour_manual(values=rev(c("#000000ff", "#542344ff", "#d81e05ff", "#fc7f3fff", "#f9e814ff", "#bcbddc", "#d1d1c6ff", "#cce226ff", "#60c659ff")), name="", drop=FALSE) + # nolint
    scale_y_discrete(rev(levels(historic$Cat)), drop=FALSE, name="Extinction risk") + # nolint
    theme_minimal() +
    xlab("") +
    ggtitle("Assessments historic")))
}

#* Plot the distributions plot from RedList API
#* @get species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param path:string Distribution Folder default RedList
#* @serializer png list(width = 800, height = 600)
#* @tag RedList
function(scientific_name, path = "") {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  #Load Map countries
  distCountries <- read_map_countries()
  #Load Distribution Species
  speciesPath <- paste0(config$distribution_path, scientific_name, "/", path) # nolint
  files <- base::list.files(path = speciesPath, pattern = "\\.shp$")
  if (length(files) == 0) {
    not_found("Shapefile of the species does not exist!") # nolint
  } else {
    distributionPath <- paste0(speciesPath, "/", files[1]) # nolint
  }
  if (file.info(distributionPath)$size >= config$distribution_size) {
      payload_too_large("The distribution is too large to be shown here")
  }
  distributions <- sf::st_read(distributionPath)
  names(distributions)[which(names(distributions) %in% c("SCINAME", "binomial", "BINOMIAL"))] <- "binomial" # nolint
  names(distributions)[which(names(distributions) %in% c("PRESENC", "PRESENCE", "presence"))] <- "presence" # nolint
  names(distributions)[which(names(distributions) %in% c("ORIGIN", "origin"))] <- "origin" # nolint
  names(distributions)[which(names(distributions) %in% c("SEASONA", "SEASONAL", "seasonal"))] <- "seasonal" # nolint
  distSP <- subset(distributions, distributions$binomial == scientific_name) # nolint 
  if (nrow(distSP) > 0) { 
    distSP$cols <- NA
    distSP$cols <- revalue(as.character(distSP$presence), c("1"=NA, "2"=NA, "3"=NA, "4"="mistyrose1", "5"="brown4", "6"="gray70")) # nolint    if (nrow(distSP) > 0) {
    for (i in which(is.na(distSP$cols))) {
        if (distSP$origin[i] == "1") {
          if(distSP$seasonal[i] == "1"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#d95f02", "2"="#fc8d62", "3"="#fc8d62"))} # nolint
          if(distSP$seasonal[i] == "2"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#1b9e77", "2"="#66c2a5", "3"="#66c2a5"))} # nolint
          if(distSP$seasonal[i] == "3"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#7570b3", "2"="#8da0cb", "3"="#8da0cb"))} # nolint
          if(distSP$seasonal[i] == "4"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="yellowgreen", "2"="yellow2", "3"="yellow2"))} # nolint
          if(distSP$seasonal[i] == "5"){ distSP$cols[i]<-"gray70"}
        } else{
          distSP$cols[i]<-revalue(as.character(distSP$origin[i]), c("2"="darkorchid1", "3"="darkorchid4", "4"="darkseagreen3", "5"="gray70", "6"="darkorchid1")) # nolint
        }   # nolint
    }
  }
  sf::sf_use_s2(FALSE)
  if (nrow(distSP) > 0) {
  return(plot(ggplot() +
              geom_sf(data = st_buffer(st_crop(distCountries, extent(distSP)),0), fill="white", col="gray50") + # nolint
              geom_sf(data = distSP, fill = distSP$cols) +
              theme_void() +
              ggtitle("Distribution")))
  } else{
    sf::sf_use_s2(FALSE)
    return(plot(ggplot() +
                geom_sf(data = st_buffer(st_crop(distCountries, extent(distSP)),0), fill="white", col="gray50") + # nolint
                theme_void() +
                ggtitle("The distribution is empty")))
  }
}


#* Species habitat preferences
#* @get species/<scientific_name>/habitat-preferences
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag RedList
function(scientific_name) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  hab_pref <- rl_habitats(scientific_name, key = config$red_list_token)#$result
  return(hab_pref)
}

#* Species altitude preferences
#* @get species/<scientific_name>/altitude-preferences
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag RedList
function(scientific_name) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  alt_pref <- rl_search(scientific_name, key = config$red_list_token)#$result
  return(alt_pref)
}