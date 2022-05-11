
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
  sRL_PlotHistory(sciname_fun=scientific_name)
}


#* Plot the distributions plot from RedList API
#* @get species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param path:string Distribution Folder default RedList
#* @serializer png list(width = 800, height = 600)
#* @tag RedList
function(scientific_name, path = "") {
  
  ### Filter param
  scientific_name <- url_decode(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  
  ### Load Distribution Species
  distributions <- sRL_ReadDistribution(scientific_name, path)
  
  ### Format the distribution file
  distSP<-sRL_PrepareDistrib(distributions, scientific_name)
  distSP<-sRL_ColourDistrib(distSP)
  
  ### Format the countries shapefile and save it in the global memory (to avoid recalculating at every step)
  CountrySP<-sRL_PrepareCountries(extent(distSP))
  assign("CountrySP_saved", CountrySP, .GlobalEnv)
  
  ### Plot the distribution
  sf::sf_use_s2(FALSE)
  if (nrow(distSP) > 0) {
    return(plot(ggplot() +
                  geom_sf(data = CountrySP, fill="gray96", col="gray50") + # nolint
                  geom_sf(data = distSP, fill = distSP$cols, col=NA) +
                  theme_void() +
                  ggtitle("Distribution")))
  } else{
    return(plot(ggplot() +
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
  
  # Filter param
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