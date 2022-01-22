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
  scientific_name <- url_decode(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  distributions <- read_distribution(scientific_name, path)
  dist_species <- subset(distributions, distributions$binomial == scientific_name) # nolint
  #TODO: call RedList API for get distribution
  return(plot(ggplot(dist_species) +
    geom_sf() +
    theme_void() +
    ggtitle("Distribution")))

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