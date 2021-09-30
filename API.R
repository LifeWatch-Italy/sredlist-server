# API.R

#* @apiTitle LifeWach-RedList-Platform API
#* @apiDescription This is a sample server for a Red List workflow.
#* @apiTOS http://example.com/terms/
#* @apiContact list(name = "API Support", url = "http://www.example.com/support", email = "support@example.com")
#* @apiLicense list(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
#* @apiVersion 1.0.0
#* @apiTag RL-Platform default api

#Load species information from RL - updated in 27-09-2021
speciesRL<-read.csv("Species/species-all-page.csv")

#* @filter cors
function(res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    plumber::forward()
}


#* Find Species
#* @get api/species/search
#* @serializer json
#* @param scientific_name:str Digit Scientific Name (min. 3 characters)
#* @tag RL-Platform
function(scientific_name) {
  if(nchar(scientific_name)<3){
    invalid_params("You must enter at least 3 characters.")
  }
  indices<-grep(tolower(scientific_name), tolower(speciesRL$scientific_name))
  return(speciesRL[indices, ])
}

#* Species Object
#* @get api/species/<scientific_name>
#* @param scientific_name:str Scientific Name
#* @serializer json
#* @tag RL-Platform
function(scientific_name) {
  species<-rl_search(scientific_name, key=config$red_list_token)#$result
  return(species)
}


#* Citation Link (link to assessment)
#* @get api/species/<scientific_name>/citation
#* @serializer unboxedJSON
#* @param scientific_name:str Scientific Name
#* @tag RL-Platform
function(scientific_name) {
  # Get link to current assessment (this link could go where I wrote "link to assessment" in the PNG file)
  cite<-unlist(strsplit(as.character(rl_sp_citation(name = scientific_name, key=config$red_list_token)$result), " "))
  return(list(link=cite[substr(cite, 1,4)=="http"]))
}

#* Plot the historic plot
#* @get api/species/<scientific_name>/historic
#* @param scientific_name:str Scientific Name
#* @serializer png
#* @tag RL-Platform
function(scientific_name) {
  historic<-rl_history(scientific_name, key=config$red_list_token)$result

  if(length(historic) == 0){
    not_found("Scientific name not found.")
  }
    historic$Cat<-revalue(historic$category, c("Least Concern"="LC", "Near Threatened"="NT", "Vulnerable"="VU", "Endangered"="EN", "Critically Endangered"="CR", "Extinct in the Wild"="EW", "Extinct"="EX", "Data Deficient"="DD"))
    historic$Cat[historic$Cat %not in% c("LC", "NT", "DD", "VU", "EN", "CR", "EW", "EX")]<-"old"
    historic$Cat<-factor(historic$Cat, c("LC", "NT", "DD", "old", "VU", "EN", "CR", "EW", "EX"))
    historic$year<-as.numeric(historic$year)

  return(plot(ggplot(historic)+
    geom_line(aes(x=year, y=as.numeric(Cat)), size=0.5)+
    geom_point(aes(x=year, col=Cat, y=Cat), shape=15, size=5, show.legend=F)+
    scale_colour_manual(values=rev(c("#000000ff", "#542344ff", "#d81e05ff", "#fc7f3fff", "#f9e814ff", "#bcbddc", "#d1d1c6ff", "#cce226ff", "#60c659ff")), name="", drop=FALSE)+
    scale_y_discrete(rev(levels(historic$Cat)), drop=FALSE, name="Extinction risk")+
    theme_minimal()+
    xlab("")+
    ggtitle("Assessments historic")))
}

#* Plot the distributions plot
#* @get api/species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @serializer png
#* @tag RL-Platform
function(scientific_name) {
  scientific_name <- url_decode(scientific_name)

  distributions<-st_read("Distributions/CHAMELEONS.shp")
  #Bradypodion setaroi
  distSP<-distributions[distributions$binomial== scientific_name, ]
  
  return(plot(ggplot(distSP)+
    geom_sf()+
    theme_void()+
    ggtitle("Distribution")))

}