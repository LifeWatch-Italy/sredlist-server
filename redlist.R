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
  Prom<-future({
    sRL_PlotHistory(sciname_fun=scientific_name)
   }, seed=T)

  return(Prom %...>% plot())
}


#* Plot the distributions plot from RedList API
#* @get species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param path:string Distribution Folder default RedList
#* @serializer png list(width = 800, height = 600)
#* @tag RedList
function(scientific_name, path = "") {

  Prom<-future({

    ### Filter param
    scientific_name <- url_decode(scientific_name)
    path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  
    ### Load Distribution Species
    distributions <- sRL_ReadDistribution(scientific_name, path)

    ### Plot (first if no distribution, then if there is one)
    if(class(distributions)[1]=="character"){
      Plot_Dist<-ggplot() +
        theme_void() +
        ggtitle("No distribution has been found")
    } else {
    
      # Format the distribution file
      distSP<-sRL_PrepareDistrib(distributions, scientific_name)
      distSP<-sRL_ColourDistrib(distSP)
  
      # Format the countries shapefile and save it in the global memory (to avoid recalculating at every step)
      CountrySP<-sRL_PrepareCountries(extent(distSP))
      assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), list(CountrySP_saved=CountrySP, Creation=Sys.time()), .GlobalEnv)
  
      # Plot
      Plot_Dist<-ggplot() +
                  geom_sf(data = CountrySP, fill="gray96", col="gray50") + # nolint
                  geom_sf(data = distSP, fill = distSP$cols, col=NA) +
                  theme_void() +
                  ggtitle("")
      }
  
    Plot_Dist
    }, seed=T) 
  
    ### Plot the distribution
    return(Prom %...>% plot())
  
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
  log_info("START - Altitude extract")

  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)

  #Extract alt_pref
  if(scientific_name %in% speciesRL$scientific_name){
  alt_pref <- rl_search(scientific_name, key = config$red_list_token)} else {
    alt_pref<-list(name=scientific_name, result=sRL_PrepareAltitudeFile(scientific_name, altitudes_pref=c(NA,NA)))
  }

  
  # If no altitude preference, take from raster
  if(is.na(alt_pref$result$elevation_lower+alt_pref$result$elevation_upper)){
    
    ### Small ranges
    Range_size<-as.numeric(sum(st_area(Storage_SP$distSP_saved)))/(10^6)
    
    if(Range_size < as.numeric(config$Size_LargeRange)){
      log_info("Run altitude extract (Small range)")
      EXTR<-round(exactextractr::exact_extract(alt_raw, Storage_SP$distSP_saved, c("min", "max")))
      if(is.na(alt_pref$result$elevation_lower)==T){alt_pref$result$elevation_lower<-min(EXTR$min, na.rm=T)}
      if(is.na(alt_pref$result$elevation_upper)==T){alt_pref$result$elevation_upper<-max(EXTR$max, na.rm=T)}
    }
    
    ### Large range
    if(Range_size >= as.numeric(config$Size_LargeRange)){
      log_info("Run altitude extract (Large range)")

      if(is.na(alt_pref$result$elevation_lower)==T){
        EXTR_min<-exactextractr::exact_extract(Alt1010_min, Storage_SP$distSP_saved, "min")
        alt_pref$result$elevation_lower<-trunc(min(EXTR_min, na.rm=T))
        }
      if(is.na(alt_pref$result$elevation_upper)==T){
        EXTR_max<-exactextractr::exact_extract(Alt1010_max, Storage_SP$distSP_saved, "max")
        alt_pref$result$elevation_upper<-ceiling(max(EXTR_max, na.rm=T))
        }
    }
  }

  # If something remains NA -> 0, 9000
  if(is.na(alt_pref$result$elevation_lower)==T){alt_pref$result$elevation_lower<-0}
  if(is.na(alt_pref$result$elevation_upper)==T){alt_pref$result$elevation_upper<-9000}
  
  log_info("END - Altitude extract")
  print(alt_pref)
  return(alt_pref)
}



