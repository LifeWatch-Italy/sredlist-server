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
  
  # Call the RL API to get information. If it does not work, use the default
  tryCatch({
    species <- rl_search(scientific_name, key = config$red_list_token)#$result
  }, error=function(e){cat("TryCatch Red List API for detail does not work")})

  if(exists("species")==F){
    species<-list(result=speciesRL[1,] %>% replace(., is.na(.)==F, NA))
    species$result$scientific_name<-sRL_decode(scientific_name)
    species$result$assessment_date<-"The Red List API is not working, sorry!"
    species$result$category<-"We cannot provide the usual information on this page, but you can click Next"
  }
  
  return(species)
}


#* Citation Link (link to assessment)
#* @get species/<scientific_name>/citation
#* @serializer unboxedJSON
#* @param scientific_name:str Scientific Name
#* @tag RedList
function(scientific_name) {
  
  tryCatch({
    # Get link to current assessment (this link could go where I wrote "link to assessment" in the PNG file) # nolint
    cite <- unlist(strsplit(as.character(rl_sp_citation(name = scientific_name, key=config$red_list_token)$result), " ")) # nolint
  }, error=function(e){cat("TryCatch Red List API for link does not work")})
  
  if(exists("cite")==F){cite<-"https://www.iucnredlist.org/"}
  
  return(list(link = cite[substr(cite, 1, 4) == "http"]))
}

#* Plot the historic plot
#* @get species/<scientific_name>/historic
#* @param scientific_name:str Scientific Name
#* @serializer png list(width = 800, height = 600)
#* @tag RedList
function(scientific_name) {
  Prom<-future({
    
    tryCatch({
      HistoPlot<-sRL_PlotHistory(sciname_fun=scientific_name)
    }, error=function(e){cat("TryCatch Red List API for historic plot does not work")})
    
    if(exists("HistoPlot")==F){HistoPlot<-ggplot()+labs(title="The Red List API is not working, sorry!", subtitle="We cannot provide the usual information on this page, but you can click Next")}
    
    HistoPlot
   }, gc=T, seed=T) %>% then(onRejected=function(err) {return(ggplot()+ggtitle("ERROR: we are not able to create this plot, please report that error")+labs(subtitle=err))})

  return(Prom %...>% plot())
}


#* Plot the distributions plot from RedList API
#* @get species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param Dist_path:string Distribution Folder default RedList
#* @serializer png list(width = 800, height = 600)
#* @tag RedList
function(scientific_name, Dist_path = "") {

  Prom<-future({

    ### Filter param
    scientific_name <- sRL_decode(scientific_name)
    Dist_path <- ifelse(Dist_path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), Dist_path ) # nolint
  
    ### Load Distribution Species
    distributions <- sRL_ReadDistribution(scientific_name, Dist_path)

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
      Storage_SP<-list(CountrySP_saved=CountrySP, Creation=Sys.time(), Output=sRL_InitLog(scientific_name, DisSource = "Red List"))
      sRL_StoreSave(scientific_name, Storage_SP)

      # Plot
      Plot_Dist<-ggplot() +
                  geom_sf(data = CountrySP, fill="gray96", col="gray50") + # nolint
                  geom_sf(data = distSP, fill = distSP$cols, col=NA) +
                  theme_void() +
                  ggtitle("")
      }
  
    Plot_Dist
    }, gc=T, seed=T) %>% then(onRejected=function(err) {return(ggplot()+ggtitle("ERROR: we are not able to create this plot, please report that error")+labs(subtitle=err))})
  
    ### Plot the distribution
    return(Prom %...>% plot())
  
  }


#* Species habitat preferences
#* @get species/<scientific_name>/habitat-preferences
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag RedList
function(scientific_name) {

Prom<-future({

  scientific_name <- sRL_decode(scientific_name)
  sRL_loginfo("START - Habitat extract", scientific_name)
  
  # Extract habitats
  tryCatch({
    hab_pref <- rl_habitats(scientific_name, key = config$red_list_token)#$result
  }, error=function(e){cat("TryCatch RL API altitude")})
  if(exists("hab_pref")==FALSE){no_hab_API()}
  
  if(is.null(nrow(hab_pref$result))==F){
    hab_pref$result <- hab_pref$result %>% distinct(., code, .keep_all=T) # Remove double (when habitats are used in several seasons)
  
    # Save in Storage SP
    Storage_SP<-sRL_StoreRead(scientific_name, MANDAT=1)
    Storage_SP<-sRL_OutLog(Storage_SP, c("AOH_HabitatORIGINAL", "AOH_HabitatMarginalORIGINAL"), c(hab_pref$result$code[hab_pref$result$suitability=="Suitable"] %>% paste(., collapse=","), hab_pref$result$code[hab_pref$result$suitability!="Suitable"] %>% paste(., collapse=",")))
    sRL_StoreSave(scientific_name, Storage_SP)
  }
  sRL_loginfo("END - Habitat extract", scientific_name)
  
  
  return(hab_pref)

}, gc=T, seed=T)
return(Prom)
}



#* Species altitude preferences
#* @get species/<scientific_name>/altitude-preferences
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag RedList
function(scientific_name) {
  
Prom<-future({
  sf::sf_use_s2(FALSE)
  sRL_loginfo("START - Altitude extract", scientific_name)

  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP=sRL_StoreRead(scientific_name, MANDAT=1)

  #Extract alt_pref
  if(scientific_name %in% speciesRL$scientific_name){
    tryCatch({alt_pref <- rl_search(scientific_name, key = config$red_list_token)}, error=function(e){cat("TryCatch RL API altitude")})
    if(exists("alt_pref")==FALSE){no_hab_API()}
    tryCatch({Storage_SP<-sRL_OutLog(Storage_SP, "Original_altpref", paste(alt_pref$result$elevation_lower[1], alt_pref$result$elevation_upper[1], "fromRL", sep=","))})
  } else {
    alt_pref<-list(name=scientific_name, result=sRL_PrepareAltitudeFile(scientific_name, altitudes_pref=c(NA,NA)))
  }

  
  # If no altitude preference, take from raster
  if(is.na(alt_pref$result$elevation_lower+alt_pref$result$elevation_upper)){
    
    CALCU<-ifelse((is.na(alt_pref$result$elevation_lower) & is.na(alt_pref$result$elevation_upper)), "Calculated", "Half_Calculated")
    ### Small ranges
    Range_size<-as.numeric(sum(st_area(Storage_SP$distSP_saved)))/(10^6)
    
    if(Range_size < as.numeric(config$Size_LargeRange)){
      sRL_loginfo("Run altitude extract (Small range)", scientific_name)
      EXTR<-round(exactextractr::exact_extract(sRL_ChargeAltRaster(), Storage_SP$distSP_saved, c("min", "max")))
      if(is.na(alt_pref$result$elevation_lower)==T){alt_pref$result$elevation_lower<-min(EXTR$min, na.rm=T)}
      if(is.na(alt_pref$result$elevation_upper)==T){alt_pref$result$elevation_upper<-max(EXTR$max, na.rm=T)}
    }
    
    ### Large range
    if(Range_size >= as.numeric(config$Size_LargeRange)){
      sRL_loginfo("Run altitude extract (Large range)", scientific_name)

      if(is.na(alt_pref$result$elevation_lower)==T){
        Alt1010_min<-raster(paste0(config$cciStack2_path, "/ElevationAgg30_MINIMUM.tif"))
        EXTR_min<-exactextractr::exact_extract(Alt1010_min, Storage_SP$distSP_saved, "min")
        alt_pref$result$elevation_lower<-trunc(min(EXTR_min, na.rm=T))
        }
      if(is.na(alt_pref$result$elevation_upper)==T){
        Alt1010_max<-raster(paste0(config$cciStack2_path, "/ElevationAgg30_MAXIMUM.tif"))
        EXTR_max<-exactextractr::exact_extract(Alt1010_max, Storage_SP$distSP_saved, "max")
        alt_pref$result$elevation_upper<-ceiling(max(EXTR_max, na.rm=T))
        }
    }
    tryCatch({Storage_SP<-sRL_OutLog(Storage_SP, "Original_altpref", paste(alt_pref$result$elevation_lower[1], alt_pref$result$elevation_upper[1], CALCU, sep=","))})
  }

  # If something remains NA -> 0, 9000
  if(is.na(alt_pref$result$elevation_lower)==T){alt_pref$result$elevation_lower<-0}
  if(is.na(alt_pref$result$elevation_upper)==T){alt_pref$result$elevation_upper<-9000}
  
  sRL_loginfo("END - Altitude extract", scientific_name)
  sRL_StoreSave(scientific_name, Storage_SP)
  print(alt_pref)
  return(alt_pref)
  
}, gc=T, seed=T)

return(Prom)
}



