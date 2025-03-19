### Search species in search bar -----
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

### Extract species info from RL -----
#* Species Object
#* @get species/<scientific_name>
#* @param scientific_name:str Scientific Name
#* @serializer json
#* @tag RedList
function(scientific_name, username) {

Prom<-future({
  
  # Call the RL API to get information. If it does not work, use the default
  tryCatch({
    scientific_name <- sRL_decode(scientific_name)
    species <- sRL_GetRLAssessment(scientific_name, key = config$red_list_token)
    
    if(length(species)>0){
      # Save Storage_SP
      Storage_SP<-list(SpeciesAssessment=species, Creation=Sys.time(), Output=sRL_InitLog(scientific_name, username, DisSource = "Red List"))
      sRL_StoreSave(scientific_name, username, Storage_SP)
      
      # Transform to list to export to client
      speciesLIST <- data.frame(scientific_name=scientific_name,
                                taxonid=ifelse("sis_id" %in% names(species$taxon), species$taxon$sis_id, ""),
                                kingdom=ifelse("kingdom_name" %in% names(species$taxon), species$taxon$kingdom_name, ""),
                                class=ifelse("class_name" %in% names(species$taxon), species$taxon$class_name, ""),
                                order=ifelse("order_name" %in% names(species$taxon), species$taxon$order_name, ""),
                                family=ifelse("family_name" %in% names(species$taxon), species$taxon$family_name, ""),
                                main_common_name=ifelse("name" %in% names(species$taxon_common_names), species$taxon_common_names$name[species$taxon_common_names$main==T], ""),
                                assessment_date=substr(species$assessment_date, 1, 10),
                                category=ifelse("code" %in% names(species$red_list_category), species$red_list_category$code, ""),
                                assessor=ifelse("assessor" %in% species$credits$credit_type_name, species$credits$value[species$credits$credit_type_name=="assessor"], ""),
                                evaluator=ifelse("evaluator" %in% species$credits$credit_type_name, species$credits$value[species$credits$credit_type_name=="evaluator"], ""),
                                population_trend=ifelse("description" %in% names(species$population), species$population_trend$description, ""),
                                url=species$url$value
      )
      
      # Add DDapp information
      if(scientific_name %in% speciesDDapp$scientific_name & species$red_list_category$code=="DD"){
        speciesLIST$DD_prioritiser <- paste0("This species is available in the DD prioritiser tool, it estimates a reassessment priority of ", round(100*speciesDDapp$Prio_rank[speciesDDapp$scientific_name==scientific_name]), "%.")
      }
      
    } else {speciesLIST <- data.frame(scientific_name=scientific_name, assessment_date="The Red List API is not working, sorry!", category="We cannot provide the usual information on this page, but you can click Next")}

  }, error=function(e){cat("TryCatch Red List API for detail does not work")})

  return(list(result=speciesLIST))
  
}, gc=T, seed=T)

return(Prom)
}

### Plot assessment history -----
#* Plot the historic plot
#* @get species/<scientific_name>/historic
#* @param scientific_name:str Scientific Name
#* @serializer png list(width = 800, height = 600)
#* @tag RedList
function(scientific_name) {
  Prom<-future({
    
    tryCatch({
      HistoPlot<-sRL_PlotHistory(sciname_fun=scientific_name, key = config$red_list_token)
    }, error=function(e){cat("TryCatch Red List API for historic plot does not work")})
    
    if(exists("HistoPlot")==F){HistoPlot<-ggplot()+labs(title="The Red List API is not working, sorry!", subtitle="We cannot provide the usual information on this page, but you can click Next")}
    
    HistoPlot
   }, gc=T, seed=T) %>% then(onRejected=function(err) {return(ggplot()+ggtitle("ERROR: we are not able to create this plot, please report that error")+labs(subtitle=err))})

  return(Prom %...>% plot())
}


### Plot distribution page 0 -----
#* Plot the distributions plot from RedList API
#* @get species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param username:string Username
#* @param Dist_path:string Distribution Folder default RedList
#* @serializer png list(width = 800, height = 600)
#* @tag RedList
function(scientific_name, username, Dist_path = "") {

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
      CountrySP<-sRL_PrepareCountries(1.2*extent(distSP))

      # Plot
      Plot_Dist<-ggplot() +
                  geom_sf(data = CountrySP, fill="gray96", col="gray50") + # nolint
                  geom_sf(data = distSP, fill = distSP$cols, col=NA) +
                  theme_void() +
                  ggtitle("")
      
      # Save
      Storage_SP<-sRL_StoreRead(scientific_name, username, MANDAT=1)
      if(exists("Storage_SP")==F){Sys.sleep(3) ; Storage_SP<-sRL_StoreRead(scientific_name, username, MANDAT=1)}
      Storage_SP$CountrySP_saved=CountrySP
      sRL_StoreSave(scientific_name, username, Storage_SP)
      }
  
    Plot_Dist
    }, gc=T, seed=T) %>% then(onRejected=function(err) {return(ggplot()+ggtitle("ERROR: we are not able to create this plot, please report that error")+labs(subtitle=err))})
  
    ### Plot the distribution
    return(Prom %...>% plot())
  
  }


### Extract habitats preferences -----
#* Species habitat preferences
#* @get species/<scientific_name>/habitat-preferences
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag RedList
function(scientific_name, username) {

  scientific_name <- sRL_decode(scientific_name)

  
  ### Only run a promise with rredlist if species in the Red List (otherwise return empty)
  if(scientific_name %in% speciesRL$scientific_name){
    
    Prom<-future({
      sRL_loginfo("START - Habitat extract", scientific_name)
      
      # Load habitats from Storage_SP
      Storage_SP<-sRL_StoreRead(scientific_name, username, MANDAT=1)
      
      tryCatch({hab_pref <- as.data.frame(Storage_SP$SpeciesAssessment$habitats)}, error=function(e){cat("TryCatch RL API habitat")})

      if(exists("hab_pref")){
        if(nrow(hab_pref)>0){
          hab_pref <- hab_pref %>% distinct(., code, .keep_all=T) # Remove double (when habitats are used in several seasons)
          hab_pref$code <- gsub("_", ".", hab_pref$code)
          # Save in Storage SP
          Storage_SP<-sRL_OutLog(Storage_SP, c("AOH_HabitatORIGINAL", "AOH_HabitatMarginalORIGINAL"), c(hab_pref$code[hab_pref$suitability=="Suitable"] %>% paste(., collapse=","), hab_pref$code[hab_pref$suitability!="Suitable"] %>% paste(., collapse=",")))
          sRL_StoreSave(scientific_name, username, Storage_SP)
        } else {hab_pref<-data.frame(code=NA, habitat=NA, suitability=NA, majorimportance=NA)[0,]}
      } else {no_hab_API()}
      
      sRL_loginfo("END - Habitat extract", scientific_name)
      
      return(hab_pref)
      
    }, gc=T, seed=T)
    
    return(Prom)
    
  ### If species not in the RL, I return an empty hab_pref  
  } else {
    
      sRL_loginfo("START - Habitat extract (not in RL)", scientific_name)
      hab_pref<-data.frame(code=NA, habitat=NA, suitability=NA, majorimportance=NA)[0,]
      sRL_loginfo("END - Habitat extract (not in RL)", scientific_name)
      return(hab_pref)
  }
  

}


### Extract elevation preferences -----
#* Species altitude preferences
#* @get species/<scientific_name>/altitude-preferences
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag RedList
function(scientific_name, username) {
  
Prom<-future({
  sf::sf_use_s2(FALSE)
  sRL_loginfo("START - Altitude extract", scientific_name)

  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP=sRL_StoreRead(scientific_name, username, MANDAT=1)

  #Extract alt_pref
  if(scientific_name %in% speciesRL$scientific_name){
    tryCatch({alt_pref <- as.data.frame(Storage_SP$SpeciesAssessment$supplementary_info) %>% subset(., select=c("upper_elevation_limit", "lower_elevation_limit")) ; names(alt_pref) <- revalue(names(alt_pref), c("upper_elevation_limit"="elevation_upper", "lower_elevation_limit"="elevation_lower"))}, error=function(e){cat("TryCatch RL API altitude")})
    if(exists("alt_pref")==FALSE){no_hab_API()}
    tryCatch({Storage_SP<-sRL_OutLog(Storage_SP, "Original_altpref", paste(alt_pref$elevation_lower[1], alt_pref$elevation_upper[1], "fromRL", sep=","))})
  } else {
    alt_pref<-sRL_PrepareAltitudeFile(scientific_name, altitudes_pref=c(NA,NA))
  }
  
  # Elevation source from Red List or calculated
  alt_pref$src_lower <- ifelse(is.na(alt_pref$elevation_lower), "The proposed lower elevation preference was calculated as the lowest elevation within the species range", "The proposed lower elevation preference was retrieved from the last assessment")
  alt_pref$src_upper <- ifelse(is.na(alt_pref$elevation_upper), "The proposed upper elevation preference was calculated as the highest elevation within the species range", "The proposed upper elevation preference was retrieved from the last assessment")
  
  # If no altitude preference, take from raster
  if(is.na(alt_pref$elevation_lower+alt_pref$elevation_upper)){
    
    CALCU<-ifelse((is.na(alt_pref$elevation_lower) & is.na(alt_pref$elevation_upper)), "Calculated", "Half_Calculated")
    ### Small ranges
    Range_size<-as.numeric(sum(st_area(Storage_SP$distSP_selected)))/(10^6)
    
    if(Range_size < 10000){
      sRL_loginfo("Run altitude extract (Small range)", scientific_name)
      EXTR<-round(exactextractr::exact_extract(sRL_ChargeAltRaster(), Storage_SP$distSP_selected, c("min", "max")))
      if(is.na(alt_pref$elevation_lower)==T){alt_pref$elevation_lower<-min(EXTR$min, na.rm=T)}
      if(is.na(alt_pref$elevation_upper)==T){alt_pref$elevation_upper<-max(EXTR$max, na.rm=T)}
    }
    
    ### Large range
    if(Range_size >= 10000){
      sRL_loginfo("Run altitude extract (Large range)", scientific_name)

      if(is.na(alt_pref$elevation_lower)==T){
        Alt1010_min<-raster(paste0(config$cciStack2_path, "/ElevationAgg30_MINIMUM.tif"))
        EXTR_min<-exactextractr::exact_extract(Alt1010_min, Storage_SP$distSP_saved, "min")
        alt_pref$elevation_lower<-trunc(min(EXTR_min, na.rm=T))
        }
      if(is.na(alt_pref$elevation_upper)==T){
        Alt1010_max<-raster(paste0(config$cciStack2_path, "/ElevationAgg30_MAXIMUM.tif"))
        EXTR_max<-exactextractr::exact_extract(Alt1010_max, Storage_SP$distSP_saved, "max")
        alt_pref$elevation_upper<-ceiling(max(EXTR_max, na.rm=T))
        }
    }
    tryCatch({Storage_SP<-sRL_OutLog(Storage_SP, "Original_altpref", paste(alt_pref$elevation_lower[1], alt_pref$elevation_upper[1], CALCU, sep=","))})
  }

  # If something remains NA -> 0, 9000
  if(is.na(alt_pref$elevation_lower)==T){alt_pref$elevation_lower<-0 ; alt_pref$src_lower<-"default"}
  if(is.na(alt_pref$elevation_upper)==T){alt_pref$elevation_upper<-9000 ; alt_pref$src_upper<-"default"}
  
  sRL_loginfo("END - Altitude extract", scientific_name)
  sRL_StoreSave(scientific_name, username, Storage_SP)
  
  return(as.list(alt_pref))
  
}, gc=T, seed=T)

return(Prom)
}



