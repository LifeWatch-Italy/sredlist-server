

### Prepare habitat preference DF (compatible with SIS Connect)
sRL_PrepareHabitatFile<-function(scientific_name, habitats_pref, habitats_pref_MARGINAL){
  
  # Columns for AOH package analyses
  habitats_pref_DF <- data.frame(code=as.character(c(habitats_pref, habitats_pref_MARGINAL)), suitability=c(rep("Suitable", length(habitats_pref)), rep("Marginal", length(habitats_pref_MARGINAL))))
  habitats_pref_DF$habitat<-crosswalk$iucn_habitat[match(habitats_pref_DF$code, crosswalk$iucn_code)]
  if(scientific_name %in% speciesRL$scientific_name) {habitats_pref_DF$id_no<-speciesRL$taxonid[speciesRL$scientific_name==scientific_name & is.na(speciesRL$population)==T]} else {habitats_pref_DF$id_no<-99999999999} # I subset for population NA for species like Panthera leo 
  habitats_pref_DF$season<-"Resident"
  
  # Remove if no habitat code
  habitats_pref_DF<-subset(habitats_pref_DF, is.na(habitats_pref_DF$code)==F)
  
  # Columns for SIS Connect
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsLookup<-habitats_pref_DF$code # Column with habitat code
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsName<-habitats_pref_DF$habitat # Column with habitat name
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.majorImportance<- NA #"No" # Assume no habitat of major importance	
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.season<-NA #"Resident" # Assume all resident habitats
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.suitability<-habitats_pref_DF$suitability #"Suitable" # Assume all suitable habitats
  habitats_pref_DF$assessment_id<-99999999999	# No assessment_id associated
  habitats_pref_DF$internal_taxon_id<-habitats_pref_DF$id_no # Column with the taxon ID	
  habitats_pref_DF$internal_taxon_name<-scientific_name # Column with the scientific name
  
  return(habitats_pref_DF)
}


### Prepare altitude preference DF (compatible with SIS Connect)
sRL_PrepareAltitudeFile<-function(scientific_name, altitudes_pref){
  
  # Create the table
  if(scientific_name %in% speciesRL$scientific_name){
    altitudes_pref_DF <- as.data.frame(speciesRL[speciesRL$scientific_name==scientific_name,])
  } else {
    altitudes_pref_DF <- as.data.frame(speciesRL[1,])
    altitudes_pref_DF[1,]<-NA
    altitudes_pref_DF$scientific_name<-scientific_name
    altitudes_pref_DF$taxonid<-99999999999
  }
  
  # Add altitude preferences (with and without uncertainty bounds)
  altitudes_pref<-gsub(" ", "", altitudes_pref)
  if(grepl("-", altitudes_pref[1])){
    ALT=unlist(strsplit(altitudes_pref[1], "-")) %>% as.numeric(.)
    altitudes_pref_DF$elevation_lower<-as.numeric(ALT[2])
    altitudes_pref_DF$elevation_lowerEXTREME<-as.numeric(ALT[1])
  } else {
    altitudes_pref_DF$elevation_lower<-as.numeric(altitudes_pref[1])
  }
  
  if(grepl("-", altitudes_pref[2])){
    ALT=unlist(strsplit(altitudes_pref[2], "-")) %>% as.numeric(.)
    altitudes_pref_DF$elevation_upper<-as.numeric(ALT[1])
    altitudes_pref_DF$elevation_upperEXTREME<-as.numeric(ALT[2])
  } else {
    altitudes_pref_DF$elevation_upper<-as.numeric(altitudes_pref[2])
  }
  
  altitudes_pref_DF$id_no<-altitudes_pref_DF$taxonid
  
  return(altitudes_pref_DF)
}






### Function to map AOH

sRL_calculateAOH<-function(rangeSP_fun, cci_fun, alt_fun, FOLDER, elevation_data_fun){

  #terra::compareGeom(cci_fun, alt_fun)
  
  AOH=create_spp_aoh_data(
    rangeSP_fun, 
    elevation_data = alt_fun,
    habitat_data = cci_fun,
    crosswalk_data = crosswalk_to_use,
    output_dir=FOLDER,
    engine=config$engine_AOH)
  
  return(lapply(AOH$path, terra::rast))
}




### Function to calculate the AOH in km2
sRL_areaAOH<-function(ras, SCALE){
  # ras2<-replace(ras, ras==0, NA)
  # area<-terra::expanse(ras2, unit="km") %>% as.numeric(.) %>% round(.)
  
  cell_presence<-global(ras, "sum", na.rm=T) %>% as.numeric(.)
  
  if(SCALE=="cci"){area<-cell_presence*0.09487}
  if(SCALE=="2x2"){area<-cell_presence*4}
  
  return(area)
}


### AOH calculate for large range species
sRL_largeAOH<-function(habitats_pref, altitudes_pref, rangeSP_clean, YR){
  
  log_info("Charge CCI_Large data")
  if(YR==config$YearAOH2){CCI_fun<-CCI_large} else {
    CCI_fun<-stackOpen(paste0(config$cciStack2_path, "/", YR, "/CCI_Stack_Agg30_Year", YR, ".stk"))
    names(CCI_fun)<-read.table(paste0(config$cciStack2_path, "/", YR, "/CCI_Stack_Agg30_Year", YR, ".stk"))[,1] %>% gsub(config$cciStack2_path, "", .) %>% substr(., 7, (nchar(.)-4)) # I have to rename them because update in raster package made a change in the names. I checked that the order of the rasters in Stack were the same as in the .stk file
  }
  
  log_info("START - Large AOH function")
  
  # Select rasters to keep
  CCI_to_keep<-NA
  for(i in 1:length(habitats_pref)){CCI_to_keep<-c(CCI_to_keep, crosswalkLARGE$Stack_Name[grepl(habitats_pref[i], crosswalkLARGE$Codes)])  %>% unique(.) %>% .[is.na(.)==F] %>% sort(.)}
  CCI_suitable<-CCI_fun[[which(names(CCI_fun) %in% paste0("Agg30_CCI", YR, "_", CCI_to_keep))]]
  print("Suitable CCI groups:") ; print(names(CCI_suitable))
  
  # Crop and sum CCIs  
  CCI_crop<-crop(CCI_suitable, extent(rangeSP_clean))
  CCI_sum<-sum(CCI_crop)

  # Crop altitude and calculate AOH
  alt_crop<-crop(alt_large, extent(rangeSP_clean))
  alt_suitable<-(alt_crop > as.numeric(altitudes_pref[1]) & alt_crop<as.numeric(altitudes_pref[2]))

  AOH<-CCI_sum*alt_suitable %>% mask(., rangeSP_clean)
  AOH<-rast(AOH)
  
  log_info("END - Large AOH function")
  
  return(AOH)
}




### Log-scale for colour plot in Large Trends
colour_bidirect_scale <- trans_new("logpeps",
                             function(x) {sign(x)*sqrt(abs(x))},
                             function(x) {sign(x)*(abs(x)^2)}) 
