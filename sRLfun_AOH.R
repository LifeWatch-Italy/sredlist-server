

### Prepare habitat preference DF (compatible with SIS Connect)
sRL_PrepareHabitatFile<-function(scientific_name, habitats_pref){
  
  # Columns for AOH package analyses
  habitats_pref_DF <- data.frame(code=as.character(habitats_pref))
  habitats_pref_DF$habitat<-crosswalk$iucn_habitat[match(habitats_pref_DF$code, crosswalk$iucn_code)]
  if(scientific_name %in% speciesRL$scientific_name) {habitats_pref_DF$id_no<-speciesRL$taxonid[speciesRL$scientific_name==scientific_name]} else {habitats_pref_DF$id_no<-99999999999} 
  habitats_pref_DF$suitability<-"Suitable"
  habitats_pref_DF$season<-"Resident"
  
  # Columns for SIS Connect
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsLookup<-habitats_pref_DF$code # Column with habitat code
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsName<-habitats_pref_DF$habitat # Column with habitat name
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.majorImportance<-"No" # Assume no habitat of major importance	
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.season<-"Resident" # Assume all resident habitats
  habitats_pref_DF$GeneralHabitats.GeneralHabitatsSubfield.suitability<-"Suitable" # Assume all suitable habitats
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
  
  altitudes_pref_DF$elevation_lower<-as.numeric(altitudes_pref[1])
  altitudes_pref_DF$elevation_upper<-as.numeric(altitudes_pref[2])
  
  altitudes_pref_DF$id_no<-altitudes_pref_DF$taxonid
  
  return(altitudes_pref_DF)
}






### Function to map AOH

sRL_calculateAOH<-function(rangeSP_fun, cci_fun, alt_fun, habitat_pref_fun, FOLDER, elevation_data_fun, crs_to_use){

  #terra::compareGeom(cci_fun, alt_fun)
  
  AOH=create_spp_aoh_data(
    rangeSP_fun, 
    elevation_data = alt_fun,
    habitat_data = cci_fun,
    crosswalk_data = crosswalk_lumbierres_data,
    output_dir=FOLDER,
    engine="terra" # GRASS is in "C:/OSGeo4W/apps/grass/grass78"
  )
  
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




