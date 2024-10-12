

### Charge rasters functions
sRL_ChargeAltRaster<-function(){alt_raw<-rast(config$alt_raster_path) ; crs(alt_raw)<-CRSMOLL ; names(alt_raw)[1]<-"Elevation_reprojMollweide3" ; return(alt_raw)}  # I ensure the CRS is correctly assigned (it was saved as a CRSMOLL raster)}

sRL_ChargeCci2Raster<-function(){cci2<-rast(config$cci2_raster_path); crs(cci2)<-CRSMOLL ; return(cci2)}

sRL_ChargeGrid22Raster<-function(){rast("resources/EmptyGrid2x2/Empty.grid.2x2.Mollweide.tif")}

sRL_ChargeCciLargeRaster<-function(){
  CCI_large<-stackOpen(paste0(config$cciStack2_path, "/2020/CCI_Stack_Agg30_Year2020.stk"))
  names(CCI_large)<-read.table(paste0(config$cciStack2_path, "/2020/CCI_Stack_Agg30_Year2020.stk"))[,1] %>% gsub(config$cciStack2_path, "", .) %>% substr(., 7, (nchar(.)-4)) # I have to rename them because update in raster package made a change in the names. I checked that the order of the rasters in Stack were the same as in the .stk file
  return(CCI_large)
}



### Prepare habitat preference DF (compatible with SIS Connect)
sRL_PrepareHabitatFile<-function(scientific_name, habitats_pref, habitats_pref_MARGINAL){
  
  # Columns for AOH package analyses
  habitats_pref_DF <- data.frame(code=as.character(c(habitats_pref, habitats_pref_MARGINAL)), suitability=c(rep("Suitable", length(habitats_pref)), rep("Unknown", length(habitats_pref_MARGINAL))))
  habitats_pref_DF$habitat<-hab_scheme$iucn_habitat[match(habitats_pref_DF$code, hab_scheme$iucn_code)]
  habitats_pref_DF$id_no<-sRL_CalcIdno(scientific_name)
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
    altitudes_pref_DF$taxonid<-sRL_CalcIdno(scientific_name)
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
  
  # Try calculate AOH with cells that have their center within the distribution
  AOH=create_spp_aoh_data(
    rangeSP_fun, 
    elevation_data = alt_fun,
    habitat_data = cci_fun,
    crosswalk_data = crosswalk_to_use,
    output_dir=FOLDER,
    rasterize_touches=F,
    engine=config$engine_AOH)
  
  AOH2<-lapply(AOH$path, terra::rast)
  
  # If the AOH is empty (ie if no raster cell overlapped had their center within the distribution), I recalculate with all cells overlapping somehow with the distribution
  if(is.nan(minmax(AOH2[[1]])[1])){
    
    sRL_loginfo("AOH will be calculated with raterize_touces", "")
    
    unlink(paste0(FOLDER, "/", list.files(FOLDER)))
    
    AOH=create_spp_aoh_data(
      rangeSP_fun, 
      elevation_data = alt_fun,
      habitat_data = cci_fun,
      crosswalk_data = crosswalk_to_use,
      output_dir=FOLDER,
      rasterize_touches=T,
      engine=config$engine_AOH)
  }
  
  
  return(lapply(AOH$path, terra::rast))
}




### Function to calculate the AOH in km2
sRL_areaAOH<-function(ras, SCALE){
  # ras2<-replace(ras, ras==0, NA)
  # area<-terra::expanse(ras2, unit="km") %>% as.numeric(.) %>% round(.)
  
  cell_presence<-global(ras, "sum", na.rm=T) %>% as.numeric(.)
  
  if(SCALE=="cci"){area<-cell_presence*0.09487}
  if(SCALE=="2x2"){area<-cell_presence*4}
  
  area<-ifelse(area<1, ceiling(area), round(area)) # I use ceiling to avoid having an AOH of 0 if some habitat is suitable
  
  return(area)
}





### AOH calculate for large range species
sRL_largeAOH<-function(alt_crop, habitats_pref, altitudes_pref, rangeSP_clean, YR, FILENAME){
  
  sRL_loginfo("Charge CCI_Large data", scientific_name)
  if(YR==config$YearAOH2){CCI_fun<-sRL_ChargeCciLargeRaster()} else {
    CCI_fun<-stackOpen(paste0(config$cciStack2_path, "/", YR, "/CCI_Stack_Agg30_Year", YR, ".stk"))
    names(CCI_fun)<-read.table(paste0(config$cciStack2_path, "/", YR, "/CCI_Stack_Agg30_Year", YR, ".stk"))[,1] %>% gsub(config$cciStack2_path, "", .) %>% substr(., 7, (nchar(.)-4)) # I have to rename them because update in raster package made a change in the names. I checked that the order of the rasters in Stack were the same as in the .stk file
  }
  sRL_loginfo("START - Large AOH function", scientific_name)
  
  # Select rasters to keep (I have to replace . by x in codes otherwise it's used as a joker in grepl; also inside the semicolumn to ensure this is the exact habitat and that 5.3 is not included in 15.3 for instance)
  CCI_to_keep<-NA
  for(i in 1:length(habitats_pref)){CCI_to_keep<-c(CCI_to_keep, crosswalkLARGE$Stack_Name[grepl(paste0(";", gsub("[.]", "x", habitats_pref[i]), ";"), gsub("[.]", "x", crosswalkLARGE$Codes))])  %>% unique(.) %>% .[is.na(.)==F] %>% sort(.)}
  CCI_suitable<-CCI_fun[[which(names(CCI_fun) %in% paste0("Agg30_CCI", YR, "_", CCI_to_keep))]]
  print("Suitable CCI groups:") ; print(names(CCI_suitable))
  
  # Crop and sum CCIs  
  print(extent(rangeSP_clean))
  print(dim(CCI_suitable))
  CCI_crop<-raster::crop(CCI_suitable, extent(rangeSP_clean), snap="out")
  print(dim(CCI_crop))
  #CCI_sum<-sum(CCI_crop, na.rm=T)
  CCI_sum <- raster::calc(CCI_crop, sum, na.rm=T)
  print(dim(CCI_sum))
  if(class(CCI_sum)[1] != "SpatRaster"){CCI_sum<-rast(CCI_sum)}

  # Crop altitude and calculate AOH
  alt_suitable<-(alt_crop > as.numeric(altitudes_pref[1]) & alt_crop<as.numeric(altitudes_pref[2]))
  if(class(alt_suitable)[1] != "SpatRaster"){alt_suitable<-rast(alt_suitable)}

  print(dim(alt_suitable))
  AOH <- (CCI_sum*alt_suitable) %>% terra::mask(., rangeSP_clean)
  print(dim(AOH))
  AOH <- replace(AOH, AOH>1000, NA) # NA is transformed as 100000 so I change everything above 1000 to NA (the max should always be 900)
  print(dim(AOH))
  
  # Save AOH
  print(FILENAME)
  if(FILENAME != ""){terra::writeRaster(AOH, filename=FILENAME, overwrite=T)}
  
  
  sRL_loginfo("END - Large AOH function", scientific_name)
  
  return(AOH)
}


### Validate AOH map with model and point prevalences
sRL_CalcAohPrevalence<-function(aoh, aoh_opt, type, points){
  
  ### Transform to percentages
  if(type=="Small"){aoh <- aoh*100 ; if(is.null(aoh_opt)==F){aoh_opt <- aoh_opt*100}}
  if(type=="Large"){aoh <- aoh/9 ; if(is.null(aoh_opt)==F){aoh_opt <- aoh_opt/9}}
  
  ### Calculate model prevalence (proportion of suitable grid cells inside the range map); works with mean for both small and large ranges
  MPrev1 <- round(global(aoh, "mean", na.rm=TRUE))
  if(is.null(aoh_opt)==F){MPrev2 <- round(global(aoh_opt, "mean", na.rm=TRUE))}
  
  Model_prevalence <- ifelse(
    exists("MPrev2"),
    paste0(MPrev1, "-", MPrev2, "%"),
    paste0(MPrev1, "%")
  )
  
  LIST_Prev <- list(Model_prevalence=Model_prevalence)
  
  ### Calculate point prevalence (proportion of point localities data falling inside the AoH map); works with mean for both small and large ranges
  if(is.null(points)==F){
    points <- st_as_sf(points)
    points$AOH <- extract(aoh, st_coordinates(points))[,1] %>% replace(., is.na(.), 0)
    if(is.null(aoh_opt)==F){points$AOH_opt <- extract(aoh_opt, st_coordinates(points))[,1] %>% replace(., is.na(.), 0)}
    
    Point_prevalence <- ifelse(
      is.null(aoh_opt),
      paste0(round(mean(points$AOH)), "%"),
      paste0(round(mean(points$AOH)), "-", round(mean(points$AOH_opt)), "%")
    )
    
    LIST_Prev$Point_prevalence=Point_prevalence
  }
  
  return(LIST_Prev)
}

### Log-scale for colour plot in Large Trends
colour_bidirect_scale <- trans_new("logpeps",
                             function(x) {sign(x)*sqrt(abs(x))},
                             function(x) {sign(x)*(abs(x)^2)}) 
