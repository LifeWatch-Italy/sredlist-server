

# Step 1 --------------------------------
sRL_FormatUploadedRecords <- function(Uploaded_Records, scientific_name, Gbif_Synonym){
  
  # Charge and deal with tab separator
  Uploaded_Records<-Uploaded_Records[[1]] 
  if(ncol(Uploaded_Records)==1){print("CSV with wrong separator with ; separator"); Uploaded_Records<-Uploaded_Records %>% separate(col=names(Uploaded_Records)[1], into=unlist(strsplit(names(Uploaded_Records), ";")), sep=";")}
  if(ncol(Uploaded_Records)==1){print("CSV with wrong separator with tab separator"); Uploaded_Records<-Uploaded_Records %>% separate(col=names(Uploaded_Records)[1], into=unlist(strsplit(names(Uploaded_Records), "\t")), sep="\t")}
  if(ncol(Uploaded_Records)==1){wrong_csv_upload()}
  
  # Check sci_name is provided (otherwise use scientific_name) and rename column + if only NA or empty, use scientific_name too
  if(!"sci_name" %in% names(Uploaded_Records)){
    names(Uploaded_Records)<-replace(names(Uploaded_Records), tolower(names(Uploaded_Records)) %in% c("sci_name", "binomial", "species", "scientific_name", "species_name"), "sci_name")
    if(! "sci_name" %in% names(Uploaded_Records)){Uploaded_Records$sci_name<-scientific_name}
  }
  Uploaded_Records$species_download<-Uploaded_Records$sci_name
  if(as.logical(table(factor(is.na(replace(Uploaded_Records$sci_name, Uploaded_Records$sci_name=="", NA)), c("TRUE", "FALSE")))["FALSE"]==0)){Uploaded_Records$sci_name<-scientific_name}
  
  # If another species name than scientific_name, return an error (I could deal with it but this might introduce errors that users won't sea)
  if(as.logical(table(factor(Uploaded_Records$sci_name %in% c(scientific_name, Gbif_Synonym, NA, ""), c("TRUE", "FALSE")))["FALSE"] >0)){wrong_species_upload()}
  
  # Assign Source to Upload (and Synonyms_Upload if synonym)
  Uploaded_Records$Source_type<-"Uploaded"
  if(Gbif_Synonym[1] != ""){
    Uploaded_Records$Source_type<-ifelse(Uploaded_Records$sci_name==scientific_name, "Uploaded", "Synonyms_Uploaded")
  }
  
  # Check longitude and latitude are provided
  if(! "dec_lat" %in% names(Uploaded_Records)){names(Uploaded_Records)<-replace(names(Uploaded_Records), tolower(names(Uploaded_Records)) %in% c("y", "dec_lat", "latitude", "lat"), "dec_lat")}
  if(! "dec_long" %in% names(Uploaded_Records)){names(Uploaded_Records)<-replace(names(Uploaded_Records), tolower(names(Uploaded_Records)) %in% c("x", "dec_long", "dec_lon", "longitude", "lon", "long"), "dec_long")}
  if((! "dec_long" %in% names(Uploaded_Records)) | (! "dec_lat" %in% names(Uploaded_Records))){no_coords_update()}
  Uploaded_Records$dec_long<-as.numeric(Uploaded_Records$dec_long)
  Uploaded_Records$dec_lat<-as.numeric(Uploaded_Records$dec_lat)
  Uploaded_Records<-subset(Uploaded_Records, is.na(Uploaded_Records$dec_long)==F & is.na(Uploaded_Records$dec_lat)==F)
  
  # Check they are within -180:180 and -90:90
  if(min(Uploaded_Records$dec_long)<(-180) |
     max(Uploaded_Records$dec_long)>(180) |
     min(Uploaded_Records$dec_lat)<(-90) |
     max(Uploaded_Records$dec_lat)>(90)){coords_outofbound()}
  
  # Make longitude and latitude numeric (includes a comma to point transformation for decimals)
  Uploaded_Records$dec_long<-Uploaded_Records$dec_long %>% sub(",", ".", .) %>% as.numeric()
  Uploaded_Records$dec_lat<-Uploaded_Records$dec_lat %>% sub(",", ".", .) %>% as.numeric()
  
  # Transform column name of year and make it numeric (if no column, I make it all NA)
  names(Uploaded_Records)<-replace(names(Uploaded_Records), tolower(names(Uploaded_Records)) %in% c("year", "event_year", "year_event"), "year")
  if(! "year" %in% names(Uploaded_Records)){Uploaded_Records$year<-NA}
  Uploaded_Records$year<-as.numeric(as.character(Uploaded_Records$year))

  # Return
  return(Uploaded_Records)
}







sRL_createDataGBIF <- function(scientific_name, GBIF_SRC, Uploaded_Records) { # nolint
  
  ### Download data
  # From GBIF
  Taxon<-name_backbone(name=scientific_name)
  if("SPECIES" %in% Taxon$rank[1]){TaxKey<-Taxon$usageKey} else {TaxKey<-NULL} # Needed to avoid a wrong species from an existing genus to download all occurrences from that genus
  if(GBIF_SRC[1]==1 & is.null(TaxKey)==F){
    
    #Calculate the total number of data in GBIF
    OCC<-occ_count(taxonKey=TaxKey, hasCoordinate = TRUE)
    
    if(OCC < config$LIM_GBIF){ # Download all data or structure download if more than LIM_GBIF
      dat_gbif <- rgbif::occ_data(scientificName=scientific_name, hasCoordinate = T, limit=config$LIM_GBIF)$data # occ_data is faster than occ_search because it omits some columns
      dat_gbif$Source_type<-"GBIF"
    } else{
      dat_gbif <- sRL_StructureGBIF(scientificName = scientific_name)
      dat_gbif$Source_type<-"GBIF sample"
    }
    
    dat_gbif$ID<-paste0(dat_gbif$decimalLongitude, dat_gbif$decimalLatitude, dat_gbif$year)
    dat_gbif$Link<-paste0("https://gbif.org/occurrence/", dat_gbif$gbifID)
    dat_gbif$species_download<-scientific_name
    
    # Extract citation (once per dataset then match)
    if(is.data.frame(dat_gbif)){
    dat_gbif$source<-NA
    datasets<-data.frame(Dataset=levels(as.factor(dat_gbif$datasetKey)), source=NA)
    for(i in 1:nrow(datasets)){
      datasets$source[i]<-as.character(unlist(gbif_citation(datasets$Dataset[i]))[2])
    }
    dat_gbif$source<-datasets$source[match(dat_gbif$datasetKey, datasets$Dataset)]} else {dat_gbif<-NULL}
    
    
  } else {dat_gbif<-NULL}
  
  # From OBIS (removing points at same location + year)
  if(GBIF_SRC[2]==1){
    dat_obis <- robis::occurrence(scientific_name)
    dat_obis$ID<-paste0(dat_obis$decimalLongitude, dat_obis$decimalLatitude, dat_obis$date_year)
    dat_obis$year<-dat_obis$date_year
    dat_obis_sub<-subset(dat_obis, !dat_obis$ID %in% dat_gbif$ID)
    dat_obis_sub$Source_type<-"OBIS"
    dat_obis_sub$source<-dat_obis_sub$bibliographicCitation
    dat_obis_sub$Link<-paste0("https://obis.org/taxon/", dat_obis_sub$aphiaID[1])
    dat_obis_sub$gbifID<-dat_obis_sub$occurrenceID
    dat_obis_sub$species_download<-scientific_name
    
    # If too many data, keep a sample (most recent)
    if(nrow(dat_obis_sub) > config$LIM_GBIF){
      dat_obis_sub$Source_type<-"OBIS sample"
      dat_obis_sub<-dat_obis_sub[order(dat_obis_sub$year, decreasing=T),]
      dat_obis_sub<-dat_obis_sub[1:config$LIM_GBIF,]
    }
  } else {dat_obis_sub<-data.frame()}
  
  # From Red List point
  if(GBIF_SRC[3]==1 & paste0(scientific_name, ".csv") %in% list.files(config$POINTdistribution_path)){
    dat_RL<-read.csv(paste0(config$POINTdistribution_path, scientific_name, ".csv"))
    dat_RL$Source_type<-"Red List"
    dat_RL$decimalLongitude<-dat_RL$longitude
    dat_RL$decimalLatitude<-dat_RL$latitude
    dat_RL$year<-dat_RL$event_year
    dat_RL$species<-dat_RL$binomial
    dat_RL$coordinateUncertaintyInMeters<-NA
    dat_RL$gbifID<-dat_RL$objectid
    dat_RL$Link<-NA
    dat_RL$species_download<-scientific_name
    names(dat_RL)<-replace(names(dat_RL), names(dat_RL)=="Source", "source")
    
    # If too many data, keep a sample (most recent)
    if(nrow(dat_RL) > config$LIM_GBIF){
      dat_RL$Source_type<-"Red List sample"
      dat_RL<-dat_RL[order(dat_RL$year, decreasing=T),]
      dat_RL<-dat_RL[1:config$LIM_GBIF,]
    }
  } else {dat_RL<-data.frame()}
  
  
  # From uploaded data
  if(!is.null(nrow(Uploaded_Records))){
    dat_upload<-Uploaded_Records
    dat_upload$decimalLongitude<-dat_upload$dec_long
    dat_upload$decimalLatitude<-dat_upload$dec_lat
    dat_upload$species<-dat_upload$sci_name
    dat_upload$coordinateUncertaintyInMeters<-NA
    dat_upload$gbifID<-paste0("Uploaded_", rownames(dat_upload))
    dat_upload$Link<-NA
    names(dat_upload)<-replace(names(dat_upload), names(dat_upload)=="Source", "source")
    
    # If too many data (with a higher threshold), keep a sample 
    if(nrow(dat_upload) > 3*config$LIM_GBIF){
      dat_upload$Source_type<-"Uploaded sample"
      dat_upload<-dat_upload[1:(3*config$LIM_GBIF),]
    }
  } else {dat_upload<-data.frame()}

  
  # Return empty df if no records
  if(is.null(nrow(dat_gbif)) & nrow(dat_obis_sub)==0 & nrow(dat_RL)==0  & nrow(dat_upload)==0) {
    dat<-data.frame()
  } else {
    # Merge otherwise
    dat<-rbind.fill(dat_gbif, dat_obis_sub, dat_RL, dat_upload)
    print(paste0("Number of data: ", nrow(dat)))
    
    # Select columns of interest
    dat <- dat %>%
      dplyr::select(any_of(c("species", "species_download", "decimalLongitude", "decimalLatitude", "countryCode", "individualCount", # nolint
                             "gbifID", "id", "objectid", "family", "taxonRank", "coordinateUncertaintyInMeters", "year",
                             "basisOfRecord", "institutionCode", "datasetName", "genericName", "specificEpithet", "Source_type", "source", "citation", "Link", "presence")))
    
    # Remove records with no spatial coordinates
    dat <- dat %>% filter(!is.na(decimalLongitude)) %>% filter(!is.na(decimalLatitude)) # nolint
    
    # Convert country code from ISO2c to ISO3c
    if(!"countryCode" %in% names(dat)){dat$countryCode<-NA} else{
      dat$countryCode <-  countrycode::countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')} # nolint
    dat <- data.frame(dat)
  }
  
  return(dat)
}




### Function to download GBIF in a spatially structured manner (to avoid big sampling biases)
sRL_StructureGBIF<-function(scientificName){
  
  ##### DEFINE THE SAMPLING PATTERN
  ### Map density of observations and extract coordinates
  Fetch<-mvt_fetch(taxonKey = name_backbone(name=scientificName)$usageKey, srs = "EPSG:4326", format="@4x.png") 
  coords<-as.data.frame(st_coordinates(Fetch))
  coords$tot<-Fetch$total
  
  ### Extract extent to define grid size
  DeltaX<-max(coords$X, na.rm=T)-min(coords$X, na.rm=T)
  DeltaY<-max(coords$Y, na.rm=T)-min(coords$Y, na.rm=T)
  
  ### Fix cell size
  # Maximum size cell in degrees
  Max_Cell=10 
  
  # If extent is small, cut the grid in ca. 100 square cells
  if(max(DeltaX, DeltaY) < (10*Max_Cell)){
    NX<- round(10*DeltaX / sqrt(DeltaX*DeltaY)) # Calculates the number of cells to have in one row so that we end up with ca. 100 square cells
    NY<- round(10*DeltaY / sqrt(DeltaX*DeltaY))
    Lon_breaks<-seq((min(coords$X, na.rm=T)-1), max(coords$X, na.rm=T), length.out=(NX+1))
    Lat_breaks<-seq((min(coords$Y, na.rm=T)-1), max(coords$Y, na.rm=T), length.out=(NY+1))
    
    # If extent is large, cut the grid in Max_Cell square cells
  } else{
    Lon_breaks<-seq((min(coords$X, na.rm=T)-1), max(coords$X, na.rm=T)+1, length.out=ceiling(DeltaX/Max_Cell))
    Lat_breaks<-seq((min(coords$Y, na.rm=T)-1), max(coords$Y, na.rm=T)+1, length.out=ceiling(DeltaY/Max_Cell))
  }
  
  ### Cut density lon/lat and create group names
  coords$Lon_group<-coords$X %>% cut(., breaks=Lon_breaks, labels=paste0("X", 1:(length(Lon_breaks)-1)))
  coords$Lat_group<-coords$Y %>% cut(., breaks=Lat_breaks, labels=paste0("Y", 1:(length(Lat_breaks)-1)))
  coords$Group<-paste(coords$Lon_group, coords$Lat_group, sep="/")
  
  
  
  ##### CREATE DOWNLOAD TABLE
  TAB<-ddply(coords, .(Lon_group, Lat_group), function(x){data.frame(
    N=sum(x$tot, na.rm=T),
    Group=paste0(x$Lon_group[1], x$Lat_group[1])
  )}) %>% subset(., .$N>0)
  
  # Extract coordinates (min and max) from Group names and cuts of lon/lat and add in TAB
  eval(parse(text=paste("TAB$Lon_min<-revalue(TAB$Lon_group, c(", paste0("'X", 1:(length(Lon_breaks)-1), "'=Lon_breaks[", 1:(length(Lon_breaks)-1), "]", collapse=","), ")) %>% as.character(.) %>% as.numeric(.) %>% replace(., .<(-180), (-180))")))
  eval(parse(text=paste("TAB$Lon_max<-revalue(TAB$Lon_group, c(", paste0("'X", 1:(length(Lon_breaks)-1), "'=Lon_breaks[", 2:length(Lon_breaks), "]", collapse=","), ")) %>% as.character(.) %>% as.numeric(.) %>% replace(., .>180, 180)")))
  eval(parse(text=paste("TAB$Lat_min<-revalue(TAB$Lat_group, c(", paste0("'Y", 1:(length(Lat_breaks)-1), "'=Lat_breaks[", 1:(length(Lat_breaks)-1), "]", collapse=","), ")) %>% as.character(.) %>% as.numeric(.) %>% replace(., .<(-90), (-90))")))
  eval(parse(text=paste("TAB$Lat_max<-revalue(TAB$Lat_group, c(", paste0("'Y", 1:(length(Lat_breaks)-1), "'=Lat_breaks[", 2:length(Lat_breaks), "]", collapse=","), ")) %>% as.character(.) %>% as.numeric(.) %>% replace(., .>90, 90)")))
  
  # Determine number of data to download per group to sum at LIM_GBIF
  TAB$N_download<-ifelse(TAB$N < (config$LIM_GBIF/nrow(TAB)), TAB$N, NA)
  TAB$N_download[is.na(TAB$N_download)]<-round((config$LIM_GBIF-sum(TAB$N_download, na.rm=T))/nrow(TAB[is.na(TAB$N_download),]))
  TAB$N_download<-ifelse((TAB$N_download>TAB$N), TAB$N, TAB$N_download)
  i=0
  while(i<config$LIM_GBIF){
    TAB$N_download[TAB$N_download<TAB$N]<-TAB$N_download[TAB$N_download<TAB$N]+1
    i=sum(TAB$N_download)
  }

  
  ##### STRUCTURE DOWNLOAD
  # Download one data (just for column names)
  dat_structured<-rgbif::occ_data(scientificName=scientificName, hasCoordinate = T, limit=1)$data 
  
  # Download group per group
  for(GR in 1:nrow(TAB)){
    dat_GR<-rgbif::occ_data(scientificName=scientificName,
                              hasCoordinate = T, 
                              limit=TAB$N_download[GR], 
                              decimalLongitude=paste(TAB$Lon_min[GR], TAB$Lon_max[GR], sep=","), 
                              decimalLatitude=paste(TAB$Lat_min[GR], TAB$Lat_max[GR], sep=",")
    )$data 
    
    if(is.null(nrow(dat_GR))==F){dat_structured<-rbind.fill(dat_structured, dat_GR)}
  }
  
  # Merge
  dat_structured<-dat_structured[2:nrow(dat_structured),]
  
  return(dat_structured)
}


# Step 2 --------------------------------
sRL_cleanDataGBIF <- function(flags, year_GBIF, uncertainty_GBIF, Gbif_yearBin, Gbif_uncertainBin, sea_GBIF, GBIF_xmin, GBIF_xmax, GBIF_ymin, GBIF_ymax) { # nolint

  flags$.summary<-NULL
  
  ### Change flagging for sea
  if(sea_GBIF==""){flags$.sea<-NULL} # If Sea=0, we should not select data based on sea
  if(sea_GBIF=="excludeland"){flags$.sea<-revalue(as.character(flags$.sea), c("TRUE"="FALSE", "FALSE"="TRUE"))} %>% as.factor(.) # If Sea=2, we keep only seas (so we flag land)
  
  ### Add flagging for year and uncertainty
  flags$year<-replace(flags$year, flags$year==0, NA)
  flags$.year <- flags$year > year_GBIF
  if(Gbif_yearBin==T){flags$.year[is.na(flags$.year)]<-F} # If 'remove NA' is clicked, I put False for the year filter
  
  if(!"coordinateUncertaintyInMeters" %in% names(flags)){flags$coordinateUncertaintyInMeters<-NA}
  flags$.uncertainty <- flags$coordinateUncertaintyInMeters < uncertainty_GBIF*1000 
  if(Gbif_uncertainBin==T){flags$.uncertainty[is.na(flags$.uncertainty)]<-F} # If 'remove NA' is clicked, I put False for the uncertainty filter
  
  ### Add flagging for points outside GBIF_xmin...
  flags$.limits<-(flags$decimalLongitude < GBIF_xmin | flags$decimalLongitude > GBIF_xmax | flags$decimalLatitude < GBIF_ymin | flags$decimalLatitude > GBIF_ymax)==F
  
  ### Flag points with presence 4,5,6 (for uploaded or RL points)
  if("presence" %in% names(flags)){flags$.pres<-as.character(! flags$presence %in% c("4", "5", "6"))}
  
  ### Add a column with pasted reasons
  flags$Reason<-apply(
    flags[, which(substr(names(flags), 1, 1)==".")], # Apply to the columns that start with a dot (i.e., the column added by clean_coordinates)
    1, # Apply by row
    function(x){
      Reas<-names(x)[x==F] %>% .[is.na(.)==F] # Extract all reasons, i.e., column names where valid==F
      if(length(Reas)==0){NA} else{ # If no reason, I put NA as the reason
        Reas %>% # If there is a reason I rename and paste the reasons
          revalue(., c(".val"="Validity", ".equ"="Equal_LonLat", ".zer"="Zero_Coordinates", ".cap"="Capitals", ".cen"="Country_centroids", ".gbf"="GBIF_headquarters", ".inst"="Institutions", ".sea"="Sea", ".year"="Year", ".uncertainty"="Coordinates_uncertainty", ".limits"="Outside_extent", ".pres"="Presence_456"), warn_missing=F) %>%
          paste(collapse="; ")
      }}
  )
  
  ### Prepare the box popup for the leaflet map
  # Popup text to have only if there are some synonyms
  flags$Only_for_syn<-"" ; if("TRUE" %in% grepl("Synonyms_", flags$Source_type)){flags$Only_for_syn<-paste0("<b>","Species: ","</b>", flags$species_download, "<br>")}
  
  # Create popup text
  flags$PopText<- paste0("<b>", revalue(as.character(is.na(flags$Reason)), c("TRUE"="VALID OBSERVATION", "FALSE"="NOT VALID OBSERVATION")),"</b>", "<br>", "<br>",
                       "<b>","Source: ","</b>", flags$Source_type, "<br>",
                       flags$Only_for_syn,
                       "<b>","Observation ID: ","</b>", ifelse(is.na(flags$Link)==F, paste0("<a href='", flags$Link, "' target='_blank'>", flags$gbifID, "</a>"), flags$gbifID), "<br>",
                       "<b>","Year: ","</b>", flags$year, "<br>",
                       "<b>","Uncertainty (km): ","</b>", as.numeric(as.character(flags$coordinateUncertaintyInMeters))/1000, "<br>")
  flags$PopText[is.na(flags$Reason)==F]<-paste0(flags$PopText[is.na(flags$Reason)==F], "<b>","Reason flagged: ","</b>", flags$Reason[is.na(flags$Reason)==F], "<br>")
  
  return(flags)
  
}





### Filter GBIF data
sRL_SubsetGbif<-function(flags, scientific_name){
  
  # Round coordinates (<1m change); needed to avoid having almost duplicate points in alpha function
  flags$decimalLongitude<-round(flags$decimalLongitude,5) 
  flags$decimalLatitude<-round(flags$decimalLatitude,5)
  
  # Prepare GBIF data for mapping
  dat_cl <- flags[is.na(flags$Reason)==T,] # Keep only data that are not flagged

  # Prepare spatial points
  dat_proj<-st_as_sf(dat_cl,coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84") %>%
    st_transform(., st_crs(CRSMOLL)) 
  
  return(dat_proj)
  
}



# Step 3 --------------------------------
sRL_MapDistributionGBIF<-function(dat, scientific_name, First_step, AltMIN, AltMAX, Buffer_km, GBIF_crop, Gbif_Param){

  ### The first step can be mcp, kernel, alpha, hydro, hydroMCP
  if(First_step=="mcp" | First_step==""){
    distGBIF<-st_as_sf(st_convex_hull(st_union(dat)))
    st_geometry(distGBIF)<-"geometry" # Rename the variable including geometry
  }
  
  if(First_step=="kernel"){
    # Remove duplicate points (points with same lon/lat)
    dat_subsample<-distinct(dat, as.character(geometry), .keep_all=T) 

    # Calculate kernels
    kernel.ref <- kernelUD(as_Spatial(dat), h = "href")  # href = the reference bandwidth
    distGBIF <- getverticeshr(kernel.ref, percent = 100*Gbif_Param[2]) %>% st_as_sf(.)
  }
  
  if(First_step=="alpha"){
      # Remove records that are too close in order to avoid convexHull to crash
      Round_Fact <- ifelse(as.numeric(max(st_distance(dat)))<10000, -1, -2) # Depending on the max distance between records I round to 100, or 10m 
      Coords_simplif<-st_coordinates(dat) %>% base::round(., digits=Round_Fact)
      dat$Coord_simplif<-paste(Coords_simplif[,1], Coords_simplif[,2], sep=":")
      dat_subsample<-dplyr::distinct(dat, Coord_simplif, .keep_all=T) 
    
      Par_alpha<-Gbif_Param[1]
      EX<-extent(dat_subsample)
      tryCatch({
        Alpha_scaled <- (0.5*Par_alpha)^2 * sqrt((EX@xmin-EX@xmax)^2 + (EX@ymin-EX@ymax)^2) %>% as.numeric(.)
        distGBIF<-spatialEco:::convexHull(dat_subsample, alpha = Alpha_scaled) # concaveman::concaveman function could work as well with something similar, but not proper alpha-hull
      } ,error=function(e){bug_alpha()})
      
      st_crs(distGBIF)<-st_crs(dat_subsample)
      
  }
  
  if(First_step=="indivsites"){
    distGBIF<-st_buffer(dat, 1) # The default is one meter, then they can add a buffer
  }
  
  if(First_step %in% c("hydro8", "hydro10", "hydro12")){
    
    # Extract level 8 in any case
    hydro8_sub<-st_crop(hydro_raw, extent(dat))
    interHyd<-st_join(dat, hydro8_sub, join=st_intersects) %>% subset(., is.na(.$hybas_id)==F) # Identify hydrobasins with data 
    distGBIF<-subset(hydro_raw, hydro_raw$hybas_id %in% interHyd$hybas_id) # Isolate these hydrobasins
    if(nrow(distGBIF)==0){no_hydrobasins()}
    
    # Extract level 10 or 12 if requested and possible (i.e., small distribution)
    if(First_step %in% c("hydro10", "hydro12")){
      
      # Return an error if too large distribution
      if(nrow(distGBIF)>10){hydro_too_large()}
      
      LEV<-ifelse(First_step=="hydro10", 10, 12)
      
      # List files to load
      Cells<-distGBIF$Grid_cells %>% paste0(., collapse="") %>% strsplit(., ",") %>% unlist(.) %>% unique(.) %>% subset(., . !="") %>% as.numeric(.)
      
      # Load shapefile of LEVEL
      Path_cells<-paste0("Hydro_cut_", LEV, "/Hydrobasins_level", LEV, "_cut_cell", Cells, ".shp") %>% paste0(sub("Hydrobasins_level8_ready.shp", "", config$hydrobasins_path), .)
      hydroLEV_raw<-st_read(Path_cells[1])
      if(length(Path_cells)>1){
        for(PATH in Path_cells[2:length(Path_cells)]){
          hydroLEV_toadd<- st_read(PATH)
          hydroLEV_raw<-rbind(hydroLEV_raw, hydroLEV_toadd)
        }
      }
      st_crs(hydroLEV_raw)<-CRSMOLL
      
      # Create distribution
      hydroLEV_sub<-st_crop(hydroLEV_raw, extent(dat))
      interHydLEV<-st_join(dat, hydroLEV_sub, join=st_intersects) %>% subset(., is.na(.$hybas_id)==F) # Identify hydrobasins with data 
      distGBIF<-subset(hydroLEV_raw, hydroLEV_raw$hybas_id %in% interHydLEV$hybas_id) # Isolate these hydrobasins
        
    }
    
  }
  
  if(First_step=="hydroMCP"){
    mcp<-st_as_sf(st_convex_hull(st_union(dat)))
    st_geometry(mcp)<-"geometry"
    hydro_sub<-st_crop(hydro_raw, extent(mcp))
    interHyd<-st_join(mcp, hydro_sub, join=st_intersects) %>% subset(., is.na(.$hybas_id)==F) # Identify hydrobasins with data 
    distGBIF<-subset(hydro_raw, hydro_raw$hybas_id %in% interHyd$hybas_id) # Isolate these hydrobasins
    if(nrow(distGBIF)==0){no_hydrobasins()}
  }
  
  ### Apply buffer
  distGBIF<-st_buffer(distGBIF, Buffer_km*1000) %>% st_as_sf()
  
  
  ### Apply crop by land/sea
  distGBIF$binomial=scientific_name
  
  # Create countries map based on the buffer
  if(GBIF_crop %in% c("cropland", "cropsea")){
    CountrySP<-st_crop(distCountries_mapping, 1.1*extent(distGBIF))
  
    # Remove land or sea if requested
    if(GBIF_crop=="cropland"){
      distGBIF<-st_intersection(distGBIF, CountrySP) %>% 
        dplyr::group_by(binomial) %>% dplyr::summarise(N = n())}

    if(GBIF_crop=="cropsea"){
      countr<-CountrySP %>% st_crop(., extent(distGBIF)) %>% dplyr::group_by() %>% dplyr::summarise(N = n())
      distGBIF<-st_difference(distGBIF, countr)}
  }
  
  ### Merge
  distGBIF<-distGBIF %>% dplyr::group_by(binomial) %>% dplyr::summarise(N = n())
  
  ### Apply crop by altitude
  if(AltMIN>0 | AltMAX<9000){
  mcp.spatial <- as_Spatial(distGBIF)
  sp.mcp.terra <- terra::vect(distGBIF)
  
  alt_raw<-sRL_ChargeAltRaster()
  dem.crop <- terra::crop(alt_raw, ext(mcp.spatial), snap="out")
  
  sp.mcp.ras <- terra::rasterize(sp.mcp.terra, dem.crop)
  dem.sp <- terra::mask(dem.crop, mask = sp.mcp.terra)
  
  m <- c(-Inf, AltMIN, 0, AltMIN, 
         AltMAX, 1, AltMAX, Inf, 0)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  
  sp.range <- terra::classify(dem.sp, rclmat) %>% 
    terra::aggregate(fact = 4, fun = 'max')
  
  sp.range[sp.range == 0] <- NA
  
  distGBIF <- as.polygons(sp.range) %>% st_as_sf(.)
  }
  
  ### Prepare to export
  distGBIF$binomial<-scientific_name
  distGBIF$id_no<-sRL_CalcIdno(scientific_name)
  distGBIF$presence<-1
  distGBIF$origin<-1
  distGBIF$seasonal<-1
  if(exists("Alpha_scaled")){distGBIF$alphaTEMPO<-Alpha_scaled} # Save alpha scaled if I use alpha hull
  if(substr(First_step, 1,5)=="hydro"){distGBIF$hybas_concat<-paste0(unique(interHyd$hybas_id), collapse=",")}
  
  return(distGBIF)
  
}


### Save distribution mapped from the GBIF procedure
sRL_saveMapDistribution <- function(scientific_name, Storage_SP) {
  
  ### Create a file path
  upload_folder_scientific_name <- R.utils::capitalize(paste0(stringr::str_replace(scientific_name, " ", "_"), format(Sys.time(), "_Created_%Y%m%d"))) # nolint
  filePath <- paste0(config$distribution_path, scientific_name, "/", upload_folder_scientific_name, "/") # nolint
  if (dir.exists(filePath)) {
    print("The directory exists")
  } else {
    # create the "my_new_folder
    dir.create(filePath, showWarnings = TRUE, recursive = TRUE)
    #dir.create(filePath)
  }
  path <- paste0(filePath, upload_folder_scientific_name, ".shp")
  st_write(Storage_SP$distSP_saved, path, append = FALSE)
  
  
  print("Write metadata file")
  text <- paste0(
    "A distribution has been stored for the species: ",
    scientific_name,
    ".\nIt was created with the mapping procedure from the sRedList platform. It is based on ", # nolint
    paste(names(table(Storage_SP$dat_proj_saved$Source_type)), table(Storage_SP$dat_proj_saved$Source_type), sep=" (") %>% paste(., collapse="), ") %>% paste0(nrow(Storage_SP$dat_proj_saved), " raw geo-referenced observations from: ", ., ")"),
    " occurrence records gathered on the ",
    Sys.time(),
    " CET."
  )
  jsonlite::write_json(list(info = text), paste0(filePath, upload_folder_scientific_name, ".json"), auto_unbox= TRUE) # nolint
  return(upload_folder_scientific_name)
}



# COO --------------------
sRL_cooExtract<-function(distSP, domain_pref, Crop_Country){
  
  ### Prepare COO terrestrial and freshwater
  if("Terrestrial" %in% domain_pref | "Freshwater" %in% domain_pref){
    
    # Extract list of countries
    coo<-coo_raw
    inter<-st_intersects(distSP, coo) %>% as.data.frame()
    coo$Dist_row<-inter$row.id[match(rownames(coo), inter$col.id)]
    coo$presence<-distSP$presence[match(coo$Dist_row, rownames(distSP))]
    coo$origin<-distSP$origin[match(coo$Dist_row, rownames(distSP))]
    coo$seasonal<-distSP$seasonal[match(coo$Dist_row, rownames(distSP))]
    
    # Prepare plot attributed
    coo$Level0_occupied<-NA ; for(i in 1:nrow(coo)){coo$Level0_occupied[i]<-max(c(0,coo$presence[coo$SIS_name0==coo$SIS_name0[i]]), na.rm=T)>=1}
    coo$Level1_occupied<-is.na(coo$Dist_row)==F
    
    coo$Popup<-paste0("<b> National entity: ","</b>", coo$SIS_name0, ifelse(coo$Level0_occupied==T, " (Present)", " (Absent)"), "<br>", "<br>",
                      "<b> Subnational entity: ","</b>", coo$SIS_name1, ifelse(is.na(coo$SIS_name1)==T, "", ifelse(coo$Level1_occupied==T, " (Present)", " (Absent)")), "<br>")
    
    coo$Domain<-"Terrestrial"
  }
  
  
  ### Prepare COO marine
  if("Marine" %in% domain_pref){
    
    # Extract list of countries
    eez<-eez_raw
    inter<-st_intersects(distSP, eez) %>% as.data.frame()
    eez$Dist_row<-inter$row.id[match(rownames(eez), inter$col.id)]
    eez$presence<-distSP$presence[match(eez$Dist_row, rownames(distSP))]
    eez$origin<-distSP$origin[match(eez$Dist_row, rownames(distSP))]
    eez$seasonal<-distSP$seasonal[match(eez$Dist_row, rownames(distSP))]
    
    
    # Prepare plot attributes
    eez$Level0_occupied<-NA ; for(i in 1:nrow(eez)){eez$Level0_occupied[i]<-max(c(0,eez$presence[eez$SIS_name0==eez$SIS_name0[i]]), na.rm=T)>=1}
    eez$Level1_occupied<-is.na(eez$Dist_row)==F
    
    eez$Popup<-paste0("<b>", "Marine EEZ","</b>", "<br>", "<br>",
                      "<b> National entity: ","</b>", eez$SIS_name0, ifelse(eez$Level0_occupied==T, " (Present)", " (Absent)"), "<br>", "<br>",
                      "<b> Subnational entity: ","</b>", eez$SIS_name1, ifelse(is.na(eez$SIS_name1)==T, "", ifelse(eez$Level1_occupied==T, " (Present)", " (Absent)")), "<br>")
    
    eez$Domain="Marine"
  }
  
  
  ### Merge
  if(!"Marine" %in% domain_pref){COO_merged<-coo}
  if("Marine" %in% domain_pref & length(domain_pref)==1){COO_merged<-eez}
  if("Marine" %in% domain_pref & ("Terrestrial" %in% domain_pref | "Freshwater" %in% domain_pref)){COO_merged<-rbind(coo, eez)}
  
  
  ### Subset if National / Regional Red Listing
  if(Crop_Country[1] != ""){
    Crop_Country1<-c()
    if(Crop_Country[1]=="Europe"){Crop_Country<-sRL_EuropeList; Crop_Country1<-sRL_EuropeList1}
    if(Crop_Country[1]=="EU27"){Crop_Country<-sRL_EU27List}
    
    COO_merged$Level0_occupied[(COO_merged$SIS_name0 %in% Crop_Country | COO_merged$SIS_name1 %in% Crop_Country1)==F]<-FALSE
    COO_merged$Level1_occupied[(COO_merged$SIS_name0 %in% Crop_Country | COO_merged$SIS_name1 %in% Crop_Country1)==F]<-FALSE
  }
  
    
  ### Return
  return(COO_merged)  
}




### Function to crop a country for National Red Listing
sRL_EuropeList<-c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova, Republic of", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom of Great Britain and Northern Ireland")
sRL_EuropeList1<-c("European Russia", "Türkiye-in-Europe") # For Europe I need to include Eastern Russia and Eastern Turkey, I list them in Crop_Country1 which is empty for non European National assessments
sRL_EU27List<-c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

sRL_CropCountry<-function(distSP, domain_pref, Crop_Country){

  # Europe
  Crop_Country1<-c()
  if(Crop_Country[1]=="Europe"){
    Crop_Country<-sRL_EuropeList
    Crop_Country1<-sRL_EuropeList1
  }
  if(Crop_Country[1]=="EU27"){
    Crop_Country<-sRL_EU27List
  }
  
  
  # Select countries depending on domain preferences
  if("Marine" %in% domain_pref){eez_Sub<-subset(eez_raw, eez_raw$SIS_name0 %in% Crop_Country | eez_raw$SIS_name1 %in% Crop_Country1) %>% st_transform(., CRSMOLL)}
  if("Terrestrial" %in% domain_pref | "Freshwater" %in% domain_pref){cou_Sub<-subset(coo_raw, (coo_raw$SIS_name0 %in% Crop_Country) | (coo_raw$SIS_name1 %in% Crop_Country1)) %>% st_transform(., CRSMOLL)}
  
  if("Marine" %in% domain_pref & length(domain_pref)==1){country_sub<-eez_Sub}
  if(!"Marine" %in% domain_pref){country_sub<-cou_Sub}
  if("Marine" %in% domain_pref & ("Terrestrial" %in% domain_pref | "Freshwater" %in% domain_pref)){
    cou_Sub$Aire<-cou_Sub$Tol<-NULL
    country_sub<-rbind(eez_Sub, cou_Sub)
  }
  country_sub<-country_sub %>% dplyr::group_by() %>% dplyr::summarise(N= n()) 
  

  
  # Crop the distribution
  distSP_crop<-st_intersection(distSP, country_sub)

  # Return
  return(distSP_crop)
}



### Functions to prepare results of COO analysis (the first one prepares and I keep coo_occ for final report, the second creates the infobox)
sRL_cooInfoBox_prepare<-function(coo, Storage_SP){
  
  # Subset coo with presence + if needed I group by (needed when marine + terrestrial since there are 2 polygons, one with occurrence and one without)
  coo_occ<-subset(coo, coo$Level1_occupied==T) 
  if(nlevels(as.factor(paste0(coo_occ$SIS_name0, coo_occ$SIS_name1))) < nrow(coo_occ)){coo_occ <- coo_occ %>% dplyr::group_by(SIS_name0, SIS_name1, lookup, lookup_SIS0) %>% dplyr::summarise(N= n())}
  
  # If occurrences, check which entities have occurrence records
  if("dat_proj_saved" %in% names(Storage_SP)){
    dat_proj<-Storage_SP$dat_proj_saved %>% st_transform(., st_crs(coo_occ))
    coo_occ$Records<-st_intersects(coo_occ, dat_proj) %>% lengths(.)>0
    coo_occ$SIS_name0[coo_occ$Records==F & is.na(coo_occ$SIS_name1)] <- paste0("<i>", coo_occ$SIS_name0[coo_occ$Records==F & is.na(coo_occ$SIS_name1)], "</i>") # Italicise countries with no occurrence records and that are not split in subnational entities
    coo_occ$SIS_name1[coo_occ$Records==F & is.na(coo_occ$SIS_name1)==F] <- paste0("<i>", coo_occ$SIS_name1[coo_occ$Records==F & is.na(coo_occ$SIS_name1)==F], "</i>") # Italicise subnational entities with no occurrence records
    coo_occ$SIS_name0[coo_occ$Records==F & is.na(coo_occ$SIS_name1)==F] <- paste0("<i>", coo_occ$SIS_name0[coo_occ$Records==F & is.na(coo_occ$SIS_name1)==F], "</i>") # Italicise country if subnational entity has no occurrence records (I later keep the non-italics name if some entities are not in italics)
  }
  
  return(coo_occ)
}

sRL_cooInfoBox_format<-function(coo_occ){
  # Extract list of COO for info.box
  RES<-NULL
  for(C in levels(droplevels(as.factor(coo_occ$lookup_SIS0)))){
    NAM<-coo_raw$SIS_name0[coo_raw$lookup_SIS0==C][1]
    Country<-subset(coo_occ, coo_occ$lookup_SIS0==C) # For each Country (level 0)
    if(nrow(Country)==1 & is.na(Country$SIS_name1[1])){ # I take the name of the country if there is a single entity + that entity is not named in Level1
      RES[length(RES)+1]<-Country$SIS_name0
    }else { # Otherwise, I take the name of the country and all subnational entities in brackets, and write name0 in italics if all name1 are in italics
      Co_name<-ifelse((sum(grepl("<i>", Country$SIS_name0))==nrow(Country)), paste0("<i>", NAM, "</i>"), NAM)
      CO1s<-Country$SIS_name1[order(gsub("<i>", "", Country$SIS_name1))] %>% .[is.na(.)==F] # I remove the NAs only here (should not be done before in case countries have subnational in terrestrial but not in marine, so the species can be present in the country but not in any of the subnational entities)
      RES[length(RES)+1]<-paste0(Co_name, " [", paste(CO1s, collapse=", "), "]")
    }
  }
  
  RES_format<-RES %>% .[order(gsub("<i>", "", .))] %>% paste(., collapse="; ")
  
  return(RES_format)
}


sRL_cooInfoBox_create<-function(RES, Realms){
  
  # Create info box
  info.box <- HTML(paste0(
    HTML('<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'),
    
    HTML(paste0('<h4>Realms: </h4>',
                paste0(unlist(strsplit(Realms, "[|]")), collapse=", "),
                '<br><br>',
                '<h4>List of countries of occurrence:</h4> <p>',
                RES,
                '</p>',
                ifelse(grepl("</i>", paste0(RES, collapse=".")), "<br><i> Entities in italics overlap with the polygon distribution but not with occurrence records (they will be included in the SIS output as 'Possibly Extant') </i>", ''),
                '<hr>'))
  ))
  
  # Return
  return(info.box)
  
}
