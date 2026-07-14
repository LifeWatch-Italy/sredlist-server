
# Merge distribution (for the quantitative analyses)
sRL_MergeDistri <- function(distSP){
  
  # Prepare columns
  distSP$presence <- distSP$origin <- distSP$seasonal <- 1
  distSP$cols <- NULL
  if(! "id_no" %in% names(distSP)){distSP$id_no <- sRL_CalcIdno(distSP$binomial[1])}
  
  # Make valid if it's not
  if("FALSE" %in% as.character(st_is_valid(distSP))){distSP <- st_make_valid(distSP)}
  
  # Merge
  distSP_merged <- distSP %>% dplyr::group_by(binomial, id_no, presence, origin, seasonal) %>% dplyr::summarise(.groups="keep") %>% ungroup(.) %>% st_cast("MULTIPOLYGON") %>% st_as_sf(.)
  
  return(distSP_merged)
}


# Create leaflet comparing distribution and GBIF occurrences (1a), first function to prepare data, second function for leaflet
sRL_prepareDataComparison <- function(flags, distSP){
  
  # Project flags and distri
  flags <- st_as_sf(flags, coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84")
  flags$decimalLongitude <- st_coordinates(flags)[,1]
  flags$decimalLatitude <- st_coordinates(flags)[,2]
  distSP <- st_transform(distSP, 4326)
  
  # Extract if record in distribution or not
  flags$InDistri <- is.na(st_join(flags, distSP, join=st_intersects)$presence)==F
  flags$ValidDistri <- ifelse(is.na(flags$Reason), ifelse(flags$InDistri, "In", "Out"), "Invalid")
  
  # Update popup
  flags$PopText <- flags$PopText %>% gsub("<b>NOT VALID OBSERVATION</b>", "", .) %>% gsub("<b>VALID OBSERVATION</b>", "", .) %>% paste0("<b>", revalue(flags$ValidDistri, c("In"="Inside distribution", "Out"="Outside distribution", "Invalid"="Invalid record")), "</b>", .)
  
  return(flags)
}

sRL_LeafletComparison <- function(flags, distSP, Comparison_result){
  
  distSP <- st_transform(distSP, 4326)
  
  # Prepare map
  Leaf <- leaflet(flags) %>%
    addTiles(group="OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("OpenTopoMap", group = "Topography") %>%
    addPolygons(data=distSP, color=distSP$cols, fillColor=distSP$cols, stroke=F, weight=2, fillOpacity=0.7) %>%
    addCircleMarkers(lng=flags$decimalLongitude,
                     lat=flags$decimalLatitude,
                     color=revalue(flags$ValidDistri, c("In"="#fdcb25ff", "Out"="#EA5F94", "Invalid"="#440154ff")),
                     fillOpacity=0.5,
                     stroke=F,
                     popup=flags$PopText,
                     radius=8,
                     group="Occurrence records") %>%
    addLegend(position="bottomleft", colors=c('#fdcb25ff', '#EA5F94', '#440154ff'), labels=c("Inside distribution", "Outside distribution", "Invalid record")) %>%
    addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), overlayGroups="Occurrence records", position="topleft") %>%
    addMouseCoordinates() %>%
    addScaleBar(position = "bottomright")
  
  # Add title
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"))
  
  TitleProportion <- tags$div(
    tag.map.title, HTML(Comparison_result)
  )
  
  Leaf <- Leaf %>%
    addControl(TitleProportion, position = "topleft", className="map-title")
  
  return(Leaf)
}


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
  
  # If another species name than scientific_name, return an error (I could deal with it but this might introduce errors that users won't see)
  NChar <- nchar(Uploaded_Records$sci_name)
  Uploaded_Records$sci_name <- ifelse(substr(Uploaded_Records$sci_name, NChar, NChar)==" ", substr(Uploaded_Records$sci_name, 1, NChar-1), Uploaded_Records$sci_name) 
  Wrong_SpName <- which(! Uploaded_Records$sci_name == scientific_name)
  if(length(Wrong_SpName)>0){wrong_species_upload(Wrong_SpName)}
  
  # Assign Source to Upload (and Synonyms_Upload if synonym)
  Uploaded_Records$Source_type<-"Uploaded"
  if(Gbif_Synonym[1] != ""){
    Uploaded_Records$Source_type<-ifelse(Uploaded_Records$sci_name==scientific_name, "Uploaded", "Synonyms_Uploaded")
  }
  
  # Check longitude and latitude are provided
  if(! "dec_lat" %in% names(Uploaded_Records)){names(Uploaded_Records)<-replace(names(Uploaded_Records), tolower(names(Uploaded_Records)) %in% c("y", "dec_lat", "latitude", "lat"), "dec_lat")}
  if(! "dec_long" %in% names(Uploaded_Records)){names(Uploaded_Records)<-replace(names(Uploaded_Records), tolower(names(Uploaded_Records)) %in% c("x", "dec_long", "dec_lon", "longitude", "lon", "long"), "dec_long")}
  if((! "dec_long" %in% names(Uploaded_Records)) | (! "dec_lat" %in% names(Uploaded_Records))){no_coords_update()}

  # Make longitude and latitude numeric (includes a comma to point transformation for decimals)
  Uploaded_Records$dec_long<-Uploaded_Records$dec_long %>% as.character(.) %>% sub(",", ".", .) %>% as.numeric()
  Uploaded_Records$dec_lat<-Uploaded_Records$dec_lat %>% as.character(.) %>% sub(",", ".", .) %>% as.numeric()

  # Check they are within -180:180 and -90:90
  Wrong_lines <- c(
    which(Uploaded_Records$dec_long<(-180)),
    which(Uploaded_Records$dec_long>(180)),
    which(Uploaded_Records$dec_lat<(-90)),
    which(Uploaded_Records$dec_lat>(90))
  ) %>% unique() %>% sort()
  if(length(Wrong_lines)>0){coords_outofbound(LINES=Wrong_lines)}

  # Subset and warning if missing lon / lat
  Lines_NA <- c(which(is.na(Uploaded_Records$dec_long)), which(is.na(Uploaded_Records$dec_lat))) %>% unique() %>% sort()
  if(length(Lines_NA)>0){
    Uploaded_Records <- subset(Uploaded_Records, is.na(dec_long)==F & is.na(dec_lat)==F)
    Warning_Upload <- paste0("Some coordinates were missing from uploaded records or incorrect (lines: ", paste0(Lines_NA[1:min(c(50, length(Lines_NA)))], collapse=", "), ifelse(length(Lines_NA)>50, ", ...)", ")"), "; they have been removed from the Uploaded data.\n")
  } else{Warning_Upload <- ""}
  
  # Check source is UTF8, if some are not, I report this in the warning and try to fix it with iconv
  if("source" %in% names(Uploaded_Records)){
    source_correct <- validUTF8(as.character(Uploaded_Records$source))
    if(F %in% source_correct){Warning_Upload <- paste0(Warning_Upload, "The column 'source' includes some forbidden characters (lines ", paste(which(source_correct==F)[1:min(10, length(which(source_correct==F)))], collapse=","), ifelse(length(which(source_correct==F))>10, "...", ""), "). These were automatically corrected but it might create an issue when downloading references in the final step.")}
    for(i in which(source_correct==F)){
      Uploaded_Records$source[i] <- try(iconv(Uploaded_Records$source[i], from = "latin1", to = "UTF-8"))
    }
  }
  
  # Transform column name of year and make it numeric (if no column, I make it all NA)
  names(Uploaded_Records)<-replace(names(Uploaded_Records), tolower(names(Uploaded_Records)) %in% c("year", "event_year", "year_event"), "year")
  if(! "year" %in% names(Uploaded_Records)){Uploaded_Records$year<-NA}
  Uploaded_Records$year<-as.numeric(as.character(Uploaded_Records$year))
  
  
  # Return
  return(list(
    Uploaded_Records=Uploaded_Records,
    Warning_Upload=Warning_Upload
  ))
}




### Prepare European cropping shapefile
sRL_EuropeList<-c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova, Republic of", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom of Great Britain and Northern Ireland")
sRL_EuropeList1<-c("European Russia", "Türkiye-in-Europe") # For Europe I need to include Eastern Russia and Eastern Turkey, I list them in Crop_Country1 which is empty for non European National assessments
sRL_EU27List<-c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

sRL_ShapeCountryNRL <- function(Country_name, scientific_name){
  
  sRL_loginfo("START - Shape Country NRL", scientific_name)
  
  ### Prepare list of countries
  if(Country_name == "Europe"){
    country_sub <- distCountries_NRL %>% subset(., .$SIS_name0 %in% sRL_EuropeList | .$SIS_name1 %in% sRL_EuropeList1) %>% st_crop(., xmin=-50, xmax=180, ymin=-90, ymax=90) # Remove Clipperton
  }
  
  if(Country_name == "EU27"){
    country_sub <- distCountries_NRL %>% subset(., .$SIS_name0 %in% sRL_EU27List) %>% st_crop(., xmin=-50, xmax=180, ymin=-90, ymax=90) # Remove Clipperton
  }
  
  if(Country_name == "Mediterranean"){
    country_sub<-st_read("Species/Map countries/Mediterranean_hotspot.shp") ; country_sub$SIS_name0<-NA
  }
  
  if(! Country_name %in% c("Europe", "EU27", "Mediterranean")){
    country_sub <- distCountries_NRL %>% subset(., .$SIS_name0 == Country_name)
  }
  
  ### Merge
  country_sub <- country_sub %>% dplyr::group_by() %>% dplyr::summarise(N= n())
  sRL_loginfo("END - Shape Country NRL", scientific_name)
  
  return(country_sub)
}



### Function to create occurrence records 
sRL_createDataGBIF <- function(scientific_name, GBIF_SRC, Gbif_Country, Uploaded_Records) { # nolint
  
  ### Reuse Warning from upload if it exists (otherwise start from empty string); used to warn if number of records above limit or if some uploaded lines were removed
  Warning_Create <- ifelse("Warning_Upload" %in% names(Uploaded_Records), Uploaded_Records$Warning_Upload, "")
  
  ### If Gbif_Country, prepare countries file
  if(Gbif_Country != ""){
    
    # Create shapefile to crop
    co_tot <- sRL_ShapeCountryNRL(Gbif_Country, scientific_name) %>% st_buffer(., 0.01)
    
    # Calculate extent
    co_EXT <- extent(co_tot) %>% as.vector(.)
    
  } else {co_EXT<-c(-180,180,-90,90) ; co_tot<-c()}

  ### Download data
  # From GBIF
  Taxon<-name_backbone(name=scientific_name, strict=T)
  if("SPECIES" %in% Taxon$rank[1]){TaxKey<-Taxon$usageKey} else {TaxKey<-NULL} # Needed to avoid a wrong species from an existing genus to download all occurrences from that genus
  if(GBIF_SRC[1]==1 & is.null(TaxKey)==F){
    sRL_loginfo("Download GBIF", scientific_name)
    
    # Calculate the total number of data in GBIF
    OCC <- occ_count(taxonKey=TaxKey, 
                     hasCoordinate = TRUE,
                     decimalLongitude=paste(co_EXT[1], co_EXT[2], sep=","), 
                     decimalLatitude=paste(co_EXT[3], co_EXT[4], sep=",")
                     )

    if(OCC < config$LIM_GBIF){ # Download all data or structure download if more than LIM_GBIF
      dat_gbif <- sRL_SimpleGBIF(scientific_name, co_EXT)
    } else{
      dat_gbif <- sRL_StructureGBIF(scientificName = scientific_name, co_EXT, co_tot)
      Warning_Create <- paste0(Warning_Create, "The number of records available on GBIF was >2000, we downloaded a representative sample.\n")
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
    sRL_loginfo("Download OBIS", scientific_name)
    dat_obis <- robis::occurrence(scientific_name)
    dat_obis$ID<-paste0(dat_obis$decimalLongitude, dat_obis$decimalLatitude, dat_obis$date_year)
    dat_obis$year<-dat_obis$date_year
    dat_obis_sub<-subset(dat_obis, !dat_obis$ID %in% dat_gbif$ID)
    dat_obis_sub$Source_type<-"OBIS"
    dat_obis_sub$source<-dat_obis_sub$bibliographicCitation
    dat_obis_sub$Link<-paste0("https://obis.org/taxon/", dat_obis_sub$aphiaID[1])
    dat_obis_sub$gbifID<-dat_obis_sub$occurrenceID
    dat_obis_sub$species_download<-scientific_name
    dat_obis_sub <- subset(dat_obis_sub, decimalLongitude > co_EXT[1] & decimalLongitude < co_EXT[2] & decimalLatitude > co_EXT[3] & decimalLatitude < co_EXT[4]) # Restrict to country of interest before subsampling
    
    # If too many data, keep a sample (most recent)
    if(nrow(dat_obis_sub) > config$LIM_GBIF){
      dat_obis_sub$Source_type<-"OBIS sample"
      dat_obis_sub<-dat_obis_sub[order(dat_obis_sub$year, decreasing=T),]
      dat_obis_sub<-dat_obis_sub[1:config$LIM_GBIF,]
      Warning_Create <- paste0(Warning_Create, "The number of records available on OBIS was >2000, we downloaded only a subset.\n")
    }
  } else {dat_obis_sub<-data.frame()}
  
  # From Red List point (first check if available in gpkg, otherwise take from csv)
  path_gpkg <- paste0(config$distribution_path, scientific_name, "/", sub(" ", "_", scientific_name), "_RL/", scientific_name, ".gpkg")
  Pts_in_gpkg <- ifelse(file.exists(path_gpkg), T, F)
  if(Pts_in_gpkg){Pts_in_gpkg <- ifelse("points" %in% st_layers(path_gpkg)$name, T, F)}
  print(Pts_in_gpkg)
  
  if(GBIF_SRC[3]==1 & ((paste0(scientific_name, ".csv") %in% list.files(config$POINTdistribution_path)) | Pts_in_gpkg==T)){
    sRL_loginfo("Download Red List", scientific_name)
    if(Pts_in_gpkg==T){
        dat_RL<-st_read(path_gpkg, layer="points")
        dat_RL$longitude <- st_coordinates(dat_RL)[,1]
        dat_RL$latitude <- st_coordinates(dat_RL)[,2]
      } else {
        dat_RL<-read.csv(paste0(config$POINTdistribution_path, scientific_name, ".csv"))
      }
    dat_RL$Source_type<-"Red List"
    dat_RL$decimalLongitude<-dat_RL$longitude
    dat_RL$decimalLatitude<-dat_RL$latitude
    dat_RL$year<-dat_RL$event_year
    if(! "event_year" %in% names(dat_RL)){dat_RL$year <- NA}
    dat_RL$species<-dat_RL$binomial
    dat_RL$coordinateUncertaintyInMeters<-NA
    dat_RL$gbifID<-paste0("RL_", rownames(dat_RL))
    dat_RL$Link<-NA
    dat_RL$species_download<-scientific_name
    names(dat_RL)<-replace(names(dat_RL), names(dat_RL)=="Source", "source")
    names(dat_RL)<-replace(names(dat_RL), names(dat_RL)=="basisofrec", "basisOfRecord")
    dat_RL <- subset(dat_RL, decimalLongitude > co_EXT[1] & decimalLongitude < co_EXT[2] & decimalLatitude > co_EXT[3] & decimalLatitude < co_EXT[4]) # Restrict to country of interest before subsampling
    
    # If too many data, keep a sample (most recent)
    if(nrow(dat_RL) > config$LIM_GBIF){
      dat_RL$Source_type<-"Red List sample"
      dat_RL<-dat_RL[order(dat_RL$year, decreasing=T),]
      dat_RL<-dat_RL[1:config$LIM_GBIF,]
      Warning_Create <- paste0(Warning_Create, "The number of records available on the Red List was >2000, we kept only a subset.\n")
    }
  } else {dat_RL<-data.frame()}
  
  
  # From uploaded data
  if("Uploaded_Records" %in% names(Uploaded_Records)){if(!is.null(nrow(Uploaded_Records$Uploaded_Records))){
    sRL_loginfo("Download Uploaded Records", scientific_name)
    dat_upload<-Uploaded_Records$Uploaded_Records
    dat_upload$decimalLongitude<-dat_upload$dec_long
    dat_upload$decimalLatitude<-dat_upload$dec_lat
    dat_upload$species<-dat_upload$sci_name
    dat_upload$coordinateUncertaintyInMeters<-NA
    dat_upload$gbifID<-paste0("Uploaded_", rownames(dat_upload))
    dat_upload$Link<-NA
    names(dat_upload)<-replace(names(dat_upload), names(dat_upload)=="Source", "source")
    names(dat_upload)<-replace(names(dat_upload), names(dat_upload)=="basisofrec", "basisOfRecord")
    dat_upload <- subset(dat_upload, decimalLongitude > co_EXT[1] & decimalLongitude < co_EXT[2] & decimalLatitude > co_EXT[3] & decimalLatitude < co_EXT[4]) # Restrict to country of interest before subsampling
    if(!"source" %in% names(dat_upload)){dat_upload$source<-NA}
    
    # If too many data (with a higher threshold), keep a sample 
    if(nrow(dat_upload) > 3*config$LIM_GBIF){
      dat_upload$Source_type<-"Uploaded sample"
      dat_upload<-dat_upload[1:(3*config$LIM_GBIF),]
      Warning_Create <- paste0(Warning_Create, "The number of records uploaded was >6000, we kept only a subset.\n")
    }
  } else {dat_upload<-data.frame()}} else {dat_upload<-data.frame()}

  
  # Return empty df if no records
  if(is.null(nrow(dat_gbif)) & nrow(dat_obis_sub)==0 & nrow(dat_RL)==0  & nrow(dat_upload)==0) {
    dat<-data.frame()
  } else {
    # Merge otherwise
    sRL_loginfo("Merge records", scientific_name)
    dat<-rbind.fill(dat_gbif, dat_obis_sub, dat_RL, dat_upload)
    print(paste0("Number of data: ", nrow(dat)))
    
    # Select columns of interest
    dat <- dat %>%
      dplyr::select(any_of(c(names(dat_upload), names(dat_RL),
                             c("species", "species_download", "decimalLongitude", "decimalLatitude", "countryCode", "individualCount", # nolint
                             "gbifID", "id", "objectid", "family", "taxonRank", "coordinateUncertaintyInMeters", "year",
                             "basisOfRecord", "institutionCode", "datasetName", "genericName", "specificEpithet", "Source_type", "source", "citation", "Link", "presence"))))
    
    # Remove records with no spatial coordinates
    dat <- dat %>% filter(!is.na(decimalLongitude)) %>% filter(!is.na(decimalLatitude)) # nolint
    
    # Convert country code from ISO2c to ISO3c
    if(!"countryCode" %in% names(dat)){dat$countryCode<-NA} else{
      dat$countryCode <-  countrycode::countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')} # nolint
    dat <- data.frame(dat)
    
    # Remove observations outside country if national Red List
    if(Gbif_Country != ""){
      sRL_loginfo("Remove observations outside NRL country", scientific_name)
      dat_proj<-st_as_sf(dat,coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84")
      dat_proj$InCountry<-st_join(dat_proj, co_tot, join=st_intersects)$N %>% is.na(.)==F
      dat<-subset(dat, dat$gbifID %in% dat_proj$gbifID[dat_proj$InCountry==T])
    }
  }
  
  return(list(
    dat=dat,
    Warning_Create=Warning_Create
    )
  )
}



### Function to download all GBIF records
sRL_SimpleGBIF<-function(scientific_name, co_EXT){
  dat_gbif <- rgbif::occ_data(scientificName=scientific_name, 
                              hasCoordinate = T, 
                              decimalLongitude=paste(co_EXT[1], co_EXT[2], sep=","), 
                              decimalLatitude=paste(co_EXT[3], co_EXT[4], sep=","),
                              limit=config$LIM_GBIF
  )$data # occ_data is faster than occ_search because it omits some columns
  
  dat_gbif$Source_type<-"GBIF"
  
  return(dat_gbif)
}


### Function to download GBIF in a spatially structured manner (to avoid big sampling biases)
sRL_StructureGBIF<-function(scientificName, co_EXT, co_tot){
  
  print("Use Structured GBIF download")

  ##### DEFINE THE SAMPLING PATTERN
  ### Map density of observations and extract coordinates
  Fetch<-mvt_fetch(taxonKey = name_backbone(name=scientificName)$usageKey, srs = "EPSG:4326", format="@4x.png") 
  Fetch<-st_crop(Fetch, xmin=max(-180,(co_EXT[1]-1)), xmax=min(180,(co_EXT[2]+1)), ymin=max(-90,(co_EXT[3]-1)), ymax=min(180,(co_EXT[4]+1))) # Crop by co_Ext with 1 degree buffer (max distance between two sampling points) to ensure we don't exclude points close to the border
  if(nrow(Fetch)==0){no_records()}
  
  ### Restrict if Crop by country
  if(is.null(co_tot)==F){
    Fetch <- st_filter(Fetch, st_buffer(co_tot, 1), join=st_intersects)
  }
  
  ### Extract extent to define grid size
  coords<-as.data.frame(st_coordinates(Fetch))
  DeltaX<-max(coords$X, na.rm=T)-min(coords$X, na.rm=T)
  DeltaY<-max(coords$Y, na.rm=T)-min(coords$Y, na.rm=T)
  
  ### Fix maximum size cell in degrees
  Max_Cell=10
  
  # If extent is small, cut the grid in ca. 100 square cells
  if(max(DeltaX, DeltaY) < (10*Max_Cell)){
    NX <- round(10*DeltaX / sqrt(DeltaX*DeltaY)) %>% min(., length(unique(coords$X))) # Calculates the number of cells to have in one row so that we end up with ca. 100 square cells; then take number of existing cells if that's lower
    NY <- round(10*DeltaY / sqrt(DeltaX*DeltaY)) %>% min(., length(unique(coords$Y)))
  # If extent is large, cut the grid in Max_Cell square cells
  } else{
    NX=ceiling(DeltaX/Max_Cell)
    NY=ceiling(DeltaY/Max_Cell)
  }
  
  ### Create download grid and filter with fetch
  buff_Fetch <- st_union(st_buffer(Fetch, 1, endCapStyle = "SQUARE"))
  grid_sf<-st_make_grid(buff_Fetch, n=c(NX, NY), what="polygons") %>% st_as_sf()
  grid_sub <- st_filter(grid_sf, buff_Fetch, join=st_intersects)
  
  ### Calculate N available per cell
  grid_sub$N<-NA
  for(i in 1:nrow(grid_sub)){
    grid_sub$N[i] <- occ_count(taxonKey=name_backbone(name=scientificName)$usageKey, hasCoordinate = TRUE, geometry=st_as_text(st_geometry(grid_sub[i,])))
  }
  grid_sub <- subset(grid_sub, grid_sub$N>0)

  ### Determine number of data to download per group to sum at LIM_GBIF (increased a bit if Crop_Country)
  LIMgbif<-ifelse(is.null(co_tot), config$LIM_GBIF, 1.25*config$LIM_GBIF)
  grid_sub$N_download <- ifelse(grid_sub$N < (LIMgbif/nrow(grid_sub)), grid_sub$N, NA)
  grid_sub$N_download[is.na(grid_sub$N_download)] <- round((LIMgbif-sum(grid_sub$N_download, na.rm=T))/nrow(grid_sub[is.na(grid_sub$N_download),]))
  grid_sub$N_download <- ifelse((grid_sub$N_download>grid_sub$N), grid_sub$N, grid_sub$N_download)
  i=0
  while(i<LIMgbif){
    if(sum(grid_sub$N_download) < sum(grid_sub$N)){ # If not all columns are complete I add only to those not full, otherwise to all columns (happens as Fetch only includes data with year)
      grid_sub$N_download[grid_sub$N_download<grid_sub$N] <- grid_sub$N_download[grid_sub$N_download<grid_sub$N]+1
    } else {grid_sub$N_download <- ceiling(grid_sub$N_download*1.05)}
    i=sum(grid_sub$N_download)
  }
  
  ##### STRUCTURE DOWNLOAD
  # Download one data (just for column names)
  dat_structured<-rgbif::occ_data(scientificName=scientificName, hasCoordinate = T, limit=1)$data[0,]
  
  # Download group per group
  for(GR in 1:nrow(grid_sub)){
    dat_GR<-rgbif::occ_data(scientificName=scientificName,
                            hasCoordinate = T, 
                            limit=grid_sub$N_download[GR], 
                            geometry=st_as_text(st_geometry(grid_sub[GR,]))
    )$data %>% as.data.frame() %>% mutate(Group=GR) 

    if(is.null(nrow(dat_GR))==F){dat_structured<-rbind.fill(dat_structured, dat_GR)}
  }
  
  ### If for some reason, we have few records (<1000), re-run a non-representative download and save an empty file to record this happened
  if(nrow(dat_structured) < (0.5*config$LIM_GBIF)){
    dat_structured <- sRL_SimpleGBIF(scientificName, co_EXT)
    print(paste0("Non-representative sample downloaded for ", scientificName, "_", as.character(Sys.Date()), ".csv"))
    write.csv("", paste0("Species/Stored_outputs/Non-representative sample downloaded for ", scientificName,  "_", as.character(Sys.Date()), ".csv"))
  }
  
  dat_structured$Source_type<-"GBIF sample"
  
  print("Finished Structured GBIF download")
  
  # ### Plot to check the download went well (comparing with all data for that species)
  # all_gbif <- rgbif::occ_data(scientificName=scientific_name, hasCoordinate = T, limit=20000)$data
  # ggplot()+
  #   geom_point(data=all_gbif, aes(x=decimalLongitude, y=decimalLatitude), col="grey")+
  #   geom_sf(data=grid_sub, fill=NA)+
  #   geom_point(data=dat_structured, aes(x=decimalLongitude, y=decimalLatitude), col="darkred")+
  #   geom_sf_text(data=grid_sub, aes(label=paste0(N_download, " / ", N)))+
  #   theme_void()+
  #   ggtitle(scientific_name)
  
  return(dat_structured)
}


# Step 2 --------------------------------
sRL_PopRecords <- function(flags){
  
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
  
  # Return
  return(flags)
}



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
  flags$.uncertainty <- as.numeric(flags$coordinateUncertaintyInMeters) < uncertainty_GBIF*1000 
  if(Gbif_uncertainBin==T){flags$.uncertainty[is.na(flags$.uncertainty)]<-F} # If 'remove NA' is clicked, I put False for the uncertainty filter
  
  ### Add flagging for points outside GBIF_xmin...
  flags$.limits<-(flags$decimalLongitude < GBIF_xmin | flags$decimalLongitude > GBIF_xmax | flags$decimalLatitude < GBIF_ymin | flags$decimalLatitude > GBIF_ymax)==F
  
  ### Flag points with presence 4,5,6,7 (for uploaded or RL points)
  if("presence" %in% names(flags)){flags$.pres<-as.character(! flags$presence %in% c("4", "5", "6", "7"))}
  
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
  flags <- sRL_PopRecords(flags)
  
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


### Create leaflet
sRL_LeafletFlags <- function(flags){
  
  Leaf <- leaflet(flags) %>%
    addTiles(group="OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("OpenTopoMap", group = "Topography") %>%
    addProviderTiles("Esri.OceanBasemap", group = "Marine", options = list(maxZoom=10)) %>%
    addCircleMarkers(lng=flags$decimalLongitude,
                     lat=flags$decimalLatitude,
                     color=ifelse(is.na(flags$Reason)==T, "#fdcb25ff", "#440154ff"),
                     fillOpacity=0.5,
                     stroke=F,
                     popup=flags$PopText,
                     radius=8,
                     group="Occurrence records") %>%
    addLegend(position="bottomleft", colors=c('#fdcb25ff', '#440154ff'), labels=c("Valid", "Not valid")) %>%
    addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography", "Marine"), overlayGroups="Occurrence records", position="topleft") %>%
    addMouseCoordinates() %>%
    addScaleBar(position = "bottomright") 
  
  return(Leaf)
}



# Step 3 --------------------------------
sRL_MapDistributionGBIF<-function(dat, scientific_name, username, First_step, AltMIN, AltMAX, Buffer_km, Gbif_Param){

  ### The first step can be mcp, kernel, alpha, indivsites, coastal, hydro8, hydro10, hydro12, hydroMCP
  if(First_step=="mcp" | First_step==""){
    distGBIF<-st_as_sf(st_convex_hull(st_union(dat)))
    st_geometry(distGBIF)<-"geometry" # Rename the variable including geometry
  }
  
  if(First_step=="kernel"){
    # Remove duplicate points (points with same lon/lat)
    dat_subsample<-distinct(dat, as.character(geometry), .keep_all=T) 

    # Calculate kernels
    try({
      kernel.ref <- kernelUD(as_Spatial(dat), h = "href", extent=1)  # href = the reference bandwidth
      distGBIF <- getverticeshr(kernel.ref, percent = 100*Gbif_Param[2]) %>% st_as_sf(.)
    }, silent=T)
    if(exists("distGBIF")==F){
      kernel.ref <- kernelUD(as_Spatial(dat), h = "href", extent=2)  # href = the reference bandwidth
      distGBIF <- getverticeshr(kernel.ref, percent = 100*Gbif_Param[2]) %>% st_as_sf(.)
    }
    
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
        distGBIF<-sRL_convexHull(dat_subsample, alpha = Alpha_scaled) # concaveman::concaveman function could work as well with something similar, but not proper alpha-hull
      } ,error=function(e){bug_alpha()})
      
      st_crs(distGBIF)<-st_crs(dat_subsample)
      
  }
  
  if(First_step=="indivsites"){
    distGBIF<-st_buffer(dat, 1) # The default is one meter, then they can add a buffer
  }
  
  
  if(First_step=="coastal"){
    pts_buff <- st_buffer(dat, Buffer_km*1000)
    # Extract coast, transform to line and create distribution
    coast <- distCountries %>% st_crop(., 1.2*ext(pts_buff)) %>% dplyr::group_by(.) %>% dplyr::summarise(N=n()) %>% st_cast(., "MULTILINESTRING") %>% st_simplify(., dTolerance=(Buffer_km*100)) # I simplify by 1/10 of the buffer in meters
    distGBIF <- st_intersection(coast, pts_buff) %>% st_buffer(., 1) %>% st_combine(.) %>% st_as_sf(.) %>% st_simplify(., dTolerance=10)
    # Return error if no coast overlap
    if(is.na(extent(distGBIF)[1])){no_coastoverlap()}
  }
  
  if(First_step=="vents"){
    vents_shp <- st_read("resources/Hydrothermal_vents/Global_2020_HydrothermalVents_InterRidgeVentsDatabasev3.4.shp") %>% st_transform(., st_crs(dat))
    vents_buffer <- st_buffer(vents_shp, Buffer_km*1000)
    
    distGBIF <- st_filter(vents_buffer, dat, .predicate = st_intersects)
    # Return error if no vents overlap
    if(is.na(extent(distGBIF)[1])){no_ventoverlap()}
  }
  
  if(substr(First_step, 1,5)=="hydro"){
    
    # Extract level 8 in any case
    hydro8_sub<-st_crop(hydro_raw, extent(st_buffer(dat,1))) # Buffer needed in case only 1 point
    interHyd<-st_join(dat, hydro8_sub, join=st_intersects) %>% subset(., is.na(.$hybas_id)==F) # Identify hydrobasins with data 
    distGBIF<-subset(hydro_raw, hydro_raw$hybas_id %in% interHyd$hybas_id) # Isolate these hydrobasins

    # Extract level 10 or 12 if requested and possible (i.e., small distribution)
    if(First_step %in% c("hydro10", "hydro12") & nrow(distGBIF)>0){
      
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
      hydroLEV_sub<-st_crop(hydroLEV_raw, extent(st_buffer(dat,1)))
      interHyd<-st_join(dat, hydroLEV_sub, join=st_intersects) %>% subset(., is.na(.$hybas_id)==F) # Identify hydrobasins with data 
      distGBIF<-subset(hydroLEV_raw, hydroLEV_raw$hybas_id %in% interHyd$hybas_id) # Isolate these hydrobasins
      
    }
    
    if(First_step=="hydroMCP"){
      mcp<-st_as_sf(st_convex_hull(st_union(dat))) ; st_geometry(mcp)<-"geometry"
      interHydMCP<-st_join(mcp, hydro8_sub, join=st_intersects) %>% subset(., is.na(.$hybas_id)==F) # Identify hydrobasins with data 
      distGBIF<-subset(hydro_raw, hydro_raw$hybas_id %in% interHydMCP$hybas_id) # Isolate these hydrobasins
    }
    
    if(nrow(distGBIF)==0){no_hydrobasins()}
    
  }
  

  
  ### Merge (not if hydrobasins, we keep 1 line per hydrobasin)
  distGBIF$binomial <- scientific_name
  if(substr(First_step, 1,5)!="hydro"){distGBIF<-distGBIF %>% dplyr::group_by(binomial) %>% dplyr::summarise(N = n())}
  
  ### Apply buffer
  distGBIF<-st_buffer(distGBIF, Buffer_km*1000) %>% st_as_sf()
  
  ### If distGBIF is bigger than the Earth limits (mapped from realms; ie., if not fully covered by realms_mcp), I crop restrict to the Earth
  if(length(st_covered_by(distGBIF, realms_mcp, sparse=T)[[1]])==0){
    sRL_loginfo("Polygon outside of Earth bounds - fixed", scientific_name)
    distGBIF$geometry<-st_intersection(distGBIF, realms_mcp)$geometry
  }
  
  
  ### Apply crop by altitude
  if(AltMIN>0 | AltMAX<9000){
    tryCatch({
      
      sRL_loginfo("START - Crop by elevation", scientific_name)
      mcp.spatial <- sf::as_Spatial(distGBIF)
      sp.mcp.terra <- terra::vect(distGBIF)
      EXT <- ext(mcp.spatial)
      
      # Load elevation raster (size depends on range of points)
      if((as.numeric(st_area(st_as_sfc(st_bbox(dat))))/10^6) > (5*10^6)){
        print("Using large elevation raster")
        
        alt_rawMIN<-rast(paste0(config$cciStack2_path, "/ElevationAgg30_MINIMUM.tif")) %>% terra::crop(., EXT, snap="out") 
        alt_rawMAX<-rast(paste0(config$cciStack2_path, "/ElevationAgg30_MAXIMUM.tif")) %>% terra::crop(., EXT, snap="out") 
        alt_rawMOY<-rast(paste0(config$cciStack2_path, "/ElevationAgg30.tif")) %>% terra::crop(., EXT, snap="out") 
        alt_rawMOY[alt_rawMIN<AltMIN] <- AltMIN-1
        alt_rawMOY[alt_rawMAX>AltMAX] <- AltMAX+1
        
        dem.crop <- alt_rawMOY %>% replace(., is.na(.), 0)
        
      } else {
        print("Using small elevation raster")
        
        alt_raw<-sRL_ChargeAltRaster()
        dem.crop <- terra::crop(alt_raw, EXT, snap="out") %>% replace(., is.na(.), 0)
      }
      
      sp.mcp.ras <- terra::rasterize(sp.mcp.terra, dem.crop, touches=T)
      dem.sp <- terra::mask(dem.crop, mask = sp.mcp.terra)
  
      m <- c(-Inf, (AltMIN-1), 0, (AltMIN-1), 
         AltMAX, 1, AltMAX, Inf, 0)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      
      sp.range <- terra::classify(dem.sp, rclmat)
      if((dim(sp.range)[1]*dim(sp.range)[2])>(10^5)){sp.range <- sp.range %>% terra::aggregate(fact = 4, fun = 'max', na.rm=T)} # Aggregate if too big
      
      distGBIF <- sRL_CropRangeElevation(distGBIF, sp.range)
      sRL_loginfo("END - Crop by elevation", scientific_name)
      
    }, error=function(e){"Bug in Crop by elevation"})
  }
  
  ### Prepare to export
  distGBIF$binomial<-scientific_name
  distGBIF$id_no<-sRL_CalcIdno(scientific_name)
  distGBIF$presence<-1
  if(First_step=="hydroMCP"){distGBIF$presence <- ifelse(distGBIF$hybas_id %in% interHyd$hybas_id, 1, 3)}
  distGBIF$origin<-1
  distGBIF$seasonal<-1
  if(exists("Alpha_scaled")){distGBIF$alphaTEMPO<-Alpha_scaled} # Save alpha scaled if I use alpha hull

  # Additional attributes
  distGBIF$yrcompiled <- format(Sys.Date(), "%Y")
  distGBIF$citation<-paste0("sRedList ", format(Sys.Date(), "%Y"))
  distGBIF$source<-"sRedList platform"
  distGBIF$compiler<-sRL_userformatted(username)
  distGBIF$data_sens<-0
  distGBIF$spatialref<-"WGS84"
  
  return(distGBIF)
  
}



### Crop by elevation
sRL_CropRangeElevation <- function(distGBIF, sp.range) {
  
  sp.range[sp.range == 1] <- NA
  
  # Transform areas not suitable to points
  data <- sp.range %>% 
    as.points() %>% 
    st_as_sf(.) %>% 
    st_transform(crs=CRSMOLL)
  
  # dataframe of combinations - based on row index
  dist_max <- 1.1*sqrt(res(sp.range)[1]^2 + res(sp.range)[2]^2)
  
  grid_points <- st_distance(data) %>% 
    as.data.frame() %>% 
    mutate(start=rownames(.)) %>% 
    reshape2::melt(id.vars="start", variable.name="end", value.name = "dist") %>% 
    mutate(start=as.numeric(start), end=as.numeric(end), dist=as.numeric(dist)) %>%
    # no line with start & end being the same point
    dplyr::filter(start != end) %>%  
    # when order doesn't matter just one direction is enough
    subset(., as.numeric(start) > as.numeric(end)) %>%
    # remove lines with points too distant
    subset(., dist < dist_max)
  
  # Add coordinates
  data_coords <- st_coordinates(data) %>% as.data.frame()
  
  grid_points$x1 <- data_coords$X[grid_points$start]
  grid_points$y1 <- data_coords$Y[grid_points$start]
  grid_points$x2 <- data_coords$X[grid_points$end]
  grid_points$y2 <- data_coords$Y[grid_points$end]
  
  # Internal functions from https://gis.stackexchange.com/questions/419232/create-sf-points-and-then-lines-from-4-columns-in-a-data-frame-while-staying-in
  make_line <- function(xy2){
    st_linestring(matrix(xy2, nrow=2, byrow=TRUE))
  }
  
  make_lines <- function(df, names=c("x1","y1","x2","y2")){
    m = as.matrix(df[,names])
    lines = apply(m, 1, make_line, simplify=FALSE)
    st_sfc(lines)
  }
  
  sf_pts_to_lines <- function(df, names=c("x1","y1","x2","y2")){
    geom = make_lines(df, names)
    df = st_sf(df, geometry=geom)
    df
  }
  
  # Create lines
  my_lines = sf_pts_to_lines(grid_points)
  
  # Transform to polygons of unsuitable elevation
  my_polygons <- my_lines %>%
    st_as_sf(., crs = sf::st_crs(data)) %>%
    st_union() %>% 
    st_polygonize() %>% 
    st_collection_extract(., "POLYGON") %>% 
    st_union() %>% 
    st_combine() %>%
    smoothr::smooth(., method="ksmooth") %>%
    st_as_sf(., crs = sf::st_crs(data))
  
  # Remove unsuitable polygons that are very small
  my_polygons <- my_polygons %>%
    st_cast(., "POLYGON") %>%
    mutate(Area=as.numeric(st_area(.))) %>%
    subset(., Area>(res(sp.range)[1]*res(sp.range)[2]) )
  
  # Remove these polygons from distGBIF (and remove small polygons that may be created at the edge)
  distGBIF_new <- st_difference(distGBIF, st_union(my_polygons)) %>%
    st_cast(., "POLYGON") %>%
    mutate(Area=as.numeric(st_area(.))) %>%
    subset(., Area>(res(sp.range)[1]*res(sp.range)[2]) )
  
  
  return(distGBIF_new)
  
}


### Prepare dist_comm for distribution shp
sRL_DistComment <- function(Output, N_dat){
  tryCatch({
    Comm<-paste0(
      "The distribution was created on the sRedList platform from ", 
      N_dat, 
      " occurrence records (source=", Output$Value[Output$Parameter=="Gbif_Source"], ") ",
      "with settings: Starting point=", Output$Value[Output$Parameter=="Mapping_Start"], 
      ifelse(Output$Value[Output$Parameter=="Mapping_Start"]=="alpha", paste0(" (alpha_parameter=", Output$Value[Output$Parameter=="Alpha_parameter"], ")"), ""),
      ifelse(Output$Value[Output$Parameter=="Mapping_Start"]=="kernel", paste0(" (kernel_parameter=", Output$Value[Output$Parameter=="Kernel_parameter"], ")"), ""),
      ", buffer=", Output$Value[Output$Parameter=="Mapping_Buffer"], 
      ", crop=", Output$Value[Output$Parameter=="Mapping_Crop"], 
      ifelse(Output$Value[Output$Parameter=="Mapping_Altitude"] == "0,9000", "", paste0(", altitude=", Output$Value[Output$Parameter=="Mapping_Altitude"])), 
      ", smooth=", Output$Value[Output$Parameter=="Mapping_Smooth"]
    ) %>% substr(., 1, 254)
  }, error=function(e){cat("Bug in creating dist_comm (comment for distribution)")})
  
  return(Comm)
}


### Alpha hull (taken from former version of spatialEco)
sRL_convexHull <- function(x, alpha = 250000)	{
  if(!any(which(utils::installed.packages()[,1] %in% "alphahull")))
    stop("please install alphahull package before running this function")
  if(!inherits(x, c("SpatialPointsDataFrame", "SpatialPoints", "sf", "sfc", "matrix")))		
    stop(deparse(substitute(x)), " must be a spatial (sf, sp) or matrix object")
  if(inherits(x, c("sf", "sfc"))) { 
    xy <- as.data.frame(sf::st_coordinates(x)) 
  } else if(inherits(x, c("SpatialPointsDataFrame", "SpatialPoints"))) {
    xy <- as.data.frame(sp::coordinates(x))
  } else if(inherits(x, "matrix")) {
    xy <- as.data.frame(x)
  } else {
    stop("Not a valid object")
  }
  xy <- xy[!duplicated(xy[c(1,2)]),]
  a <- alphahull::ashape(as.matrix(xy), alpha = alpha)$edges[,3:6]
  a <- apply(a, 1, function(x)  {
    v <- as.numeric(x[c(1,3,2,4)])
    m <- matrix(v, nrow = 2)
    return(sf::st_sfc(sf::st_linestring(m)))
  })
  a <- Reduce(c, a) |>
    sf::st_combine() |>		
    sf::st_as_sf() |>
    sf::st_polygonize() |>	
    sf::st_collection_extract(type = "POLYGON")
  sf::st_geometry(a) <- "geometry"
  a$ID <- 1
  if(!is.na(sf::st_crs(x))) {
    sf::st_crs(a) <- sf::st_crs(x) 
  }	  
  return( a )	
}


### Crop distribution (not integrated in sRL_MapDistribution to enable saving the intermediate for smoothing)
sRL_CropDistributionGBIF <- function(distGBIF, GBIF_crop){
  
  ### Apply crop by land/sea
  if(GBIF_crop %in% c("cropland", "cropsea")){
    
    # Create countries map based on the buffer
    CountrySP<-st_crop(distCountries_mapping, 1.1*extent(distGBIF))
    
    # Remove land or sea if requested
    if(GBIF_crop=="cropland"){
      if(nrow(CountrySP)==0){no_land_map()}
      distGBIF<-st_intersection(distGBIF, CountrySP) %>% 
        dplyr::group_by(binomial, presence, origin, seasonal) %>% dplyr::summarise(N = n())
      }
    
    if(GBIF_crop=="cropsea" & nrow(CountrySP)>0){ # If nrow==0 it means there is no overlap between countries and the extent of distGBIF, so everything is already at sea
      countr<-CountrySP %>% st_crop(., extent(distGBIF)) %>% dplyr::group_by() %>% dplyr::summarise(N = n())
      distGBIF<-st_difference(distGBIF, countr) %>% 
        dplyr::group_by(binomial, presence, origin, seasonal) %>% dplyr::summarise(N = n())
    }
    
    # 1m buffer to avoid having lines at some borders (very quick)
    distGBIF <- st_buffer(distGBIF, 1)

  }
  
  return(distGBIF)
}


### Save distribution mapped from the GBIF procedure
sRL_saveMapDistribution <- function(scientific_name, Storage_SP) {
  tryCatch({
    
    ### Create a file path
    upload_folder_scientific_name <- R.utils::capitalize(paste0(stringr::str_replace(scientific_name, " ", "_"), ifelse("dat_proj_saved" %in% names(Storage_SP), "_Created_", "_Edited_"), format(Sys.time(), "%Y%m%d"))) # nolint
    filePath <- paste0(config$distribution_path, scientific_name, "/", upload_folder_scientific_name, "/") # nolint
    if (dir.exists(filePath)==F) {dir.create(filePath, showWarnings = TRUE, recursive = TRUE)}
    path <- paste0(filePath, upload_folder_scientific_name, ".gpkg")
    distSP_saved <- Storage_SP$distSP_saved
    
    # Remove hybas columns if hydrobasins
    distSP_saved$Popup <- distSP_saved$hybas_id <- distSP_saved$next_down <- distSP_saved$next_sink <- distSP_saved$ID <- NULL
    
    # Save
    st_write(distSP_saved, path, layer="polygon", append = FALSE)
    
    if("dat_proj_saved" %in% names(Storage_SP)){
      # Basic text for 1b
      text <- paste0(
        "A distribution has been stored for the species: ",
        scientific_name,
        ".\nIt was created with the mapping procedure from the sRedList platform. It is based on ", # nolint
        paste(names(table(Storage_SP$dat_proj_saved$Source_type)), table(Storage_SP$dat_proj_saved$Source_type), sep=" (") %>% paste(., collapse="), ") %>% paste0(nrow(Storage_SP$dat_proj_saved), " raw geo-referenced observations from: ", ., ")"),
        " occurrence records gathered on the ",
        Sys.time(),
        " CET."
      )
      # Add a note if manually edited with EditPoly
      if(is.na(Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Gbif_EditPoly"])==F){text <- paste0(text, " It was then manually edited on sRedList.")}
    } else{
      # Basic text for 1a
      text <- paste0(
        "A distribution has been stored for the species: ",
        scientific_name,
        " on the ", Sys.time(),
        ".\nIt was manually edited on the sRedList platform from a distribution ",
        revalue(Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Distribution_Source"], c("Red List"="published in the last Red List assessment.", 
                                                                                               "Uploaded"="previously uploaded on the sRedList platform.",
                                                                                               "StoredOnPlatform"="previously created on the sRedList platform.",
                                                                                               "Edited"="previously edited on the sRedList platform.")
        )
      )
    }
    
    # Save JSON
    jsonlite::write_json(list(info = text), paste0(filePath, upload_folder_scientific_name, ".json"), auto_unbox= TRUE) # nolint
    
    return(upload_folder_scientific_name)
    
  }, error=function(e){"Edited distribution not saved on the platform"})
}


### Merge SF polygons with different columns
sRL_rbindfillSF <- function(poly1, poly2){
  
  # If one of them is null, return the other one
  if(is.null(poly1) | is.null(nrow(poly1))){return(poly2)}
  if(is.null(poly2) | is.null(nrow(poly2))){return(poly1)}
  
  ### Homogenise column names
  st_geometry(poly1) <- "geometry"
  st_geometry(poly2) <- "geometry"
  
  ### Add missing columns to poly1
  for(C1 in names(poly2)[! names(poly2) %in% names(poly1)]){poly1[,C1]<-NA}
  
  ### Add missing columns to poly2
  for(C2 in names(poly1)[! names(poly1) %in% names(poly2)]){poly2[,C2]<-NA}
  
  ### Merge both
  poly_merged <- rbind(poly1, poly2)
  
  ### If binomial or id_no present but not complete, complete them
  if("binomial" %in% names(poly_merged)){if(T %in% is.na(poly_merged$binomial)){poly_merged$binomial[is.na(poly_merged$binomial)] <- poly_merged$binomial[is.na(poly_merged$binomial)==F][1]}}
  if("id_no" %in% names(poly_merged)){if(T %in% is.na(poly_merged$id_no)){poly_merged$id_no[is.na(poly_merged$id_no)] <- poly_merged$id_no[is.na(poly_merged$id_no)==F][1]}}
  
  ### Return
  return(poly_merged)
}



# COO --------------------
sRL_cooExtract<-function(distSP, domain_pref, Crop_Country){
  
  ### Prepare COO terrestrial and freshwater
  if("Terrestrial" %in% domain_pref | "Freshwater" %in% domain_pref){
    
    ### Extract list of countries
    coo<-coo_raw
    
    # Map intersection and summarise attributes
    inter<-st_intersection(distSP, coo) %>% 
      as.data.frame() %>% 
      ddply(., .(SIS_name0, SIS_name1, lookup, lookup_SIS0), function(x){data.frame(presence=paste(sort(unique(x$presence)), collapse="|"), origin=paste(sort(unique(x$origin)), collapse="|"), seasonal=paste(sort(unique(x$seasonal)), collapse="|"))})
    
    # Match attributes
    coo$presence<-inter$presence[match(coo$lookup, inter$lookup)] %>% sRL_SelectUniquePres(.)
    coo$origin<-inter$origin[match(coo$lookup, inter$lookup)] %>% sRL_SelectUniqueOrig(.)
    coo$seasonal<-inter$seasonal[match(coo$lookup, inter$lookup)]
    
    ### Prepare plot attributed
    coo$Level0_occupied<-coo$SIS_name0 %in% inter$SIS_name0
    coo$Level1_occupied<-is.na(coo$presence)==F
    
    coo$Popup<-paste0("<b> National entity: ","</b>", coo$SIS_name0, ifelse(coo$Level0_occupied==T, " (Present)", " (Absent)"), "<br>", "<br>",
                      "<b> Subnational entity: ","</b>", coo$SIS_name1, ifelse(is.na(coo$SIS_name1)==T, "", ifelse(coo$Level1_occupied==T, " (Present)", " (Absent)")), "<br>")
    
    coo$Domain<-"Terrestrial"
  }
  
  
  ### Prepare COO marine
  if("Marine" %in% domain_pref){
    
    ### Extract list of countries
    eez<-eez_raw
    
    # Map intersection and summarise attributes
    inter<-st_intersection(st_make_valid(distSP), eez) %>% as.data.frame() %>% ddply(., .(SIS_name0, SIS_name1, lookup, lookup_SIS0), function(x){data.frame(presence=paste(unique(x$presence), collapse="|"), origin=paste(unique(x$origin), collapse="|"), seasonal=paste(unique(x$seasonal), collapse="|"))})
    
    # Match attributes
    eez$presence<-inter$presence[match(eez$lookup, inter$lookup)]
    eez$origin<-inter$origin[match(eez$lookup, inter$lookup)]
    eez$seasonal<-inter$seasonal[match(eez$lookup, inter$lookup)]
    
    ### Prepare plot attributes
    eez$Level0_occupied<-eez$SIS_name0 %in% inter$SIS_name0
    eez$Level1_occupied<-is.na(eez$presence)==F
    
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
  if(Crop_Country[1] != "" & Crop_Country != "Mediterranean"){
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
sRL_CropCountry<-function(distSP, Crop_Country, scientific_name, hydro=F){
  

  ### Select and merge countries (needed to avoid complicate distributions)
  country_sub <- sRL_ShapeCountryNRL(Crop_Country, scientific_name)
  country_sub <- country_sub %>% st_transform(., CRSMOLL)
  
  # Crop the distribution (if regular range map)
  if(hydro==F){
    distSP_crop<-st_intersection(distSP, country_sub)
  }

  # Subset (if hydrobasins)
  if(hydro==T){
    distSP_crop <- st_filter(distSP, country_sub, .predicate = st_intersects)
  }
  
  # Return
  return(distSP_crop)
}



### Functions to prepare results of COO analysis (the first one prepares and I keep coo_occ for final report, the second creates the infobox)
sRL_cooInfoBox_prepare<-function(coo, Storage_SP){
  
  # Subset coo with presence + if needed I group by (needed when marine + terrestrial since there are 2 polygons, one with occurrence and one without)
  coo_occ<-subset(coo, coo$Level1_occupied==T) 
  if(nlevels(as.factor(paste0(coo_occ$SIS_name0, coo_occ$SIS_name1))) < nrow(coo_occ)){coo_occ <- coo_occ %>% dplyr::group_by(SIS_name0, SIS_name1, lookup, lookup_SIS0, presence, origin, seasonal) %>% dplyr::summarise(N= n())}
  
  if("dat_proj_saved" %in% names(Storage_SP)){
    dat_proj<-Storage_SP$dat_proj_saved %>% st_transform(., st_crs(coo_occ))
    coo_occ$Records<-st_intersects(coo_occ, dat_proj) %>% lengths(.)>0
  }
  
  return(coo_occ)
}

sRL_cooInfoBox_format<-function(coo_occ, Storage_SP){
  
  # If occurrences, check which entities have occurrence records
  if("dat_proj_saved" %in% names(Storage_SP)){
    coo_occ$SIS_name0[coo_occ$Records==F & is.na(coo_occ$SIS_name1)] <- paste0("<i>", coo_occ$SIS_name0[coo_occ$Records==F & is.na(coo_occ$SIS_name1)], "</i>") # Italicise countries with no occurrence records and that are not split in subnational entities
    coo_occ$SIS_name1[coo_occ$Records==F & is.na(coo_occ$SIS_name1)==F] <- paste0("<i>", coo_occ$SIS_name1[coo_occ$Records==F & is.na(coo_occ$SIS_name1)==F], "</i>") # Italicise subnational entities with no occurrence records
    coo_occ$SIS_name0[coo_occ$Records==F & is.na(coo_occ$SIS_name1)==F] <- paste0("<i>", coo_occ$SIS_name0[coo_occ$Records==F & is.na(coo_occ$SIS_name1)==F], "</i>") # Italicise country if subnational entity has no occurrence records (I later keep the non-italics name if some entities are not in italics)
  }
  
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


sRL_CountriesAttributes <- function(Attributes, COL, TransDir){
  
  ### Prepare translation table
  if(COL=="presence"){TAB <- data.frame(Num=1:7, Char=c("Extant", "Probably Extant", "Possibly Extant", "Possibly Extinct", "Extinct Post-1500", "Presence Uncertain", "Expected Additional Range"))}
  if(COL=="origin"){TAB <- data.frame(Num=1:6, Char=c("Native", "Reintroduced", "Introduced", "Vagrant", "Origin Uncertain", "Assisted Colonisation"))}
  if(COL=="seasonal"){TAB <- data.frame(Num=1:5, Char=c("Resident", "Breeding Season", "Non-Breeding Season", "Passage", "Seasonal Occurrence Uncertain"))}
  
  ### Transform from character to numeric (in reverse order otherwise "Extant" is replaced before "Possibly Extant")
  if(TransDir=="char2num"){
    for(i in nrow(TAB):1){Attributes <- gsub(TAB$Char[i], TAB$Num[i], Attributes)}
  }
  
  ### Transform from numeric to character
  if(TransDir=="num2char"){
    Attributes <- as.character(Attributes)
    for(i in 1:nrow(TAB)){Attributes <- gsub(TAB$Num[i], TAB$Char[i], Attributes)}
  }
  
  return(Attributes)
}






sRL_LeafCountry <- function(coo, distSP_WGS, realms_raw, Storage_SP){
  
  # Prepare extent
  EXT<-1.2*extent(coo[coo$Level0_occupied==T,])
  if(is.na(EXT[1]) | is.na(EXT[2]) | is.na(EXT[3]) | is.na(EXT[4])){EXT<-1.2*extent(distSP_WGS)} # In case there is no overlap with countries (e.g., distribution at sea because of simplification)
  
  
  # Create leaflet
  Leaflet_COOtoexport<-leaflet() %>%
    fitBounds(lng1=EXT[1], lng2=EXT[2], lat1=EXT[3], lat2=EXT[4]) %>%
    addPolygons(data=coo,
                color=ifelse(coo$Level1_occupied==T, "black", "grey"),
                fillColor=coo$colour,
                popup=coo$Popup,
                stroke=T, weight=2, fillOpacity=1) %>%
    addPolygons(data=distSP_WGS, color="#D69F32", fill=F, group="Distribution")
  GROUPS<-c("Distribution")
  
  # Add realms (only if nrow>0 as I used empty df to say I don't want realms in Shiny_Countries); also add legend only for the first leaflet
  if(nrow(realms_raw)>0){
    Leaflet_COOtoexport<-Leaflet_COOtoexport %>%
      addLegend(position="bottomleft", colors=c(sRL_COOColours$Col[sRL_COOColours$Col %in% coo$colour], "#D69F32"), labels=c(sRL_COOColours$Label[sRL_COOColours$Col %in% coo$colour], "Distribution"), opacity=1) %>%
      addPolygons(data=realms_raw, group="Realms", fillOpacity=0.5)
    GROUPS <- c(GROUPS, "Realms")
  }
  
  # Add points if we had occurrences and add layer control
  if("dat_proj_saved" %in% names(Storage_SP)){
    
    Coords<-Storage_SP$dat_proj_saved %>% st_transform(., st_crs(4326)) %>% st_coordinates(.) %>% as.data.frame()
    Leaflet_COOtoexport<-Leaflet_COOtoexport %>%
      addCircleMarkers(lng=Coords[,1], lat=Coords[,2], color="black", fillOpacity=0.3, stroke=F, radius=2, group="Occurrence records") %>%
      addLayersControl(overlayGroups=c("Distribution", "Occurrence records", "Realms"), position="topleft", options=layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup("Realms")
    
    GROUPS <- c(GROUPS, "Occurrence records")
    
  }
  
  # Manage groups
  Leaflet_COOtoexport<-Leaflet_COOtoexport %>%
    addLayersControl(overlayGroups=sort(GROUPS), position="topleft", options=layersControlOptions(collapsed = FALSE))
  
  if("Realms" %in% GROUPS){Leaflet_COOtoexport<-Leaflet_COOtoexport %>% hideGroup("Realms")}
  
  # Return leaflet
  return(Leaflet_COOtoexport)
}



sRL_COOColours<-data.frame(
  Code=c("MarineFALSEFALSE", "MarineTRUEFALSE", "MarineTRUETRUE", "TerrestrialFALSEFALSE", "TerrestrialTRUEFALSE", "TerrestrialTRUETRUE"),
  Col=c("#D2D2D2", "#9595C3", "#5757A9", "white", "#F17777", "#8C2316"),
  Label=c("Absent (marine)", "Absent from subnational (marine)", "Present (marine)", "Absent (terrestrial)", "Absent from subnational (terrestrial)", "Present (terrestrial)")
)

