



sRL_createDataGBIF <- function(scientific_name, GBIF_SRC) { # nolint
  
  ### Reclassify GBIF_SRC
  if(GBIF_SRC==1){GBIF_source<-c("GBIF")}
  if(GBIF_SRC==2){GBIF_source<-c("OBIS")}
  if(GBIF_SRC==3){GBIF_source<-c("RL")}
  if(GBIF_SRC==4){GBIF_source<-c("GBIF", "OBIS")}
  if(GBIF_SRC==5){GBIF_source<-c("GBIF", "RL")}
  if(GBIF_SRC==6){GBIF_source<-c("OBIS", "RL")}
  if(GBIF_SRC==7){GBIF_source<-c("GBIF", "OBIS", "RL")}

  
  ### Download data
  # From GBIF
  if("GBIF" %in% GBIF_source){
    
    #Calculate the total number of data in GBIF
    OCC<-occ_count(taxonKey=name_backbone(name=scientific_name)$speciesKey, georeferenced = TRUE)
    
    if(OCC < config$LIM_GBIF){ # Download all data or structure download if more than LIM_GBIF
        dat_gbif <- rgbif::occ_data(scientificName=scientific_name, hasCoordinate = T, limit=config$LIM_GBIF)$data # occ_data is faster than occ_search because it omits some columns
    } else{
        dat_gbif <- sRL_StructureGBIF(scientificName = scientific_name)
    }
    
        dat_gbif$ID<-paste0(dat_gbif$decimalLongitude, dat_gbif$decimalLatitude, dat_gbif$year)
    dat_gbif$Source<-"GBIF"
  } else {dat_gbif<-NULL}
  
  # From OBIS (removing points at same location + year)
  if("OBIS" %in% GBIF_source){
    dat_obis <- robis::occurrence(scientific_name)
    dat_obis$ID<-paste0(dat_obis$decimalLongitude, dat_obis$decimalLatitude, dat_obis$date_year)
    dat_obis$year<-dat_obis$date_year
    dat_obis_sub<-subset(dat_obis, !dat_obis$ID %in% dat_gbif$ID)
    dat_obis_sub$Source<-"OBIS"
  } else {dat_obis_sub<-data.frame()}
  
  # From Red List point
  if("RL" %in% GBIF_source & paste0(scientific_name, ".csv") %in% list.files(config$POINTdistribution_path)){
    dat_RL<-read.csv(paste0(config$POINTdistribution_path, scientific_name, ".csv"))
    dat_RL$Source<-"RL"
    dat_RL$decimalLongitude<-dat_RL$longitude
    dat_RL$decimalLatitude<-dat_RL$latitude
    dat_RL$year<-dat_RL$event_year
    dat_RL$species<-dat_RL$binomial
    dat_RL$coordinateUncertaintyInMeters<-NA
  } else {dat_RL<-data.frame()}
  
  # Return error if no data found
  if(is.null(nrow(dat_gbif)) & nrow(dat_obis_sub)==0 & nrow(dat_RL)==0) {
    not_found("No data found! Check whether the scientific name of the species has been typed correctly or select other data sources") # nolint
    dat<-NULL
  } else {
    # Merge
    dat<-rbind.fill(dat_gbif, dat_obis_sub, dat_RL)
    print(paste0("Number of data: ", nrow(dat)))
  }
  
  #Select columns of interest
  dat <- dat %>%
    dplyr::select(any_of(c("species", "decimalLongitude", "decimalLatitude", "countryCode", "individualCount", # nolint
                  "gbifID", "id", "objectid", "family", "taxonRank", "coordinateUncertaintyInMeters", "year",
                  "basisOfRecord", "institutionCode", "datasetName", "Source", "source", "citation")))
  
  #Remove records with no spatial coordinates
  dat <- dat %>% filter(!is.na(decimalLongitude)) %>% filter(!is.na(decimalLatitude)) # nolint
  
  #Convert country code from ISO2c to ISO3c
  if(!"countryCode" %in% names(dat)){dat$countryCode<-NA} else{
  dat$countryCode <-  countrycode::countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')} # nolint
  dat <- data.frame(dat)
  
  
  return(dat)
}




### Function to download GBIF in a spatially structured manner (to avoid big sampling biases)
sRL_StructureGBIF<-function(scientificName){
  
  ##### DEFINE THE SAMPLING PATTERN
  ### Map density of observations and extract coordinates
  Fetch<-mvt_fetch(taxonKey = name_backbone(name=scientificName)$speciesKey, srs = "EPSG:4326", format="@4x.png") 
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
  eval(parse(text=paste("TAB$Lon_min<-revalue(TAB$Lon_group, c(", paste0("'X", 1:(length(Lon_breaks)-1), "'=Lon_breaks[", 1:(length(Lon_breaks)-1), "]", collapse=","), ")) %>% as.character(.) %>% as.numeric(.)")))
  eval(parse(text=paste("TAB$Lon_max<-revalue(TAB$Lon_group, c(", paste0("'X", 1:(length(Lon_breaks)-1), "'=Lon_breaks[", 2:length(Lon_breaks), "]", collapse=","), ")) %>% as.character(.) %>% as.numeric(.)")))
  eval(parse(text=paste("TAB$Lat_min<-revalue(TAB$Lat_group, c(", paste0("'Y", 1:(length(Lat_breaks)-1), "'=Lat_breaks[", 1:(length(Lat_breaks)-1), "]", collapse=","), ")) %>% as.character(.) %>% as.numeric(.)")))
  eval(parse(text=paste("TAB$Lat_max<-revalue(TAB$Lat_group, c(", paste0("'Y", 1:(length(Lat_breaks)-1), "'=Lat_breaks[", 2:length(Lat_breaks), "]", collapse=","), ")) %>% as.character(.) %>% as.numeric(.)")))
  
  # Determine number of data to download per group to sum at LIM_GBIF
  TAB$N_download<-ifelse(TAB$N < (config$LIM_GBIF/nrow(TAB)), TAB$N, NA)
  TAB$N_download[is.na(TAB$N_download)]<-round((config$LIM_GBIF-sum(TAB$N_download, na.rm=T))/nrow(TAB[is.na(TAB$N_download),]))
  
  
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


### Function that subsets the observations to flag
sRL_cleanDataGBIF <- function(flags, year_GBIF, uncertainty_GBIF, keepyearNA_GBIF, sea_GBIF, GBIF_xmin, GBIF_xmax, GBIF_ymin, GBIF_ymax) { # nolint

  flags$.summary<-NULL
  
  ### Change flagging for sea
  if(sea_GBIF==0){flags$.sea<-NULL} # If Sea=0, we should not select data based on sea
  if(sea_GBIF==2){flags$.sea<-revalue(as.character(flags$.sea), c("TRUE"="FALSE", "FALSE"="TRUE"))} %>% as.factor(.) # If Sea=2, we keep only seas (so we flag land)
  
  ### Add flagging for year and uncertainty
  flags$.year <- flags$year > year_GBIF
  if(keepyearNA_GBIF==F){flags$.year[is.na(flags$.year)]<-F}
  
  if(!"coordinateUncertaintyInMeters" %in% names(flags)){flags$coordinateUncertaintyInMeters<-NA}
  flags$.uncertainty <- flags$coordinateUncertaintyInMeters < uncertainty_GBIF*1000 
  
  ### Add flagging for points outside GBIF_xmin...
  flags$.limits<-(flags$decimalLongitude < GBIF_xmin | flags$decimalLongitude > GBIF_xmax | flags$decimalLatitude < GBIF_ymin | flags$decimalLatitude > GBIF_ymax)==F
  
  ### Add a column with pasted reasons
  flags$Reason<-apply(
    flags[, which(substr(names(flags), 1, 1)==".")], # Apply to the columns that start with a dot (i.e., the column added by clean_coordinates)
    1, # Apply by row
    function(x){
      Reas<-names(x)[x==F] %>% .[is.na(.)==F] # Extract all reasons, i.e., column names where valid==F
      if(length(Reas)==0){NA} else{ # If no reason, I put NA as the reason
        Reas %>% # If there is a reason I rename and paste the reasons
          revalue(., c(".val"="Validity", ".equ"="Equal_LonLat", ".zer"="Zero_Coordinates", ".cap"="Capitals", ".cen"="Country_centroids", ".gbf"="GBIF_headquarters", ".inst"="Institutions", ".sea"="Sea", ".year"="Year", ".uncertainty"="Coordinates_uncertainty", ".limits"="Outside_extent"), warn_missing=F) %>%
          paste(collapse="; ")
      }}
  )
  
  return(flags)
  
}





### Filter GBIF data
sRL_SubsetGbif<-function(flags, scientific_name){
  
  # Prepare GBIF data for mapping
  dat_cl <- flags[is.na(flags$Reason)==T,] # Keep only data that are not flagged
  gbif_data_number <- nrow(dat_cl)
  
  assign(paste0("gbif_number_saved_", sub(" ", "_", scientific_name)), gbif_data_number, .GlobalEnv)
  
  # Prepare spatial points
  dat_proj<-st_as_sf(dat_cl,coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84") %>%
    st_transform(., st_crs(CRSMOLL)) 
  
  return(dat_proj)
  
}



### Create Distribution map from GBIF data
sRL_MapDistributionGBIF<-function(dat, scientific_name, First_step, AltMIN, AltMAX, Buffer_km2, GBIF_crop){

  ### The first step must be EOO, or Kernel, or Hydrobasins
  if(First_step=="EOO"){
    distGBIF<-st_as_sf(st_convex_hull(st_union(dat)))
    st_geometry(distGBIF)<-"geometry" # Rename the variable including geometry
  }
  
  if(First_step=="Kernel"){
    # Remove duplicate points (points with same lon/lat)
    dat_subsample<-distinct(dat, as.character(geometry), .keep_all=T) 

    # Calculate kernels
    kernel.ref <- kernelUD(as_Spatial(dat), h = "href")  # href = the reference bandwidth
    distGBIF <- getverticeshr(kernel.ref, percent = 99) %>% st_as_sf(.)
  }
  
  if(First_step=="Hydrobasins"){
    hydro_sub<-st_crop(hydro_raw, extent(dat))
    interHyd<-st_join(dat, hydro_sub, join=st_intersects) %>% subset(., is.na(.$hybas_id)==F) # Identify hydrobasins with data 
    distGBIF<-subset(hydro_raw, hydro_raw$hybas_id %in% interHyd$hybas_id) # Isolate these hydrobasins
  }
  
  if(First_step=="MCP-Hydrobasins"){
    EOO<-st_as_sf(st_convex_hull(st_union(dat)))
    st_geometry(EOO)<-"geometry"
    hydro_sub<-st_crop(hydro_raw, extent(EOO))
    interHyd<-st_join(EOO, hydro_sub, join=st_intersects) %>% subset(., is.na(.$hybas_id)==F) # Identify hydrobasins with data 
    distGBIF<-subset(hydro_raw, hydro_raw$hybas_id %in% interHyd$hybas_id) # Isolate these hydrobasins
  }
  
  ### Apply buffer
  distGBIF<-st_buffer(distGBIF, Buffer_km2*1000) %>% st_as_sf()
  
  
  ### Apply crop by land/sea
  # Create countries map based on the buffer
  CountrySP<-st_crop(distCountries_notsimplif, 1.1*extent(distGBIF))
  
  distGBIF$binomial=scientific_name
  
  # Remove land or sea if requested
  if(GBIF_crop=="Land"){
    distGBIF<-st_intersection(distGBIF, CountrySP) %>% 
      dplyr::group_by(binomial) %>% dplyr::summarise(N = n())}

  if(GBIF_crop=="Sea"){
    countr<-CountrySP %>% st_crop(., extent(distGBIF)) %>% dplyr::group_by() %>% dplyr::summarise(N = n())
    distGBIF<-st_difference(distGBIF, countr)}

  
  ### Merge
  distGBIF<-distGBIF %>% dplyr::group_by(binomial) %>% dplyr::summarise(N = n())
  
  ### Apply crop by altitude
  if(AltMIN>0 | AltMAX<9000){
  mcp.spatial <- as_Spatial(distGBIF)
  sp.mcp.terra <- terra::vect(distGBIF)
  
  dem.crop <- terra::crop(alt_raw, ext(mcp.spatial))
  
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
  
  ### Restrict CountrySP in case the altitude reduced it a lot, and store in Storage_SP
  Storage_SP=sRL_reuse(scientific_name)
  Storage_SP$CountrySP_saved<-CountrySP
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  
  ### Prepare to export
  distGBIF$binomial<-scientific_name
  distGBIF$id_no<-ifelse(scientific_name %in% speciesRL$scientific_name, speciesRL$taxonid[speciesRL$scientific_name==scientific_name], 99999999999)
  distGBIF$presence<-1
  distGBIF$origin<-1
  distGBIF$seasonal<-1

  return(distGBIF)
  
}


### Save distribution mapped from the GBIF procedure
sRL_saveMapDistribution <- function(scientific_name) {
  
  Storage_SP<-sRL_reuse(url_decode(scientific_name))
  

  ### Create a file path E.g: Distributions/Nile tilapia/Nile_tilapia_GBIF_20211207/ # nolint
  upload_folder_scientific_name <- R.utils::capitalize(paste0(stringr::str_replace(scientific_name, " ", "_"), format(Sys.time(), "_GBIF_%Y%m%d"))) # nolint
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
    paste(names(table(Storage_SP$dat_proj_saved$Source)), table(Storage_SP$dat_proj_saved$Source), sep=" (") %>% paste(., collapse="), ") %>% paste0(nrow(Storage_SP$dat_proj_saved), " raw geo-referenced observations from: ", ., ")"),
    " occurrence data downloaded from GBIF on the ",
    Sys.time(),
    " CET."
  )
  jsonlite::write_json(list(info = text), paste0(filePath, upload_folder_scientific_name, ".json"), auto_unbox= TRUE) # nolint
  return(upload_folder_scientific_name)
  # writeLines(text, paste0(filePath, upload_folder_scientific_name, ".txt"))
}




