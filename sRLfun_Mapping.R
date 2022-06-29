



sRL_createDataGBIF <- function(scientific_name, LIM_GBIF) { # nolint
  #Download data
  dat <- rgbif::occ_search(scientificName = scientific_name, hasCoordinate = T, limit=10000)$data # nolint
  #print(dat)
  if (is.null(dat)) {
    not_found("GBIF occurrences of the species could not be found! Check whether the scientific name of the species has been typed correctly!") # nolint
  }
  #Select columns of interest
  dat <- dat %>%
    dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount, # nolint
                  gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                  basisOfRecord, institutionCode, datasetName)
  
  #Remove records with no spatial coordinates
  dat <- dat %>% filter(!is.na(decimalLongitude)) %>% filter(!is.na(decimalLatitude)) # nolint
  
  #Convert country code from ISO2c to ISO3c
  dat$countryCode <-  countrycode::countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c') # nolint
  dat <- data.frame(dat)
  
  
  return(dat)
}




### Function that subsets the observations to flag
sRL_cleanDataGBIF <- function(flags, year_GBIF, uncertainty_GBIF, keepyearNA_GBIF, cleaningpar_GBIF, GBIF_xmin, GBIF_xmax, GBIF_ymin, GBIF_ymax) { # nolint

  ### Keep only columns of CleanCoordinate parameters I want to keep
  if("capitals" %not in% cleaningpar_GBIF){flags$.cap<-NULL}
  if("centroids" %not in% cleaningpar_GBIF){flags$.cen<-NULL}
  if("equal" %not in% cleaningpar_GBIF){flags$.equ<-NULL}
  if("gbif" %not in% cleaningpar_GBIF){flags$.gbf<-NULL}
  if("institutions" %not in% cleaningpar_GBIF){flags$.inst<-NULL}
  if("zeros" %not in% cleaningpar_GBIF){flags$.zer<-NULL}
  if("seas" %not in% cleaningpar_GBIF){flags$.sea<-NULL}
  flags$.summary<-NULL
  
  ### Add flagging for year and uncertainty
  flags$.year <- flags$year > year_GBIF
  if(keepyearNA_GBIF==F){flags$.year[is.na(flags$.year)]<-F}
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





### Create EOO polygon from GBIF data
sRL_MapEOOGBIF<-function(flags, scientific_name){

# Prepare GBIF data for mapping
dat_cl <- flags[is.na(flags$Reason)==T,] # Keep only data that are not flagged
gbif_data_number <- nrow(dat_cl)

assign(paste0("gbif_number_saved_", sub(" ", "_", scientific_name)), gbif_data_number, .GlobalEnv)

# Prepare spatial points
dat_proj<-st_geometry(st_as_sf(dat_cl,coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84")) %>%
  st_transform(., st_crs(CRSMOLL))

# Create EOO
EOO_GBIF<-st_as_sf(st_convex_hull(st_union(dat_proj)))
st_geometry(EOO_GBIF)<-"geometry" # Rename the variable including geometry

return(EOO_GBIF)

}




### Create Distribution map from GBIF data
sRL_MapDistributionGBIF<-function(EOO_GBIF, GBIF_BUFF_km2, GBIF_crop, scientific_name){
  
  # Use a buffer around the EOO
  distGBIF<-st_buffer(EOO_GBIF, GBIF_BUFF_km2*1000) %>% st_as_sf()
  distGBIF$binomial<-as.factor(scientific_name)
  
  ### Create countries map based on the buffer
  CountrySP<-st_crop(distCountries_notsimplif, extent(distGBIF))
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), list(CountrySP_saved=CountrySP, Creation=Sys.time()), .GlobalEnv)
  
  # Remove land or sea if requested
  if(GBIF_crop=="Land"){
    distGBIF<-st_intersection(distGBIF, CountrySP) %>% 
      dplyr::group_by(binomial) %>% dplyr::summarise(N = n())}
  
  if(GBIF_crop=="Sea"){
    countr<-CountrySP %>% st_crop(., extent(distGBIF)) %>% dplyr::group_by() %>% dplyr::summarise(N = n())
    distGBIF<-st_difference(distGBIF, countr)}

  # Set default columns to fit with final expected format
  distGBIF$id_no<-ifelse(scientific_name %in% speciesRL$scientific_name, speciesRL$taxonid[speciesRL$scientific_name==scientific_name], 99999999999)
  distGBIF$presence<-1
  distGBIF$origin<-1
  distGBIF$seasonal<-1

  # Export
  return(distGBIF)
  
}


### Save distribution mapped from the GBIF procedure
sRL_saveMapDistribution <- function(scientific_name, distSP, gbif_nb) {
  # Create a file path E.g: Distributions/Nile tilapia/Nile_tilapia_GBIF_20211207/ # nolint
  upload_folder_scientific_name <- R.utils::capitalize(paste0(stringr::str_replace(scientific_name, " ", "_"), format(Sys.time(), "_GBIF_%Y%m%d"))) # nolint
  # Create a file path
  filePath <- paste0(config$distribution_path, scientific_name, "/", upload_folder_scientific_name, "/") # nolint
  if (dir.exists(filePath)) {
    print("The directory exists")
  } else {
    # create the "my_new_folder
    dir.create(filePath, showWarnings = TRUE, recursive = TRUE)
    #dir.create(filePath)
  }
  path <- paste0(filePath, upload_folder_scientific_name, ".shp")
  st_write(distSP, path, append = FALSE)
  
  print("Write metadata file")
  text <- paste0(
    "A distribution has been stored for the species: ",
    scientific_name,
    ".\nIt was created with the GBIF procedure from the sRedList platform, based on ", # nolint
    gbif_nb,
    " occurrence data downloaded from GBIF on the ",
    Sys.time(),
    " CET."
  )
  jsonlite::write_json(list(info = text), paste0(filePath, upload_folder_scientific_name, ".json"), auto_unbox= TRUE) # nolint
  return(upload_folder_scientific_name)
  # writeLines(text, paste0(filePath, upload_folder_scientific_name, ".txt"))
}





