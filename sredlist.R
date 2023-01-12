


# M1: Charge distributions ----------------------------------------------------------------


#* Upload Distribution species
#* @post species/<scientific_name>/distribution
#* @param req:file Distribution file (shp,.shx,.prj,.dbf,.cpg)
#* @param scientific_name:str Insert Species Name
#* @serializer unboxedJSON
#* @parser multi
#* @tag sRedList1
function(scientific_name, req) {
  scientific_name <- url_decode(scientific_name)
  scientific_name <- R.utils::capitalize(trim(gsub("[[:punct:]]", " ", scientific_name))) # nolint
  
  # Required for multiple file uploads
  names(req)
  
  # Parses into a Rook multipart file type;needed for API conversions
  fileInfo <- list(formContents = Rook::Multipart$parse(req)) # nolint
  # This is where the file name is stored
  # print(fileInfo$formContents$file$filename) # nolint
  file_name <- fileInfo$formContents$req$filename
  
  print(file_ext(file_name))
  if(file_ext(file_name) %not in% extensions) {
    invalid_extension(file_name)
  }
  
  # The file is downloaded in a temporary folder
  tmpfile <- fileInfo$formContents$req$tempfile
  #print(fileInfo) # nolint
  upload_folder_scientific_name <- R.utils::capitalize(paste0(stringr::str_replace(scientific_name, " ", "_"), format(Sys.time(), "_%Y%m%d"))) # nolint
  # Create a file path E.g: Distributions/Nile tilapia/Nile_tilapia_20211207/
  filePath <- paste0(config$distribution_path, scientific_name, "/", upload_folder_scientific_name, "/") # nolint
  if (dir.exists(filePath)) {
    print("The directory exists")
  } else {
    # create the "my_new_folder
    dir.create(filePath, showWarnings = TRUE, recursive = TRUE)
  }
  print(file_name)
  new_file_name2 = paste0(upload_folder_scientific_name, ".", file_ext(file_name)) # nolint
  fn <- paste0(filePath, new_file_name2, sepp = "")
  print(fn)
  
  #Copies the file into the designated folder
  file.copy(tmpfile, fn)
  #file.rename(fn , fn)
  

  print(paste0("Your file is now stored in ", fn))
  return(list(path = fn))
}



#* Info distribution species from sRedList platform
#* @get species/<scientific_name>/distribution/info
#* @param scientific_name:string Scientific Name
#* @param path:string Distribution Folder default RedList
#* @tag sRedList1
function(scientific_name, path = "") {
  
  scientific_name <- url_decode(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  
  speciesPath <- paste0(config$distribution_path, scientific_name, "/", path) # nolint
  files <- base::list.files(path = speciesPath, pattern = "\\.shp$")
  
  if (length(files) == 0) {
    not_found("Shapefile of the species does not exist!") # nolint
  } else {
    distributionPath <- paste0(speciesPath, "/", files[1]) # nolint
  }
  
  print(distributionPath)
  distributions <- sf::st_read(distributionPath)
  
  ### Clean the distribution
  distSP <-sRL_PrepareDistrib(distributions, scientific_name) # nolint
  
  return(list(
    presences = union(c(1, 2, 3), unique(distSP$presence)),
    seasons = union(c(1, 2), unique(distSP$seasonal)),
    origins = union(c(1, 2), unique(distSP$origin))
  ))
}



#* Plot the distributions plot from sRedList platform
#* @get species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param presences:[int] presences (1, 2)
#* @param seasons:[int] seasons (1, 2)
#* @param origins:[int] origins (1, 2)
#* @param path:string Distribution Folder default RedList
#* @serializer png list(width = 800, height = 600)
#* @tag sRedList1
function(scientific_name, presences = list(), seasons = list() , origins = list(), path = "") { # nolint
  
  if(paste0("Storage_SP_", sub(" ", "_", url_decode(scientific_name))) %not in% ls(name=".GlobalEnv")){assign(paste0("Storage_SP_", sub(" ", "_", url_decode(scientific_name))), list(Creation=Sys.time(), Output=sRL_InitLog(scientific_name, DisSource = "Red List")), .GlobalEnv)}
  
  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  if (length(presences) != 0) presences <- as.character(presences);
  if (length(seasons) != 0) seasons <- as.character(seasons);
  if (length(origins) != 0) origins <- as.character(origins);
  

  ### Load Distribution Species
  distributions <- sRL_ReadDistribution(scientific_name, path) %>% sRL_PrepareDistrib(., scientific_name)
  distSP_full <- subset(distributions, distributions$binomial == scientific_name) # nolint 
  choice_presence <- c(presences)
  choice_season <- c(seasons)
  choice_origin <- c(origins)
  
  distSP <- subset(distSP_full, # nolint
                   distSP_full$presence %in% choice_presence &
                     distSP_full$seasonal %in% choice_season &
                     distSP_full$origin %in% choice_origin)
  
  ### Colour the distributions
  distSP<-sRL_ColourDistrib(distSP)
  
  ### Save the distribution in memory
  Storage_SP$distSP_saved<-distSP
  Storage_SP$distSP_savedORIGINAL <- distSP # I need to save it twice for country croping for National RL
  
  ### Prepare countries if they were not charged + crop depending on the current selection of range
  if("CountrySP_saved" %not in% names(Storage_SP)){Storage_SP$CountrySP_saved<-sRL_PrepareCountries(extent(distSP_full))} 
  CountrySP<-st_crop(Storage_SP$CountrySP_saved, extent(distSP))
  Storage_SP<-sRL_OutLog(Storage_SP, c("Distribution_Presence", "Distribution_Seasonal", "Distribution_Origin"), c(paste0(presences, collapse=","), paste0(seasons, collapse=","), paste0(origins, collapse=",")))
  Storage_SP<-sRL_OutLog(Storage_SP, "Distribution_Source", ifelse(substr(path, nchar(path)-2, nchar(path))=="_RL", "Red List", "Uploaded")) # If path ends by _RL it comes from the RL, uploaded otherwise
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  ### Plot
  if (nrow(distSP) > 0) {
    return(plot(ggplot() +
                  geom_sf(data = CountrySP, fill="gray96", col="gray50") + # nolint
                  geom_sf(data = distSP, fill = distSP$cols) +
                  theme_void() +
                  ggtitle("")))
  } else{
    sf::sf_use_s2(FALSE)
    return(plot(ggplot() +
                  geom_sf(data = CountrySP, fill="white96", col="gray50") + # nolint
                  theme_void() +
                  ggtitle("The distribution is empty")))
  }
  
}









# M2: Mapping ----------------------------------------------------------------
## a: Download ----------------------------------------------------------------

#* Global Biodiversity Information Facility Step 1
#* @post species/<scientific_name>/gbif
#* @param scientific_name:string Scientific Name
#* @param Gbif_Source:[string] Gbif_Source
#* @parser multi
#* @parser csv
#* @param Uploaded_Records:file A file
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, Gbif_Source=list(), Uploaded_Records="") {

  ### Clean-string from user
  scientific_name <- url_decode(scientific_name)
  scientific_name <- R.utils::capitalize(trim(gsub("[[:punct:]]", " ", scientific_name))) # nolint
  print(scientific_name)
  print(Gbif_Source)
  
  # Uploaded Records if we uploaded data (it's a list with 1 element being the title of the uploaded csv file); I edit the csv if separator not good
  if(Uploaded_Records != ""){
    Uploaded_Records<-Uploaded_Records[[1]] 
    if(ncol(Uploaded_Records)==1){print("CSV with wrong separator with ; separator"); Uploaded_Records<-Uploaded_Records %>% separate(col=names(Uploaded_Records)[1], into=unlist(strsplit(names(Uploaded_Records), ";")), sep=";")}
    if(ncol(Uploaded_Records)==1){print("CSV with wrong separator with tab separator"); Uploaded_Records<-Uploaded_Records %>% separate(col=names(Uploaded_Records)[1], into=unlist(strsplit(names(Uploaded_Records), "\t")), sep="\t")}
    if(ncol(Uploaded_Records)==1){wrong_csv_upload()}
    
    # Check longitude and latitude are provided
    if(! "latitude" %in% names(Uploaded_Records)){names(Uploaded_Records)<-replace(names(Uploaded_Records), names(Uploaded_Records) %in% c("y", "Y", "Latitude", "lat", "Lat"), "latitude")}
    if(! "longitude" %in% names(Uploaded_Records)){names(Uploaded_Records)<-replace(names(Uploaded_Records), names(Uploaded_Records) %in% c("x", "X", "Longitude", "lon", "Lon"), "longitude")}
    if((! "longitude" %in% names(Uploaded_Records)) | (! "latitude" %in% names(Uploaded_Records))){no_coords_update()}
    Uploaded_Records$longitude<-as.numeric(Uploaded_Records$longitude) ; Uploaded_Records$latitude<-as.numeric(Uploaded_Records$latitude)
    
    print(Uploaded_Records)
  }

  ### Create storage folder if it does not exist
  dir.create(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots"), recursive=T)

  ### GBIF procedure
  log_info("START - Create data")
  dat <- sRL_createDataGBIF(scientific_name, Gbif_Source, Uploaded_Records)

  # ### Plot
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_data.png"), plot(
    ggplot() +
      coord_fixed() +
      borders("world", colour = "gray86", fill = "gray80") +
      geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude, col=Source_type), size = 1) +
      scale_colour_brewer(type="qual", palette=2, name="Source")+
      ggtitle(paste(names(table(dat$Source_type)), table(dat$Source_type), sep=" (") %>% paste(., collapse="), ") %>% paste0("Raw geo-referenced observations (N=", nrow(dat), ") from: ", ., ")") )+
      sRLTheme_maps %+replace%   theme(legend.position="top")
    ), width=18, height=5.5) # nolint
  plot1 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_data.png"), mime = "image/png")
  log_info("END - Create data")
  log_info("START - Clean coordinates")


  # Prepare countries
  LIMS<-c(xmin=min(dat$decimalLongitude), xmax=max(dat$decimalLongitude), ymin=min(dat$decimalLatitude), ymax=max(dat$decimalLatitude))
  LIMS<-c(LIMS["xmin"] - 0.1*abs(LIMS["xmin"]-LIMS["xmax"]),    LIMS["xmax"] + 0.1*abs(LIMS["xmin"]-LIMS["xmax"]),
          LIMS["ymin"] - 0.1*abs(LIMS["ymin"]-LIMS["ymax"]),    LIMS["ymax"] + 0.1*abs(LIMS["ymin"]-LIMS["ymax"]))
  CountrySP_WGS<-st_crop(distCountries_WGS, LIMS)
  if(nrow(CountrySP_WGS)==0){
    Skip_country=T ; Tests_to_run=c("capitals", "centroids", "equal", "gbif", "institutions", "zeros")}else{
      Skip_country=F; Tests_to_run=c("capitals", "centroids", "equal", "gbif", "institutions", "zeros", "seas")}

  # Flag observations to remove
  flags_raw <- clean_coordinates(x = dat,
                                 lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 countries = "countryCode",
                                 species = "species",
                                 seas_ref=CountrySP_WGS %>% as_Spatial(.),
                                 tests = Tests_to_run)
  if(Skip_country==T){flags_raw$.sea<-FALSE}

  # # Extract elevation (need to transform the CRS for it)
  # log_info("START - Extracting elevation values")
  # flags_foralt<-st_geometry(st_as_sf(flags_raw,coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84")) %>%
  #   st_transform(., st_crs(CRSMOLL)) %>%
  #   st_as_sf(.)
  #
  # flags_raw$Alt_points=terra::extract(alt_raw, st_coordinates(flags_foralt), method="simple")$Elevation_reprojMollweide3
  # flags_raw$Alt_points<-replace(flags_raw$Alt_points, is.nan(flags_raw$Alt_points)==T, NA)
  # log_info("END - Extracting elevation values")


  # Assign
  output_to_save<-sRL_InitLog(scientific_name, DisSource = "Created") ; output_to_save$Value[output_to_save$Parameter=="Gbif_Source"]<-Gbif_Source
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), list(flags_raw_saved=flags_raw, CountrySP_WGS_saved=CountrySP_WGS, Creation=Sys.time(), Output=output_to_save), .GlobalEnv)

  
  return(list(
   plot_data = plot1))
  
}


## b: Filter ----------------------------------------------------------------
#* GBIF year
#* @get species/<scientific_name>/gbif-year
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {return(list(Gbif_Year = 1900))}

#* GBIF uncertainty
#* @get species/<scientific_name>/gbif-uncertainty
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {return(list(Gbif_Uncertainty = 100))}

#* GBIF Extent
#* @get species/<scientific_name>/gbif-extent
#* @param scientific_name:[string] Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  Gbif_Extent = data.frame(xmin=-180, xmax=180, ymin=-90, ymax=90)
  return(Gbif_Extent)}

#* Global Biodiversity Information Facility Step 2
#* @get species/<scientific_name>/gbif2
#* @param scientific_name:string Scientific Name
#* @param Gbif_Year:int Gbif_Year
#* @param Gbif_Uncertainty:int Gbif_Uncertainty
#* @param Gbif_Extent:[int] Gbif_Extent
#* @param Gbif_Sea:string Gbif_Sea
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, Gbif_Year= -1, Gbif_Uncertainty=-1, Gbif_Extent=list(), Gbif_Sea="") {
  
  log_info("START - GBIF Step 2")
  
  ### Transform parameters GBIF filtering
  scientific_name <- url_decode(scientific_name)
  print(Gbif_Year)
  print(Gbif_Uncertainty)
  Gbif_Extent<-as.numeric(Gbif_Extent) ; print(Gbif_Extent)
  print(Gbif_Sea)
  
  ### Charge downloaded data
  Storage_SP<-sRL_reuse(scientific_name)
  flags_raw<-Storage_SP$flags_raw_saved
  
  ### Subset the observations user wants to keep (can be run several times if users play with parameters)
  flags <- sRL_cleanDataGBIF(flags_raw, as.numeric(Gbif_Year), as.numeric(Gbif_Uncertainty), keepyearNA_GBIF=T, Gbif_Sea, Gbif_Extent[1], Gbif_Extent[2], Gbif_Extent[3], Gbif_Extent[4])
  dat_proj=sRL_SubsetGbif(flags, scientific_name)
  
  ### Assign in Storage_SP
  Storage_SP$dat_proj_saved<-dat_proj
  Storage_SP$flags<-flags
  Storage_SP<-sRL_OutLog(Storage_SP, c("Gbif_Year", "Gbif_Uncertainty", "Gbif_Sea", "Gbif_Extent"), c(Gbif_Year, Gbif_Uncertainty, Gbif_Sea, paste0(Gbif_Extent, collapse=",")))
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  log_info("END - GBIF Step 2")
  
  return(
    leaflet(flags) %>%
      addTiles() %>%
      addCircleMarkers(lng=flags$decimalLongitude,
                       lat=flags$decimalLatitude,
                       color=ifelse(is.na(flags$Reason)==T, "#fde725ff", "#440154ff"),
                       fillOpacity=0.5,
                       stroke=F,
                       popup=flags$PopText,
                       radius=8) %>%
      addLegend(position="bottomleft", colors=c('#fde725ff', '#440154ff'), labels=c("Valid", "Not valid")) %>%
      addMouseCoordinates() %>%
      addScaleBar(position = "bottomright")
  )
  
}




## c: Map ----------------------------------------------------------------
#* GBIF start
#* @get species/<scientific_name>/gbif-start
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {return(list(Gbif_Start = ""))}

#* GBIF buffer
#* @get species/<scientific_name>/gbif-buffer
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {return(list(Gbif_Buffer = 0))}

#* GBIF Altitude
#* @get species/<scientific_name>/gbif-altitude
#* @param scientific_name:[string] Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  Gbif_Altitude = data.frame(AltMIN=0, AltMAX=4000)
  return(Gbif_Altitude)}


#* Global Biodiversity Information Facility step 3
#* @get species/<scientific_name>/gbif3
#* @param scientific_name:string Scientific Name
#* @param Gbif_Start:string Gbif_Start
#* @param Gbif_Param:[int] Gbif_Param
#* @param Gbif_Buffer:int Gbif_Buffer
#* @param Gbif_Altitude:[int] Gbif_Altitude
#* @param Gbif_Crop:string Gbif_Crop
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, Gbif_Start="", Gbif_Param=list(), Gbif_Buffer=-1, Gbif_Altitude=list(), Gbif_Crop="") {
  
  # Transform parameters GBIF filtering
  scientific_name <- url_decode(scientific_name)
  Gbif_Buffer<-replace(Gbif_Buffer, Gbif_Buffer<0, 0)
  print(Gbif_Start)
  print(Gbif_Buffer)
  print(Gbif_Altitude)
  print(Gbif_Crop)
  Gbif_Param<-as.numeric(Gbif_Param) ; print(Gbif_Param)
  
  #GBIF STEP 3: Map distribution from GBIF
  log_info("START - Maps the distribution")
  
  # Get back GBIF observations
  Storage_SP=sRL_reuse(scientific_name)
  dat_proj=Storage_SP$dat_proj_saved
  if(nrow(dat_proj)==0){no_gbif_data()}
  
  
  # Create distribution
  distSP<-sRL_MapDistributionGBIF(dat_proj, scientific_name,
                                  First_step=Gbif_Start,
                                  AltMIN=as.numeric(Gbif_Altitude[1]), AltMAX=as.numeric(Gbif_Altitude[2]),
                                  Buffer_km2=as.numeric(Gbif_Buffer),
                                  GBIF_crop=Gbif_Crop,
                                  Gbif_Param=Gbif_Param)
  log_info("Map Distribution halfway")
  # Store and calculate area
  Storage_SP$gbif_number_saved=eval(parse(text=paste0("gbif_number_saved_", sub(" ", "_", scientific_name))))

  # Plot distribution
  Storage_SP$CountrySP_saved<-sRL_reuse(scientific_name)$CountrySP_saved

  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/eoo.png"), plot(
    ggplot() + 
      geom_sf(data=Storage_SP$CountrySP_saved, fill="gray70")+
      geom_sf(data = distSP, fill="darkred") + 
      geom_sf(data=dat_proj)+
      ggtitle("")+
      sRLTheme_maps
  ), width=18, height=5.5) # nolint
  plot3 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/eoo.png"), mime = "image/png") # nolint
  log_info("END - Maps the distribution")
  
  # Keep distribution in memory
  Storage_SP$distSP3_saved=distSP
  Storage_SP<-sRL_OutLog(Storage_SP, c("Mapping_Start", "Mapping_Crop", "Mapping_Buffer", "Mapping_Altitude"), c(Gbif_Start, Gbif_Crop, Gbif_Buffer, paste0(Gbif_Altitude, collapse=",")))
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  return(list(
    plot_eoo = plot3,
    gbif_data_number  = as.numeric(Storage_SP$gbif_number_saved)
  ))
  
}



## d: Smooth ----------------------------------------------------------------
#* GBIF smooth
#* @get species/<scientific_name>/gbif-smooth
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {return(list(Gbif_Smooth = 0))}

#* Global Biodiversity Information Facility 4 (smooth)
#* @get species/<scientific_name>/gbif4
#* @param scientific_name:string Scientific Name
#* @param Gbif_Smooth:num Gbif_Smooth
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, Gbif_Smooth=-1) {
  
  ### Transform parameters GBIF filtering
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)
  
  ### Smooth if parameter >0
  Gbif_Smooth=as.numeric(Gbif_Smooth) ; print(Gbif_Smooth)
  if(Gbif_Smooth>0){
  distSP<-smooth(Storage_SP$distSP3_saved, method = "ksmooth", smoothness=Gbif_Smooth, max_distance=10000)} else{
    distSP<-Storage_SP$distSP3_saved
  }
  distSP<-st_make_valid(distSP)
  
  
  ### If smooth and the distribution should be cropped by land/sea, we crop again after smoothing
  Crop_par<-Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Mapping_Crop"]
  if(Gbif_Smooth>0 & Crop_par %in% c("Land", "Sea")){
    if(Crop_par=="Land"){
      distSP<-st_intersection(distSP, Storage_SP$CountrySP_saved) %>% 
        dplyr::group_by(binomial) %>% dplyr::summarise(N = n())}
    
    if(Crop_par=="Sea"){
      countr<-Storage_SP$CountrySP_saved %>% st_crop(., extent(distSP)) %>% dplyr::group_by() %>% dplyr::summarise(N = n())
      distSP<-st_difference(distSP, countr)}
  }
  
  ### Keep distribution in memory
  Storage_SP$distSP_saved <- distSP
  Storage_SP$distSP_savedORIGINAL <- distSP # I need to save it twice for country croping for National RL
  Storage_SP<-sRL_OutLog(Storage_SP, "Mapping_Smooth", Gbif_Smooth)
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  ### Plot
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_final.png"), plot(
    ggplot() + 
      geom_sf(data=Storage_SP$CountrySP_saved, fill="gray70")+
      geom_sf(data = distSP, fill="darkred") + 
      geom_sf(data=Storage_SP$dat_proj_saved)+
      ggtitle("")+
      sRLTheme_maps
  ), width=18, height=5.5) # nolint
  plot_final <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_final.png"), mime = "image/png") # nolint
  
  # Save distribution in the platform
  gbif_path <- sRL_saveMapDistribution(scientific_name)
  
 return(list(
    plot_eoo = plot_final,
    eoo_km2 = 1, # eoo_km2 parameter is used in the client to know if we come from GBIF or RL distribution, so I have to keep it here or to change the client
    eoo_rating = 1,
    gbif_path = gbif_path
  ))
  
}










# M3: Countries of Occurrence ----------------------------------------------------------------

#* Countries of occurrence
#* @get species/<scientific_name>/analysis/coo
#* @param scientific_name:string Scientific Name
#* @param domain_pref:[str] domain_pref
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, domain_pref=list(), Crop_Country="") {

  # Filter parameters
  scientific_name<-url_decode(scientific_name)
  Storage_SP<-sRL_reuse(scientific_name)
  rownames(coo_raw)<-1:nrow(coo_raw)
  distSP<-Storage_SP$distSP_savedORIGINAL
  domain_pref<-revalue(as.character(domain_pref), c("1"="Terrestrial", "2"="Marine", "3"="Freshwater"))
  print(domain_pref)
  print(Crop_Country)
  
  # Crop for National Red Listing
  if(Crop_Country != ""){
    if(!Crop_Country %in% c(distCountries$SIS_name0, "Europe")){no_countries_crop()} else{
    distSP<-sRL_CropCountry(distSP, domain_pref, Crop_Country)
    Storage_SP$distSP_saved<-distSP
    }}
  
  # Prepare distribution and calculate COO
  distSP<-distSP %>% dplyr::group_by(origin, presence, seasonal) %>% dplyr::summarise(N= n()) %>% st_transform(., st_crs(coo_raw))
  distWGS<-st_transform(distSP, st_crs(coo_raw))
  coo<-sRL_cooExtract(distSP, domain_pref)
  
  # Simplify distribution if large distribution
  if((extent(distWGS)@xmax-extent(distWGS)@xmin)>50){distWGS<-st_simplify(distWGS, dTolerance=0.05)}
  
  # Create table of colours / labels and assign colours
  col.df<-data.frame(
    Code=c("MarineFALSEFALSE", "MarineTRUEFALSE", "MarineTRUETRUE", "TerrestrialFALSEFALSE", "TerrestrialTRUEFALSE", "TerrestrialTRUETRUE"),
    Col=c("#D2D2D2", "#9595C3", "#5757A9", "white", "#F17777", "#8C2316"),
    Label=c("Empty (marine)", "Subnational empty (marine)", "Occupied (marine)", "Empty (terrestrial)", "Subnational empty (terrestrial)", "Occupied (terrestrial)")
  )
  
  coo$colour<-paste0(coo$Domain, coo$Level0_occupied, coo$Level1_occupied) 
  coo$colour<-col.df$Col[match(coo$colour, col.df$Code)]
  
  # Save for SIS
  Storage_SP$countries_SIS<-sRL_OutputCountries(scientific_name, subset(coo, coo$presence>0), Storage_SP$AltPref_saved)
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  # Plot
  return(
    leaflet() %>%
      setView(lat = 10, lng = 0, zoom = 2) %>%
      addPolygons(data=coo,
                color=ifelse(coo$Level0_occupied==T, "black", "grey"),
                fillColor=coo$colour,
                popup=coo$Popup,
                stroke=T, weight=2, fillOpacity=1) %>%
      addPolygons(data=distWGS, color="#D69F32", fillOpacity=0.4) %>%
      addLegend(position="bottomleft", colors=c(col.df$Col[col.df$Col %in% coo$colour], "#D69F32"), labels=c(col.df$Label[col.df$Col %in% coo$colour], "Distribution"), opacity=1)
  )
}









# M4: EOO ----------------------------------------------------------------


#* Estimate the Extent of Occurrence (EOO) from range
#* @get species/<scientific_name>/analysis/eoo
#* @param scientific_name:string Scientific Name
#* @param presences:[int] presences (1, 2)
#* @param seasons:[int] seasons (1, 2)
#* @param origins:[int] origins (1, 2)
#* @param path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, presences = list(), seasons = list() , origins = list(), path = "") { # nolint

  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)

  # Load distribution
  distSP<-Storage_SP$distSP_saved

  # Create storage directory if it does not exist
  dir.create(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots"), recursive=T)

  ### Plot EOO
  log_info("START - Plot EOO \n")
  EOO <- st_as_sf(st_convex_hull(st_union(distSP))) ## Needed to avoid having different sub-polygons
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_eoo.png"), plot(
    ggplot() +
      geom_sf(data=EOO, fill="#ef3b2c", col=NA) +
      geom_sf(data=distSP, fill="#fcbba1", col=NA) +
      geom_sf(data=st_crop(Storage_SP$CountrySP_saved, extent(EOO)), fill=NA, col="black")+
      ggtitle(paste0("EOO of ", scientific_name)) +
      sRLTheme_maps
  ), width=6, height=6) # nolint
  plot3 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_eoo.png"), mime = "image/png") # nolint
  log_info("END - Plot EOO \n")

  ### Calculate EOO area
  EOO_km2 <- round(as.numeric(st_area(EOO))/1000000)


  return(list(
    eoo_km2 = EOO_km2,
    plot_eoo = plot3
  ))

}





# M5: AOH ----------------------------------------------------------------
## a: Data APIs ----------------------------------------------------------------


#* Species density preferences
#* @get species/<scientific_name>/density-preferences
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  
  density = density$Density[density$Species == scientific_name] %>% round(., 2)
  if(length(density)>1){density<-mean(density, na.rm=T)}
  
  return(list(
    density=density
  ));
}


#* Species generation length
#* @get species/<scientific_name>/generation-length
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  #Filter param
  scientific_name <- url_decode(scientific_name)

  # If value in GL_file we take it, otherwise default=1
  GL_species = ifelse(scientific_name %in% GL_file$internal_taxon_name, GL_file$GL_estimate[GL_file$internal_taxon_name==scientific_name][1], 1)

  return(list(GL_species = GL_species))
}


## b: Current AOH ----------------------------------------------------------------
#* Calculate Area of Habitat (AOH)
#* @get species/<scientific_name>/analysis/aoh
#* @param scientific_name:string Scientific Name
#* @param habitats_pref:[str] habitats_pref
#* @param habitats_pref_MARGINAL:[str] habitats_pref_MARGINAL
#* @param altitudes_pref:[int] altitudes_pref
#* @param density_pref:numeric density_pref
#* @param isGbifDistribution:boolean isGbifDistribution
#* @param path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, habitats_pref= list(), habitats_pref_MARGINAL=list(), altitudes_pref= list(), density_pref= '-1', isGbifDistribution = FALSE, path = "") { # nolint    
  
  # Clean memory
  log_info("START - Cleaning memory")
  sRL_cleaningMemory(Time_limit=90)
  log_info("END - Cleaning memory")
  
  if(length(habitats_pref)==0){no_habitat_pref()}
  
  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)
  distSP=Storage_SP$distSP_saved
  
  # Habitat table (for aoh analysis and for SIS Connect)
  if(! "habitats_pref_MARGINAL" %in% ls()){habitats_pref_MARGINAL=NA}
  habitats_pref_DF<-sRL_PrepareHabitatFile(scientific_name, habitats_pref, habitats_pref_MARGINAL)
  Storage_SP$habitats_SIS=habitats_pref_DF
  
  # Altitude table (for aoh analysis and for SIS Connect)
  if(length(altitudes_pref)==0){altitudes_pref<-c(0,9000)}
  altitudes_pref_DF<-sRL_PrepareAltitudeFile(scientific_name, altitudes_pref)
  Storage_SP$AltPref_saved=altitudes_pref_DF
  density_pref <- gsub(" ", "", density_pref)
  
  # Do I need to calculate 2 aoh (yes if uncertainty in habitats or altitude)
  Uncertain<-ifelse("Marginal" %in% habitats_pref_DF$suitability | TRUE %in% grepl("-", altitudes_pref), "Uncertain_yes", "Uncertain_no")
  Storage_SP$Uncertain<-Uncertain
  
  print(paste0("Habitat suitable: ", paste(habitats_pref, collapse=", "), "   ;   Habitat marginal: ", paste(habitats_pref_MARGINAL, collapse=", ")))
  print(altitudes_pref)
  print(density_pref)
  print(Uncertain)
  
  # Charge distribution
  distSP$binomial<-as.character(distSP$binomial)
  
  ### Prepare distribution, altitude, and preference files (i.e., part of the AOH analysis that has to be run only once)
  # Distribution (assuming seasonal=resident after users decided what they keep)
  distSP$seasonal=1
  
  rangeSP_clean<-create_spp_info_data(distSP, 
                                      keep_iucn_rl_presence = 1:6, # The selection has already been made, so I give fake numbers here
                                      keep_iucn_rl_seasonal = 1:5, 
                                      keep_iucn_rl_origin = 1:6,
                                      spp_summary_data = altitudes_pref_DF,
                                      spp_habitat_data = habitats_pref_DF, # Will only keep suitable habitat
                                      key=red_list_token,
                                      crs=st_crs(CRSMOLL))
  Storage_SP$RangeClean_saved=rangeSP_clean
  
  # Remove old stored AOH
  output_dir<-paste0("resources/AOH_stored/", sub(" ", "_", scientific_name))
  dir.create(paste0(output_dir, "/Current"), recursive=T)
  dir.create(paste0(output_dir, "/Current_optimistic"), recursive=T)
  dir.create(paste0(output_dir, "/Temporary"))
  do.call(file.remove, list(list.files(output_dir, full.names = TRUE, recursive=T)))
  terraOptions(tempdir=paste0(output_dir, "/Temporary"), memmax=config$RAMmax_GB)
  rasterOptions(tmpdir=paste0(output_dir, "/Temporary"), maxmemory=config$RAMmax_GB)
  
  
  ### SMALL-RANGES
  Range_size<-as.numeric(st_area(rangeSP_clean))/10^6 ; print(Range_size)
  AOH_type<-ifelse(Range_size < as.numeric(config$Size_LargeRange), "Small", "Large") ; print(AOH_type)
  if(AOH_type=="Small"){
    
    log_info("START - Small: Cropping rasters")
    alt_crop=crop(alt_raw, extent(distSP)) 
    Storage_SP$alt_crop_saved=alt_crop
    cci2_crop<-crop(cci2, extent(distSP))
    gc()
    log_info("END - Small: Cropping rasters")
    
    # Calculate AOH with "Suitable" habitats only (ie pessimistic)
    AOH2<-sRL_calculateAOH(rangeSP_fun=rangeSP_clean, 
                           cci_fun=cci2_crop, 
                           alt_fun=alt_crop,
                           FOLDER=paste0(output_dir, "/Current"),
                           elevation_data_fun=altitudes_pref_DF)
    
    # Calculate AOH with "Suitable" and "Marginal" habitats (ie optimistic)
    if(Uncertain == "Uncertain_yes"){
      # Include marginal habitats in the full_habitat_code cell
      rangeSP_cleanOPT<-rangeSP_clean
      rangeSP_cleanOPT$full_habitat_code<-paste(habitats_pref_DF$code, collapse="|")
      Storage_SP$RangeCleanOPT_saved=rangeSP_cleanOPT
      
      # Include extreme elevation if they exist
      if("elevation_lowerEXTREME" %in% names(altitudes_pref_DF)){rangeSP_cleanOPT$elevation_lower<-altitudes_pref_DF$elevation_lowerEXTREME[1]}
      if("elevation_upperEXTREME" %in% names(altitudes_pref_DF)){rangeSP_cleanOPT$elevation_upper<-altitudes_pref_DF$elevation_upperEXTREME[1]}
      
      # Calculate optimistic AOH: but if marginal habitats all point at CCI modalities already included with habitats_pref and elevation are not different, I use AOH2 directly as there won't be any difference
      if(length(unique(crosswalk_to_use$value[crosswalk_to_use$code %in% habitats_pref]))==length(unique(crosswalk_to_use$value[crosswalk_to_use$code %in% c(habitats_pref, habitats_pref_MARGINAL)])) & !"elevation_lowerEXTREME" %in% names(altitudes_pref_DF) & !"elevation_upperEXTREME" %in% names(altitudes_pref_DF)){
            AOH2_opt<-AOH2; log_info("Identical AOH, no need to calculate")} else{
      
            AOH2_opt<-sRL_calculateAOH(rangeSP_fun=rangeSP_cleanOPT, 
                                 cci_fun=cci2_crop, 
                                 alt_fun=alt_crop,
                                 FOLDER=paste0(output_dir, "/Current_optimistic"),
                                 elevation_data_fun=altitudes_pref_DF)
            log_info("Optimistic AOH calculated")
        }
      }
    
    ### PLOTS
    # Only pessimistic scenario
    if(Uncertain=="Uncertain_no"){
      plot1 <- gplot(AOH2[[1]]) +
        coord_fixed()+
        geom_tile(aes(fill = factor(value, levels=c("0", "1")))) +
        scale_fill_manual(values=c("#dfc27d", "#018571", NA), labels=c("Unsuitable", "Suitable", ""), name="", na.translate=F, drop=F) +
        ggtitle("Area of Habitat in 2020") +
        sRLTheme_maps
    }
    
    # Both scenario
    if(Uncertain=="Uncertain_yes"){

      plot1<-gplot(AOH2[[1]]+AOH2_opt[[1]])+
        coord_fixed()+
        geom_tile(aes(fill = factor(value, levels=c("0", "1", "2")))) +
        scale_fill_manual(values=c("#FBCB3C", "#54B967", "#004D40", NA), labels=c("Unsuitable", "Unknown", "Suitable", ""), name="", na.translate=F, drop=F) +
        ggtitle(paste0("Area of Habitat in ", config$YearAOH2)) +
        sRLTheme_maps
    }
    
    
    ### LARGE-RANGES
  } else {
    
    AOH2<-sRL_largeAOH(habitats_pref, altitudes_pref_DF[, c("elevation_lower", "elevation_upper")], rangeSP_clean, config$YearAOH2)
    
    if(Uncertain=="Uncertain_no"){
      plot1 <- plot(gplot((AOH2[[1]]/9)) + # Divide by 9 to get percents
        coord_fixed()+
        geom_tile(aes(fill = value)) +
        scale_fill_gradient(low="#dfc27d", high="#018571", name="Suitability (%)", limits=c(0,100), na.value=NA)+
        ggtitle(paste0("Area of Habitat in ", config$YearAOH2)) +
        sRLTheme_maps)
      
    } else{
      
      ## Calculate optimistic AOH
      alt_pref_extreme<-c(min(c(altitudes_pref_DF$elevation_lower, altitudes_pref_DF$elevation_lowerEXTREME), na.rm=T),
                          max(c(altitudes_pref_DF$elevation_upper, altitudes_pref_DF$elevation_upperEXTREME), na.rm=T)) ; print(alt_pref_extreme)
      
      # If there is no need to calculate a new one (no new CCI modalities or no new elevation limits, use the last one)
      if(length(unique(crosswalk_to_use$value[crosswalk_to_use$code %in% habitats_pref]))==length(unique(crosswalk_to_use$value[crosswalk_to_use$code %in% c(habitats_pref, habitats_pref_MARGINAL)])) & !"elevation_lowerEXTREME" %in% names(altitudes_pref_DF) & !"elevation_upperEXTREME" %in% names(altitudes_pref_DF)){
          AOH2_opt<-AOH2; log_info("Identical AOH, no need to calculate")} else{
          
          AOH2_opt<-sRL_largeAOH(c(habitats_pref, habitats_pref_MARGINAL), alt_pref_extreme, rangeSP_clean, config$YearAOH2)
          log_info("Optimistic AOH calculated")
      }
      
      plot1 <- grid.arrange(
        gplot((AOH2[[1]]/9)) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient(low="#dfc27d", high="#018571", name="Suitability (%)", limits=c(0,100), na.value=NA)+
          ggtitle("Pessimistic AOH") +
          labs(subtitle= "(marginal habitats / extreme elevations excluded)") +
          sRLTheme_maps,
        
        gplot((AOH2_opt[[1]]/9)) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient(low="#dfc27d", high="#018571", name="Suitability (%)", limits=c(0,100), na.value=NA)+
          ggtitle("Optimistic AOH") +
          labs(subtitle= "(marginal habitats / extreme elevations included)") +
          sRLTheme_maps,
        
        top=paste0("Area of Habitat in ", config$YearAOH2), ncol=1)
    }
    
  }
  
  Storage_SP$AOH2_saved=AOH2
  if(Uncertain=="Uncertain_yes"){Storage_SP$AOH2_opt_saved=AOH2_opt}
  Storage_SP$AOH_type<-AOH_type
  
  # Plot AOH and calculate area
  log_info("START - Plot AOH")
  
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoh.png"), plot = plot1, width=6, height=ifelse(Uncertain=="Uncertain_no" | AOH_type=="Small", 6, 10))
  plot1 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoh.png"), mime = "image/png", encoding = "base64") # nolint
  log_info("END - Plot AOH")
  
  AOH_km2 <-  sRL_areaAOH(AOH2[[1]], "cci") # Same scale in small or large AOH because the unit is always 1 cell of the fine raster
  if(Uncertain=="Uncertain_yes"){AOH_km2_opt <-  sRL_areaAOH(AOH2_opt[[1]], "cci") ;  Storage_SP$AOHkm2OPT_saved<-AOH_km2_opt}
  Storage_SP$AOHkm2_saved<-AOH_km2
  
  
  ### Calculate Area of Habitat in a resolution of 2x2km (as is the map of altitude provided), in order to use it as an upper bound of species Area of Occupancy under criterion B2 (each cell covers 4km2)
  grid22_crop<-crop(grid22, AOH2[[1]])
  aoh_22<-resample(AOH2[[1]], grid22_crop, method="max")>0
  
  if(Uncertain=="Uncertain_no"){
    plot2 <- plot(gplot(aoh_22[[1]]>0) +
      coord_fixed()+
      geom_tile(aes(fill = factor(as.character(value), c("0", "1")))) +
      scale_fill_manual(values=c("#dfc27d", "#018571"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
      labs(title="Area of Habitat (2x2km)", subtitle=ifelse(AOH_type=="Large", "Likely slightly overestimated (using a 10x10km aggregate raster)", ""))+
      sRLTheme_maps)
  }
  
  if(Uncertain=="Uncertain_yes"){
    aoh_22_opt<-resample(AOH2_opt[[1]], grid22_crop, method="max")>0
    
    # Plot on a single plot if small range, on two if large range
    if(AOH_type=="Small"){
      plot2<-gplot((aoh_22[[1]]>0)+(aoh_22_opt[[1]]>0))+
        coord_fixed()+
        geom_tile(aes(fill = factor(value, levels=c("0", "1", "2")))) +
        scale_fill_manual(values=c("#FBCB3C", "#54B967", "#004D40", NA), labels=c("Unsuitable", "Unknown", "Suitable", ""), name="", na.translate=F, drop=F) +
        ggtitle("Area of Habitat (2x2km); Likely slightly overestimated") +
        sRLTheme_maps
      
    } else{
    plot2 <- grid.arrange(
      gplot(aoh_22[[1]]>0) +
        coord_fixed()+
        geom_tile(aes(fill = factor(as.character(value), c("0", "1")))) +
        scale_fill_manual(values=c("#dfc27d", "#018571"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
        labs(title="Pessimistic")+
        sRLTheme_maps,
      
      gplot(aoh_22_opt[[1]]>0) +
        coord_fixed()+
        geom_tile(aes(fill = factor(as.character(value), c("0", "1")))) +
        scale_fill_manual(values=c("#dfc27d", "#018571"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
        labs(title="Optimistic")+
        sRLTheme_maps,
      
      top="Area of Habitat (2x2km); Likely slightly overestimated", ncol=1)
  }
  }

  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoo.png"), plot = plot2, width=6, height=ifelse(Uncertain=="Uncertain_no" | AOH_type=="Small", 6, 10))
  plot2 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoo.png"), mime = "image/png", encoding = "base64") # nolint

  AOO_km2<- sRL_areaAOH(aoh_22[[1]], SCALE="2x2")
  if(Uncertain=="Uncertain_yes"){AOO_km2_opt<- sRL_areaAOH(aoh_22_opt[[1]], SCALE="2x2")}
  Storage_SP$density_saved<-density_pref
  
  ### Save parameters and results
  Storage_SP<-sRL_OutLog(Storage_SP, c("AOH_HabitatPreference", "AOH_MarginalHabitatPreference", "AOH_ElevationPreference", "AOH_Density"), c(paste0(habitats_pref, collapse=","), paste0(habitats_pref_MARGINAL, collapse=","), paste0(altitudes_pref, collapse=", "), density_pref))
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  terraOptions(tempdir=tempdir())
  rasterOptions(tmpdir=tempdir())
  gc()

  ### Calculate population size (with uncertainty to due uncertainty in AOH and in density estimate)
  log_info("START - Calcualte population size")
  if (density_pref[1] != '-1') {
    density_pref <- unlist(strsplit(as.character(density_pref), "-")) %>% as.numeric(.) ; print(density_pref) # Density_pref has one value if certain, 2 values otherwise
    
    if(Uncertain=="Uncertain_no"){pop_size <- paste(round(AOH_km2 * density_pref), collapse="-")} # Multiplies AOH by both density estimates (or one if only one available)
    if(Uncertain=="Uncertain_yes"){pop_size <- paste(c(round(AOH_km2 * min(density_pref)), round(AOH_km2_opt * max(density_pref))), collapse="-")} # Multiplies pessimistic aoh by pessimistic density, optimistic AOH by optimistic density (or a single density if only one provided)
    print(pop_size)
  }
  log_info("END - Calcualte population size")
  
  ### Return list of arguments + calculate population size
  LIST=list(
    aoh_km2 = ifelse(Uncertain=="Uncertain_no", ceiling(AOH_km2), paste(ceiling(AOH_km2), ceiling(AOH_km2_opt), sep="-")), # I use ceiling to avoid having a 0 which is problematic
    aoo_km2 = ifelse(Uncertain=="Uncertain_no", round(AOO_km2), paste(round(AOO_km2), round(AOO_km2_opt), sep="-")),
    plot_aoh = plot1,
    plot_aoh_2x2 = plot2
  )

  if (density_pref[1] != '-1') {
    LIST$pop_size <- pop_size
  }

  return(LIST)

}



## c: Trends in AOH ----------------------------------------------------------------
#* Estimate trends in AOH as a proxy of population trends (Criterion A2)
#* @get species/<scientific_name>/analysis/trends-aoh
#* @param scientific_name:string Scientific Name
#* @param presences:[int] presences (1, 2)
#* @param seasons:[int] seasons (1, 2)
#* @param origins:[int] origins (1, 2)
#* @param habitats_pref:[str] habitats_pref
#* @param altitudes_pref:[int] altitudes_pref
#* @param GL_species:numeric GL_species
#* @param isGbifDistribution:boolean isGbifDistribution
#* @param path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, GL_species=1) { # nolint
  
  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)
  GL_species<-as.numeric(GL_species)
  distSP=Storage_SP$distSP_saved
  alt_crop=Storage_SP$alt_crop_saved
  rangeSP_clean=Storage_SP$RangeClean_saved
  habitats_pref_DF=Storage_SP$habitats_SIS
  altitude_pref_DF=Storage_SP$AltPref_saved
  AOH2=Storage_SP$AOH2_saved
  AOH_km2<-Storage_SP$AOHkm2_saved
  AOH_type<-Storage_SP$AOH_type
  rangeSP_cleanOPT=Storage_SP$RangeCleanOPT_saved
  
  # Charge distribution
  distSP$binomial<-as.character(distSP$binomial)
  
  # Output directory + options
  output_dir<-paste0("resources/AOH_stored/", sub(" ", "_", scientific_name))
  dir.create(paste0(output_dir, "/Initial"));   dir.create(paste0(output_dir, "/Initial_optimistic"))
  terraOptions(tempdir=paste0(output_dir, "/Temporary"), memmax=config$RAMmax_GB)
  rasterOptions(tmpdir=paste0(output_dir, "/Temporary"), maxmemory=config$RAMmax_GB)
  
  # Choose year to use 
  Year1_theo<-config$YearAOH2-max(10, round(3*GL_species))
  Year1<-max(Year1_theo, 1992) ; print(Year1)
  
  ### SMALL-RANGES
  if(AOH_type=="Small"){
    
    # Charge and crop CCI1
    log_info("START - Cropping rasters")
    cci1<-rast(sub("XXXX", Year1, config$cci1_raster_path)) ; crs(cci1)<-CRSMOLL # I ensure the CRS is correctly assigned
    cci1_crop<-crop(cci1, extent(distSP))
    log_info("END - Cropping rasters")
  
    # Calculate AOH
    AOH1<-sRL_calculateAOH(rangeSP_fun=rangeSP_clean,
                         cci_fun=cci1_crop,
                         alt_fun=alt_crop,
                         FOLDER=paste0(output_dir, "/Initial"),
                         elevation_data_fun=altitude_pref_DF)
    
    if(Storage_SP$Uncertain=="Uncertain_yes"){
      if("elevation_lowerEXTREME" %in% names(altitude_pref_DF)){rangeSP_cleanOPT$elevation_lower<-altitude_pref_DF$elevation_lowerEXTREME[1]}
      if("elevation_upperEXTREME" %in% names(altitude_pref_DF)){rangeSP_cleanOPT$elevation_upper<-altitude_pref_DF$elevation_upperEXTREME[1]}
      
      AOH1_opt<-sRL_calculateAOH(rangeSP_fun=rangeSP_cleanOPT,
                           cci_fun=cci1_crop,
                           alt_fun=alt_crop,
                           FOLDER=paste0(output_dir, "/Initial_optimistic"),
                           elevation_data_fun=altitude_pref_DF)
    }
  
   # Create plot
    if(Storage_SP$Uncertain=="Uncertain_no"){
      plot1 <- gplot((AOH2[[1]]*2+3)-AOH1[[1]]) + 
        coord_fixed()+
        geom_tile(aes(fill = factor(as.character(value), c("2", "3", "4", "5"))))+
        scale_fill_manual(values=c("#a6611a", "gray90", "#80cdc1", "#018571"), labels=c("Newly unsuitable", "Unsuitable", "Suitable", "Newly suitable"), name="", na.translate=F, drop=F)+
        ggtitle(paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2))+
        sRLTheme_maps
      
    } else{
      
      plot1 <-grid.arrange( 
        gplot((AOH2[[1]]*2+3)-AOH1[[1]]) +
          coord_fixed()+
          geom_tile(aes(fill = factor(as.character(value), c("2", "3", "4", "5")))) +
          scale_fill_manual(values=c("#a6611a", "gray90", "#80cdc1", "#018571"), labels=c("Newly unsuitable", "Unsuitable", "Suitable", "Newly suitable"), name="", na.translate=F, drop=F)+
          ggtitle("Pessimistic AOH") +
          labs(subtitle= "(marginal habitats / extreme elevations excluded)") +
          sRLTheme_maps,
        
        gplot((Storage_SP$AOH2_opt_saved[[1]]*2+3)-AOH1_opt[[1]]) +
          coord_fixed()+
          geom_tile(aes(fill = factor(as.character(value), c("2", "3", "4", "5")))) +
          scale_fill_manual(values=c("#a6611a", "gray90", "#80cdc1", "#018571"), labels=c("Newly unsuitable", "Unsuitable", "Suitable", "Newly suitable"), name="", na.translate=F, drop=F)+
          ggtitle("Optimistic AOH") +
          labs(subtitle= "(marginal habitats / extreme elevations included)") +
          sRLTheme_maps,
        
        top=paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2), ncol=1)
    }
  
   
  } else {
  ### LARGE-RANGES

    # Calculate AOH
    AOH1<-sRL_largeAOH(habitats_pref_DF$code[habitats_pref_DF$suitability=="Suitable"], altitude_pref_DF[, c("elevation_lower", "elevation_upper")], rangeSP_clean, Year1)
    
    # If no uncertainty, plot directly
    if(Storage_SP$Uncertain=="Uncertain_no"){
      plot1 <- plot(gplot((AOH2[[1]]-AOH1[[1]])/9) + 
        coord_fixed()+
        geom_tile(aes(fill = value))+
        scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="Suitability change (%)", limits=c(-100,100), na.value=NA, trans=colour_bidirect_scale, breaks=c(-100, -50, -10, 0, 10, 50, 100))+
        ggtitle(paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2))+
        sRLTheme_maps)
      
    } else { # If uncertainty, calculate other trends and plot

      alt_pref_extreme<-c(min(c(altitude_pref_DF$elevation_lower, altitude_pref_DF$elevation_lowerEXTREME), na.rm=T),
                          max(c(altitude_pref_DF$elevation_upper, altitude_pref_DF$elevation_upperEXTREME), na.rm=T)) ; print(alt_pref_extreme)

      AOH1_opt<-sRL_largeAOH(habitats_pref_DF$code, alt_pref_extreme, rangeSP_clean, Year1)

      plot1 <- grid.arrange(
        gplot((AOH2[[1]]-AOH1[[1]])/9) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="Suitability change (%)", limits=c(-100,100), na.value=NA, trans=colour_bidirect_scale, breaks=c(-100, -50, -10, 0, 10, 50, 100))+
          ggtitle("Pessimistic AOH") +
          labs(subtitle= "(marginal habitats / extreme elevations excluded)") +
          sRLTheme_maps,
        
        gplot((Storage_SP$AOH2_opt_saved[[1]]-AOH1_opt[[1]])/9) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="Suitability change (%)", limits=c(-100,100), na.value=NA, trans=colour_bidirect_scale, breaks=c(-100, -50, -10, 0, 10, 50, 100))+
          ggtitle("Optimistic AOH") +
          labs(subtitle= "(marginal habitats / extreme elevations included)") +
          sRLTheme_maps,
        
        top=paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2), ncol=1)
    }

    
  }  
  
  # Plot
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/trends_aoh.png"), plot = plot1, width=6, height=ifelse(Storage_SP$Uncertain=="Uncertain_no", 6, 10))
  plot1 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/trends_aoh.png"), mime = "image/png", encoding = "base64") # nolint
  
  # Calculate area and trends
  AOH_old_km2<-sRL_areaAOH(AOH1[[1]], SCALE="cci")
  AOH_lost<- (1-(AOH_km2/AOH_old_km2)) %>% as.numeric(.) %>% round(., digits=2)*100 ; if(AOH_old_km2==0){AOH_lost<-0} # Cannot divide by 0
  if(Storage_SP$Uncertain=="Uncertain_yes"){
    AOH_old_km2OPT<-sRL_areaAOH(AOH1_opt[[1]], SCALE="cci")
    AOH_lostOPT<- (1-(Storage_SP$AOHkm2OPT_saved/AOH_old_km2OPT)) %>% as.numeric(.) %>% round(., digits=2)*100
  }
  
  ### Store parameters and results
  Storage_SP$GL_saved<-GL_species
  Storage_SP$aoh_lost_saved=AOH_lost
  if(Storage_SP$Uncertain=="Uncertain_yes"){Storage_SP$aoh_lostOPT_saved=AOH_lostOPT}
  Storage_SP$Year1_saved<-Year1 ; Storage_SP$Year1theo_saved<-Year1_theo
  Storage_SP<-sRL_OutLog(Storage_SP, "AOH_GenerationLength", GL_species)
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  # Output to return
  Out_area<-ifelse(Storage_SP$Uncertain=="Uncertain_no", 
                   ceiling(AOH_old_km2),
                   paste(ceiling(AOH_old_km2), ceiling(AOH_old_km2OPT), sep="-"))
  
  Out_loss<-ifelse(Storage_SP$Uncertain=="Uncertain_no", 
                   paste0(Year1, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), abs(AOH_lost), "%"), # Give trend in AOH rather than loss,
                   paste0(Year1, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), abs(as.numeric(AOH_lost)), "% (Pessimistic) or ", revalue(as.factor(sign(AOH_lostOPT)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), abs(AOH_lostOPT), "% (Optimistic)")
  )
  
  
  # With extrapolation
  FACT_Extrap<-(config$YearAOH2-Year1_theo)/(config$YearAOH2-Year1) # Exponential extrapolation (formula from guidelines)
  Out_loss_extrap<-ifelse(Year1==Year1_theo, "", 
        ifelse(Storage_SP$Uncertain=="Uncertain_no", 
              paste0(Year1_theo, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), round(abs(100*(1-(1-AOH_lost/100)^FACT_Extrap))), "%"), # Give trend in AOH rather than loss,
              paste0(Year1_theo, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), round(abs(100*(1-(1-as.numeric(AOH_lost)/100)^FACT_Extrap))), "% (Pessimistic) or ", revalue(as.factor(sign(AOH_lostOPT)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), round(abs(100*(1-(1-AOH_lostOPT/100)^FACT_Extrap))), "% (Optimistic)")
  ))
  
  
  ### Return
  LIST<-list(
    aoh_lost_km2 = Out_area,
    aoh_lost = Out_loss,
    plot_trends_aoh = plot1
  )
  if(Out_loss_extrap != ""){LIST$aoh_lost_extrap = Out_loss_extrap}
  
  return(LIST)
  
}





# M6: Optional analyses ----------------------------------------------------------------


## a: Fragmentation -------------------------------
#* Species dispersion preferences
#* @get species/<scientific_name>/dispersion-preferences
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  
  dispersion=10
  
  
  return(list(
    dispersion=dispersion
  ));
}


#* Plot fragmentation
#* @get species/<scientific_name>/analysis/fragmentation
#* @param scientific_name:string Scientific Name
#* @param dispersion:string dispersion
#* @serializer png list(width = 1200, height = 600)
#* @tag sRedList
function(scientific_name, dispersion="-1") {
  
  #Prom<-future({
  
    ### Filter param
    scientific_name<-url_decode(scientific_name)
    Storage_SP<-sRL_reuse(scientific_name)
    aoh<-Storage_SP$AOH2_saved[[1]]
    aoh_type<-Storage_SP$AOH_type
    dispersion<-as.numeric(dispersion)*1000 ; print(dispersion)

    ### Calculate fragmentation
    res<-sRL_fragmentation(aoh, aoh_type, dispersion, Storage_SP$density_saved)
    
    
    ### Plots
    # AOH and clusters
    aohDF<-as.data.frame(aoh, xy = TRUE); names(aohDF)[3]<-"lyr1"
    G1<-ggplot() + 
      geom_tile(data = aohDF, aes(x = x, y = y, fill = factor(lyr1, levels=c("0", "1"))), alpha = 0.5, show.legend=F)+
      geom_sf(data=st_transform(res$clusters, crs(aoh)), fill=NA)+
      ggtitle(paste0("Population fragmentation in ", nrow(res$clusters), " clusters"))+
      scale_fill_manual(values=c("#dfc27d", "#018571", NA), labels=c("Unsuitable", "Suitable", ""), name="", na.translate=F, drop=F) +
      sRLTheme_maps
    
    # Cumulative fragmentation (depends if one or two density estimates)
    if(! "pop2" %in% names(res$prop.fragm)){
      G2<-ggplot(res$prop.fragm)+
        geom_step(aes(x=pop, y=CumSum), col="darkred", lwd=2)+
        geom_vline(xintercept=min(res$prop.fragm$pop[res$prop.fragm$prop.pop>0.5], na.rm=T), linetype="dashed")+
        geom_hline(yintercept=0.5, linetype="dashed")+
        xlab("How many individuals do you consider to be a 'small' population?")+ylab("Proportion of the population that is fragmented")+
        ylim(c(0,1))+
        labs(subtitle=paste0("Fragmented if you consider that a population with ", round(min(res$prop.fragm$pop[res$prop.fragm$prop.pop>0.5], na.rm=T)), " individuals is 'small'"))+
        theme_minimal()
    } else {
      G2<-ggplot(res$prop.fragm)+
        geom_step(aes(x=pop2, y=CumSum), col="coral2", lwd=2)+
        geom_step(aes(x=pop, y=CumSum), col="darkred", lwd=2)+
        geom_vline(xintercept=min(res$prop.fragm$pop[res$prop.fragm$prop.pop>0.5], na.rm=T), linetype="dashed")+
        geom_vline(xintercept=min(res$prop.fragm$pop2[res$prop.fragm$prop.pop2>0.5], na.rm=T), linetype="dashed")+
        geom_hline(yintercept=0.5, linetype="dashed")+
        xlab("How many individuals do you consider to be a 'small' population?")+ylab("Proportion of the population that is fragmented")+
        ylim(c(0,1))+
        labs(subtitle=paste0("Fragmented if you consider that a population with ", round(min(res$prop.fragm$pop[res$prop.fragm$prop.pop>0.5], na.rm=T)), "-", round(min(res$prop.fragm$pop2[res$prop.fragm$prop.pop2>0.5], na.rm=T)), " individuals is 'small'"))+
        theme_minimal()
      }
    
    Plot_Fragm<-grid.arrange(G1, G2, ncol=2)
    
    #Plot_Fragm
    
  #}, seed=T) 
  
  ### Plot the distribution
  #return(Prom %...>% plot())
  return(Plot_Fragm)
}





## b: Remote sensing products ---------------------------

#* Calculate trends in RS products
#* @get species/<scientific_name>/analysis/RS_analysis
#* @param scientific_name:string Scientific Name
#* @param RSproduct:string RSproduct
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, RSproduct = "") { # nolint    
  
  # Charge parameters
  scientific_name<-url_decode(scientific_name)
  Storage_SP<-sRL_reuse(scientific_name)
  distSP<-Storage_SP$RangeClean_saved
  GL<-Storage_SP$GL_saved
  print(RSproduct)
  
  # Run functions to calculate trends
  if(RSproduct=="Human_density"){List_trendsRS<-sRL_CalcHumandensity(scientific_name, distSP, GL)}
  if(RSproduct=="Forest_cover"){List_trendsRS<-sRL_CalcForestchange(scientific_name, distSP)}
  if(RSproduct=="NDVI"){List_trendsRS<-sRL_CalcNDVIchange(scientific_name, distSP, GL)}

  # Return
  return(List_trendsRS)
  
}







# M7: Outputs ----------------------------------------------------------------

#* Upload all parameters
#* @get species/<scientific_name>/species/final-estimates
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(){
  
  Estimates<-data.frame(T1="test_from_R", T2="test2 from R")
  
  return(list(T1="test_from_R", T2="test_from_R2"))
}


#* Plot Red List category
#* @get species/<scientific_name>/assessment/red-list-criteria
#* @param scientific_name:string Scientific Name
#* @param aoh_lost:int AOH_lost
#* @param eoo_km2:int EOO_km2
#* @param aoo_km2:int AOO_km2
#* @param pop_size:int Pop_size
#* @serializer png list(width = 800, height = 600)
#* @tag sRedList
function(scientific_name, eoo_km2, aoo_km2, pop_size, 
         Estimates, pastTrends_qual, pastTrends_basis, pastTrends_reversible, pastTrends_understood, pastTrends_ceased, fragment,
         Extreme_EOO, Extreme_AOO, Extreme_Pop, Extreme_NLoc, Extreme_NSub, Extreme_EOO_justif, Extreme_AOO_justif, Extreme_Pop_justif, Extreme_NLoc_justif, Extreme_NSub_justif,
         Continuing_EOO, Continuing_AOO, Continuing_Hab, Continuing_Pop, Continuing_NLoc, Continuing_NSub, Continuing_EOO_justif, Continuing_AOO_justif, Continuing_Hab_justif, Continuing_Pop_justif, Continuing_NLoc_justif, Continuing_NSub_justif,
         locationNumber,	locationNumber_justif, locationSub,	locationSub_justif,	Num_Largest, OneSubpop,	VeryRestricted,	VeryRestricted_justif,
         populationTrend, currentTrends_basis, currentTrends_years, futureTrends_quality, futureTrends_basis, futureTrends, futureTrends_justif, ongoingTrends_NY, ongoingTrends_basis, ongoingTrends_reversible, ongoingTrends_understood, ongoingTrends_ceased, ongoingTrends, ongoingTrends_justif,
         C_igen_value, C_igen_qual, C_igen_justif, C_iigen_value, C_iigen_qual, C_iigen_justif, C_iiigen_value, C_iiigen_qual, C_iiigen_justif
         ) {
  
  log_info("Pop")
  print(locationNumber)
  print(populationTrend)
  print(Estimates)
  
  log_info("Extreme fluctuations")
  print(Extreme_EOO) ; print(Extreme_AOO) ; print(Extreme_Pop) ; print(Extreme_NLoc) ; print(Extreme_NSub)
  print(Extreme_EOO_justif) ; print(Extreme_AOO_justif) ; print(Extreme_Pop_justif) ; print(Extreme_NLoc_justif) ; print(Extreme_NSub_justif)
  
  log_info("Continuing declines")
  print(Continuing_EOO) ; print(Continuing_AOO) ; print(Continuing_Hab) ; print(Continuing_Pop) ; print(Continuing_NLoc) ; print(Continuing_NSub)
  print(Continuing_EOO_justif) ; print(Continuing_AOO_justif) ; print(Continuing_Hab_justif) ; print(Continuing_Pop_justif) ; print(Continuing_NLoc_justif) ; print(Continuing_NSub_justif)
  
  log_info("Over")
  
  
  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)
  aoh_lost<-ifelse("aoh_lost_saved" %in% names(Storage_SP), 
    ifelse(Storage_SP$Uncertain=="Uncertain_no", Storage_SP$aoh_lost_saved, paste(Storage_SP$aoh_lost_saved, Storage_SP$aoh_lostOPT_saved, sep="/")),
    NA)    
  
  
  # Calculate criteria
  criteria<-sRL_CalculateCriteria(aoh_lost, eoo_km2, aoo_km2, pop_size)

  
  ### Prepare SIS Connect files
  AltPref_saved=Storage_SP$AltPref_saved
  habitats_SIS=Storage_SP$habitats_SIS[,6:13] ; habitats_SIS$assessment_id<-NA ; habitats_SIS$internal_taxon_id<-NA

  allfields_SIS<-sRL_CreateALLFIELDS(scientific_name, aoh_lost, eoo_km2, aoo_km2, pop_size, AltPref_saved)
  countries_SIS<-Storage_SP$countries_SIS
  ref_SIS<-sRL_OutputRef(scientific_name, AltPref_saved)
  
  # Save csv files in a folder
  output_dir<-paste0(sub(" ", "_", scientific_name), "_sRedList")
  dir.create(output_dir)
  write.csv(allfields_SIS, paste0(output_dir, "/allfields.csv"), row.names = F)
  write.csv(countries_SIS, paste0(output_dir, "/countries.csv"), row.names = F)
  write.csv(ref_SIS, paste0(output_dir, "/references.csv"), row.names = F)
  write.csv(habitats_SIS, paste0(output_dir, "/habitats.csv"), row.names = F)
  write.csv(Storage_SP$Output, paste0(output_dir, "/00.Output_log.csv"), row.names = F)
  
  # Save distribution and occurrences if from GBIF
  if(is.null(Storage_SP$gbif_number_saved)==F){
    st_write(sRL_OutputDistribution(scientific_name), paste0(output_dir, "/sRedList_", gsub(" ", ".", scientific_name), "_Distribution.shp"), append=F)
    st_write(sRL_OutputOccurrences(scientific_name), paste0(output_dir, "/sRedList_", gsub(" ", ".", scientific_name), "_Occurrences.shp"), append=F)
  }
  
  # Zip that folder and delete it + Storage_SP
  #zip(zipfile = output_dir, files = output_dir,  zip = "C:/Program Files/7-Zip/7Z", flags="a -tzip")
  zip(zipfile = output_dir, files = output_dir)
  unlink(output_dir, recursive=T)
  eval(parse(text=paste0("rm(Storage_SP_", sub(" ", "_", scientific_name), ", envir=.GlobalEnv)")))  # Removes Storage_SP
  
  # Remove the AOH files stored
  unlink(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name)), recursive=T)
  terraOptions(tempdir=tempdir())
  rasterOptions(tmpdir=tempdir())
  gc()
  
  # Plot
  return(plot(
    ggplot(criteria) +
      geom_point(aes(x = Value, y = Crit, col = Value), size = 40, show.legend=F) +
      geom_text(aes(x=Value, y=Crit, label=Value), col="white", size=10)+
      scale_x_discrete(drop = F) + scale_y_discrete(drop=F) +
      xlab("Red List Category triggered") + ylab("Criteria")+
      scale_colour_manual(drop = F, values=c("#006666ff", "#cc9900ff", "#cc6633ff", "#cc3333ff"))+
      ggtitle(paste0("The species seems to meet the ", as.character(max(criteria$Value, na.rm=T)), " category under criteria ", paste(criteria$Crit[criteria$Value==max(criteria$Value, na.rm=T)], collapse=" / "), ". Please check subcriteria!"))+
      theme_bw()
  ))
  
}



#* Download .zip assesment file of the species
#* @get species/<scientific_name>/assessment/red-list-criteria/zip
#* @param scientific_name:string Scientific Name
#* @serializer contentType list(type="application/octet-stream")
#* @tag sRedList
function(scientific_name) {
  scientific_name <- url_decode(scientific_name)
  
  # Prepare the ZIP to return
  zip_to_extract<-readBin(paste0(gsub(" ", "_", scientific_name), "_sRedList.zip"), "raw", n = file.info(paste0(gsub(" ", "_", scientific_name), "_sRedList.zip"))$size)
  
  # Remove the local file
  unlink(paste0(gsub(" ", "_", scientific_name), "_sRedList.zip"), recursive=T)
  return(zip_to_extract)
}





#* Download .json Red List category
#* @get species/<scientific_name>/assessment/red-list-criteria/json
#* @param scientific_name:string Scientific Name
#* @param aoh_lost:int AOH_lost
#* @param eoo_km2:int EOO_km2
#* @param aoo_km2:int AOO_km2
#* @param pop_size:int Pop_size
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, aoh_lost=aoh_lost_saved, eoo_km2, aoo_km2, pop_size) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  
  # Calculate criteria
  criteria<-sRL_CalculateCriteria(aoh_lost, eoo_km2, aoo_km2, pop_size)
  
  json <- list(
    Species = scientific_name,
    Date_processed = Sys.time(),
    EOO = eoo_km2,
    AOO = aoo_km2,
    "Percentage_of_habitat_lost" = aoh_lost,
    Pop.size = Pop_size,
    Criterias = paste0(criteria$Value, " (", criteria$Crit, ")") %>% paste(., collapse="; "),
    Highest_category = criteria$Value[which(as.numeric(criteria$Value)==max(as.numeric(criteria$Value), na.rm=T))] %>% unique()
  )
  
  return(json);
}







# MX: Other functions (platform buidling) ----------------------------------------------------------------



#* All distributions of paginated species on the platform
#* @get distributions
#* @param start:int start
#* @param end:int end
#* @param filter:string filter element by scientific_name
#* @serializer unboxedJSON
#* @tag sRedList
function(start = 0, end = 10, filter = "") {
  if (nchar(filter) < 3 && filter != "") {
    invalid_params("You must enter at least 3 characters.")
  }
  species_distribution <- as.data.frame(list.files(config$distribution_path))
  colnames(species_distribution) <- "scientific_name"
  if (filter != "") {
    indices <- grep(tolower(filter), tolower(species_distribution$scientific_name))  # nolint
    species_distribution <- na.omit(species_distribution[indices, ][start:end])
  } else{
    species_distribution <- na.omit(species_distribution$scientific_name[start:end])  # nolint
  }
  # File size in bytes
  distributions <- list()
  for (directoryName in species_distribution) {
    files <- list()
    directorySize <- 0
    edit <- TRUE
    for(fileName in list.files(paste0(config$distribution_path, directoryName))) {
      # Red list distributions cannot be deleted
      if (grepl("_RL", fileName)) edit <- FALSE;  # nolint
      subDirectorySize <- 0
      subFiles <- list()
      for( subFileName in list.files(paste0(config$distribution_path, directoryName, "/", fileName))) { # nolint 
        # GBIF distributions INFO
        metadata <- NULL
        if (file_ext(subFileName) == "json") {
          metadata = jsonlite::read_json(paste0(config$distribution_path, directoryName, "/", fileName, "/", subFileName), simplifyVector = FALSE)  # nolint 
        }
        subFileSize <- (file.info(paste0(config$distribution_path, directoryName, "/", fileName, "/", subFileName))$size) / 1024 # nolint 
        subFileCreated <- (file.info(paste0(config$distribution_path, directoryName, "/", fileName, "/", subFileName))$ctime) # nolint 
        subFiles <- append(subFiles, list(list(data = list(
          name = subFileName,
          size = subFileSize, # nolint
          type = "file",
          path = fileName, # nolint 
          metadata = metadata,
          created = subFileCreated,
          edit = edit
        ))))
        subDirectorySize <- subDirectorySize + subFileSize
      }
      fileCreated <- (file.info(paste0(config$distribution_path, directoryName, "/", fileName))$ctime) # nolint 
      files <- append(files, list(list(
        data = list(
          name = fileName,
          size = subDirectorySize, # nolint
          type = "folder",
          path = directoryName, # nolint 
          created = fileCreated,
          edit = edit),
        children = subFiles
      )))
      directorySize <- directorySize + subDirectorySize
    }
    distributions <- append(distributions, list(
      list(
        data = list(
          name = directoryName,
          size = directorySize,
          type = "folder",
          edit = FALSE),
        children = files
      )));
  }
  return(distributions)
}

#* Number of distributions on the platform
#* @get distribution/count
#* @serializer unboxedJSON
#* @tag sRedList
function() {
  return(length(list.files(config$distribution_path)))
}

#* All species distributions folder on the platform
#* @get distribution/search
#* @serializer json
#* @param scientific_name:str Digit Scientific Name (min. 3 characters)
#* @tag sRedList
function(scientific_name) {
  if (nchar(scientific_name) < 3) {
    invalid_params("You must enter at least 3 characters.")
  }
  species_distribution <- as.data.frame(list.files(config$distribution_path))
  colnames(species_distribution) <- "scientific_name"
  indices <- grep(tolower(scientific_name), tolower(species_distribution$scientific_name))  # nolint
  return(species_distribution[indices, ])
}

#* Delete distribution from sRedList platform
#* @delete species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param file_name:string file_name
#* @param path:string path
#* @param type:string type
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, file_name, path, type) {
  scientific_name <- url_decode(scientific_name)
  file_name <- url_decode(file_name)
  path <- url_decode(path)
  type <- url_decode(type)
  if ((scientific_name %in% list.files(config$distribution_path)) && !grepl("_RL", file_name) && !grepl("_RL", path)) { # nolint
    if (type == "folder") {
      if (file_name %in% list.files(paste0(config$distribution_path, path))) {
        log_info(paste0("Delete distribution:", config$distribution_path, path, '/', file_name)) # nolint
        return(list(response = unlink(paste0(config$distribution_path, path, '/', file_name), recursive = TRUE))) # nolint
      }
    }else {
      if(file_name %in% list.files(paste0(config$distribution_path, scientific_name, '/', path))){ # nolint
        log_info(paste0("Delete distribution:", config$distribution_path, scientific_name, '/', path, "/", file_name)) # nolint
        return(list(response = unlink(paste0(config$distribution_path, scientific_name, "/", path, "/", file_name)))) # nolint
      }
    }
    not_found("Species distribution not exist!") # nolint
  }else {
    not_found("Species distribution not exist!") # nolint
  }
}

#* Get distributions species from sRedList platform excluding GBIF distributions
#* @get species/<scientific_name>/distributions
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  if (scientific_name %in% list.files(config$distribution_path)) { # nolint
    distributions <- list()
    for(distributionFolder in list.files(paste0(config$distribution_path, scientific_name))) { # nolint
      # GBIF distributions cannot be selected
      if (!grepl("_GBIF", distributionFolder)){
        files <- list();
        directorySize <- 0;
        for(fileName in list.files(paste0(config$distribution_path, scientific_name, "/", distributionFolder))) {  # nolint
          fileSize <- (file.info(paste0(config$distribution_path, scientific_name, "/", distributionFolder, "/", fileName))$size) / 1024 # nolint 
          fileCreated <- (file.info(paste0(config$distribution_path, scientific_name, "/", distributionFolder, "/", fileName))$ctime) # nolint 
          files <- append(files, list(
            list(
              data = list(
                name = fileName,
                size = fileSize,
                created = fileCreated,
                path = distributionFolder,
                type = "file")
            )));
          directorySize <- directorySize + fileSize;
        }
        folderCreated <- (file.info(paste0(config$distribution_path, scientific_name, "/", distributionFolder))$ctime) # nolint 
        distributions <- append(distributions, list(
          list(
            data = list(
              name = distributionFolder,
              size = directorySize,
              created = folderCreated,
              path = distributionFolder,
              type = "folder"),
            children = files
          )));
      }
    }
    return(distributions)
    
  }else { not_found("Species distribution not exist!") } # nolint
}




#* Merge ZIP files
#* @post assessment/merge-zip
#* @param Uploaded_Zips:[file] A zip files
#* @serializer  contentType list(type="application/octet-stream")
#* @tag sRedList
function(Uploaded_Zips=list()) {
  print(names(Uploaded_Zips))

  # Error if not all zips
  extensions<-substr(names(Uploaded_Zips), nchar(names(Uploaded_Zips))-3, nchar(names(Uploaded_Zips))) %>% unique(.)
  if(length(extensions)>1 | !".zip" %in% extensions){wrong_zip_extension()}
  
  # Create a folder to store results
  Zip_Path<-paste0("Unzipped", sample(1:1000,1))
  unlink(Zip_Path, recursive=T)
  dir.create(Zip_Path)
  
  # Unzip loop
  for(i in 1:length(Uploaded_Zips)){
    writeBin(object=Uploaded_Zips[[i]], con=paste0(Zip_Path, "/Zip", i, ".zip"))
    unzip(paste0(Zip_Path, "/Zip", i, ".zip"), exdir=Zip_Path)
    unlink(paste0(Zip_Path, "/Zip", i, ".zip"))
  }
  
  ### MERGE
  # Merge habitats
  Hab_files<-list.files(Zip_Path, recursive = T)[grepl('habitats.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("habitatsM<-rbind(",
                      paste0("read.csv(Hab_files[", 1:length(Hab_files), "])", collapse=','),")"
               )))
  
  
  # Merge countries
  Coun_files<-list.files(Zip_Path, recursive = T)[grepl('countries.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("countriesM<-rbind(",
                      paste0("read.csv(Coun_files[", 1:length(Coun_files), "])", collapse=','),")"
               )))
  
  # Merge references
  Ref_files<-list.files(Zip_Path, recursive = T)[grepl('references.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("referencesM<-rbind(",
                      paste0("read.csv(Ref_files[", 1:length(Ref_files), "])", collapse=','),")"
               )))
  
  # Merge allfields
  All_files<-list.files(Zip_Path, recursive = T)[grepl('allfields.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("allfieldsM<-rbind(",
                      paste0("read.csv(All_files[", 1:length(All_files), "])", collapse=','),")"
               )))
  
  
  # Merge log
  Log_files<-list.files(Zip_Path, recursive = T)[grepl('00.Output_log.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("LogM<-rbind(",
                      paste0("read.csv(Log_files[", 1:length(Log_files), "])", collapse=','),")"
               )))
  
  
  # Merge Distributions
  if(TRUE %in% grepl('_Distribution.shp', list.files(Zip_Path, recursive = T))){
    
    Dist_files<-list.files(Zip_Path, recursive = T)[grepl('_Distribution.shp', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
    
    eval(parse(text=
                 paste0("DistM<-rbind(",
                        paste0("st_read(Dist_files[", 1:length(Dist_files), "])", collapse=','),")"
                 )))
  }
  
  # Merge Occurrences
  if(TRUE %in% grepl('_Occurrences.shp', list.files(Zip_Path, recursive = T))){
    
    Occ_files<-list.files(Zip_Path, recursive = T)[grepl('_Occurrences.shp', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
    
    eval(parse(text=
                 paste0("OccM<-rbind(",
                        paste0("st_read(Occ_files[", 1:length(Occ_files), "])", collapse=','),")"
                 )))
  }
  
  
  
  ### Save in a merged ZIP file
  unlink(paste0(Zip_Path, "/", list.files(Zip_Path)), recursive=T)
  write.csv(allfieldsM, paste0(Zip_Path, "/allfields.csv"), row.names = F)
  write.csv(countriesM, paste0(Zip_Path, "/countries.csv"), row.names = F)
  write.csv(referencesM, paste0(Zip_Path, "/references.csv"), row.names = F)
  write.csv(habitatsM, paste0(Zip_Path, "/habitats.csv"), row.names = F)
  write.csv(LogM, paste0(Zip_Path, "/00.Output_log.csv"), row.names = F)
  
  # Save distribution and occurrences if from GBIF
  if("DistM" %in% ls()){
    st_write(DistM, paste0(Zip_Path, "/sRedList_Distribution.shp"), append=F)
    st_write(OccM, paste0(Zip_Path, "/sRedList_Occurrences.shp"), append=F)
  }
  
  # Zip that folder and delete it
  Zip_name<-Sys.time() %>% gsub("-", "_", .) %>% gsub(" ", "_", .) %>% gsub (":", "_", .) %>% paste0(Zip_Path, "/sRedList_mergedZIP_", ., ".zip")
  zip(zipfile = Zip_name, files = paste0(Zip_Path, "/", list.files(Zip_Path)))
  zip_to_extract<-readBin(Zip_name, "raw", n = file.info(Zip_name)$size)
  unlink(Zip_Path, recursive=T)
  print(Zip_name)
  
  return(zip_to_extract)
}



