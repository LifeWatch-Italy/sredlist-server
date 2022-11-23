


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

#* GBIF source
#* @get species/<scientific_name>/gbif-source
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  log_info("START source")
  scientific_name<-url_decode(scientific_name)
  
  # Check if the species is in the RL points folder
  RL<-scientific_name %in% sub(".csv", "", list.files("resources/Point.distributions"))
  
  # Is the species likely marine
  TAXO<-speciesRL[speciesRL$scientific_name==scientific_name,]
  Marine <- ifelse(nrow(TAXO)==0, F, TAXO$marine_system[1])
  
  SRC<-1 # Only GBIF
  if(RL==T & Marine==T){SRC=7}
  if(RL==F & Marine==T){SRC=4}
  if(RL==T & Marine==F){SRC=5}
  
  print(SRC)
  
  return(list(Gbif_Source = SRC))}

#* Global Biodiversity Information Facility Step 1
#* @get species/<scientific_name>/gbif
#* @param scientific_name:string Scientific Name
#* @param Gbif_Source:int Gbif_Source
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, Gbif_Source=-1) {
  
  ### Clean-string from user
  scientific_name <- url_decode(scientific_name)
  scientific_name <- R.utils::capitalize(trim(gsub("[[:punct:]]", " ", scientific_name))) # nolint
  print(scientific_name)
  print(Gbif_Source)
  
  ### Create storage folder if it does not exist
  dir.create(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots"), recursive=T)
  
  ### GBIF procedure 
  log_info("START - Create data")
  dat <- sRL_createDataGBIF(scientific_name, Gbif_Source)
  
  # ### Plot
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_data.png"), plot(
    ggplot() +
      coord_fixed() +
      borders("world", colour = "gray86", fill = "gray80") +
      geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude, col=Source), size = 1) +
      scale_colour_brewer(type="qual", palette=2)+
      ggtitle(paste(names(table(dat$Source)), table(dat$Source), sep=" (") %>% paste(., collapse="), ") %>% paste0("Raw geo-referenced observations (N=", nrow(dat), ") from: ", ., ")") )+
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

#* GBIF sea
#* @get species/<scientific_name>/gbif-sea
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {return(list(Gbif_Sea = 0))}

#* Global Biodiversity Information Facility Step 2
#* @get species/<scientific_name>/gbif2
#* @param scientific_name:string Scientific Name
#* @param Gbif_Year:int Gbif_Year
#* @param Gbif_Uncertainty:int Gbif_Uncertainty
#* @param Gbif_Extent:[int] Gbif_Extent
#* @param Gbif_Sea:int Gbif_Sea
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, Gbif_Year= -1, Gbif_Uncertainty=-1, Gbif_Extent=list(), Gbif_Sea=-1) {
  
  log_info("START - GBIF Step 2")
  
  ### Transform parameters GBIF filtering
  scientific_name <- url_decode(scientific_name)
  print(Gbif_Year)
  print(Gbif_Uncertainty)
  Gbif_Extent<-as.numeric(Gbif_Extent) ; print(Gbif_Extent)
  
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
      addMouseCoordinates()
  )
  
}




## c: Map ----------------------------------------------------------------
#* GBIF start
#* @get species/<scientific_name>/gbif-start
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {return(list(Gbif_Start = 0))}

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

#* GBIF crop
#* @get species/<scientific_name>/gbif-crop
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {return(list(Gbif_Crop = 0))}


#* Global Biodiversity Information Facility step 3
#* @get species/<scientific_name>/gbif3
#* @param scientific_name:string Scientific Name
#* @param Gbif_Start:int Gbif_Start
#* @param Gbif_Buffer:int Gbif_Buffer
#* @param Gbif_Altitude:[int] Gbif_Altitude
#* @param Gbif_Crop:int Gbif_Crop
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, Gbif_Start=-1, Gbif_Buffer=-1, Gbif_Altitude=list(), Gbif_Crop=-1) {
  
  # Transform parameters GBIF filtering
  scientific_name <- url_decode(scientific_name)
  Gbif_Start<-revalue(as.character(Gbif_Start), c("-1"="EOO", "0"="EOO", "1"="Kernel", "2"="Hydrobasins", "3"="MCP-Hydrobasins"))
  Gbif_Crop<-revalue(as.character(Gbif_Crop), c("0"="0", "1"="Land", "2"="Sea"))
  Gbif_Buffer<-replace(Gbif_Buffer, Gbif_Buffer<0, 0)
  print(Gbif_Start)
  print(Gbif_Buffer)
  print(Gbif_Altitude)
  print(Gbif_Crop)
  
  #GBIF STEP 3: Map distribution from GBIF
  log_info("START - Maps the distribution")
  
  # Get back GBIF observations
  Storage_SP=sRL_reuse(scientific_name)
  dat_proj=Storage_SP$dat_proj_saved
  
  
  # Create distribution
  distSP<-sRL_MapDistributionGBIF(dat_proj, scientific_name,
                                  First_step=Gbif_Start,
                                  AltMIN=as.numeric(Gbif_Altitude[1]), AltMAX=as.numeric(Gbif_Altitude[2]),
                                  Buffer_km2=as.numeric(Gbif_Buffer),
                                  GBIF_crop=Gbif_Crop)
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
  Storage_SP$distSP_saved=distSP
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





# M3: EOO ----------------------------------------------------------------


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





# M4: AOH ----------------------------------------------------------------
## a: Data APIs ----------------------------------------------------------------


#* Species density preferences
#* @get species/<scientific_name>/density-preferences
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  return(list(
    density = density$Density[density$Species == scientific_name]
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
#* Estimate the Area of Habitat (AOH)
#* @get species/<scientific_name>/analysis/aoh
#* @param scientific_name:string Scientific Name
#* @param habitats_pref:[str] habitats_pref
#* @param altitudes_pref:[int] altitudes_pref
#* @param density_pref:numeric density_pref
#* @param isGbifDistribution:boolean isGbifDistribution
#* @param path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, habitats_pref= list(), altitudes_pref= list(), density_pref= -1, isGbifDistribution = FALSE, path = "") { # nolint    
  
  # Clean memory
  log_info("START - Cleaning memory")
  sRL_cleaningMemory(Time_limit=90)
  log_info("END - Cleaning memory")
  
  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)
  distSP=Storage_SP$distSP_saved

  # Habitat table (for aoh analysis and for SIS Connect)
  habitats_pref_DF<-sRL_PrepareHabitatFile(scientific_name, habitats_pref)
  Storage_SP$habitats_SIS=habitats_pref_DF

  # Altitude table (for aoh analysis and for SIS Connect)
  if(length(altitudes_pref)==0){altitudes_pref<-c(0,9000)}
  altitudes_pref_DF<-sRL_PrepareAltitudeFile(scientific_name, altitudes_pref)
  Storage_SP$AltPref_saved=altitudes_pref_DF
  density_pref <- as.numeric(density_pref)
  
  print(habitats_pref)
  print(altitudes_pref)
  print(density_pref)

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
                                      spp_habitat_data = habitats_pref_DF,
                                      key=red_list_token,
                                      crs=st_crs(CRSMOLL))
  Storage_SP$RangeClean_saved=rangeSP_clean
  
  # Remove old stored AOH
  output_dir<-paste0("resources/AOH_stored/", sub(" ", "_", scientific_name))
  dir.create(paste0(output_dir, "/Current"), recursive=T)
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
  
  
    AOH2<-sRL_calculateAOH(rangeSP_fun=rangeSP_clean, 
                         cci_fun=cci2_crop, 
                         alt_fun=alt_crop,
                         FOLDER=paste0(output_dir, "/Current"),
                         elevation_data_fun=altitudes_pref_DF)
    
    plot1 <- gplot(AOH2[[1]]) +
      coord_fixed()+
      geom_tile(aes(fill = as.factor(value))) +
      scale_fill_manual(values=c("#dfc27d", "#018571", NA), labels=c("Unsuitable", "Suitable", ""), name="", na.translate=F) +
      ggtitle("Area of Habitat in 2020") +
      sRLTheme_maps
    

  ### LARGE-RANGES
  } else {
    AOH2<-sRL_largeAOH(habitats_pref, altitudes_pref, rangeSP_clean, config$YearAOH2)
    
    plot1 <- gplot((AOH2[[1]]/9)) + # Divide by 9 to get percents
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(low="#dfc27d", high="#018571", name="Suitability (%)", limits=c(0,100), na.value=NA)+
      ggtitle("Area of Habitat in 2020") +
      sRLTheme_maps
    
  }
  
  Storage_SP$AOH2_saved=AOH2
  Storage_SP$AOH_type<-AOH_type

  # Plot
  log_info("START - Plot AOH")
  
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoh.png"), plot = plot(plot1), width=6, height=6)
  
  plot1 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoh.png"), mime = "image/png", encoding = "base64") # nolint
  log_info("END - Plot AOH")
  
  AOH_km2 <-  sRL_areaAOH(AOH2[[1]], "cci") # Same scale in small or large AOH because the unit is always 1 cell of the fine raster
  Storage_SP$AOHkm2_saved<-AOH_km2


  #Calculate Area of Habitat in a resolution of 2x2km (as is the map of altitude provided), in order to use it as an upper bound of species Area of Occupancy under criterion B2 (each cell covers 4km2)
  grid22_crop<-crop(grid22, AOH2[[1]])
  aoh_22<-resample(AOH2[[1]], grid22_crop, method="max")>0

  plot2 <- gplot(aoh_22[[1]]>0) +
    coord_fixed()+
    geom_tile(aes(fill = factor(as.character(value), c("0", "1")))) +
    scale_fill_manual(values=c("#dfc27d", "#018571"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
    labs(title="Area of Habitat (2x2km)", subtitle="Likely slightly overestimated (using a 10x10km aggregate raster)")+
    sRLTheme_maps
  
  
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoo.png"), plot = plot(plot2), width=6, height=6)
  plot2 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoo.png"), mime = "image/png", encoding = "base64") # nolint

  AOO_km2<- sRL_areaAOH(aoh_22[[1]], SCALE="2x2")
  
  
  ### Save parameters and results
  Storage_SP<-sRL_OutLog(Storage_SP, c("AOH_HabitatPreference", "AOH_ElevationPreference", "AOH_Density"), c(paste0(habitats_pref, collapse=","), paste0(altitudes_pref, collapse=","), density_pref))
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  terraOptions(tempdir=tempdir())
  rasterOptions(tmpdir=tempdir())
  gc()
  
  # Calculate population size
  if (density_pref != -1) {
    density_sp <- density_pref
    pop_size <- AOH_km2 * density_sp
    return(list(
      aoh_km2 = ceiling(AOH_km2), # I use ceiling to avoid having a 0 which is problematic
      aoo_km2 = round(AOO_km2),
      plot_aoh = plot1,
      plot_aoh_2x2 = plot2,
      pop_size = round(pop_size)
    ));
  }
  
  return(list(
    aoh_km2 = ceiling(AOH_km2),
    aoo_km2 = round(AOO_km2),
    plot_aoh = plot1,
    plot_aoh_2x2 = plot2
  ));
  
  
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
  
  # Charge distribution
  distSP$binomial<-as.character(distSP$binomial)
  
  # Output directory + options
  output_dir<-paste0("resources/AOH_stored/", sub(" ", "_", scientific_name))
  dir.create(paste0(output_dir, "/Initial"))
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
                         elevation_data_fun=altitudes_pref_DF)
  
   # Create plot
    plot1 <- gplot((AOH2[[1]]*2+3)-AOH1[[1]]) + 
      coord_fixed()+
      geom_tile(aes(fill = factor(as.character(value), c("2", "3", "4", "5"))))+
      scale_fill_manual(values=c("#a6611a", "gray90", "#80cdc1", "#018571"), labels=c("Newly unsuitable", "Unsuitable", "Suitable", "Newly suitable"), name="", na.translate=F, drop=F)+
      ggtitle(paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2))+
      sRLTheme_maps
  
   
  } else {
  ### LARGE-RANGES
    
    # Calculate AOH
    AOH1<-sRL_largeAOH(habitats_pref_DF$code, altitude_pref_DF[, c("elevation_lower", "elevation_upper")], rangeSP_clean, Year1)
    
    # Create plot
    plot1 <- gplot((AOH2[[1]]-AOH1[[1]])/9) + 
      coord_fixed()+
      geom_tile(aes(fill = value))+
      scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="Suitability change (%)", limits=c(-100,100), na.value=NA, trans=colour_bidirect_scale, breaks=c(-100, -50, -10, 0, 10, 50, 100))+
      ggtitle(paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2))+
      sRLTheme_maps
  }
  
  # Plot
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/trends_aoh.png"), plot = plot(plot1), width=6, height=6)
  plot1 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/trends_aoh.png"), mime = "image/png", encoding = "base64") # nolint
  
  # Calculate area and trends
  AOH_old_km2<-sRL_areaAOH(AOH1[[1]], SCALE="cci")
  AOH_lost<- (1-(AOH_km2/AOH_old_km2))
  
  ### Store parameters and results
  Storage_SP$GL_saved<-GL_species
  Storage_SP$aoh_lost_saved=round(as.numeric(AOH_lost)*100)
  Storage_SP$RangeClean_saved=Storage_SP$AOH2_saved=Storage_SP$alt_crop_saved=NULL
  Storage_SP$Year1_saved<-Year1 ; Storage_SP$Year1theo_saved<-Year1_theo
  Storage_SP<-sRL_OutLog(Storage_SP, "AOH_GenerationLength", GL_species)
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  # Remove the AOH files stored
  unlink(output_dir, recursive=T)
  terraOptions(tempdir=tempdir())
  rasterOptions(tmpdir=tempdir())
  gc()
  
  return(list(
    aoh_lost_km2 = ceiling(AOH_old_km2),
    aoh_lost = paste0(Year1, "-", config$YearAOH2, ": ", revalue(as.factor(sign(Storage_SP$aoh_lost_saved)), c("-1"="AOH gain of ", "1"="AOH loss of ")), abs(Storage_SP$aoh_lost_saved)), # Give trend in AOH rather than loss
    plot_trends_aoh = plot1
  ))
  
}







# M5: Outputs ----------------------------------------------------------------


#* Plot Red List category
#* @get species/<scientific_name>/assessment/red-list-criteria
#* @param scientific_name:string Scientific Name
#* @param aoh_lost:int AOH_lost
#* @param eoo_km2:int EOO_km2
#* @param aoo_km2:int AOO_km2
#* @param pop_size:int Pop_size
#* @serializer png list(width = 800, height = 600)
#* @tag sRedList
function(scientific_name, eoo_km2, aoo_km2, pop_size) {
  
  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)
  aoh_lost=Storage_SP$aoh_lost_saved
  
  
  # Calculate criteria
  criteria<-sRL_CalculateCriteria(aoh_lost, eoo_km2, aoo_km2, pop_size)

  
  ### Prepare SIS Connect files
  AltPref_saved=Storage_SP$AltPref_saved
  habitats_SIS=Storage_SP$habitats_SIS[,6:13] ; habitats_SIS$assessment_id<-NA ; habitats_SIS$internal_taxon_id<-NA

  allfields_SIS<-sRL_CreateALLFIELDS(scientific_name, aoh_lost, eoo_km2, aoo_km2, pop_size, AltPref_saved)
  countries_SIS<-sRL_OutputCountries(scientific_name, distSP_saved=Storage_SP$distSP_saved, CountrySP_saved=Storage_SP$CountrySP_saved, AltPref_saved)
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
