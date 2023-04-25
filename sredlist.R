


# M1: Charge distributions ----------------------------------------------------------------


#* Upload Distribution species
#* @post species/<scientific_name>/distribution
#* @param req:file Distribution file (shp,.shx,.prj,.dbf,.cpg)
#* @param scientific_name:str Insert Species Name
#* @serializer unboxedJSON
#* @parser multi
#* @tag sRedList1
function(scientific_name, req) {
  scientific_name <- sRL_decode(scientific_name)
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
  
  scientific_name <- sRL_decode(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  
  speciesPath <- paste0(config$distribution_path, scientific_name, "/", path) # nolint
  files <- base::list.files(path = speciesPath, pattern = "\\.shp$")
  
  if (length(files) == 0) {
    not_found() # nolint
  } else {
    distributionPath <- paste0(speciesPath, "/", files[1]) # nolint
  }
  
  print(distributionPath)
  
  # Charge distributions (and return error if it does not work)
  tryCatch({
    distributions <- sf::st_read(distributionPath)
  } ,error=function(e){bug_distribution_loading()})
  
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

Prom<-future({
  sf::sf_use_s2(FALSE)

  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP=sRL_StoreRead(scientific_name, MANDAT=0) ; print(names(Storage_SP))
  
  # If outlog not present (I don't think this should happen but just in case)
  if(! "Output" %in% names(Storage_SP)){Storage_SP$Output<-sRL_InitLog(scientific_name, DisSource = "Unknown")}
  
  print(path)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  if (length(presences) != 0) presences <- as.character(presences);
  if (length(seasons) != 0) seasons <- as.character(seasons);
  if (length(origins) != 0) origins <- as.character(origins);
  

  ### Load Distribution Species
  sRL_loginfo("START - Prepare distribution", scientific_name)
  distributions <- sRL_ReadDistribution(scientific_name, path) %>% sRL_PrepareDistrib(., scientific_name)
  distSP_full <- subset(distributions, distributions$binomial == scientific_name) # nolint 
  choice_presence <- c(presences)
  choice_season <- c(seasons)
  choice_origin <- c(origins)
  
  distSP <- subset(distSP_full, # nolint
                   distSP_full$presence %in% choice_presence &
                     distSP_full$seasonal %in% choice_season &
                     distSP_full$origin %in% choice_origin)
  
  ### Add id_no if not present
  names(distSP)<-replace(names(distSP), tolower(names(distSP)) %in% c("id_no", "sisid"), "id_no")
  distSP$id_no<-sRL_CalcIdno(scientific_name)
  sRL_loginfo("END - Prepare distribution", scientific_name)
  
  
  ### Colour the distributions
  sRL_loginfo("START - Colour distribution", scientific_name)
  distSP<-sRL_ColourDistrib(distSP)
  
  ### Save the distribution in memory
  Storage_SP$distSP_saved<-distSP
  Storage_SP$distSP_savedORIGINAL <- distSP # I need to save it twice for country croping for National RL
  
  ### Prepare countries if they were not charged + crop depending on the current selection of range
  if("CountrySP_saved" %not in% names(Storage_SP)){Storage_SP$CountrySP_saved<-sRL_PrepareCountries(extent(distSP_full))} 
  CountrySP<-st_crop(Storage_SP$CountrySP_saved, extent(distSP))
  Storage_SP<-sRL_OutLog(Storage_SP, c("Distribution_Presence", "Distribution_Seasonal", "Distribution_Origin"), c(paste0(presences, collapse=","), paste0(seasons, collapse=","), paste0(origins, collapse=",")))
  DisSource<-ifelse(substr(path, nchar(path)-2, nchar(path))=="_RL", "Red List", ifelse(is.na(as.numeric(substr(path, nchar(path)-2, nchar(path))))==F, "StoredOnPlatform", "Uploaded"))
  Storage_SP<-sRL_OutLog(Storage_SP, "Distribution_Source", DisSource) # If path ends by _RL it comes from the RL, uploaded otherwise
  sRL_StoreSave(scientific_name, Storage_SP)
  sRL_loginfo("Plot distribution", scientific_name)
  
  ### Plot
  if (nrow(distSP) > 0) {
    return(ggplot() +
                  geom_sf(data = CountrySP, fill="gray96", col="gray50") + # nolint
                  geom_sf(data = distSP, fill = distSP$cols) +
                  theme_void() +
                  ggtitle(""))
  } else{
    sf::sf_use_s2(FALSE)
    return(ggplot() +
                  geom_sf(data = CountrySP, fill="white96", col="gray50") + # nolint
                  theme_void() +
                  ggtitle("The distribution is empty"))
  }

}, seed=T) %>% then(onRejected=function(err) {return(ggplot()+ggtitle("ERROR: we are not able to create this plot, please report that error")+labs(subtitle=err))})

return(Prom %...>% plot())
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

Prom<-future({
  sf::sf_use_s2(FALSE)
    
  ### Clean-string from user
  scientific_name <- sRL_decode(scientific_name)
  print(scientific_name)
  print(Gbif_Source)
  
  # Uploaded Records if we uploaded data (it's a list with 1 element being the title of the uploaded csv file); I edit the csv if separator not good
  if(Uploaded_Records != ""){
    Uploaded_Records<-sRL_FormatUploadedRecords(Uploaded_Records, scientific_name)
    print(head(Uploaded_Records))
  }

  ### GBIF procedure
  sRL_loginfo("START - Create data", scientific_name)
  dat <- sRL_createDataGBIF(scientific_name, Gbif_Source, Uploaded_Records)

  ### Create storage folder if it does not exist
  dir.create(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots"), recursive=T)
  
  # ### Plot
  sRL_loginfo("START - Plot data", scientific_name)
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_data.png"), (
    ggplot() +
      coord_fixed() +
      geom_sf(data=distCountries_light, colour = "gray86", fill = "gray80")+
      geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude, col=Source_type), size = 1) +
      scale_colour_brewer(type="qual", palette=2, name="Source")+
      ggtitle(paste(names(table(dat$Source_type)), table(dat$Source_type), sep=" (") %>% paste(., collapse="), ") %>% paste0("Raw geo-referenced observations (N=", nrow(dat), ") from: ", ., ")") )+
      sRLTheme_maps %+replace%   theme(legend.position="top")
    ), width=18, height=5.5) # nolint
  plot1 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_data.png"), mime = "image/png")
  sRL_loginfo("END - Create data", scientific_name)
  sRL_loginfo("START - Clean coordinates", scientific_name)


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
  # sRL_loginfo("START - Extracting elevation values")
  # flags_foralt<-st_geometry(st_as_sf(flags_raw,coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84")) %>%
  #   st_transform(., st_crs(CRSMOLL)) %>%
  #   st_as_sf(.)
  #
  # flags_raw$Alt_points=terra::extract(alt_raw, st_coordinates(flags_foralt), method="simple")$Elevation_reprojMollweide3
  # flags_raw$Alt_points<-replace(flags_raw$Alt_points, is.nan(flags_raw$Alt_points)==T, NA)
  # sRL_loginfo("END - Extracting elevation values")


  # Assign
  output_to_save<-sRL_InitLog(scientific_name, DisSource = "Created") ; output_to_save$Value[output_to_save$Parameter=="Gbif_Source"]<-c(ifelse(Gbif_Source[1]==1, "GBIF", ""), ifelse(Gbif_Source[2]==1, "OBIS", ""), ifelse(Gbif_Source[3]==1, "Red_List", ""), ifelse(is.null(nrow(Uploaded_Records)), "", "Uploaded")) %>% .[.!=""] %>% paste(., collapse=" + ")
  Storage_SP<-list(flags_raw_saved=flags_raw, Creation=Sys.time(), Output=output_to_save)
  sRL_StoreSave(scientific_name, Storage_SP)
  
  return(list(plot_data=plot1))
  
}, seed=T)
  
  return(Prom)
  
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
function(scientific_name) {return(list(Gbif_Uncertainty = 10))}

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
function(scientific_name, Gbif_Year= -1, Gbif_Uncertainty=-1, Gbif_Extent=list(), Gbif_Sea="", Gbif_yearBin="", Gbif_uncertainBin="") {

Prom<-future({
  sf::sf_use_s2(FALSE)  

  sRL_loginfo("START - GBIF Step 2", scientific_name)


  ### Transform parameters GBIF filtering
  scientific_name <- sRL_decode(scientific_name)
  print(Gbif_Year)
  print(Gbif_Uncertainty)
  Gbif_Extent<-as.numeric(Gbif_Extent) ; print(Gbif_Extent)
  print(Gbif_Sea)
  Gbif_yearBin<-Gbif_yearBin=="true" ; print(Gbif_yearBin)
  Gbif_uncertainBin<-Gbif_uncertainBin=="true" ; print(Gbif_uncertainBin)
  
  ### Charge downloaded data
  Storage_SP<-sRL_StoreRead(scientific_name, MANDAT=1) ; print(names(Storage_SP))
  flags_raw<-Storage_SP$flags_raw_saved
  
  ### Subset the observations user wants to keep (can be run several times if users play with parameters)
  flags <- sRL_cleanDataGBIF(flags_raw, as.numeric(Gbif_Year), as.numeric(Gbif_Uncertainty), Gbif_yearBin, Gbif_uncertainBin, Gbif_Sea, Gbif_Extent[1], Gbif_Extent[2], Gbif_Extent[3], Gbif_Extent[4])
  dat_proj=sRL_SubsetGbif(flags, scientific_name)

  ### Assign in Storage_SP
  Storage_SP$dat_proj_saved<-dat_proj
  Storage_SP$flags<-flags
  Storage_SP<-sRL_OutLog(Storage_SP, c("Gbif_Year", "Gbif_Uncertainty", "Gbif_Sea", "Gbif_Extent", "Gbif_yearBin", "Gbif_uncertainBin"), c(Gbif_Year, Gbif_Uncertainty, Gbif_Sea, paste0(Gbif_Extent, collapse=","), Gbif_yearBin, Gbif_uncertainBin))
  sRL_StoreSave(scientific_name, Storage_SP)
  
  sRL_loginfo("END - GBIF Step 2", scientific_name)
  
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
  
}, seed=T)

return(Prom)
  
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
function(scientific_name, Gbif_Start="", Gbif_Param=list(), Gbif_Buffer=-1, Gbif_Altitude=list(), Gbif_Crop="", Gbif_RLDistBin="") {

# Parameter error
if(Gbif_Start=="alpha" & Gbif_Param[1] <= 0){neg_alpha()}
if(Gbif_Start=="kernel" & Gbif_Param[2] <= 0){neg_kernel()}
  
  
Prom<-future({
  sf::sf_use_s2(FALSE)

  # Transform parameters GBIF filtering
  scientific_name <- sRL_decode(scientific_name)
  Gbif_Buffer<-replace(Gbif_Buffer, Gbif_Buffer<0, 0)
  if(Gbif_Start==""){Gbif_Start<-"mcp"}
  print(Gbif_Start)
  print(Gbif_Buffer)
  print(Gbif_Altitude)
  print(Gbif_Crop)
  Gbif_Param<-as.numeric(Gbif_Param) ; print(Gbif_Param)
  Gbif_RLDistBin<-Gbif_RLDistBin=="true" ; print(Gbif_RLDistBin)
  
  #GBIF STEP 3: Map distribution from GBIF
  sRL_loginfo("START - Maps the distribution", scientific_name)
  
  # Get back GBIF observations
  Storage_SP=sRL_StoreRead(scientific_name, MANDAT=1)
  dat_proj=Storage_SP$dat_proj_saved
  if(nrow(dat_proj)==0){no_gbif_data()}
  
  
  # Display some errors
  if(nrow(dat_proj)<2 & Gbif_Start %in% c("mcp", "kernel", "alpha")){too_few_occurrences()}
  
  # Create distribution
  distSP<-sRL_MapDistributionGBIF(dat_proj, scientific_name,
                                  First_step=Gbif_Start,
                                  AltMIN=as.numeric(Gbif_Altitude[1]), AltMAX=as.numeric(Gbif_Altitude[2]),
                                  Buffer_km2=as.numeric(Gbif_Buffer),
                                  GBIF_crop=Gbif_Crop,
                                  Gbif_Param=Gbif_Param)
  sRL_loginfo("Map Distribution halfway", scientific_name)
  
  # Merge with published range map
  if(Gbif_RLDistBin==T){
    sRL_loginfo("Merge with Red List map", scientific_name)
    tryCatch({
      # Load distribution
      distRL0_path <- paste0(config$distribution_path, scientific_name, "/", sub(" ", "_", scientific_name), "_RL/", scientific_name, ".shp")
      distRL <- sRL_PrepareDistrib(st_read(distRL0_path), scientific_name) # nolint
      distRL <- subset(distRL, distRL$presence %in% c(1,2) & distRL$origin %in% c(1,2) & distRL$seasonal %in% c(1,2))
      
      # Merge with created range map
      distSPM<-st_union(distSP, distRL) %>% dplyr::group_by("binomial") %>% dplyr::summarise(N= n()) 
      distSP$geometry[1]<-distSPM$geometry[1]
    
    }, error=function(e){"Merging with published range map did not work"})
  }
  
  
  # Map countries (keeping max extent between points and polygons)
  EXT_max <-  do.call(bind, sapply(c(extent(distSP), extent(dat_proj)), FUN = function(x){as(x, 'SpatialPolygons')}))  %>% bbox(.) %>% extent(.)
  CountrySP<-st_crop(distCountries, 1.15*EXT_max)
  
  # Store and calculate area
  Storage_SP$CountrySP_saved<-CountrySP 
  Storage_SP$gbif_number_saved=nrow(dat_proj)
  
  # Plot distribution
  GPlot<-ggplot() + 
    geom_sf(data=CountrySP, fill="gray70")+
    geom_sf(data = distSP, fill="darkred") + 
    geom_sf(data=dat_proj)+
    labs(caption=ifelse("alphaTEMPO" %in% names(distSP), paste0("The true alpha tension parameter used is ", round(distSP$alphaTEMPO[1],2)), ""))+
    sRLTheme_maps
  
  if(exists("distSPM")){GPlot<-GPlot+geom_sf(data=distRL, fill="gold", alpha=0.5)+labs(caption="Polygons from published map are shown in orange")}
  
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/gbifStep3.png"), GPlot, width=18, height=5.5) # nolint
  plot3 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/gbifStep3.png"), mime = "image/png") # nolint
  sRL_loginfo("END - Maps the distribution", scientific_name)
  
  # Keep distribution in memory
  Storage_SP$distSP3_saved=distSP[, names(distSP) != "alphaTEMPO"]
  Storage_SP<-sRL_OutLog(Storage_SP, c("Mapping_Start", "Mapping_Crop", "Mapping_Buffer", "Mapping_Altitude", "Kernel_parameter", "Alpha_parameter", "Mapping_Merge"), c(Gbif_Start, Gbif_Crop, Gbif_Buffer, paste0(Gbif_Altitude, collapse=","), ifelse(Gbif_Start=="kernel", Gbif_Param[2], NA), ifelse(Gbif_Start=="alpha", Gbif_Param[1], NA), Gbif_RLDistBin))
  sRL_StoreSave(scientific_name, Storage_SP)
  
  return(list(
    plot_eoo = plot3,
    gbif_data_number  = as.numeric(Storage_SP$gbif_number_saved)
  ))
  
}, seed=T)

return(Prom)
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
  
Prom<-future({
  TRY=0
  tryCatch({
    sf::sf_use_s2(FALSE)
    
    ### Transform parameters GBIF filtering
    sRL_loginfo("Start GBIF 4", scientific_name)
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name, MANDAT=1)
    Crop_par<-Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Mapping_Crop"]
    
    ### Smooth if parameter >0
    Gbif_Smooth=as.numeric(Gbif_Smooth)/10 ; print(Gbif_Smooth) # I divide by 10 so that we can have a range of 0-100 on the client (decimal don't work correctly with the slider)
    if(Gbif_Smooth>0){
      
      if(Crop_par %in% c("cropland", "cropsea")){
        distSP<-sRL_MapDistributionGBIF(Storage_SP$dat_proj_saved, scientific_name,
                                        First_step=Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Mapping_Start"],
                                        AltMIN=Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Mapping_Altitude"] %>% strsplit(., ",") %>% unlist(.) %>% as.numeric(.) %>% .[1], 
                                        AltMAX=Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Mapping_Altitude"] %>% strsplit(., ",") %>% unlist(.) %>% as.numeric(.) %>% .[2],
                                        Buffer_km2=as.numeric(Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Mapping_Buffer"]),
                                        GBIF_crop="",
                                        Gbif_Param=c(as.numeric(Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Kernel_parameter"]), as.numeric(Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Alpha_parameter"])))
      } else {distSP<-Storage_SP$distSP3_saved}
      distSP<-smooth(distSP, method = "ksmooth", smoothness=Gbif_Smooth, max_distance=10000)} else{
        distSP<-Storage_SP$distSP3_saved
      }
    distSP<-st_make_valid(distSP)
    
    
    ### If smooth and the distribution should be cropped by land/sea, we crop again after smoothing
    if(Gbif_Smooth>0 & Crop_par %in% c("cropland", "cropsea")){
      CountrySP<-Storage_SP$CountrySP_saved
      
      if(Crop_par=="cropland"){
        distSP<-st_intersection(distSP, CountrySP) %>% 
          dplyr::group_by(binomial) %>% dplyr::summarise(N = n())}
      
      if(Crop_par=="cropsea"){
        countr<-distCountries_mapping %>% st_crop(., extent(distSP)) %>% dplyr::group_by() %>% dplyr::summarise(N = n())
        distSP<-st_difference(distSP, countr)}
      
      distSP$id_no<-sRL_CalcIdno(scientific_name)
      distSP$seasonal<-distSP$origin<-distSP$presence<-1
    }
    TRY=1
  }, error=function(e){cat("Smooth did not work")})
  
  # If problem in the TryCatch (i.e., smoothing not possible), I return the original distribution
  if(TRY==0){distSP<-Storage_SP$distSP3_saved %>% st_make_valid(.)}
  
  ### Keep distribution in memory
  Storage_SP$distSP_saved <- distSP
  Storage_SP$distSP_savedORIGINAL <- distSP # I need to save it twice for country croping for National RL
  Storage_SP<-sRL_OutLog(Storage_SP, "Mapping_Smooth", Gbif_Smooth)
  sRL_StoreSave(scientific_name, Storage_SP)
    
  ### Plot
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_final.png"), (
    ggplot() + 
      geom_sf(data=Storage_SP$CountrySP_saved, fill="gray70")+
      geom_sf(data = distSP, fill="darkred") + 
      geom_sf(data=Storage_SP$dat_proj_saved) +
      ggtitle("")+
      sRLTheme_maps
  ), width=18, height=5.5) # nolint
  plot_final <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_final.png"), mime = "image/png") # nolint
    
  # Save distribution in the platform
  gbif_path <- sRL_saveMapDistribution(scientific_name, Storage_SP)
  sRL_loginfo("End GBIF 4", scientific_name)
  
  return(list(
    plot_eoo = plot_final,
    eoo_km2 = 1, # eoo_km2 parameter is used in the client to know if we come from GBIF or RL distribution, so I have to keep it here or to change the client
    eoo_rating = 1,
    gbif_path = gbif_path
  ))

}, seed=T)

return(Prom) 
}










# M3: Countries of Occurrence ----------------------------------------------------------------

#* Countries of occurrence
#* @get species/<scientific_name>/analysis/coo
#* @param scientific_name:string Scientific Name
#* @param domain_pref:[str] domain_pref
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, domain_pref=list(), Crop_Country="") {

### Manage case in Crop_Country
if(!Crop_Country %in% c(coo_raw$SIS_name0, "", "Europe", "EU27")){
  if(tolower(Crop_Country) %in% tolower(coo_raw$SIS_name0)){
    Crop_Country<-coo_raw$SIS_name0[tolower(coo_raw$SIS_name0)==tolower(Crop_Country)][1]
  }
  if(tolower(Crop_Country)=="europe"){Crop_Country<-"Europe"}
  if(tolower(Crop_Country)=="eu27"){Crop_Country<-"EU27"}
}
  
Prom<-future({
  sf::sf_use_s2(FALSE)

  # Filter parameters
  scientific_name<-sRL_decode(scientific_name)
  Storage_SP<-sRL_StoreRead(scientific_name, MANDAT=1) ; print(names(Storage_SP))
  rownames(coo_raw)<-1:nrow(coo_raw)
  distSP<-Storage_SP$distSP_savedORIGINAL
  domain_pref<-revalue(as.character(domain_pref), c("1"="Terrestrial", "2"="Marine", "3"="Freshwater"))
  print(domain_pref)
  print(Crop_Country)
  
  # Crop for National Red Listing
  if(Crop_Country != ""){
    if(Crop_Country %in% c(coo_raw$SIS_name0, "", "Europe", "EU27")){
      distSP<-sRL_CropCountry(distSP, domain_pref, Crop_Country)
      Storage_SP<-sRL_OutLog(Storage_SP, "Crop_Country", Crop_Country)} 
    else {
      distSP<-data.frame()
      }
  }
  
  # If empty, return an empty plot
  if(nrow(distSP)==0){
    TITLE<-ifelse(Crop_Country %in% c(coo_raw$SIS_name0, "", "Europe", "EU27"),
                  "<center><font size='6' color='#ad180d'><b>This country does not overlap with species distribution <br><br> Try changing country name or leave the field empty</b></center>",
                  "<center><font size='6' color='#ad180d'><b>This country is not in the list of Red List countries <br><br> Check countries name and orthograph on SIS or on the interactive map by leaving this field empty</b></center>"
                  )
    return(
      leaflet() %>% addControl(TITLE, position = "topleft", className="map-title")
    )
  } else {
    
    Storage_SP$distSP_saved<-distSP # I have to save it even if Crop_Country is empty because maybe it was not empty at previous call
    
    # Prepare distribution and calculate COO
    distSP_WGS<-distSP %>% dplyr::group_by(origin, presence, seasonal) %>% dplyr::summarise(N= n()) %>% st_transform(., st_crs(coo_raw))
    coo<-sRL_cooExtract(distSP_WGS, domain_pref, Crop_Country)
    
    # Simplify distribution if large distribution
    if((extent(distSP_WGS)@xmax-extent(distSP_WGS)@xmin)>50){distSP_WGS<-st_simplify(distSP_WGS, dTolerance=0.05)}
    
    # Create table of colours / labels and assign colours
    col.df<-data.frame(
      Code=c("MarineFALSEFALSE", "MarineTRUEFALSE", "MarineTRUETRUE", "TerrestrialFALSEFALSE", "TerrestrialTRUEFALSE", "TerrestrialTRUETRUE"),
      Col=c("#D2D2D2", "#9595C3", "#5757A9", "white", "#F17777", "#8C2316"),
      Label=c("Absent (marine)", "Absent from subnational (marine)", "Present (marine)", "Absent (terrestrial)", "Absent from subnational (terrestrial)", "Present (terrestrial)")
    )
    
    coo$colour<-paste0(coo$Domain, coo$Level0_occupied, coo$Level1_occupied) 
    coo$colour<-col.df$Col[match(coo$colour, col.df$Code)]
    
    # Save for SIS
    Storage_SP$countries_SIS<-sRL_OutputCountries(scientific_name, subset(coo, coo$presence>0))
    sRL_StoreSave(scientific_name, Storage_SP)
    
    # Prepare extent
    EXT<-1.2*extent(coo[coo$Level0_occupied==T,])
    if(is.na(EXT[1]) | is.na(EXT[2]) | is.na(EXT[3]) | is.na(EXT[4])){EXT<-1.2*extent(distSP_WGS)} # In case there is no overlap with countries (e.g., distribution at sea because of simplification)
    
    # Plot
    return(
      leaflet() %>%
        fitBounds(lng1=EXT[1], lng2=EXT[2], lat1=EXT[3], lat2=EXT[4]) %>%
        addPolygons(data=coo,
                    color=ifelse(coo$Level0_occupied==T, "black", "grey"),
                    fillColor=coo$colour,
                    popup=coo$Popup,
                    stroke=T, weight=2, fillOpacity=1) %>%
        addPolygons(data=distSP_WGS, color="#D69F32", fillOpacity=0.4) %>%
        addLegend(position="bottomleft", colors=c(col.df$Col[col.df$Col %in% coo$colour], "#D69F32"), labels=c(col.df$Label[col.df$Col %in% coo$colour], "Distribution"), opacity=1)
    )
  }
  
}, seed=T)

return(Prom)

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

Prom<-future({
  sf::sf_use_s2(FALSE)
    
  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP=sRL_StoreRead(scientific_name, MANDAT=1) ; print(names(Storage_SP))

  # Load distribution
  distSP<-Storage_SP$distSP_saved

  # Create storage directory if it does not exist
  dir.create(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots"), recursive=T)

  ### Plot EOO
  sRL_loginfo("START - Plot EOO \n", scientific_name)
  EOO <- st_as_sf(st_convex_hull(st_union(distSP))) ## Needed to avoid having different sub-polygons
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_eoo.png"), 
    ggplot() +
      geom_sf(data=EOO, fill="#ef3b2c", col=NA) +
      geom_sf(data=distSP, fill="#fcbba1", col=NA) +
      geom_sf(data=st_crop(Storage_SP$CountrySP_saved, extent(EOO)), fill=NA, col="black")+
      ggtitle("EOO map") +
      sRLTheme_maps, 
    width=6, height=6) # nolint
  plot3 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/plot_eoo.png"), mime = "image/png") # nolint
  sRL_loginfo("END - Plot EOO \n", scientific_name)

  ### Calculate EOO area
  EOO_km2 <- round(as.numeric(st_area(EOO))/1000000)
  
  ### Save EOO area
  Storage_SP$eoo_km2<-EOO_km2
  sRL_StoreSave(scientific_name, Storage_SP)
  

  return(list(
    eoo_km2 = EOO_km2,
    plot_eoo = plot3
  ))
  
}, seed=T)

return(Prom)
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
  scientific_name <- sRL_decode(scientific_name)
  
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
  scientific_name <- sRL_decode(scientific_name)

  # If value in GL_file we take it, otherwise default=1
  GL_species = ifelse(scientific_name %in% GL_file$internal_taxon_name, GL_file$GL_estimate[GL_file$internal_taxon_name==scientific_name][1], 1)

  return(list(GL_species = as.character(GL_species)))
}


## b: Current AOH ----------------------------------------------------------------
#* Calculate Area of Habitat (AOH)
#* @get species/<scientific_name>/analysis/aoh
#* @param scientific_name:string Scientific Name
#* @param habitats_pref:[str] habitats_pref
#* @param habitats_pref_MARGINAL:[str] habitats_pref_MARGINAL
#* @param altitudes_pref:[int] altitudes_pref
#* @param density_pref:string density_pref
#* @param isGbifDistribution:boolean isGbifDistribution
#* @param path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, habitats_pref= list(), habitats_pref_MARGINAL=list(), altitudes_pref= list(), density_pref= '-1', isGbifDistribution = FALSE, path = "") { # nolint    

# If no habitat preference or habitats not in crosswalk, return error
if(length(habitats_pref)==0){no_habitat_pref()}
if(!"TRUE" %in% (c(habitats_pref, habitats_pref_MARGINAL) %in% crosswalk_to_use$code)){no_habitats_crosswalk()}


# Clean memory
Prom_clean<-future({
  sRL_loginfo("START - Cleaning memory", scientific_name)
  sRL_cleaningMemory(Time_limit=45)
  sRL_loginfo("END - Cleaning memory", scientific_name)
}, seed=T)  
Prom_clean %...>% print(.)


Prom<-future({
  sf::sf_use_s2(FALSE)
  sRL_loginfo("START - AOH API", scientific_name)
  
  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP=sRL_StoreRead(scientific_name, MANDAT=1) ; print(names(Storage_SP))
  distSP=Storage_SP$distSP_saved

  # Habitat table (for aoh analysis and for SIS Connect)
  print(habitats_pref_MARGINAL)
  if(! exists("habitats_pref_MARGINAL")){habitats_pref_MARGINAL=NA}
  print(habitats_pref_MARGINAL)
  habitats_pref_DF<-sRL_PrepareHabitatFile(scientific_name, habitats_pref, habitats_pref_MARGINAL)
  print(habitats_pref_DF)
  Storage_SP$habitats_SIS=habitats_pref_DF

  # Altitude table (for aoh analysis and for SIS Connect)
  if(length(altitudes_pref)==0){altitudes_pref<-c(0,9000)}
  altitudes_pref_DF<-sRL_PrepareAltitudeFile(scientific_name, altitudes_pref)
  Storage_SP$AltPref_saved=altitudes_pref_DF
  density_pref <- density_pref %>% gsub(" ", "", .) %>% gsub(",", ".", .)

  # Do I need to calculate 2 aoh (yes if uncertainty in habitats or altitude)
  Uncertain<-ifelse("Unknown" %in% habitats_pref_DF$suitability | TRUE %in% grepl("-", altitudes_pref), "Uncertain_yes", "Uncertain_no")
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
  names(distSP)<-replace(names(distSP), tolower(names(distSP)) %in% c("terrestrial", "terrestial", "terr"), "terrestrial")
  distSP$terrestrial<-"true" # I have to make it manually so that it does not exclude pure freshwater species (it will be users choice in any case)
  
  rangeSP_clean<-create_spp_info_data(distSP,
                                      keep_iucn_rl_presence = 1:6, # The selection has already been made, so I give fake numbers here
                                      keep_iucn_rl_seasonal = 1:5,
                                      keep_iucn_rl_origin = 1:6,
                                      spp_summary_data = altitudes_pref_DF,
                                      spp_habitat_data = habitats_pref_DF[habitats_pref_DF$suitability=="Suitable",], # Will only keep suitable habitat
                                      key=config$red_list_token,
                                      crs=st_crs(CRSMOLL))

  Storage_SP$RangeClean_saved=rangeSP_clean

  # Remove old stored AOH
  output_dir<-paste0("resources/AOH_stored/", sub(" ", "_", scientific_name))
  dir.create(paste0(output_dir, "/Current"), recursive=T)
  dir.create(paste0(output_dir, "/Current_optimistic"), recursive=T)
  dir.create(paste0(output_dir, "/Temporary"))
  do.call(file.remove, list.files(output_dir, full.names = TRUE, recursive=T) %>% .[! grepl("Storage_SP", .)] %>% list(.))
  terraOptions(tempdir=paste0(output_dir, "/Temporary"), memmax=config$RAMmax_GB)
  rasterOptions(tmpdir=paste0(output_dir, "/Temporary"), maxmemory=config$RAMmax_GB)

  ### SMALL-RANGES
  Range_size<-as.numeric(st_area(rangeSP_clean))/10^6 ; print(Range_size)
  AOH_type<-ifelse(Range_size < as.numeric(config$Size_LargeRange), "Small", "Large") ; print(AOH_type)
  if(AOH_type=="Small"){

    sRL_loginfo("START - Small: Cropping rasters", scientific_name)
    alt_raw<-sRL_ChargeAltRaster()
    alt_crop=crop(alt_raw, extent(distSP))
    writeRaster(alt_crop, paste0(output_dir, "/alt_crop.tif"))
    cci2<-sRL_ChargeCci2Raster()
    cci2_crop<-crop(cci2, extent(distSP))

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
            AOH2_opt<-AOH2; sRL_loginfo("Identical AOH, no need to calculate", scientific_name)
            terra::writeRaster(rast(AOH2_opt), paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "/Current_optimistic/Optimistic_identical.tif"))
            } else{

            AOH2_opt<-sRL_calculateAOH(rangeSP_fun=rangeSP_cleanOPT,
                                 cci_fun=cci2_crop,
                                 alt_fun=alt_crop,
                                 FOLDER=paste0(output_dir, "/Current_optimistic"),
                                 elevation_data_fun=altitudes_pref_DF)
            sRL_loginfo("Maximum AOH calculated", scientific_name)
        }
      }

    ### PLOTS
    # Only pessimistic scenario
    if(Uncertain=="Uncertain_no"){
      plot1 <- gplot(AOH2[[1]]) +
        coord_fixed()+
        geom_tile(aes(fill = factor(value, levels=c("0", "1")))) +
        scale_fill_manual(values=c("#FBCB3C", "#0D993F", NA), labels=c("Unsuitable", "Suitable", ""), name="", na.translate=F, drop=F) +
        ggtitle("Area of Habitat in 2020") +
        sRLTheme_maps
    }

    # Both scenario
    if(Uncertain=="Uncertain_yes"){

      plot1<-gplot(AOH2[[1]]+AOH2_opt[[1]])+
        coord_fixed()+
        geom_tile(aes(fill = factor(value, levels=c("0", "1", "2")))) +
        scale_fill_manual(values=c("#FBCB3C", "#90D79E", "#0D993F", NA), labels=c("Unsuitable", "Unknown", "Suitable", ""), name="", na.translate=F, drop=F) +
        ggtitle(paste0("Area of Habitat in ", config$YearAOH2)) +
        sRLTheme_maps
    }


    ### LARGE-RANGES
  } else {

    # Prepare altitude raster
    sRL_loginfo("START - Large altitude crop", scientific_name)
    alt_large<-raster(paste0(config$cciStack2_path, "/ElevationAgg30.tif"))
    alt_crop<-crop(alt_large, extent(rangeSP_clean))
    writeRaster(alt_crop, paste0(output_dir, "/alt_crop.tif"), overwrite=T)
    sRL_loginfo("END - Large altitude crop", scientific_name)

    # Calculate AOH
    AOH2<-sRL_largeAOH(alt_crop, habitats_pref, altitudes_pref_DF[, c("elevation_lower", "elevation_upper")], rangeSP_clean, config$YearAOH2, paste0(output_dir, "/Current/aoh.tif"))

    if(Uncertain=="Uncertain_no"){
      plot1 <- gplot((AOH2[[1]]/9)) + # Divide by 9 to get percents
        coord_fixed()+
        geom_tile(aes(fill = value)) +
        scale_fill_gradient(low="#FBCB3C", high="#0D993F", name="Suitability (%)", limits=c(0,100), na.value=NA)+
        ggtitle(paste0("Area of Habitat in ", config$YearAOH2)) +
        sRLTheme_maps

    } else{

      ## Calculate optimistic AOH
      alt_pref_extreme<-c(min(c(altitudes_pref_DF$elevation_lower, altitudes_pref_DF$elevation_lowerEXTREME), na.rm=T),
                          max(c(altitudes_pref_DF$elevation_upper, altitudes_pref_DF$elevation_upperEXTREME), na.rm=T)) ; print(alt_pref_extreme)

      # If there is no need to calculate a new one (no new CCI modalities or no new elevation limits, use the last one)
      if(length(unique(crosswalk_to_use$value[crosswalk_to_use$code %in% habitats_pref]))==length(unique(crosswalk_to_use$value[crosswalk_to_use$code %in% c(habitats_pref, habitats_pref_MARGINAL)])) & !"elevation_lowerEXTREME" %in% names(altitudes_pref_DF) & !"elevation_upperEXTREME" %in% names(altitudes_pref_DF)){
          sRL_loginfo("Identical AOH, no need to calculate", scientific_name)
          AOH2_opt<-AOH2
          terra::writeRaster(AOH2_opt, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "/Current_optimistic/Optimistic_identical.tif"))
          
        } else{

          AOH2_opt<-sRL_largeAOH(alt_crop, c(habitats_pref, habitats_pref_MARGINAL), alt_pref_extreme, rangeSP_clean, config$YearAOH2, FILENAME=paste0(output_dir, "/Current_optimistic/aoh.tif"))
          sRL_loginfo("Maximum AOH calculated", scientific_name)
      }

      plot1 <- cowplot::plot_grid(
        gplot((AOH2[[1]]/9)) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient(low="#FBCB3C", high="#0D993F", name="Suitability (%)", limits=c(0,100), na.value=NA)+
          ggtitle(paste0("Minimum AOH in ", config$YearAOH2)) +
          labs(subtitle= "(marginal or unknown habitats / extreme elevations excluded)") +
          sRLTheme_maps,

        gplot((AOH2_opt[[1]]/9)) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient(low="#FBCB3C", high="#0D993F", name="Suitability (%)", limits=c(0,100), na.value=NA)+
          ggtitle(paste0("Maximum AOH in ", config$YearAOH2)) +
          labs(subtitle= "(marginal or unknown habitats / extreme elevations included)") +
          sRLTheme_maps,

        ncol=1)
    }

  }

  Storage_SP$AOH_type<-AOH_type

  # Plot AOH and calculate area
  sRL_loginfo("START - Plot AOH", scientific_name)

  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoh.png"), plot = plot1, width=9, height=ifelse(Uncertain=="Uncertain_no" | AOH_type=="Small", 9, 15))
  plot1 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoh.png"), mime = "image/png", encoding = "base64") # nolint
  print("END - Plot AOH")

  AOH_km2 <-  sRL_areaAOH(AOH2[[1]], "cci") # Same scale in small or large AOH because the unit is always 1 cell of the fine raster
  if(Uncertain=="Uncertain_yes"){AOH_km2_opt <-  sRL_areaAOH(AOH2_opt[[1]], "cci") ;  Storage_SP$AOHkm2OPT_saved<-AOH_km2_opt}
  Storage_SP$AOHkm2_saved<-AOH_km2

  ### Calculate Area of Habitat in a resolution of 2x2km (as is the map of altitude provided), in order to use it as an upper bound of species Area of Occupancy under criterion B2 (each cell covers 4km2)
  grid22<-sRL_ChargeGrid22Raster()
  grid22_crop<-crop(grid22, AOH2[[1]])
  aoh_22<-terra::resample(AOH2[[1]], grid22_crop, method="max")>0


  if(Uncertain=="Uncertain_no"){
    plot2 <- cowplot::plot_grid(gplot(aoh_22[[1]]>0) +
      coord_fixed()+
      geom_tile(aes(fill = factor(as.character(as.numeric(value)), levels=c("0", "1")))) +
      scale_fill_manual(values=c("#FBCB3C", "#0D993F"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
      labs(title="", subtitle=ifelse(AOH_type=="Large", "Likely slightly overestimated (using a 10x10km aggregate raster)", ""))+
      sRLTheme_maps,
      ncol=1)
  }

  if(Uncertain=="Uncertain_yes"){
    aoh_22_opt<-terra::resample(AOH2_opt[[1]], grid22_crop, method="max")>0

    # Plot on a single plot if small range, on two if large range
    if(AOH_type=="Small"){
      plot2<-gplot((aoh_22[[1]]>0)+(aoh_22_opt[[1]]>0))+
        coord_fixed()+
        geom_tile(aes(fill = factor(as.character(as.numeric(value)), levels=c("0", "1", "2")))) +
        scale_fill_manual(values=c("#FBCB3C", "#90D79E", "#0D993F", NA), labels=c("Unsuitable", "Unknown", "Suitable", ""), name="", na.translate=F, drop=F) +
        ggtitle("") +
        sRLTheme_maps

    } else{
    plot2 <- cowplot::plot_grid(
      gplot(aoh_22[[1]]>0) +
        coord_fixed()+
        geom_tile(aes(fill = factor(as.character(as.numeric(value)), c("0", "1")))) +
        scale_fill_manual(values=c("#FBCB3C", "#0D993F"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
        labs(title="Likely slightly overestimated (using a 10x10km aggregate raster) \n\n Minimum AOH at 2x2km scale", subtitle= "(marginal or unknown habitats / extreme elevations excluded)")+
        sRLTheme_maps,

      gplot(aoh_22_opt[[1]]>0) +
        coord_fixed()+
        geom_tile(aes(fill = factor(as.character(as.numeric(value)), c("0", "1")))) +
        scale_fill_manual(values=c("#FBCB3C", "#0D993F"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
        labs(title="Maximum AOH at 2x2km scale", subtitle= "(marginal or unknown habitats / extreme elevations included)")+
        sRLTheme_maps,

      ncol=1)
  }
  }

  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoo.png"), plot = plot2, width=9, height=ifelse(Uncertain=="Uncertain_no" | AOH_type=="Small", 9, 15))

  plot2 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/aoo.png"), mime = "image/png", encoding = "base64") # nolint

  AOO_km2<- sRL_areaAOH(aoh_22[[1]], SCALE="2x2")
  if(Uncertain=="Uncertain_yes"){AOO_km2_opt<- sRL_areaAOH(aoh_22_opt[[1]], SCALE="2x2") ; Storage_SP$aoo_km2_opt<-AOO_km2_opt}
  if (density_pref[1] != '-1') {Storage_SP$density_saved<-density_pref}
  Storage_SP$aoo_km2<-AOO_km2
  

  ### Save parameters and results
  Storage_SP<-sRL_OutLog(Storage_SP, c("AOH_HabitatPreference", "AOH_MarginalHabitatPreference", "AOH_ElevationPreference", "AOH_Density"), c(paste0(habitats_pref, collapse=","), paste0(habitats_pref_MARGINAL, collapse=","), paste0(altitudes_pref, collapse=", "), ifelse(density_pref=='-1', NA, density_pref)))
  terraOptions(tempdir=tempdir())
  rasterOptions(tmpdir=tempdir())
  gc()

  ### Calculate population size (with uncertainty to due uncertainty in AOH and in density estimate)
  sRL_loginfo("START - Calcualte population size", scientific_name)
  if (density_pref[1] != '-1') {
    density_pref <- unlist(strsplit(as.character(density_pref), "-")) %>% as.numeric(.) ; print(density_pref) # Density_pref has one value if certain, 2 values otherwise

    if(Uncertain=="Uncertain_no"){pop_size <- paste(round(AOH_km2 * density_pref), collapse="-")} # Multiplies AOH by both density estimates (or one if only one available)
    if(Uncertain=="Uncertain_yes"){pop_size <- paste(c(round(AOH_km2 * min(density_pref)), round(AOH_km2_opt * max(density_pref))), collapse="-")} # Multiplies pessimistic aoh by pessimistic density, optimistic AOH by optimistic density (or a single density if only one provided)
    print(pop_size)
    Storage_SP$pop_size<-pop_size
  }
  sRL_loginfo("END - Calcualte population size", scientific_name)
  sRL_StoreSave(scientific_name, Storage_SP)
  
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
}, seed=T)

return(Prom)
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
#* @param GL_species:string GL_species
#* @param isGbifDistribution:boolean isGbifDistribution
#* @param path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, GL_species="1") { # nolint
  
Prom<-future({
  sf::sf_use_s2(FALSE)
  
  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP=sRL_StoreRead(scientific_name, MANDAT=1)
  GL_species<-GL_species %>% gsub(" ", "", .) %>% sub(",", ".", .) %>% as.numeric(.) ; print(GL_species) ; if(is.na(GL_species)){incorrect_GL()}

  distSP=Storage_SP$distSP_saved
  alt_crop=rast(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "/alt_crop.tif")) ; crs(alt_crop)<-CRSMOLL
  rangeSP_clean=Storage_SP$RangeClean_saved
  habitats_pref_DF=Storage_SP$habitats_SIS
  altitude_pref_DF=Storage_SP$AltPref_saved
  AOH_km2<-Storage_SP$AOHkm2_saved
  AOH_type<-Storage_SP$AOH_type
  rangeSP_cleanOPT=Storage_SP$RangeCleanOPT_saved
  
  # AOH charged directly
  AOH2<-rast(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "/Current/", list.files(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "/Current"))[1])) ; crs(AOH2[[1]])<-CRSMOLL
  if(Storage_SP$Uncertain=="Uncertain_yes"){AOH2_opt<-rast(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "/Current_optimistic/", list.files(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "/Current_optimistic"))[1])) ; crs(AOH2_opt[[1]])<-CRSMOLL}
  
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
    sRL_loginfo("START - Cropping rasters", scientific_name)
    cci1<-rast(sub("XXXX", Year1, config$cci1_raster_path)) ; crs(cci1)<-CRSMOLL # I ensure the CRS is correctly assigned
    cci1_crop<-crop(cci1, extent(distSP))
    sRL_loginfo("END - Cropping rasters", scientific_name)
  
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
      
      plot1 <- cowplot::plot_grid( 
        gplot((AOH2[[1]]*2+3)-AOH1[[1]]) +
          coord_fixed()+
          geom_tile(aes(fill = factor(as.character(value), c("2", "3", "4", "5")))) +
          scale_fill_manual(values=c("#a6611a", "gray90", "#80cdc1", "#018571"), labels=c("Newly unsuitable", "Unsuitable", "Suitable", "Newly suitable"), name="", na.translate=F, drop=F)+
          ggtitle("\n\n Minimum AOH") +
          sRLTheme_maps,
        
        gplot((AOH2_opt[[1]]*2+3)-AOH1_opt[[1]]) +
          coord_fixed()+
          geom_tile(aes(fill = factor(as.character(value), c("2", "3", "4", "5")))) +
          scale_fill_manual(values=c("#a6611a", "gray90", "#80cdc1", "#018571"), labels=c("Newly unsuitable", "Unsuitable", "Suitable", "Newly suitable"), name="", na.translate=F, drop=F)+
          ggtitle("Maximum AOH") +
          sRLTheme_maps,
        
        labels=paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2), 
        ncol=1)
    }
  
   
  } else {
  ### LARGE-RANGES

    # Calculate AOH
    AOH1<-sRL_largeAOH(alt_crop, habitats_pref_DF$code[habitats_pref_DF$suitability=="Suitable"], altitude_pref_DF[, c("elevation_lower", "elevation_upper")], rangeSP_clean, Year1, FILENAME="")
    
    # If no uncertainty, plot directly
    if(Storage_SP$Uncertain=="Uncertain_no"){
      plot1 <- gplot((AOH2[[1]]-AOH1[[1]])/9) + 
        coord_fixed()+
        geom_tile(aes(fill = value))+
        scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="Suitability change (%)", limits=c(-100,100), na.value=NA, trans=colour_bidirect_scale, breaks=c(-100, -50, -10, 0, 10, 50, 100))+
        ggtitle(paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2))+
        sRLTheme_maps
      
    } else { # If uncertainty, calculate other trends and plot

      alt_pref_extreme<-c(min(c(altitude_pref_DF$elevation_lower, altitude_pref_DF$elevation_lowerEXTREME), na.rm=T),
                          max(c(altitude_pref_DF$elevation_upper, altitude_pref_DF$elevation_upperEXTREME), na.rm=T)) ; print(alt_pref_extreme)

      AOH1_opt<-sRL_largeAOH(alt_crop, habitats_pref_DF$code, alt_pref_extreme, rangeSP_clean, Year1, FILENAME="")

      plot1 <- cowplot::plot_grid(
        gplot((AOH2[[1]]-AOH1[[1]])/9) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="Suitability change (%)", limits=c(-100,100), na.value=NA, trans=colour_bidirect_scale, breaks=c(-100, -50, -10, 0, 10, 50, 100))+
          ggtitle("\n\n Minimum AOH") +
          #labs(subtitle= "(marginal habitats / extreme elevations excluded)") +
          sRLTheme_maps,
        
        gplot((AOH2_opt[[1]]-AOH1_opt[[1]])/9) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="Suitability change (%)", limits=c(-100,100), na.value=NA, trans=colour_bidirect_scale, breaks=c(-100, -50, -10, 0, 10, 50, 100))+
          ggtitle("Maximum AOH") +
          #labs(subtitle= "(marginal habitats / extreme elevations included)") +
          sRLTheme_maps,
        
        labels=paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2), 
        ncol=1)
    }

    
  }  
  
  # Plot
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/trends_aoh.png"), plot = plot1, width=8, height=ifelse(Storage_SP$Uncertain=="Uncertain_no", 8, 13))
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
  
  # Output to return
  Out_area<-ifelse(Storage_SP$Uncertain=="Uncertain_no", 
                   ceiling(AOH_old_km2),
                   paste(ceiling(AOH_old_km2), ceiling(AOH_old_km2OPT), sep="-"))
  
  Out_loss<-ifelse(Storage_SP$Uncertain=="Uncertain_no", 
                   paste0(Year1, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), abs(AOH_lost), "%"), # Give trend in AOH rather than loss,
                   paste0(Year1, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), abs(as.numeric(AOH_lost)), "% (Min AOH) or ", revalue(as.factor(sign(AOH_lostOPT)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), abs(AOH_lostOPT), "% (Max AOH)")
  )
  
  
  # With extrapolation
  FACT_Extrap<-(config$YearAOH2-Year1_theo)/(config$YearAOH2-Year1) # Exponential extrapolation (formula from guidelines)
  AOH_lost_extrap<-round(abs(100*(1-(1-as.numeric(AOH_lost)/100)^FACT_Extrap)))
  if(Storage_SP$Uncertain=="Uncertain_yes"){
    AOH_lostOPT_extrap<-round(abs(100*(1-(1-AOH_lostOPT/100)^FACT_Extrap)))
    Storage_SP$aoh_lostOPT_extrap=AOH_lostOPT_extrap
  }
  
  Out_loss_extrap<-ifelse(Year1==Year1_theo, "", 
        ifelse(Storage_SP$Uncertain=="Uncertain_no", 
              paste0(Year1_theo, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), AOH_lost_extrap, "%"), # Give trend in AOH rather than loss,
              paste0(Year1_theo, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), AOH_lost_extrap, "% (Min AOH) or ", revalue(as.factor(sign(AOH_lostOPT)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of ")), AOH_lostOPT_extrap, "% (Max AOH)")
  ))
  Storage_SP$aoh_lost_extrap=AOH_lost_extrap
  
  
  ### Return
  sRL_StoreSave(scientific_name, Storage_SP)
  
  LIST<-list(
    aoh_lost_km2 = Out_area,
    aoh_lost = Out_loss,
    plot_trends_aoh = plot1
  )
  if(Out_loss_extrap != ""){LIST$aoh_lost_extrap = Out_loss_extrap}
  
  return(LIST)
  
}, seed=T)
 
return(Prom)
}





# M6: Optional analyses ----------------------------------------------------------------


## a: Fragmentation -------------------------------
#* Species dispersion preferences
#* @get species/<scientific_name>/dispersion-preferences
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  
  return(list(
    dispersion=10
  ));
}


#* Plot fragmentation
#* @get species/<scientific_name>/analysis/fragmentation
#* @param scientific_name:string Scientific Name
#* @param dispersion:string dispersion
#* @serializer png list(width = 1200, height = 600)
#* @tag sRedList
function(scientific_name, dispersion="-1", density_pref= '-1') {
  
  # Check dispersion
  dispersion <- dispersion %>% gsub(" ", "", .) %>% gsub(",", ".", .) %>% as.character(.) %>% strsplit(., "-") %>% unlist() %>% as.numeric(.)*1000 ; print(dispersion)
  if(is.na(sum(dispersion))){wrong_dispersion()}
  
  # Check density
  if(density_pref =="-1" | density_pref=="" | is.null(density_pref)){no_density_fragm()}
  density_pref <- density_pref %>% gsub(" ", "", .) %>% gsub(",", ".", .) %>% as.character(.)  %>% strsplit(., "-") %>% unlist(.) %>% as.numeric(.) ; print(density_pref)
  if(NA %in% density_pref){wrong_density()}
  
Prom<-future({
    sf::sf_use_s2(FALSE)
  
    ### Filter param
    scientific_name<-sRL_decode(scientific_name)
    Storage_SP<-sRL_StoreRead(scientific_name, MANDAT=1)
    aoh<-rast(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "/Current/", list.files(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "/Current"))[1]))[[1]]  ; crs(aoh)<-CRSMOLL
    aoh_type<-Storage_SP$AOH_type
    
    ### Save in output log
    Storage_SP<-sRL_OutLog(Storage_SP, c("Fragmentation_Isolation", "Fragmentation_Density"), c(paste(dispersion/1000, collapse="-"), paste(density_pref, collapse="-")))
    sRL_StoreSave(scientific_name, Storage_SP)
    
    ### If large range, I have to binarize the suitable habitat
    if(aoh_type=="Large"){aoh<-ifel(aoh<=90, 0, 1)}
    
    ### Calculate fragmentation
    res<-sRL_fragmentation(aoh, aoh_type, min(dispersion), density_pref)
    if(length(dispersion)>1){
      res2<-sRL_fragmentation(aoh, aoh_type, max(dispersion), density_pref) 
    } else {res2<-res} # I create a res2 even if no uncertainty in isolation distance to avoid having to code 2 plots
    
    
    ### Plots
    ### AOH and clusters
    aohDF<-as.data.frame(aoh, xy = TRUE); names(aohDF)[3]<-"lyr1"
    
    NClust<-unique(c(nrow(res2$clusters), nrow(res$clusters)))
    
    G1<-ggplot() + 
      geom_tile(data = aohDF, aes(x = x, y = y, fill = factor(lyr1, levels=c("0", "1"))), alpha = 0.5, col=NA)+
      geom_sf(data=st_transform(res2$clusters, crs(aoh)), aes(col="disp2"), fill=NA, lwd=2, show.legend = "line")+
      geom_sf(data=st_transform(res$clusters, crs(aoh)), aes(col="disp1"), fill=NA, lwd=2, show.legend = "line")+
      ggtitle(paste0("Population fragmentation in ", paste(NClust, collapse="-"), " clusters"))+
      scale_fill_manual(values=c("#FBCB3C", "#0D993F", NA), labels=c("Unsuitable", "Suitable", ""), name="", na.translate=F, drop=F) +
      scale_colour_manual(name="", breaks=c("disp1", "disp2"), values=c("#A383FF", "#FF7F5E"), labels=c(paste0("Isolation ", min(dispersion)/1000, " km"), paste0("Isolation ", max(dispersion)/1000, " km")), na.translate=F, drop=T)+
      sRLTheme_maps %+replace%   theme(legend.position="bottom") +
      guides(fill = guide_legend(override.aes = list(col=NA)))
    
        
    ### Cumulative fragmentation (depends if one or two density estimates)
    # Subtitle for cumulative plot
    V1<-min(res$prop.fragm$pop[res$prop.fragm$CumSum>0.5], na.rm=T)
    V2<-min(res2$prop.fragm$pop[res2$prop.fragm$CumSum>0.5], na.rm=T)
    V3<-ifelse("pop2" %in% names(res$prop.fragm), min(res$prop.fragm$pop2[res$prop.fragm$CumSum>0.5], na.rm=T), NA)
    V4<-ifelse("pop2" %in% names(res$prop.fragm), min(res2$prop.fragm$pop2[res2$prop.fragm$CumSum>0.5], na.rm=T), NA)
    VTOT<-c(min(c(V1, V2, V3, V4), na.rm=T), max(c(V1, V2, V3, V4), na.rm=T)) %>% round(.) %>% unique(.)
    
    SubTitle<-paste0("Fragmented if you consider that a population lower \n than ", paste(VTOT, collapse="-"), " mature individuals is 'small'")
    
    G2<-ggplot()+
      geom_step(data=res$prop.fragm, aes(x=pop, y=CumSum, col="ShortMin"), linewidth=2.8)+
      geom_vline(xintercept=min(res$prop.fragm$pop[res$prop.fragm$CumSum>0.5], na.rm=T), linetype="dashed")+
      geom_hline(yintercept=0.5, linetype="dashed")+
      xlab("How many individuals constitute a 'small' population?")+ylab("Proportion of the population that is fragmented")+
      ylim(c(0,1))+
      labs(subtitle=SubTitle)+
      theme_minimal() %+replace%   theme(text=element_text(size=15), plot.subtitle=element_text(hjust=0.5), legend.position="bottom")
    
    LABS<-c("Cumulated population", "", "", "")
    
    # If uncertainty in density
    if("pop2" %in% names(res$prop.fragm)){
      G2<-G2+
        geom_step(data=res$prop.fragm, aes(x=pop2, y=CumSum, col="ShortMax"), linewidth=2.2)+
        geom_vline(xintercept=min(res$prop.fragm$pop2[res$prop.fragm$CumSum>0.5], na.rm=T), linetype="dashed")
      
      LABS<-c("Small density", "High density", "", "")
    }  
    
    # If uncertainty in isolation distance
    if(length(dispersion)==2){
      G2<-G2+
        geom_step(data=res2$prop.fragm, aes(x=pop, y=CumSum, col="LongMin"), linewidth=1.8)+
        geom_vline(xintercept=min(res2$prop.fragm$pop[res2$prop.fragm$CumSum>0.5], na.rm=T), linetype="dashed")
      
      LABS<-c("Short isolation distance", "", "Long isolation distance", "")
      
      # If also uncertainty in density
      if("pop2" %in% names(res2$prop.fragm)){
        G2<-G2+
          geom_step(data=res2$prop.fragm, aes(x=pop2, y=CumSum, col="LongMax"), linewidth=1.6)+
          geom_vline(xintercept=min(res2$prop.fragm$pop2[res2$prop.fragm$CumSum>0.5], na.rm=T), linetype="dashed")
        
        LABS<-c("Short isolation / small density", "Short isolation / high density", "Long isolation / small density", "Long isolation / high density")
      }
    }  
    
    # Add legend
    G2<-G2+
      scale_colour_manual(name="", breaks=c("ShortMin", "ShortMax", "LongMin", "LongMax"), 
                          values=c("#A383FF", "#2A2ABF", "#FF7F5E", "darkred"), 
                          labels=LABS, 
                          drop=T)+
      guides(col=guide_legend(nrow=2,byrow=TRUE))
    
    ### Final plot
    Plot<-cowplot::plot_grid(G1, G2, ncol=2)
    return(Plot)
    
  }, seed=T) %>% then(onRejected=function(err) {return(ggplot()+ggtitle("ERROR: we are not able to create this plot, please report that error")+labs(subtitle=err))})
  
  ### Plot the distribution
  return(Prom %...>% plot())
}





## b: Remote sensing products ---------------------------

#* Calculate trends in RS products
#* @get species/<scientific_name>/analysis/RS_analysis
#* @param scientific_name:string Scientific Name
#* @param RSproduct:string RSproduct
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, RSproduct = "") { # nolint    
  
Prom<-future({
  sf::sf_use_s2(FALSE)
  
  # Charge parameters
  scientific_name<-sRL_decode(scientific_name)
  Storage_SP<-sRL_StoreRead(scientific_name, MANDAT=1)
  distSP<-Storage_SP$RangeClean_saved
  GL<-Storage_SP$GL_saved
  print(RSproduct)
  
  # Run functions to calculate trends
  if(RSproduct=="Human_density"){List_trendsRS<-sRL_CalcHumandensity(scientific_name, distSP, GL)}
  if(RSproduct=="Forest_cover"){List_trendsRS<-sRL_CalcForestchange(scientific_name, distSP)}
  if(RSproduct=="NDVI"){List_trendsRS<-sRL_CalcNDVIchange(scientific_name, distSP, GL)}
  
  # Save usage
  RS_stored<-Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Usage_RS"]
  Storage_SP<-sRL_OutLog(Storage_SP, "Usage_RS", paste(RS_stored, RSproduct, sep="."))
  sRL_StoreSave(scientific_name, Storage_SP)
  
  # Return
  return(List_trendsRS)

}, seed=T)

return(Prom)
}







# M7: Outputs ----------------------------------------------------------------

#* Upload all parameters
#* @get species/<scientific_name>/final-estimates
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name){
  scientific_name<-sRL_decode(scientific_name)
  Storage_SP<-sRL_StoreRead(scientific_name, MANDAT=1) ; print(names(Storage_SP))
  
  ### MANAGE TAXONOMY ###
  ### Extract existing taxonomy
  if(scientific_name %in% speciesRL$scientific_name){
    # If species already in the Red List, we use its information
    Official<-speciesRL[speciesRL$scientific_name == scientific_name,][1,]} else {
    # Otherwise we look if there is another species of the same genus (except for authority)
      Genus<-scientific_name %>% strsplit(., " ") %>% unlist(.) %>% .[1]
      if(Genus %in% speciesRL$genus){Official<-speciesRL[speciesRL$genus==Genus,][1,] ; Official$authority<-NA}
    }
  
  ### Assign to that species
  kingdom   <-  ifelse(exists("Official"), Official$kingdom[1], NA)
  phylum    <-  ifelse(exists("Official"), Official$phylum[1], NA)
  classname <-  ifelse(exists("Official"), Official$class[1], NA)
  ordername <-  ifelse(exists("Official"), Official$order[1], NA)
  family    <-  ifelse(exists("Official"), Official$family[1], NA)
  taxonomicAuthority <- ifelse(exists("Official"), Official$authority[1], NA)

  

  
  ### MANAGE ESTIMATES ###
  
  ### EOO
  EOO_val <- ifelse("eoo_km2" %in% names(Storage_SP), Storage_SP$eoo_km2, NA)
  EOO_justif <- ifelse("eoo_km2" %in% names(Storage_SP), "The EOO has been estimated as the Minimum Convex Polygon around the distribution on the sRedList platform.", NA)
  Storage_SP<-sRL_OutLog(Storage_SP, "Estimated_EOO_raw", EOO_val)
  
  
  ### AOO
  if("aoo_km2" %in% names(Storage_SP)){
    AOO_val <- ifelse((Storage_SP$Uncertain=="Uncertain_no"), 
           round(Storage_SP$aoo_km2), 
           paste(round(Storage_SP$aoo_km2), round(Storage_SP$aoo_km2_opt), sep="-")
           )
  } else {
    AOO_val <- NA
  }
  AOO_justif <- ifelse("aoo_km2" %in% names(Storage_SP), "The upper bound of AOO has been estimated on the sRedList Platform by rescaling the Area of Habitat to a 2x2km2 grid.", NA)
  Storage_SP<-sRL_OutLog(Storage_SP, "Estimated_AOO_raw", AOO_val)
  
  
  ### Pop size
  Pop_val <- ifelse("pop_size" %in% names(Storage_SP), Storage_SP$pop_size, NA)
  Storage_SP<-sRL_OutLog(Storage_SP, "Estimated_PopSize_raw", Pop_val)
  
  
  ### Trends
  if(!"aoh_lost_saved" %in% names(Storage_SP)){Trends_dir <- Trends_val <- Trends_justif<- NA} else {
    
    # In any case I take the pessimistic AOH (extrapolated) and its sign
    signP<-sign(Storage_SP$aoh_lost_saved) %>% replace(., .==0, -1)
    aohP<-abs(Storage_SP$aoh_lost_extrap)
    
    # If no uncertainty, I use aohP and signP as suggested estimates
    if(Storage_SP$Uncertain=="Uncertain_no" | (! "aoh_lostOPT_saved" %in% names(Storage_SP))){
      Trends_dir<-revalue(as.character(signP), c("-1"="+", "1"="-"))
      Trends_val<-aohP
      
      # If there is uncertainty it depends if optimistic and pessimistic are in the same direction (both decreasing or both increasing)
    } else {
      signO<-sign(Storage_SP$aoh_lostOPT_saved) %>% replace(., .==0, -1)
      aohO<-abs(Storage_SP$aoh_lostOPT_extrap)
      
      # If optimistic and pessimistic have the same sign, I use this
      if(signP==signO){
        Trends_dir<-revalue(as.character(signP), c("-1"="+", "1"="-"))
        Trends_val<-paste(min(c(aohP, aohO)), max(c(aohP, aohO)), sep="-")
        # If they have different sign, I put the negative and suggest it can go down to 0
      } else {
        Trends_dir<-"Reduction"
        Trends_val<-paste(0, aohP, sep="-")
      }
    }
    
    Trends_justif<-paste0("This trend has been measured from the sRedList platform as the trend in Area of Habitat between ", 
                          paste0(Storage_SP$Year1theo_saved, " and ",config$YearAOH2), # Give years if no extrapolation
                          ifelse(Storage_SP$Year1theo_saved==Storage_SP$Year1_saved, "", " (using expontential extrapolation for the period before 1992)")) # Specify extrapolation if needed
    
    Storage_SP<-sRL_OutLog(Storage_SP, "Estimated_PopTrends_raw", Trends_val)
  }
  
  
  ### Save Storage_SP
  sRL_StoreSave(scientific_name, Storage_SP)
  
  Estimates_df=data.frame(
    kingdom=kingdom,
    phylum=phylum,
    classname=classname,
    ordername=ordername,
    family=family,
    taxonomicAuthority=taxonomicAuthority,
    EOO_val=EOO_val, 
    EOO_justif=EOO_justif, 
    AOO_val=AOO_val, 
    AOO_justif=AOO_justif, 
    Pop_val=Pop_val, 
    Trends_dir=Trends_dir, 
    Trends_val=Trends_val, 
    Trends_justif=Trends_justif
    )
  
  return(list(Estimates=Estimates_df))
}


#* Plot Red List category
#* @post species/<scientific_name>/assessment/red-list-criteria
#* @param scientific_name:string Scientific Name
#* @serializer png list(width = 800, height = 600)
#* @tag sRedList
function(scientific_name,  
         Estimates, 
         pastTrends_dir, pastTrends_qual, pastTrends_basis, pastTrends_reversible, pastTrends_understood, pastTrends_ceased, fragment, Fragment_justif,
         Extreme_EOO, Extreme_AOO, Extreme_Pop, Extreme_NLoc, Extreme_NSub, Extreme_EOO_justif, Extreme_AOO_justif, Extreme_Pop_justif, Extreme_NLoc_justif, Extreme_NSub_justif,
         Continuing_EOO, Continuing_AOO, Continuing_Hab, Continuing_Pop, Continuing_NLoc, Continuing_NSub, Continuing_EOO_justif, Continuing_AOO_justif, Continuing_Hab_justif, Continuing_Pop_justif, Continuing_NLoc_justif, Continuing_NSub_justif,
         locationNumber,	locationNumber_justif, SubNumber,	SubNumber_justif,	Num_Largest, Percent_Largest, OneSubpop,	VeryRestricted,	VeryRestricted_justif,
         populationTrend, currentTrends_basis, currentPop_years, futureTrends_quality, futureTrends_basis, futureTrends, futureTrends_dir, futureTrends_justif, ongoingTrends_NY, ongoingTrends_quality, ongoingTrends_basis, ongoingTrends_reversible, ongoingTrends_understood, ongoingTrends_ceased, ongoingTrends, ongoingTrends_dir, ongoingTrends_justif,
         C_igen_value, C_igen_qual, C_igen_justif, C_iigen_value, C_iigen_qual, C_iigen_justif, C_iiigen_value, C_iiigen_qual, C_iiigen_justif
         ) {

  Estimates<-replace(Estimates, Estimates %in% c("undefined", " "), NA)
  print(Estimates)

  #Filter param
  sRL_loginfo("Start Criteria calculation", scientific_name)
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP<-sRL_StoreRead(scientific_name, MANDAT=1)
  print(names(Storage_SP))

  ### Prepare SIS Connect files
  habitats_SIS<-Storage_SP$habitats_SIS[,6:13]
  #habitats_SIS$assessment_id<-NA
  habitats_SIS$internal_taxon_id<-sRL_CalcIdno(scientific_name)
   
  sRL_loginfo("Start Allfields", scientific_name)
  
  # Charge empty allfields
  allfields<-read.csv("Species/SIS_allfields_empty.csv")[1,]
  allfields$X<-NULL

  # # Take data from saved prepared dataset
  allfields$internal_taxon_name<-scientific_name
  #allfields$assessment_id<-NA
  allfields$internal_taxon_id<-sRL_CalcIdno(scientific_name)
  
  if("AltPref_saved" %in% names(Storage_SP)){
    # Charge AltPref_saved
    AltPref_saved=Storage_SP$AltPref_saved
    
    # Add elevation limits
    allfields$ElevationLower.limit<-paste0(c(AltPref_saved$elevation_lowerEXTREME[1], AltPref_saved$elevation_lower[1]), collapse="-")
    allfields$ElevationUpper.limit<-paste0(c(AltPref_saved$elevation_upper[1], AltPref_saved$elevation_upperEXTREME[1]), collapse="-")
        
  }
  
  ### Save parameters from analyses
  # Generation length
  allfields$GenerationLength.range<-Storage_SP$GL_saved
  
  # EOO
  allfields$EOO.range<-Estimates[7]
  allfields$EOO.justification<-Estimates[8]
  
  # AOO
  allfields$AOO.range<-Estimates[9]
  allfields$AOO.justification<-Estimates[10]
  
  # Population size
  allfields$PopulationSize.range<-Estimates[11]
  
  # Decline for A2
  if(Estimates[12] %in% c("+", "-")){allfields$PopulationReductionPast.direction<-revalue(Estimates[12], c("-"="Reduction", "+"="Increase"))} # Replace by Increase or Reduction (if users wrote something else, we don't report it in allfields)
  if(is.na(Estimates[12])==T & is.na(Estimates[13])==F){allfields$PopulationReductionPast.direction<-"Increase"}
  allfields$PopulationReductionPast.range<-Estimates[13]
  allfields$PopulationReductionPast.justification<-Estimates[14]

  
  # Population trends details
  allfields$PopulationReductionPast.qualifier<-pastTrends_qual
  allfields$PopulationReductionPastBasis.value<-pastTrends_basis
  allfields$PopulationReductionPastReversible.value<-pastTrends_reversible
  allfields$PopulationReductionPastUnderstood.value<-pastTrends_understood
  allfields$PopulationReductionPastCeased.value<-pastTrends_ceased

  # Fragmentation
  allfields$SevereFragmentation.isFragmented<-ifelse(fragment %in% c("true", "TRUE", "T"), "Yes", "No")
  allfields$SevereFragmentation.justification<-Fragment_justif

  # Population details
  allfields$LocationsNumber.range<-locationNumber
  allfields$LocationsNumber.justification<-locationNumber_justif
  allfields$YearOfPopulationEstimate.value<-currentPop_years
  allfields$AreaRestricted.isRestricted<-VeryRestricted
  allfields$AreaRestricted.justification<-VeryRestricted_justif
  allfields$SubpopulationNumber.range<-SubNumber
  allfields$SubpopulationNumber.justification<-SubNumber_justif
  allfields$MaxSubpopulationSize.range<-Num_Largest
  allfields$MatureIndividualsSubpopulation.value<-Percent_Largest
  allfields$SubpopulationSingle.value<-OneSubpop

  # Population trends
  allfields$TrendInWildOfftake.value<-populationTrend
  allfields$CurrentTrendDataDerivation.value<-currentTrends_basis

  allfields$PopulationReductionFuture.direction<-futureTrends_dir
  allfields$PopulationReductionFuture.range<-futureTrends
  allfields$PopulationReductionFuture.justification<-futureTrends_justif
  allfields$PopulationReductionFuture.qualifier<-futureTrends_quality
  allfields$PopulationReductionFutureBasis.value<-futureTrends_basis

  allfields$PopulationReductionPastandFuture.direction<-ongoingTrends_dir
  allfields$PopulationReductionPastandFuture.range<-ongoingTrends
  allfields$PopulationReductionPastandFuture.justification<-ongoingTrends_justif
  allfields$PopulationReductionPastandFuture.qualifier<-ongoingTrends_quality
  allfields$PopulationReductionPastandFutureBasis.value<-ongoingTrends_basis
  allfields$PopulationReductionPastandFuture.numYears<-ongoingTrends_NY
  allfields$PopulationReductionPastandFutureCeased.value<-ongoingTrends_ceased
  allfields$PopulationReductionPastandFutureReversible.value<-ongoingTrends_reversible
  allfields$PopulationReductionPastandFutureUnderstood.value<-ongoingTrends_understood

  # Population C1
  allfields$PopulationDeclineGenerations1.range<-C_igen_value
  allfields$PopulationDeclineGenerations1.qualifier<-C_igen_qual
  allfields$PopulationDeclineGenerations1.justification<-C_igen_justif

  allfields$PopulationDeclineGenerations2.range<-C_iigen_value
  allfields$PopulationDeclineGenerations2.qualifier<-C_iigen_qual
  allfields$PopulationDeclineGenerations2.justification<-C_iigen_justif

  allfields$PopulationDeclineGenerations3.range<-C_iiigen_value
  allfields$PopulationDeclineGenerations3.qualifier<-C_iiigen_qual
  allfields$PopulationDeclineGenerations3.justification<-C_iiigen_justif

  # Extreme fluctuations
  allfields$EOOExtremeFluctuation.isFluctuating<-Extreme_EOO
  allfields$EOOExtremeFluctuation.justification<-Extreme_EOO_justif
  allfields$AOOExtremeFluctuation.isFluctuating<-Extreme_AOO
  allfields$AOOExtremeFluctuation.justification<-Extreme_AOO_justif
  allfields$PopulationExtremeFluctuation.isFluctuating<-Extreme_Pop
  allfields$PopulationExtremeFluctuation.justification<-Extreme_Pop_justif
  allfields$LocationExtremeFluctuation.isFluctuating<-Extreme_NLoc
  allfields$LocationExtremeFluctuation.justification<-Extreme_NLoc_justif
  allfields$SubpopulationExtremeFluctuation.isFluctuating<-Extreme_NSub
  allfields$SubpopulationExtremeFluctuation.justification<-Extreme_NSub_justif

  # Continuing declines
  allfields$EOOContinuingDecline.isContinuingDecline<-Continuing_EOO
  allfields$EOOContinuingDecline.justification<-Continuing_EOO_justif
  allfields$AOOContinuingDecline.isInContinuingDecline<-Continuing_AOO
  allfields$AOOContinuingDecline.justification<-Continuing_AOO_justif
  allfields$HabitatContinuingDecline.isDeclining<-Continuing_Hab
  allfields$HabitatContinuingDecline.justification<-Continuing_Hab_justif
  allfields$LocationContinuingDecline.inDecline<-Continuing_NLoc
  allfields$LocationContinuingDecline.justification<-Continuing_NLoc_justif
  allfields$PopulationContinuingDecline.isDeclining<-Continuing_Pop
  allfields$PopulationContinuingDecline.justification<-Continuing_Pop_justif
  allfields$SubpopulationContinuingDecline.isDeclining<-Continuing_NSub
  allfields$SubpopulationContinuingDecline.justification<-Continuing_NSub_justif
  
  # Remove empty columns (important to avoid overwriting data in reassessments)
  allfields_to_save<-allfields[,which(is.na(allfields[1,])==F & allfields[1,] != "Unknown" & allfields[1,] != "NA")]

  
  
  
  sRL_loginfo("Start Countries and refs", scientific_name)
  
  # Countries (but enabling skipping step)
  if("countries_SIS" %in% names(Storage_SP)){
    countries_SIS<-Storage_SP$countries_SIS
    write.csv(countries_SIS, paste0(output_dir, "/countries.csv"), row.names = F)
  }
  
  ref_SIS<-sRL_OutputRef(scientific_name, Storage_SP) 
  taxo_SIS<-sRL_OutputTaxo(scientific_name, Estimates)
  
  # Save csv files in a folder
  sRL_loginfo("Start writting", scientific_name)
  output_dir<-paste0(sub(" ", "_", scientific_name), "_sRedList")
  dir.create(output_dir)
  write.csv(replace(allfields_to_save, is.na(allfields_to_save), ""), paste0(output_dir, "/allfields.csv"), row.names = F)
  write.csv(taxo_SIS, paste0(output_dir, "/taxonomy.csv"), row.names = F)
  write.csv(ref_SIS, paste0(output_dir, "/references.csv"), row.names = F)
  write.csv(replace(habitats_SIS, is.na(habitats_SIS), ""), paste0(output_dir, "/habitats.csv"), row.names = F)
  write.csv(Storage_SP$Output, paste0(output_dir, "/00.Output_log.csv"), row.names = F)
  
  # Download tracking files
  if(scientific_name=="Download tracker"){
    filesOut<-list.files("Species/Stored_outputs")
    file.copy(paste0("Species/Stored_outputs/", filesOut), paste0(output_dir, "/Outputs_", filesOut))
  }
  
  
  # Save distribution and occurrences if from GBIF
  if(is.null(Storage_SP$gbif_number_saved)==F){
    distSIS<-sRL_OutputDistribution(scientific_name, Storage_SP)
    if("hybas_concat" %in% names(Storage_SP$distSP_saved)){
      hydroSIS<-sRL_OutputHydrobasins(distSIS, Storage_SP)
      write.csv(hydroSIS, paste0(output_dir, "/sRedList_", gsub(" ", ".", scientific_name), "_Hydrobasins.csv"), row.names=F)
    }
   st_write(distSIS, paste0(output_dir, "/sRedList_", gsub(" ", ".", scientific_name), "_Distribution.shp"), append=F)
   write.csv(sRL_OutputOccurrences(scientific_name, Storage_SP), paste0(output_dir, "/sRedList_", gsub(" ", ".", scientific_name), "_Occurrences.csv"), row.names=F)
  }
  
  # Zip that folder
  #zip(zipfile = output_dir, files = output_dir,  zip = "C:/Program Files/7-Zip/7z", flags="a -tzip")
  zip(zipfile = output_dir, files = output_dir)


  ### Calculate criteria
  sRL_loginfo("Start Criteria calculation", scientific_name)
  criteria<-sRL_CriteriaCalculator(allfields[1,])
  
  
  ### Plot
  sRL_loginfo("Start Plotting", scientific_name)
  criteria$Cat_ThresholdMIN <- criteria$Cat_ThresholdMIN %>% replace(., .=="LC", "LC/NT") %>% factor(., c("LC/NT", "VU", "EN", "CR"))
  criteria$Cat_ThresholdMAX <- criteria$Cat_ThresholdMAX %>% replace(., .=="LC", "LC/NT") %>% factor(., c("LC/NT", "VU", "EN", "CR"))
  criteria$criterion<-factor(criteria$criterion, levels=sort(criteria$criterion, decreasing=T))
  
  CAT_MAX<-sort(criteria$Cat_ThresholdMAX[criteria$Subcrit==1], decreasing=T)[1]
  if(is.na(CAT_MAX)){CAT_MAX<-"LC"}
  CRIT_MAX<-criteria$criterion[criteria$Cat_ThresholdMAX==CAT_MAX & criteria$Subcrit==1] %>% .[is.na(.)==F] %>% paste(., collapse=" / ") %>% paste0(" under crit. ", .)
  if(CAT_MAX=="LC"){CRIT_MAX<-""}
  
  criteria$ColMin<-paste0(criteria$Cat_ThresholdMIN, criteria$Subcrit) %>% factor(., c("LC/NT1", "VU1", "EN1", "CR1", "LC/NT0", "VU0", "EN0", "CR0"))
  criteria$ColMax<-paste0(criteria$Cat_ThresholdMAX, criteria$Subcrit) %>% factor(., c("LC/NT1", "VU1", "EN1", "CR1", "LC/NT0", "VU0", "EN0", "CR0"))
  
  # Prepare plot title
  TITLE<-ifelse(CAT_MAX=="LC", "The species does not seem to trigger a threatened category under any criterion; \n it could thus be LC or NT) \n", paste0("The species seems to meet the ", CAT_MAX, " category", CRIT_MAX, "\n"))
  SUBTITLE<-ifelse(nrow(speciesRL[speciesRL$scientific_name == scientific_name,])==1, paste0("Last published category: ", speciesRL$category[speciesRL$scientific_name == scientific_name], "\n"), "")
  
  return(
    plot(ggplot(criteria, aes(y = criterion)) +
      geom_linerange(aes(xmin=Cat_ThresholdMIN, xmax=Cat_ThresholdMAX), linewidth=10, colour="gray75")+
      geom_point(aes(x = Cat_ThresholdMIN, col = ColMin), size = 20, stroke=6, show.legend=F) +
      geom_text(aes(x=Cat_ThresholdMIN, label=Cat_ThresholdMIN), col="white", size=5)+
      geom_point(aes(x = Cat_ThresholdMAX, col = ColMax), size = 20, stroke=6, show.legend=F) +
      geom_text(aes(x=Cat_ThresholdMAX, label=Cat_ThresholdMAX), col="white", size=5)+
      scale_x_discrete(drop = F, na.translate = FALSE) + scale_y_discrete(drop=F, na.translate = FALSE) +
      xlab("Red List Category triggered") + ylab("Criteria")+
      scale_colour_manual(drop = F, values=c("#006666ff", "#cc9900ff", "#cc6633ff", "#cc3333ff", "#b3d1d1ff", "#f0e1b3ff", "#f0d1c2ff", "#f0c2c2ff"))+
      ggtitle(TITLE, subtitle=SUBTITLE)+
      theme_bw()  %+replace% theme(text=element_text(size=18), plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size=15))
    )
  )


}



#* Download .zip assesment file of the species
#* @get species/<scientific_name>/assessment/red-list-criteria/zip
#* @param scientific_name:string Scientific Name
#* @serializer contentType list(type="application/octet-stream")
#* @tag sRedList
function(scientific_name) {
  scientific_name <- sRL_decode(scientific_name)
  
  sRL_loginfo("Start Zipping", scientific_name)
  
  # Prepare the ZIP to return
  zip_to_extract<-readBin(paste0(gsub(" ", "_", scientific_name), "_sRedList.zip"), "raw", n = file.info(paste0(gsub(" ", "_", scientific_name), "_sRedList.zip"))$size)
  
  # Prepare Outputs (remove definitions, empty fields, those at default)
  output_species<-sRL_StoreRead(scientific_name, MANDAT=1)$Output
  output_species$Definition<-NULL
  output_species$Value<-replace(output_species$Value, is.na(output_species$Value), "") # I replace NA to make the next line work and then remove all empty fields
  output_species$Value[output_species$Parameter=="Original_GL"] <- ifelse(scientific_name %in% GL_file$internal_taxon_name, GL_file$GL_estimate[GL_file$internal_taxon_name==scientific_name][1], NA)
  output_species$Value[output_species$Parameter=="Original_density"] <- density$Density[density$Species == scientific_name] %>% round(., 2) %>% mean(., na.rm=T)
  if(output_species$Value[output_species$Parameter=="Gbif_Extent"]=="-180,180,-90,90"){output_species<-output_species[-which(output_species$Parameter=="Gbif_Extent"),]}
  if(output_species$Value[output_species$Parameter=="Gbif_yearBin"]=="FALSE"){output_species<-output_species[-which(output_species$Parameter=="Gbif_yearBin"),]}
  if(output_species$Value[output_species$Parameter=="Gbif_uncertainBin"]=="FALSE"){output_species<-output_species[-which(output_species$Parameter=="Gbif_uncertainBin"),]}
  if(output_species$Value[output_species$Parameter=="Mapping_Buffer"]=="0"){output_species<-output_species[-which(output_species$Parameter=="Mapping_Buffer"),]}
  if(output_species$Value[output_species$Parameter=="Mapping_Altitude"]=="0,9000"){output_species<-output_species[-which(output_species$Parameter=="Mapping_Altitude"),]}
  if(output_species$Value[output_species$Parameter=="Mapping_Merge"]=="FALSE"){output_species<-output_species[-which(output_species$Parameter=="Mapping_Merge"),]}
  if(output_species$Value[output_species$Parameter=="Mapping_Smooth"]=="0"){output_species<-output_species[-which(output_species$Parameter=="Mapping_Smooth"),]}
  if(output_species$Value[output_species$Parameter=="Distribution_Presence"]=="1,2"){output_species<-output_species[-which(output_species$Parameter=="Distribution_Presence"),]}
  if(output_species$Value[output_species$Parameter=="Distribution_Origin"]=="1,2"){output_species<-output_species[-which(output_species$Parameter=="Distribution_Origin"),]}
  if(output_species$Value[output_species$Parameter=="Distribution_Seasonal"]=="1,2"){output_species<-output_species[-which(output_species$Parameter=="Distribution_Seasonal"),]}
  output_species$Value[output_species$Parameter=="Col_allfields"]<-paste(names(read.csv(paste0(sub(" ", "_", scientific_name), "_sRedList/allfields.csv"))), collapse=",")
  output_species<-subset(output_species, is.na(output_species$Value)==F & output_species$Value != "" & substr(output_species$Parameter, 1, 9) != "Estimated")
  output_species$Date<-Sys.time() %>% as.character(.)
  
  # Save Outputs
  tryCatch({
    FileStored<-paste0("Species/Stored_outputs/Stored_", substr(Sys.Date(), 1, 7), ".csv")
    if(file.exists(FileStored)){
      Saved_output<-read.csv(FileStored)
    } else {Saved_output<-read.csv("Species/Output_save_empty.csv")}
    Saved_output<-rbind.fill(Saved_output, output_species)
    write.csv(Saved_output, FileStored, row.names=F)
  })
  
  # Remove the local files
  unlink(paste0(gsub(" ", "_", scientific_name), "_sRedList"), recursive=T)
  unlink(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name)), recursive=T)
  unlink(paste0(gsub(" ", "_", scientific_name), "_sRedList.zip"), recursive=T)
  
  # Return
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
  scientific_name <- sRL_decode(scientific_name)
  
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
  scientific_name <- sRL_decode(scientific_name)
  file_name <- url_decode(file_name)
  path <- url_decode(path)
  type <- url_decode(type)
  if ((scientific_name %in% list.files(config$distribution_path)) && !grepl("_RL", file_name) && !grepl("_RL", path)) { # nolint
    if (type == "folder") {
      if (file_name %in% list.files(paste0(config$distribution_path, path))) {
        sRL_loginfo(paste0("Delete distribution:", config$distribution_path, path, '/', file_name), scientific_name) # nolint
        return(list(response = unlink(paste0(config$distribution_path, path, '/', file_name), recursive = TRUE))) # nolint
      }
    }else {
      if(file_name %in% list.files(paste0(config$distribution_path, scientific_name, '/', path))){ # nolint
        sRL_loginfo(paste0("Delete distribution:", config$distribution_path, scientific_name, '/', path, "/", file_name), scientific_name) # nolint
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
  scientific_name <- sRL_decode(scientific_name)
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
               paste0("habitatsM<-rbind.fill(",
                      paste0("read.csv(Hab_files[", 1:length(Hab_files), "])", collapse=','),")"
               )))
  
  
  # Merge countries
  Coun_files<-list.files(Zip_Path, recursive = T)[grepl('countries.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("countriesM<-rbind.fill(",
                      paste0("read.csv(Coun_files[", 1:length(Coun_files), "])", collapse=','),")"
               )))
  
  # Merge references
  Ref_files<-list.files(Zip_Path, recursive = T)[grepl('references.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("referencesM<-rbind.fill(",
                      paste0("read.csv(Ref_files[", 1:length(Ref_files), "])", collapse=','),")"
               )))
  
  # Merge allfields
  All_files<-list.files(Zip_Path, recursive = T)[grepl('allfields.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("allfieldsM<-rbind.fill(",
                      paste0("read.csv(All_files[", 1:length(All_files), "])", collapse=','),")"
               )))
  
  
  # Merge log
  Log_files<-list.files(Zip_Path, recursive = T)[grepl('00.Output_log.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("LogM<-rbind.fill(",
                      paste0("read.csv(Log_files[", 1:length(Log_files), "])", collapse=','),")"
               )))
  
  
  # Merge Distributions
  if(TRUE %in% grepl('_Distribution.shp', list.files(Zip_Path, recursive = T))){
    
    Dist_files<-list.files(Zip_Path, recursive = T)[grepl('_Distribution.shp', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
    
    eval(parse(text=
                 paste0("DistM<-rbind.fill(",
                        paste0("st_read(Dist_files[", 1:length(Dist_files), "])", collapse=','),")"
                 )))
  }
  
  # Merge Occurrences
  if(TRUE %in% grepl('_Occurrences.shp', list.files(Zip_Path, recursive = T))){
    
    Occ_files<-list.files(Zip_Path, recursive = T)[grepl('_Occurrences.shp', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
    
    eval(parse(text=
                 paste0("OccM<-rbind.fill(",
                        paste0("st_read(Occ_files[", 1:length(Occ_files), "])", collapse=','),")"
                 )))
  }
  
  
  
  ### Save in a merged ZIP file
  unlink(paste0(Zip_Path, "/", list.files(Zip_Path)), recursive=T)
  write.csv(replace(allfieldsM, is.na(allfieldsM), ""), paste0(Zip_Path, "/allfields.csv"), row.names = F)
  write.csv(replace(countriesM, is.na(countriesM), ""), paste0(Zip_Path, "/countries.csv"), row.names = F)
  write.csv(replace(referencesM, is.na(referencesM), ""), paste0(Zip_Path, "/references.csv"), row.names = F)
  write.csv(replace(habitatsM, is.na(habitatsM), ""), paste0(Zip_Path, "/habitats.csv"), row.names = F)
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



