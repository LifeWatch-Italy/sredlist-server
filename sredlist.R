


######################################
### MODULE 1: CHARGE DISTRIBUTIONS ###
######################################


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
  if("CountrySP_saved" %not in% names(Storage_SP)){Storage_SP$CountrySP_saved<-sRL_PrepareCountries(extent(distSP))} 
  CountrySP<-st_crop(Storage_SP$CountrySP_saved, extent(distSP))
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









#########################
### MODULE 2: MAPPING ###
#########################


#* Global Biodiversity Information Facility
#* @get species/<scientific_name>/gbif
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name) {
  
  #GBIF STEP 1
  ### Clean-string from user
  scientific_name <- url_decode(scientific_name)
  scientific_name <- R.utils::capitalize(trim(gsub("[[:punct:]]", " ", scientific_name))) # nolint
  print(scientific_name)
  
  ### GBIF procedure 
  log_info("START - Create data")
  dat <- sRL_createDataGBIF(scientific_name, LIM_GBIF)
  
  # ### Plot
  wm <- borders("world", colour = "gray70", fill = "gray70")
  ggsave("plot_data.png", plot(
    ggplot() +
      coord_fixed() +
      wm +
      geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 0.5) +
      theme_bw())) # nolint
  plot1 <- base64enc::dataURI(file = "plot_data.png", mime = "image/png")
  log_info("END - Create data")
  log_info("START - Clean coordinates")
  
  
  # Prepare countries
  CountrySP_WGS<-st_crop(distCountries_WGS, c(xmin=min(dat$decimalLongitude), xmax=max(dat$decimalLongitude), ymin=min(dat$decimalLatitude), ymax=max(dat$decimalLatitude)))
  
  # Flag observations to remove
  flags_raw <- clean_coordinates(x = dat,
                                 lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 countries = "countryCode",
                                 species = "species",
                                 seas_ref=CountrySP_WGS %>% as_Spatial(.),
                                 tests = c("capitals", "centroids", "equal", "gbif", "institutions", "zeros", "seas"))
  
  #GBIF STEP 2  
  # Subset the observations user wants to keep (can be run several times if users play with parameters)
  flags <- sRL_cleanDataGBIF(flags_raw, year_GBIF, uncertainty_GBIF, keepyearNA_GBIF, cleaningpar_GBIF, GBIF_xmin, GBIF_xmax, GBIF_ymin, GBIF_ymax)
  
  # Plot
  ggsave("clean_coordinates.png", plot(
    
    ### This is a static plot that works 
    ggplot()+
      coord_fixed()+
      geom_sf(data=CountrySP_WGS, fill="gray70")+
      geom_point(data = flags, aes(x = decimalLongitude, y = decimalLatitude, col=factor(is.na(Reason), c("FALSE", "TRUE"))), size = 1.3)+
      scale_colour_manual(values=c("#440154ff", "#fde725ff"), name="Valid observation", drop=F)+
      xlab("")+ylab("")+
      theme_bw() %+replace%   theme(legend.position="bottom")
    
    ### This is an interactive plot that I'd like to include but I didn't manage to include on the platform
    # flags$Valid<-paste(revalue(as.character(is.na(flags$Reason)), c("TRUE"="Valid observation", "FALSE"="Invalid observation")), "\n Gbif_ID: ", flags$gbifID, '\n Year:', flags$year) ; flags$Valid[is.na(flags$Reason)==F]<-paste0(flags$Valid[is.na(flags$Reason)==F], '\n Reason_flagged:', flags$Reason[is.na(flags$Reason)==F])
    # 
    # ggplotly(
    #   ggplot()+
    #     coord_fixed()+
    #     geom_sf(data=sf::st_cast(CountrySP_WGS, "MULTIPOLYGON"), fill="#999999ff")+
    #     geom_point(data = flags, aes(x = decimalLongitude, y = decimalLatitude, col=factor(is.na(Reason), c("FALSE", "TRUE")), label=Valid), size = 1.3)+
    #     scale_colour_manual(values=c("#440154ff", "#fde725ff"), name="Valid observation", drop=F)+
    #     xlab("")+ylab("")+
    #     theme_bw() %+replace%   theme(legend.position="bottom"),
    #   tooltip="label"
    # )
    
  ))
  plot2 <- base64enc::dataURI(file = "clean_coordinates.png", mime = "image/png") # nolint
  log_info("END - Clean coordinates")
  
  
  #GBIF STEP 3  
  # Map distribution from GBIF
  log_info("START - Maps the distribution")
  
  distGBIF0<-sRL_MapEOOGBIF(flags, scientific_name)
  distSP<-sRL_MapDistributionGBIF(distGBIF0, GBIF_BUFF_km2, GBIF_crop, scientific_name)
  Storage_SP=eval(parse(text=paste0("Storage_SP_", sub(" ", "_", scientific_name))))
  Storage_SP$gbif_number_saved=eval(parse(text=paste0("gbif_number_saved_", sub(" ", "_", scientific_name))))
  EOO_km2 <- round(as.numeric(st_area(distSP))/1000000) # nolint
  EOO_rating <- EOORating(EOO_km2) # nolint
  
  # Plot distribution
  gbif_path <- sRL_saveMapDistribution(scientific_name, distSP, gbif_nb=Storage_SP$gbif_number_saved)
  ggsave("eoo.png", plot(
    ggplot() + 
      geom_sf(data=Storage_SP$CountrySP_saved, fill="gray70")+
      geom_sf(data = distSP, fill="darkred") + 
      ggtitle("")+
      theme_bw()
  )) # nolint
  plot3 <- base64enc::dataURI(file = "eoo.png", mime = "image/png") # nolint
  log_info("END - Maps the distribution")
  
  # Keep distribution in memory
  Storage_SP$distSP_saved=distSP
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  
  file.remove("plot_data.png")
  file.remove("clean_coordinates.png")
  file.remove("eoo.png")
  return(list(
    plot_data = plot1,
    plot_clean_coordinates = plot2,
    eoo_km2 = EOO_km2,
    eoo_rating = EOO_rating,
    plot_eoo = plot3,
    gbif_data_number  = as.numeric(Storage_SP$gbif_number_saved),
    gbif_path = gbif_path
  ))
  
}





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
  
  ### Plot EOO
  log_info("START - Plot EOO")
  EOO <- st_as_sf(st_convex_hull(st_union(distSP))) ## Needed to avoid having different sub-polygons
  ggsave(paste0(scientific_name, ".png"), plot(
    ggplot() + 
      geom_sf(data=EOO, fill="#ef3b2c", col=NA) + 
      geom_sf(data=distSP, fill="#fcbba1", col=NA) + 
      geom_sf(data=st_crop(Storage_SP$CountrySP_saved, extent(EOO)), fill=NA, col="black")+
      ggtitle(paste0("EOO of ", scientific_name))) +
      theme_bw()
  ) # nolint
  plot3 <- base64enc::dataURI(file = paste0(scientific_name, ".png"), mime = "image/png") # nolint
  log_info("END - Plot EOO")
  file.remove(paste0(scientific_name, ".png"))
  
  ### Calculate EOO area
  EOO_km2 <- round(as.numeric(st_area(EOO))/1000000)
  
  return(list(
    eoo_km2 = EOO_km2,
    plot_eoo = plot3
  ))
  
}










##############################
### MODULE 3: AOH ANALYSES ###
##############################


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
  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)
  distSP=Storage_SP$distSP_saved
  
  # Habitat table (for aoh analysis and for SIS Connect)
  habitats_pref_DF<-sRL_PrepareHabitatFile(scientific_name, habitats_pref)
  Storage_SP$habitats_SIS=habitats_pref_DF
  
  # Altitude table (for aoh analysis and for SIS Connect)
  altitudes_pref_DF<-sRL_PrepareAltitudeFile(scientific_name, altitudes_pref)
  Storage_SP$AltPref_saved=altitudes_pref_DF
  
  density_pref <- as.numeric(density_pref)
  
  print(habitats_pref)
  print(altitudes_pref)
  print(density_pref)
  print(isGbifDistribution)
  
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

  # Altitude
  terraOptions(tempdir=paste0(output_dir, "/Temporary"))
  alt_crop=crop(alt_raw, extent(distSP)) 
  Storage_SP$alt_crop_saved=alt_crop
  cci2_crop<-crop(cci2, extent(distSP))
    
  AOH2<-sRL_calculateAOH(rangeSP_fun=rangeSP_clean, 
                         cci_fun=cci2_crop, 
                         alt_fun=alt_crop,
                         habitat_pref_fun=habitats_pref_DF,
                         FOLDER=paste0(output_dir, "/Current"),
                         elevation_data_fun=altitudes_pref_DF)
  Storage_SP$AOH2_saved=AOH2
  
  
  # Plot
  log_info("START - Plot AOH")
  
  plot1 <- gplot(AOH2[[1]]) +
    coord_fixed()+
    geom_tile(aes(fill = as.factor(value))) +
    scale_fill_manual(values=c("#dfc27d", "#018571", NA), labels=c("Unsuitable", "Suitable", ""), name="", na.translate=F) +
    ggtitle("Area of Habitat in 2020") +
    theme_void() %+replace%   theme(plot.title=element_text(hjust=0.5, size=14, face="bold"))
  
  ggsave(filename = "aoh.png", plot = plot(plot1))
  
  plot1 <- base64enc::dataURI(file = "aoh.png", mime = "image/png", encoding = "base64") # nolint
  file.remove("aoh.png")
  log_info("END - Plot AOH")
  
  AOH_km2 <-  sRL_areaAOH(AOH2[[1]], SCALE="cci")
  
  #Calculate Area of Habitat in a resolution of 2x2km (as is the map of altitude provided), in order to use it as an upper bound of species Area of Occupancy under criterion B2 (each cell covers 4km2)
  grid22_crop<-crop(grid22, AOH2[[1]])
  aoh_22<-resample(AOH2[[1]], grid22_crop, method="max")
  
  plot2 <- gplot(aoh_22[[1]]) +
    coord_fixed()+
    geom_tile(aes(fill = factor(as.character(value), c("0", "1")))) +
    scale_fill_manual(values=c("#dfc27d", "#018571"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
    ggtitle("Area of Habitat (2x2km)") +
    theme_void() %+replace%   theme(plot.title=element_text(hjust=0.5, size=14, face="bold"))
  
  
  ggsave(filename = "aoo.png", plot = plot(plot2))
  plot2 <- base64enc::dataURI(file = "aoo.png", mime = "image/png", encoding = "base64") # nolint
  file.remove("aoo.png")
  
  AOO_km2<- sRL_areaAOH(aoh_22[[1]], SCALE="2x2")
  
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  
  # Calculate population size
  if (density_pref != -1) {
    density_sp <- density_pref
    pop_size <- AOH_km2 * density_sp
    return(list(
      aoh_km2 = round(AOH_km2),
      aoo_km2 = round(AOO_km2),
      plot_aoh = plot1,
      plot_aoh_2x2 = plot2,
      pop_size = round(pop_size)
    ));
  }
  
  return(list(
    aoh_km2 = round(AOH_km2),
    aoo_km2 = round(AOO_km2),
    plot_aoh = plot1,
    plot_aoh_2x2 = plot2
  ));
  
  
}







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

  # # Save GL in Storage
  print(paste0("Generation length is: ", GL_species))

  # Charge distribution
  distSP$binomial<-as.character(distSP$binomial)
  
  # Output directory
  output_dir<-paste0("resources/AOH_stored/", sub(" ", "_", scientific_name))
  dir.create(paste0(output_dir, "/Initial"))
  
  # Charge CCI1
  log_info("ok3")
  terraOptions(tempdir=paste0(output_dir, "/Temporary"))
  log_info("ok3b")
  Year1_theo<-config$YearAOH2-max(10, round(3*GL_species))
  log_info("ok3c")
  Year1<-max(Year1_theo, 1992) ; print(Year1)
  log_info("ok4")
  cci1<-rast(sub("XXXX", Year1, config$cci1_raster_path)) ; crs(cci1)<-CRSMOLL # I ensure the CRS is correctly assigned
  
  # Crop CCI1
  cci1_crop<-crop(cci1, extent(distSP))

  
  # Calculate AOH
  AOH1<-sRL_calculateAOH(rangeSP_fun=rangeSP_clean,
                         cci_fun=cci1_crop,
                         alt_fun=alt_crop,
                         habitat_pref_fun=habitats_pref_DF,
                         FOLDER=paste0(output_dir, "/Initial"),
                         elevation_data_fun=altitudes_pref_DF)
  
  # Area
  AOH_km2<-sRL_areaAOH(AOH2[[1]], SCALE="cci")
  AOH_old_km2<-sRL_areaAOH(AOH1[[1]], SCALE="cci")
  
  # Calculate trends in AOH and plot
  AOH_lost<- (1-(AOH_km2/AOH_old_km2))
  
  plot1 <- gplot((AOH2[[1]]*2+3)-AOH1[[1]]) + 
    coord_fixed()+
    geom_tile(aes(fill = factor(as.character(value), c("2", "3", "4", "5"))))+
    scale_fill_manual(values=c("#a6611a", "gray90", "#80cdc1", "#018571"), labels=c("Newly unsuitable", "Unsuitable", "Suitable", "Newly suitable"), name="", na.translate=F, drop=F)+
    ggtitle(paste0("Trends in Area of Habitat between ", Year1, " and ", config$YearAOH2))+
    theme_void() %+replace%   theme(plot.title=element_text(hjust=0.5, size=14, face="bold"))
  
  ggsave(filename = "trends-aoh.png", plot = plot(plot1))
  plot1 <- base64enc::dataURI(file = "trends-aoh.png", mime = "image/png", encoding = "base64") # nolint
  file.remove("trends-aoh.png")
  
  Storage_SP$aoh_lost_saved=round(as.numeric(AOH_lost)*100)
  Storage_SP$RangeClean_saved=Storage_SP$AOH2_saved=Storage_SP$alt_crop_saved=NULL
  Storage_SP$Year1_saved<-Year1 ; Storage_SP$Year1theo_saved<-Year1_theo
  assign(paste0("Storage_SP_", sub(" ", "_", scientific_name)), Storage_SP, .GlobalEnv)
  
  # Remove the AOH files stored
  unlink(output_dir, recursive=T)
  terraOptions(tempdir=tempdir())
  
  return(list(
    aoh_lost_km2 = round(AOH_old_km2),
    aoh_lost = Storage_SP$aoh_lost_saved,
    plot_trends_aoh = plot1
  ))
  
}




#########################
### MODULE N: OUTPUTS ###
#########################


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



#* Download .CSV Red List category
#* @get species/<scientific_name>/assessment/red-list-criteria/csv
#* @param scientific_name:string Scientific Name
#* @param aoh_lost:int AOH_lost
#* @param eoo_km2:int EOO_km2
#* @param aoo_km2:int AOO_km2
#* @param pop_size:int Pop_size
#* @serializer csv
#* @tag sRedList
function(scientific_name, eoo_km2, aoo_km2, pop_size, res) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  Storage_SP=sRL_reuse(scientific_name)
  
  aoh_lost=Storage_SP$aoh_lost_saved
  AltPref_saved=Storage_SP$AltPref_saved
  habitats_SIS=Storage_SP$habitats_SIS
  
  # Calculate criteria
  criteria<-sRL_CalculateCriteria(aoh_lost, eoo_km2, aoo_km2, pop_size)
  
  # Prepare file to extract
  allfields_SIS<-sRL_CreateALLFIELDS(scientific_name, aoh_lost, eoo_km2, aoo_km2, pop_size, AltPref_saved)
  
  # Prepare countries
  countries_SIS<-sRL_OutputCountries(scientific_name, distSP_saved=Storage_SP$distSP_saved, CountrySP_saved=Storage_SP$CountrySP_saved, AltPref_saved)
  
  # Prepare references
  ref_SIS<-sRL_OutputRef(scientific_name, AltPref_saved)
  
  # Prepare distributions
  distSP_SIS<-sRL_OutputDistribution(scientific_name, Storage_SP$distSP_saved)
  
  #filename <- paste0('assessment-', scientific_name, '-', Sys.Date(), '.csv') 
  #pathToSaveAssessment <- paste0("Assessments/", filename)
  
  eval(parse(text=paste0("rm(Storage_SP_", sub(" ", "_", scientific_name), ")")))  #write.csv(allfields_SIS, pathToSaveAssessment, row.names = F)
  return(allfields_SIS) # We also want to extract: habitats_SIS, countries_SIS, ref_SIS (best would be all in 1 zip file); we also want to download the distribution with a second button. You can use st_write(distSP_SIS, paste0("sRedList_Distribution_", gsub(" ", ".", scientific_name), ".shp"), append=F)
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







###########################################
### OTHER FUNCTIONS (PLATFORM BUILDING) ###
###########################################


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