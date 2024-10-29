


# Step 1a: Charge distributions ----------------------------------------------------------------

## Upload distribution ----
#* Upload Distribution species
#* @post species/<scientific_name>/distribution
#* @param req:file Distribution file (shp,.shx,.prj,.dbf,.cpg)
#* @param scientific_name:str Insert Species Name
#* @serializer unboxedJSON
#* @parser multi
#* @tag sRedList1
function(scientific_name, req) {
  
  sRL_loginfo("START - Upload Distribution", scientific_name)
  scientific_name <- sRL_decode(scientific_name)
  scientific_name <- R.utils::capitalize(trim(gsub("[[:punct:]]", " ", scientific_name))) # nolint
  
  # Required for multiple file uploads
  names(req)
  
  # Parses into a Rook multipart file type;needed for API conversions
  fileInfo <- list(formContents = Rook::Multipart$parse(req)) # nolint
  # This is where the file name is stored
  file_name <- fileInfo$formContents$req$filename
  
  print(file_ext(file_name))
  if(file_ext(file_name) %not in% extensions) {
    invalid_extension(file_name)
  }
  
  # The file is downloaded in a temporary folder
  tmpfile <- fileInfo$formContents$req$tempfile
  #print(fileInfo) # nolint
  upload_folder_scientific_name <- R.utils::capitalize(paste0(stringr::str_replace(scientific_name, " ", "_"), "_Uploaded", format(Sys.time(), "_%Y%m%d"))) # nolint
  # Create a file path E.g: Distributions/Nile tilapia/Nile_tilapia_20211207/
  filePath <- paste0(config$distribution_path, scientific_name, "/", upload_folder_scientific_name, "/") # nolint
  if (! dir.exists(filePath)) {dir.create(filePath, showWarnings = TRUE, recursive = TRUE)}
  print(file_name)
  new_file_name2 = paste0(upload_folder_scientific_name, ".", file_ext(file_name)) # nolint
  fn <- paste0(filePath, new_file_name2)

  # Remove the file if already exists (to allow overwriting)
  unlink(fn)
  # Copies the file into the designated folder
  file.copy(tmpfile, fn)

  # Save a json
  text <- paste0(
    "A distribution has been stored for the species: ",
    scientific_name,
    ".\nIt was uploaded to the sRedList platform on the ",
    Sys.time(),
    " CET from a file named ", sub(".shx", ".shp", file_name)
  )
  tryCatch({jsonlite::write_json(list(info = text), paste0(filePath, upload_folder_scientific_name, ".json"), auto_unbox= TRUE)}, error=function(e){cat("TryCatch JSON upload failed")})
  
  print(paste0("Your file is now stored in ", fn))
  sRL_loginfo("END - Upload Distribution", scientific_name)
  

  return(list(path = fn))
}


##  Distribution info ----
#* Info distribution species from sRedList platform (get attributes available for this species)
#* @get species/<scientific_name>/distribution/info
#* @param scientific_name:string Scientific Name
#* @param Dist_path:string Distribution Folder default RedList
#* @tag sRedList1
function(scientific_name, username) {
  
  sRL_loginfo("START - Distribution info", scientific_name)
  
  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  distSP_full <- sRL_StoreRead(scientific_name,  username, MANDAT=0)$distSP_saved
  
  sRL_loginfo("END - Distribution info", scientific_name)
  
  return(list(
    presences = union(c(1, 2), unique(distSP_full$presence)),
    seasons = union(c(1, 2), unique(distSP_full$seasonal)),
    origins = union(c(1, 2), unique(distSP_full$origin))
  )
  )
}



## Plot edited distribution (1a) ----
#* Plot the distributions plot from sRedList platform
#* @get species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param Dist_path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList1
 function(scientific_name, username, Crop_Country="", Dist_path = "") { # nolint

   Prom<-future({
     sf::sf_use_s2(FALSE)
     
     #Filter param
     sRL_loginfo("START - Prepare distribution 1a", scientific_name)
     scientific_name <- sRL_decode(scientific_name)
     Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=0) ; print(names(Storage_SP))
     Storage_SP<-subset(Storage_SP, names(Storage_SP) %in% c("CountrySP_saved", "Creation", "Output")) # Remove other elements from Storage_SP which could come from a previous process on the platform
     if(Crop_Country=="Keep all countries"){Crop_Country<-""}
     print(Crop_Country)
     Dist_path <- ifelse(Dist_path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), Dist_path ) # nolint
     
     # If outlog not present (I don't think this should happen but just in case)
     if(! "Output" %in% names(Storage_SP)){Storage_SP$Output<-sRL_InitLog(scientific_name, username, DisSource = "Unknown")}
     
     ### Load Distribution Species
     distributions <- sRL_ReadDistribution(scientific_name, Dist_path) %>% sRL_PrepareDistrib(., scientific_name)
     distSP <- subset(distributions, distributions$binomial == scientific_name) # nolint
     
     ### Crop_Country for National RL
     Storage_SP<-sRL_OutLog(Storage_SP, "Crop_Country", Crop_Country)
     if(Crop_Country != ""){
       sRL_loginfo("START - Crop country \n", scientific_name)
       distSP<-sRL_CropCountry(distSP, Crop_Country, scientific_name)
       sRL_loginfo("END - Crop country \n", scientific_name)
     }
     
     if (nrow(distSP) > 0) {
       
       ### Add id_no if not present
       names(distSP)<-replace(names(distSP), tolower(names(distSP)) %in% c("id_no", "sisid"), "id_no")
       distSP$id_no<-sRL_CalcIdno(scientific_name)
       
       ### Colour the distributions
       sRL_loginfo("START - Colour distribution", scientific_name)
       distSP<-sRL_ColourDistrib(distSP)
       
       ### Prepare countries if they were not charged or if we are not using the RL distribution + crop depending on the current selection of range
       if("CountrySP_saved" %not in% names(Storage_SP) | (! grepl("_RL", Dist_path))){Storage_SP$CountrySP_saved<-sRL_PrepareCountries(1.2*extent(distSP))}
       CountrySP<-Storage_SP$CountrySP_saved
       if(nrow(CountrySP)>0){CountrySP<-st_crop(CountrySP, 1.2*extent(distSP))} # Crop again in case national Red Listing or selection that reduces the extent
       
       sRL_loginfo("Plot distribution", scientific_name)
       
       ### Plot
       plot_dist<-ggplot() +
         geom_sf(data = CountrySP, fill="gray96", col="gray50") + # nolint
         geom_sf(data = distSP, fill = distSP$cols) +
         theme_void() +
         ggtitle("")
     } else{
       sf::sf_use_s2(FALSE)
       plot_dist<-ggplot() +
         geom_sf(data = distCountries, fill="gray96", col="gray50") + # nolint
         sRLTheme_maps +
         ggtitle("The distribution is empty")
     }
     
     ### Save the distribution in memory
     Storage_SP$distSP_saved<-distSP
     DisSource<-ifelse(substr(Dist_path, nchar(Dist_path)-2, nchar(Dist_path))=="_RL", "Red List", ifelse(grepl("Created", Dist_path), "StoredOnPlatform", ifelse(grepl("Uploaded", Dist_path), "Uploaded", "Edited")))
     Storage_SP<-sRL_OutLog(Storage_SP, "Distribution_Source", DisSource) # If Dist_path ends by _RL it comes from the RL, uploaded otherwise
     sRL_StoreSave(scientific_name, username,  Storage_SP)
     
     # Save plot
     EXT <- extent(distSP) ; size_scale <- (EXT[2]-EXT[1])/(EXT[4]-EXT[3])
     dir.create(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots"), recursive=T)
     ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/plot_dist.png"), plot_dist, width=10, height=8/size_scale)
     plot_distENC <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/plot_dist.png"), mime = "image/png")
     
     sRL_loginfo("END - Prepare distribution 1a", scientific_name)
     
  # Return
  return(list(plot_edit=plot_distENC))

}, gc=T, seed=T)

return(Prom)
}


## Compare distribution and GBIF (1a) ----
#* Compare distribution and GBIF available occurrences
#* @get species/<scientific_name>/comparison-distri-GBIF
#* @param scientific_name:string Scientific Name
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, username) {
  
  Prom<-future({
    sf::sf_use_s2(FALSE)
    
    sRL_loginfo("START - Compare distribution and GBIF occurrences", scientific_name)
    
    ### Load distribution
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=0)
    distSP <- Storage_SP$distSP_saved
    
    ### Download GBIF data
    dat <- sRL_createDataGBIF(scientific_name, c(1,0,0), "", "")
    if(nrow(dat)==0){return(leaflet() %>% addControl(HTML("We did not find any GBIF record for this species"), position = "topleft", className="map-title"))}
    
    # Apply automated filters
    tryCatch({
      flags <- clean_coordinates(x = dat,
                                     lon = "decimalLongitude",
                                     lat = "decimalLatitude",
                                     countries = "countryCode",
                                     species = "species",
                                     capitals_rad = 1000,
                                     tests = c("capitals", "equal", "centroids", "gbif", "institutions", "zeros")
                                     )
    }, error=function(e){"Bug in clean coordinates"})
    if(exists("flags")==F){cat("Bug in clean coordinates"); flags<-dat ; flags$.val<-flags$.equ<-flags$.zer<-flags$.cap<-flags$.sea<-flags$.gbf<-flags$.inst<-flags$.cen<-TRUE}
    
    # Prepare and subset records
    flags <- sRL_cleanDataGBIF(flags, 1900, 10, F, F, "", -180, 180, -90, 90) %>% sRL_prepareDataComparison(., distSP)
    
    # Prepare result sentence
    Comparison_result <- paste0(round(100*length(which(flags$ValidDistri=="In"))/length(which(flags$ValidDistri %in% c("In", "Out")))), "% of valid GBIF occurrence records were found inside the current distribution")
    if(flags$Source_type[1]=="GBIF sample"){Comparison_result <- paste0(Comparison_result, " (based on an incomplete but geographically representative sample of ", nrow(flags), " occurrence records)")}
    
    ### Create Leaflet
    Leaflet_DistriComparison <- sRL_LeafletComparison(flags, distSP, Comparison_result)
    
    ### Assign in Storage_SP
    Storage_SP$data_comparison <- flags
    Storage_SP$comparison_result <- Comparison_result
    Storage_SP$Leaflet_comparison <- Leaflet_DistriComparison
    Storage_SP <- sRL_OutLog(Storage_SP, "Distri_comparison", "yes")
    sRL_StoreSave(scientific_name, username,  Storage_SP)
    
    sRL_loginfo("END - Compare distribution and GBIF occurrences", scientific_name)
    
    return(Leaflet_DistriComparison)
    
  }, gc=T, seed=T)
  
  return(Prom)
  
}



## Plot selected distribution attributes (1a and 1b) ----
#* @get species/<scientific_name>/distribution-attributes
#* @param scientific_name:string Scientific Name
#* @param presences:[int] presences (1, 2)
#* @param seasons:[int] seasons (1, 2)
#* @param origins:[int] origins (1, 2)
#* @serializer unboxedJSON
#* @tag sRedList1
function(scientific_name, username, presences = list(), seasons = list() , origins = list()) { # nolint
  
  Prom<-future({
    
    #Filter param
    sRL_loginfo("START - Prepare attributes", scientific_name)
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=0)
    distSP_full <- Storage_SP$distSP_saved

    ### Subset distribution
    distSP <- subset(distSP_full, # nolint
                       distSP_full$presence %in% c(as.character(presences)) &
                       distSP_full$seasonal %in% c(as.character(seasons)) &
                       distSP_full$origin %in% c(as.character(origins)))
    
    ### Colour distribution
    distSP <- sRL_ColourDistrib(distSP)
    
    ### Plot
    plot_dist<-ggplot() +
      geom_sf(data = Storage_SP$CountrySP_saved, fill="gray96", col="gray50") + # nolint
      geom_sf(data = distSP, fill = distSP$cols) +
      theme_void() +
      ggtitle("")

    ### Save the distribution in memory (after merging it)
    Storage_SP$distSP_selected <- sRL_MergeDistri(distSP) %>% st_transform(., CRSMOLL)
    Storage_SP<-sRL_OutLog(Storage_SP, c("Distribution_Presence", "Distribution_Seasonal", "Distribution_Origin"), c(paste0(presences, collapse=","), paste0(seasons, collapse=","), paste0(origins, collapse=",")))
    sRL_StoreSave(scientific_name, username,  Storage_SP)

    # Save plot for RMarkDown (and create repository)
    ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/plot_selected.png"), plot_dist, width=10, height=8)
    plot_distENC <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/plot_selected.png"), mime = "image/png")
    
    sRL_loginfo("END - Prepare attributes", scientific_name)
    
    # Return
    return(list(plot_selected=plot_distENC))
    
  }, gc=T, seed=T)
  
  return(Prom)
}






# Step 1b: Mapping ----------------------------------------------------------------
## 1: Download ----------------------------------------------------------------

#* Global Biodiversity Information Facility Step 1
#* @post species/<scientific_name>/gbif
#* @param scientific_name:string Scientific Name
#* @param Gbif_Source:[string] Gbif_Source
#* @parser multi
#* @parser csv
#* @param Uploaded_Records:file A file
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username, Gbif_Source=list(), Gbif_Synonym="", Gbif_Country="", Uploaded_Records="") {

Prom<-future({
  sf::sf_use_s2(FALSE)
  
  ### Print sessionInfo (to have it always accessible in the log)
  print("Session Info:")
  print(sessionInfo())
  
  ### Clean-string from user
  scientific_name <- sRL_decode(scientific_name)
  print(scientific_name)
  print(Gbif_Source)
  if(Gbif_Country=="Keep all countries"){Gbif_Country<-""} ; print(Gbif_Country)
  
  # Prepare synonyms
  if(Gbif_Synonym != ""){Gbif_Synonym <- Gbif_Synonym %>% gsub("  ", " ", .) %>% strsplit(., "[,;]+") %>% unlist(.) %>% ifelse(substr(., 1, 1)==" ", substr(., 2, 1000), .) %>% sRL_decode(.) %>% .[. != scientific_name]}
  print(Gbif_Synonym)
  
  # Uploaded Records if we uploaded data (it's a list with 1 element being the title of the uploaded csv file); I edit the csv if separator not good
  if(Uploaded_Records != ""){
    Uploaded_Records<-sRL_FormatUploadedRecords(Uploaded_Records, scientific_name, Gbif_Synonym)
    print(head(Uploaded_Records))
  }

  ### GBIF procedure
  sRL_loginfo("START - Create data", scientific_name)
  dat <- sRL_createDataGBIF(scientific_name, Gbif_Source, Gbif_Country, Uploaded_Records)
  
  
  ## If there are synonyms
  if(Gbif_Synonym[1] != ""){
    
    # Remove synonyms already in the downloaded data (dat)
    if("genericName" %in% names(dat) & "specificEpithet" %in% names(dat)){Gbif_Synonym <- subset(Gbif_Synonym, ! Gbif_Synonym %in% levels(as.factor(paste(dat$genericName, dat$specificEpithet, sep=" "))))}
    print(Gbif_Synonym)
    
    # Run again the data collection (in a tryCatch to avoid errors if the name does not exist)
    for(SY in 1:length(Gbif_Synonym)){
      tryCatch({
        dat_syn<-sRL_createDataGBIF(Gbif_Synonym[SY], Gbif_Source, Gbif_Country, "") # Same Source options as it can be useful for GBIF, OBIS but also Red List (eg species name was changed)
        dat_syn$species<-scientific_name
        dat_syn$Source_type=paste0("Synonyms_", dat_syn$Source_type)
        dat_syn<-subset(dat_syn, ! paste0(dat_syn$decimalLongitude, dat_syn$decimalLatitude) %in% paste0(dat$decimalLongitude, dat$decimalLatitude)) # Remove the synonym observations that are already at location of the focal species (to avoid duplicated observations, see for instance Cheilosia hercyniae and C. means)
        dat<-rbind.fill(dat, dat_syn)
      }, error=function(e){paste0("The synonym ", Gbif_Synonym[SY], " was not downloaded")})
    }
    
  }
  
  # If data is empty: error
  if(nrow(dat)==0){no_records()}
  
  ### Create storage folder if it does not exist
  dir.create(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots"), recursive=T)
  
  # ### Plot
  sRL_loginfo("START - Plot data", scientific_name)
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/plot_data.png"), (
    ggplot() +
      coord_fixed() +
      geom_sf(data=distCountries_light, colour = "gray86", fill = "gray80")+
      geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude, col=Source_type), size = 1) +
      scale_colour_brewer(type="qual", palette=2, name="Source")+
      ggtitle(paste(names(table(dat$Source_type)), table(dat$Source_type), sep=" (") %>% paste(., collapse="), ") %>% paste0("Raw geo-referenced observations (N=", nrow(dat), ") from: ", ., ")") )+
      sRLTheme_maps %+replace%   theme(legend.position="top")
    ), width=18, height=5.5) # nolint
  plot1 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/plot_data.png"), mime = "image/png")
  sRL_loginfo("END - Create data", scientific_name)
  sRL_loginfo("START - Clean coordinates", scientific_name)


  # Prepare countries (max between 1 and limits for cases with a single site)
  LIMS<-c(xmin=min(dat$decimalLongitude), xmax=max(dat$decimalLongitude), ymin=min(dat$decimalLatitude), ymax=max(dat$decimalLatitude))
  LIMS<-c(xmin=max(-179.9, LIMS["xmin"] - 0.1*max(1,abs(LIMS["xmin"]-LIMS["xmax"]))),   xmax=min(179.9, LIMS["xmax"] + 0.1*max(1,abs(LIMS["xmin"]-LIMS["xmax"]))),
          ymin=max(-89.9, LIMS["ymin"] - 0.1*max(1,abs(LIMS["ymin"]-LIMS["ymax"]))),    ymax=min(89.9, LIMS["ymax"] + 0.1*max(1,abs(LIMS["ymin"]-LIMS["ymax"])))) ; print(LIMS)
  CountrySP_WGS<-distCountries_WGS
  tryCatch({CountrySP_WGS<-st_crop(CountrySP_WGS, LIMS)}, error=function(e){"Bug in cropping country"})
  CountrySP_WGS$land<-"TRUE"
  sRL_loginfo("Country cropped", scientific_name)
  
  # Flag observations to remove
  Tests_to_run=c("capitals", "equal", "centroids", "gbif", "institutions", "zeros")

  tryCatch({
    ## Apply automatic filters
    sRL_loginfo("START - CoordinateCleaner", scientific_name)
    flags_raw <- clean_coordinates(x = dat,
                                 lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 countries = "countryCode",
                                 species = "species",
                                 capitals_rad = 1000,
                                 tests = Tests_to_run)
    sRL_loginfo("END - CoordinateCleaner", scientific_name)
    
    ## Apply sea/land filtering (not through CoordinateCleaner, huge delay introduced when they updated to remove rgeos)
    if(nrow(CountrySP_WGS)==0){
      flags_raw$.sea<-FALSE
    }else{
      dat_proj<-dat %>% st_as_sf(., coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84") %>%  st_transform(., st_crs(CountrySP_WGS)) 
      flags_raw$.sea<-st_join(dat_proj, CountrySP_WGS, join=st_intersects)$land %>% replace(., is.na(.), "FALSE") %>% as.logical(.)
    }
  }, error=function(e){"Bug in clean coordinates"})
  if(exists("flags_raw")==F){cat("Bug in clean coordinates"); flags_raw<-dat ; flags_raw$.val<-flags_raw$.equ<-flags_raw$.zer<-flags_raw$.cap<-flags_raw$.sea<-flags_raw$.gbf<-flags_raw$.inst<-flags_raw$.cen<-TRUE}
  sRL_loginfo("END - Clean coordinates", scientific_name)
  
  # Assign + count use of step 1
  sRL_loginfo("START - Save gbif output files", scientific_name)
  output_to_save<-sRL_InitLog(scientific_name, username, DisSource = "Created") ; output_to_save$Value[output_to_save$Parameter=="Gbif_Source"]<-c(ifelse(Gbif_Source[1]==1, "GBIF", ""), ifelse(Gbif_Source[2]==1, "OBIS", ""), ifelse(Gbif_Source[3]==1, "Red_List", ""), ifelse(is.null(nrow(Uploaded_Records)), "", "Uploaded")) %>% .[.!=""] %>% paste(., collapse=" + ")
  output_to_save$Count[output_to_save$Parameter=="Gbif_Source"]<-ifelse(file.exists(paste0("resources/AOH_stored/", gsub(" ", "_", sRL_decode(scientific_name)), "_", sRL_userdecode(username), "/Storage_SP.rds")), (sRL_StoreRead(scientific_name,  username, 1)$Output$Count[2]+1), 1)
  output_to_save$Value[output_to_save$Parameter=="Gbif_Synonyms"]<-ifelse(Gbif_Synonym=="", NA, paste(Gbif_Synonym, collapse="+"))
  Storage_SP<-list(flags_raw_saved=flags_raw, Creation=Sys.time(), Output=output_to_save)
  Storage_SP<-sRL_OutLog(Storage_SP, "Crop_Country", Gbif_Country)
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  sRL_loginfo("END - Save gbif output files", scientific_name)
  
  return(list(plot_data=plot1))
  
}, gc=T, seed=T)
  
return(Prom)
  
}


## 2: Filter ----------------------------------------------------------------
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
function(scientific_name, username, Gbif_Year= -1, Gbif_Uncertainty=-1, Gbif_Extent=list(), Gbif_Sea="", Gbif_automatedBin="", Gbif_yearBin="", Gbif_uncertainBin="") {

Prom<-future({
  sf::sf_use_s2(FALSE)
  
  sRL_loginfo("START - GBIF Step 2", scientific_name)


  ### Transform parameters GBIF filtering
  scientific_name <- sRL_decode(scientific_name)
  print(Gbif_automatedBin)
  print(Gbif_Year)
  if(is.na(Gbif_Uncertainty) | Gbif_Uncertainty==-1){Gbif_Uncertainty<-10000} ; print(Gbif_Uncertainty) # If no Gbif_Uncertainty provided, I put a high threshold so that it's never triggered
  Gbif_Extent<-as.numeric(Gbif_Extent) ; print(Gbif_Extent)
  print(Gbif_Sea)
  Gbif_yearBin<-Gbif_yearBin=="true" ; print(Gbif_yearBin)
  Gbif_uncertainBin<-Gbif_uncertainBin=="true" ; print(Gbif_uncertainBin)
  
  ### Charge downloaded data
  Storage_SP<-sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
  flags_raw<-Storage_SP$flags_raw_saved
  if(Gbif_automatedBin != "true"){flags_raw$.val<-flags_raw$.equ<-flags_raw$.zer<-flags_raw$.cap<-flags_raw$.cen<-flags_raw$.gbf<-flags_raw$.inst<-TRUE}
  
  ### Subset the observations user wants to keep (can be run several times if users play with parameters)
  flags <- sRL_cleanDataGBIF(flags_raw, as.numeric(Gbif_Year), as.numeric(Gbif_Uncertainty), Gbif_yearBin, Gbif_uncertainBin, Gbif_Sea, Gbif_Extent[1], Gbif_Extent[2], Gbif_Extent[3], Gbif_Extent[4])
  dat_proj=sRL_SubsetGbif(flags, scientific_name)

  ### Create Leaflet
  Leaflet_Filter <- sRL_LeafletFlags(flags)
  
  ### Assign in Storage_SP
  Storage_SP$dat_proj_saved<-dat_proj
  Storage_SP$flags<-flags
  Storage_SP<-sRL_OutLog(Storage_SP, c("Gbif_Year", "Gbif_Uncertainty", "Gbif_Sea", "Gbif_Extent", "Gbif_automatedBin", "Gbif_yearBin", "Gbif_uncertainBin"), c(Gbif_Year, Gbif_Uncertainty, Gbif_Sea, paste0(Gbif_Extent, collapse=","), Gbif_automatedBin=="true", Gbif_yearBin, Gbif_uncertainBin))
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  
  sRL_loginfo("END - GBIF Step 2", scientific_name)
  
  return(
    Leaflet_Filter
  )
  
}, gc=T, seed=T)

return(Prom)
  
}



#* Extract occurrence records elevation
#* @get species/<scientific_name>/extract-elevation
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username) {
  
  Prom<-future({
    
    scientific_name <- sRL_decode(scientific_name)
    
    Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1)
    flags<-Storage_SP$flags %>% subset(., is.na(Reason)==T)
    alt_raw<-sRL_ChargeAltRaster()
    
    # Extract elevation (need to transform the CRS for it)
    sRL_loginfo("START - Extracting elevation values", scientific_name)
    flags_foralt<-st_geometry(st_as_sf(flags, coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84")) %>%
     st_transform(., st_crs(CRSMOLL)) %>%
     st_as_sf(.)
    
    flags$Alt_points=terra::extract(alt_raw, st_coordinates(flags_foralt), method="simple")$Elevation_reprojMollweide3
    flags$Alt_points<-replace(flags$Alt_points, is.nan(flags$Alt_points)==T, NA)
    sRL_loginfo("END - Extracting elevation values", scientific_name)
    
    # Plot
    SUBTT<-ifelse(length(which(!is.na(flags$Alt_points)))>0,
                  paste0("Elevation ranges from ", trunc(min(flags$Alt_points[is.na(flags$Reason)==T], na.rm=T)), " to ", ceiling(max(flags$Alt_points[is.na(flags$Reason)==T], na.rm=T)), "m"),
                  "We were not able to calculate elevation for any of the occurrence records (maybe they are all at sea)")
    G_elev<-ggplot(flags)+
      geom_histogram(aes(x=Alt_points))+
      ggtitle(paste0("Elevation of valid observations (N=", nrow(flags[is.na(flags$Reason)==T,]), ")"))+
      xlab("Elevation (m)")+ylab("N")+
      labs(subtitle=SUBTT)+
      theme_minimal() + theme(plot.background=element_rect(fill="white"))
    
    # Save and return plot
    ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/gbifElevExtract.png"), G_elev, width=9, height=6) # nolint
    plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/gbifElevExtract.png"), mime = "image/png") # nolint
    
    Storage_SP<-sRL_OutLog(Storage_SP, "Gbif_Extract_Elevation", "Yes")
    sRL_StoreSave(scientific_name, username,  Storage_SP)
    
    return(list(
      plot_extract_elevation = plot
    ))
    
  }, gc=T, seed=T)
  
  return(Prom)
}  


## 3: Map ----------------------------------------------------------------
### Data APIs ------ 
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


### Map API ----
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
function(scientific_name, username, Gbif_Start="", Gbif_Param=list(), Gbif_Buffer=-1, Gbif_Altitude=list(), Gbif_Crop="", Gbif_RLDistBin="") {

# Parameter error
if(Gbif_Start=="alpha" & Gbif_Param[1] <= 0){neg_alpha()}
if(Gbif_Start=="kernel" & Gbif_Param[2] <= 0){neg_kernel()}
if(Gbif_Start=="coastal" & (Gbif_Buffer==0 | Gbif_Crop=="")){no_gbif_coastal()}
if(grepl("hydro", Gbif_Start) & (Gbif_Buffer>0 | Gbif_Crop!="" | Gbif_Altitude[1]!="0" | Gbif_Altitude[2]!="9000" | Gbif_RLDistBin=="true")){hydro_modified()}


# Check the Step 2 has been run since Step 1 was last updated  
scientific_name <- sRL_decode(scientific_name)
Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1)
if(! "dat_proj_saved" %in% names(Storage_SP)){run_Step2()}  

Prom<-future({
  sf::sf_use_s2(FALSE)

  # Transform parameters GBIF filtering
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
  dat_proj=Storage_SP$dat_proj_saved
  if(nrow(dat_proj)==0){no_gbif_data()}
  
  # Save leaflet for RMD report
  Storage_SP$Leaf_saved <- sRL_LeafletFlags(Storage_SP$flags)
  
  # Display some errors
  if(nrow(dat_proj)<=2 & Gbif_Start %in% c("mcp", "kernel", "alpha")){too_few_occurrences()}
  
  # Create distribution
  distSP_BeforeCrop <- sRL_MapDistributionGBIF(dat_proj, scientific_name, username,
                                  First_step=Gbif_Start,
                                  AltMIN=as.numeric(Gbif_Altitude[1]), AltMAX=as.numeric(Gbif_Altitude[2]),
                                  Buffer_km=as.numeric(Gbif_Buffer),
                                  Gbif_Param=Gbif_Param) 

  # Merge with published range map
  if(Gbif_RLDistBin==T){
    sRL_loginfo("Merge with Red List map", scientific_name)
    tryCatch({
      # Load distribution
      distRL0_path <- paste0(config$distribution_path, scientific_name, "/", sub(" ", "_", scientific_name), "_RL/", scientific_name, ".shp")
      distRL <- sRL_PrepareDistrib(st_read(distRL0_path), scientific_name) # nolint
      distRL <- subset(distRL, distRL$presence %in% c(1,2) & distRL$origin %in% c(1,2) & distRL$seasonal %in% c(1,2))
      
      # Merge with created range map
      distSPM<-st_union(distSP_BeforeCrop, distRL) %>% dplyr::group_by(binomial) %>% dplyr::summarise(N= n()) 
      distSP_BeforeCrop$geometry[1]<-distSPM$geometry[1]
    
    }, error=function(e){"Merging with published range map did not work"})
  }
  
  # Crop by land/sea
  distSP <- sRL_CropDistributionGBIF(distSP_BeforeCrop, Gbif_Crop)
  
  # Crop by country
  Crop_Country<-Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Crop_Country"]
  if(is.na(Crop_Country) == F & Crop_Country != ""){distSP <- sRL_CropCountry(distSP, Crop_Country, scientific_name)}

  # Map countries (keeping max extent between points and polygons)
  EXT_max <-  do.call(raster::bind, sapply(c(extent(distSP), extent(dat_proj)), FUN = function(x){as(x, 'SpatialPolygons')}))  %>% sp::bbox(.) %>% extent(.)
  
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
  
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/gbifStep3.png"), GPlot, width=18, height=5.5) # nolint
  plot3 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/gbifStep3.png"), mime = "image/png") # nolint
  sRL_loginfo("END - Maps the distribution", scientific_name)
  
  # Keep distribution in memory
  Storage_SP<-sRL_OutLog(Storage_SP, c("Mapping_Start", "Mapping_Crop", "Mapping_Buffer", "Mapping_Altitude", "Kernel_parameter", "Alpha_parameter", "Mapping_Merge"), c(Gbif_Start, Gbif_Crop, Gbif_Buffer, paste0(Gbif_Altitude, collapse=", "), ifelse(Gbif_Start=="kernel", Gbif_Param[2], NA), ifelse(Gbif_Start=="alpha", Gbif_Param[1], NA), Gbif_RLDistBin))
  distSP$dist_comm <-sRL_DistComment(Output=Storage_SP$Output, N_dat=nrow(dat_proj))
  Storage_SP$distSP_saved=distSP[, names(distSP) != "alphaTEMPO"]
  Storage_SP$distSP3_BeforeCrop <- distSP_BeforeCrop
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  
  # Save distribution in the platform
  gbif_path <- sRL_saveMapDistribution(scientific_name, Storage_SP)
  
  
  return(list(
    plot_CreatedDistri = plot3,
    gbif_data_number  = as.numeric(Storage_SP$gbif_number_saved),
    gbif_path = gbif_path
  ))
  
}, gc=T, seed=T)

return(Prom)
}


### Showcase -------
#* @get species/<scientific_name>/gbif3-showcase
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username) {
  
  Prom<-future({
    
    sf::sf_use_s2(FALSE)
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name, username, MANDAT=1)
    if(! "dat_proj_saved" %in% names(Storage_SP)){run_Step2()} 
    
    #GBIF STEP 3: Map distribution from GBIF
    sRL_loginfo("START - Map showcase", scientific_name)
    
    # Get back GBIF observations
    dat_proj=Storage_SP$dat_proj_saved
    if(nrow(dat_proj)==0){no_gbif_data()}
    
    # Map countries (keeping max extent between points and polygons)
    EXT_max <-  extent(dat_proj)
    CountrySP<-st_crop(distCountries, 1.2*EXT_max)
    
    # Calculate number of sites
    NSites <- paste(st_coordinates(dat_proj)[,1], st_coordinates(dat_proj)[,2], sep="/") %>% unique(.) %>% length(.) %>% as.numeric(.)
    
    ### Create distributions
    # MCP
    dist_mcp<-data.frame() ; tryCatch({dist_mcp<-sRL_MapDistributionGBIF(dat_proj, scientific_name, username, First_step="mcp", AltMIN=0, AltMAX=9000, Buffer_km=0, Gbif_Param=c(0,0))}, error=function(e){"Error in mapping showcase"})
    
    G_mcp<-ggplot()+geom_sf(data=CountrySP, fill="gray70")+geom_sf(data=dist_mcp, fill="darkred")+geom_sf(data=dat_proj)+sRLTheme_maps+ggtitle("Minimum Convex Polygon")
    
    # Individual sites
    if(NSites<=50){
      Buff_raw<- sqrt((EXT_max[2]-EXT_max[1])^2+(EXT_max[4]-EXT_max[3])^2)/20000 # The buffer will be a 20th of the diagonal and in km
      Buff <- Buff_raw %>% round(.) %>% round(., -1*(nchar(.)-1)) %>% max(., 1)
      dist_indiv<-data.frame() ; tryCatch({dist_indiv<-sRL_MapDistributionGBIF(dat_proj, scientific_name, username, First_step="indivsites", AltMIN=0, AltMAX=9000, Buffer_km=Buff, Gbif_Param=c(0,0))}, error=function(e){"Error in mapping showcase"})
      
      G_indiv<-ggplot()+geom_sf(data=CountrySP, fill="gray70")+geom_sf(data=dist_indiv, fill="darkred")+geom_sf(data=dat_proj)+sRLTheme_maps+ggtitle(paste0("Individual localities (buffer of ", Buff, "km)"))
    } else {G_indiv<-ggplot()+theme_void()}
    
    # Alpha
    alphaA=0.5
    alphaB=1
    alphaC=10
    
    dist_alphaA<-data.frame() ; tryCatch({dist_alphaA<-sRL_MapDistributionGBIF(dat_proj, scientific_name, username, First_step="alpha", AltMIN=0, AltMAX=9000, Buffer_km=0, Gbif_Param=c(alphaA,0))}, error=function(e){"Error in mapping showcase"})
    dist_alphaB<-data.frame() ; tryCatch({dist_alphaB<-sRL_MapDistributionGBIF(dat_proj, scientific_name, username, First_step="alpha", AltMIN=0, AltMAX=9000, Buffer_km=0, Gbif_Param=c(alphaB,0))}, error=function(e){"Error in mapping showcase"})
    dist_alphaC<-data.frame() ; tryCatch({dist_alphaC<-sRL_MapDistributionGBIF(dat_proj, scientific_name, username, First_step="alpha", AltMIN=0, AltMAX=9000, Buffer_km=0, Gbif_Param=c(alphaC,0))}, error=function(e){"Error in mapping showcase"})
    
    G_alphaA<-ggplot()+geom_sf(data=CountrySP, fill="gray70")+geom_sf(data=dist_alphaA, fill="darkred")+geom_sf(data=dat_proj)+sRLTheme_maps+ggtitle(paste0("Alpha hull (parameter = ", alphaA, ")"))
    G_alphaB<-ggplot()+geom_sf(data=CountrySP, fill="gray70")+geom_sf(data=dist_alphaB, fill="darkred")+geom_sf(data=dat_proj)+sRLTheme_maps+ggtitle(paste0("Alpha hull (parameter = ", alphaB, ")"))
    G_alphaC<-ggplot()+geom_sf(data=CountrySP, fill="gray70")+geom_sf(data=dist_alphaC, fill="darkred")+geom_sf(data=dat_proj)+sRLTheme_maps+ggtitle(paste0("Alpha hull (parameter = ", alphaC, ")"))
    
    
    # Kernel
    if(NSites>3){
      kernelA=0.7
      kernelB=0.9
      kernelC=0.99
      
      dist_kernelA<-data.frame() ; tryCatch({dist_kernelA<-sRL_MapDistributionGBIF(dat_proj, scientific_name, username, First_step="kernel", AltMIN=0, AltMAX=9000, Buffer_km=0, Gbif_Param=c(0,kernelA))}, error=function(e){"Error in mapping showcase"})
      dist_kernelB<-data.frame() ; tryCatch({dist_kernelB<-sRL_MapDistributionGBIF(dat_proj, scientific_name, username, First_step="kernel", AltMIN=0, AltMAX=9000, Buffer_km=0, Gbif_Param=c(0,kernelB))}, error=function(e){"Error in mapping showcase"})
      dist_kernelC<-data.frame() ; tryCatch({dist_kernelC<-sRL_MapDistributionGBIF(dat_proj, scientific_name, username, First_step="kernel", AltMIN=0, AltMAX=9000, Buffer_km=0, Gbif_Param=c(0,kernelC))}, error=function(e){"Error in mapping showcase"})
      
      G_kernelA<-ggplot()+geom_sf(data=CountrySP, fill="gray70")+geom_sf(data=dist_kernelA, fill="darkred")+geom_sf(data=dat_proj)+sRLTheme_maps+ggtitle(paste0("Kernel (parameter = ", kernelA, ")"))
      G_kernelB<-ggplot()+geom_sf(data=CountrySP, fill="gray70")+geom_sf(data=dist_kernelB, fill="darkred")+geom_sf(data=dat_proj)+sRLTheme_maps+ggtitle(paste0("Kernel (parameter = ", kernelB, ")"))
      G_kernelC<-ggplot()+geom_sf(data=CountrySP, fill="gray70")+geom_sf(data=dist_kernelC, fill="darkred")+geom_sf(data=dat_proj)+sRLTheme_maps+ggtitle(paste0("Kernel (parameter = ", kernelC, ")"))
    } else {G_kernelA<-G_kernelB<-G_kernelC<-ggplot()+theme_void()}
    
    ### Save plot
    # Calculate ratio (in order to map with the right dimensions)
    ratio<-(EXT_max[1]-EXT_max[2])/(EXT_max[3]-EXT_max[4])
    ratio<-ifelse(is.na(ratio), 1, ifelse(ratio<0.7, 0.7, ifelse(ratio>1.6, 1.6, ratio)))

    # Save
    G_total<-cowplot::plot_grid(G_mcp, G_indiv, ggplot()+theme_void(), G_alphaA, G_alphaB, G_alphaC, G_kernelA, G_kernelB, G_kernelC, ncol=3)
    ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/gbifStep3showcase.png"), G_total, width=min(15, 15*ratio), height=min(15, 15/ratio)) # nolint
    plot3showcase <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/gbifStep3showcase.png"), mime = "image/png") # nolint
    sRL_loginfo("END - Map showcase", scientific_name)
    
    
    gc()
    return(list(
      plot_showcase = plot3showcase
    ))
    
  }, gc=T, seed=T)
  
  gc()
  return(Prom)
}





# Step 2: Countries of Occurrence ----------------------------------------------------------------

#* Countries of occurrence
#* @get species/<scientific_name>/analysis/coo
#* @param scientific_name:string Scientific Name
#* @param domain_pref:[str] domain_pref
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, username, domain_pref=list()) {

Prom<-future({
  sf::sf_use_s2(FALSE)

  # Filter parameters
  scientific_name<-sRL_decode(scientific_name)
  Storage_SP<-sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
  rownames(coo_raw)<-1:nrow(coo_raw)
  distSP<-Storage_SP$distSP_saved ; if(nrow(distSP)==0){return(leaflet() %>% addControl("<center><font size='6' color='#ad180d'><b>The distribution is empty, go back to Step 1</b></center>", position = "topleft", className="map-title"))}
  domain_pref<-revalue(as.character(domain_pref), c("1"="Terrestrial", "2"="Marine", "3"="Freshwater"), warn_missing = F)
  print(domain_pref)
  Storage_SP<-sRL_OutLog(Storage_SP, "System_pref", paste(domain_pref, collapse="|"))
  
  # Prepare distribution and calculate COO
  distSP_WGS<-distSP %>% st_transform(., st_crs(coo_raw)) 
  coo<-sRL_cooExtract(distSP_WGS, domain_pref, Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Crop_Country"])
  distSP_WGS <- distSP_WGS %>% dplyr::group_by(origin, presence, seasonal) %>% dplyr::summarise(N= n())
  
  # Simplify distribution if large distribution
  if((extent(distSP_WGS)@xmax-extent(distSP_WGS)@xmin)>50){distSP_WGS<-st_simplify(distSP_WGS, dTolerance=0.05)}
  
  # Assign colours
  coo$colour<-paste0(coo$Domain, coo$Level0_occupied, coo$Level1_occupied) 
  coo$colour<-sRL_COOColours$Col[match(coo$colour, sRL_COOColours$Code)]
  
  # Extract realms
  Realms<-st_join(distSP_WGS, realms_raw, join=st_intersects)$realm %>% unique(.) %>% paste0(., collapse="|")
  print(Realms)
  
  # Prepare command for results button
  coo_occ<-sRL_cooInfoBox_prepare(coo, Storage_SP)
  coo_res<-sRL_cooInfoBox_format(coo_occ, Storage_SP)
  info.box<-sRL_cooInfoBox_create(coo_res, Realms)
  
  # Create plot (first the one to export without the result - it makes the rmarkdown bug and it's not needed - and second adding the text results)
  Leaflet_COOtoexport <- sRL_LeafCountry(coo, distSP_WGS, realms_raw, Storage_SP)

  Leaflet_COO<-Leaflet_COOtoexport %>%
    leaflet.extras::addBootstrapDependency() %>% # Add Bootstrap to be able to use a modal
    addEasyButton(easyButton(
      icon = "fa fa-list-ul", 
      title = "Check out the list of countries of occurrence",
      onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
    )) %>% # Trigger the infobox
    htmlwidgets::appendContent(info.box)  
  

  # Save for SIS
  Storage_SP$Realms_saved<-Realms
  Storage_SP$coo<-coo
  Storage_SP$coo_occ<-coo_occ
  Storage_SP$Leaflet_COO<-Leaflet_COOtoexport
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  
  
  # Plot
  return(Leaflet_COO)

}, gc=T, seed=T)

return(Prom)

}









# Step 3: EOO ----------------------------------------------------------------

## Calculation -----
#* Estimate the Extent of Occurrence (EOO) from range
#* @get species/<scientific_name>/analysis/eoo
#* @param scientific_name:string Scientific Name
#* @param presences:[int] presences (1, 2)
#* @param seasons:[int] seasons (1, 2)
#* @param origins:[int] origins (1, 2)
#* @param Dist_path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username, presences = list(), seasons = list() , origins = list(), Dist_path = "") { # nolint

Prom<-future({
  sf::sf_use_s2(FALSE)
    
  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))

  # Load distribution
  distSP<-Storage_SP$distSP_selected ; if(nrow(distSP)==0){empty_distrib()}

  # Create storage directory if it does not exist
  dir.create(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots"), recursive=T)

  ### Plot EOO
  sRL_loginfo("START - Plot EOO \n", scientific_name)
  EOO <- st_as_sf(st_convex_hull(st_union(distSP))) ## Needed to avoid having different sub-polygons
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/plot_eoo.png"), 
    ggplot() +
      geom_sf(data=distSP, fill="#fcbba1", col=NA) +
      geom_sf(data=EOO, col="#ef3b2c", fill=NA, lwd=2) +
      geom_sf(data=st_crop(Storage_SP$CountrySP_saved, 1.2*extent(EOO)), fill=NA, col="black")+
      ggtitle("EOO map") +
      sRLTheme_maps, 
    width=6, height=6) # nolint
  plot3 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/plot_eoo.png"), mime = "image/png") # nolint
  sRL_loginfo("END - Plot EOO \n", scientific_name)

  ### Calculate EOO area (it cannot be lower than 4km2 so I keep the max between both); I transform to WGS84 for a more accurate area calculation (following Rewild comment)
  EOO_km2 <- round(as.numeric(st_area(st_transform(EOO, st_crs(4326))))/1000000) %>% max(c(., 4), na.rm=T)
  
  ### Save EOO area
  Storage_SP$eoo_km2<-EOO_km2
  sRL_StoreSave(scientific_name, username,  Storage_SP)

  return(list(
    eoo_km2 = EOO_km2,
    plot_eoo = plot3
  ))
  
}, gc=T, seed=T)

return(Prom)
}


## Leaflet -----
#* EOO Leaflet
#* @get species/<scientific_name>/analysis/eoo_leaflet
#* @param scientific_name:string Scientific Name
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, username) { # nolint
  
  Prom<-future({
    sf::sf_use_s2(FALSE)
    
    #Filter param
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
    
    # Load distribution and EOO
    distSP<-Storage_SP$distSP_selected %>% st_transform(st_crs(4326))
    EOO <- st_as_sf(st_convex_hull(st_union(distSP))) ## Needed to avoid having different sub-polygons
    
    ### Plot EOO
    EOO_leaflet<-leaflet() %>%
      addTiles(group="OpenStreetMap") %>%
      addEsriBasemapLayer(esriBasemapLayers$Imagery, group = "Satellite") %>%
      addEsriBasemapLayer(esriBasemapLayers$Topographic, group = "Topography") %>%
      addPolygons(data=distSP, color="#D69F32", fillOpacity=0.5, group="Distribution") %>% 
      addPolygons(data=EOO, color="#ef3b2c", fillOpacity=0, group="EOO") %>% 
      addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), overlayGroups=c("Distribution", "EOO"), position="topleft") %>% 
      addMouseCoordinates() 
    
    ### Store usage
    Storage_SP<-sRL_OutLog(Storage_SP, "EOO_leaflet", "Used")
    Storage_SP$EOO_leaflet<-EOO_leaflet
    sRL_StoreSave(scientific_name, username,  Storage_SP)
    
    return(EOO_leaflet)
    
  }, gc=T, seed=T)
  
  return(Prom)
}




# Step 4: AOH ----------------------------------------------------------------
## a: Data APIs ----------------------------------------------------------------


#* Load species density preferences
#* @get species/<scientific_name>/density-preferences
#* @param scientific_name:string Scientific Name
#* @serializer json
#* @tag sRedList
function(scientific_name) {
  
  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  
  density_estimate = density_data$Density[density_data$Species == scientific_name] %>% round(., 2)
  if(length(density_estimate)>1){density_estimate<-mean(density_estimate, na.rm=T)}
  
  # Load source of density
  SRC <- ifelse(is.na(density_estimate), 
                        "We did not find density estimate to suggest.",
                        paste0("Density estimate extracted from ", density_data$Source[density_data$Species == scientific_name][1])
                        )
  
  return(list(
    raw_density=as.character(density_estimate),
    density_src=SRC,
    perc_mature=as.character(100),
    perc_suitable=as.character(100)
  ));
}


#* Species density calculation
#* @get species/<scientific_name>/density-calculation
#* @param scientific_name:string Scientific Name
#* @param CALCdensity:string CALCdensity
#* @param CALCperc_mature:string CALCperc_mature
#* @param CALCperc_suitable:string CALCperc_suitable
#* @serializer json
#* @tag sRedList
function(scientific_name, username, CALCdensity, CALCperc_mature, CALCperc_suitable) {

  # Prepare variables
  CALCdensity <- sRL_UncertToVector(CALCdensity)
  CALCperc_mature <- sRL_UncertToVector(CALCperc_mature)
  CALCperc_suitable <- sRL_UncertToVector(CALCperc_suitable)
  
  # Error if invalid percentage (not betweeen 0 and 100)
  if(max(CALCperc_mature)>100 | min(CALCperc_mature)<0 | max(CALCperc_suitable)>100 | min(CALCperc_suitable)<0){wrong_percentages()}
  
  # Calculate
  final_density <- as.numeric(CALCdensity) * 0.01*as.numeric(CALCperc_mature) * 0.01*as.numeric(CALCperc_suitable)
  final_density <- as.character(final_density)
  
  # Merge if uncertainty
  if(length(final_density)>1){final_density<-paste0(final_density, collapse="-")}
  
  # Record usage
  Storage_SP <- sRL_StoreRead(sRL_decode(scientific_name), username, MANDAT=1)
  Storage_SP <- sRL_OutLog(Storage_SP, "Density_Calculator", paste(paste(CALCdensity, collapse="-"), paste0(CALCperc_mature, collapse="-"), paste0(CALCperc_suitable, collapse="-"), final_density, sep=" / "))
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  
  # Return
  return(list(
    final_density=final_density
  ))
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
  
  # Get GL source
  SRC <- ifelse(scientific_name %in% GL_file$internal_taxon_name, paste0("A generation length was found in ", GL_file$Source[GL_file$internal_taxon_name==scientific_name][1]), "default")
  
  
  return(list(
    GL_species = as.character(GL_species),
    GL_src = SRC
    ))
}



#* Extract occurrence records habitats
#* @get species/<scientific_name>/extract-habitats
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username) {
  
  Prom<-future({
    
    sf::sf_use_s2(FALSE)
    sRL_loginfo("START - Calculate habitat preferences", scientific_name)
    
    #Filter param
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
    dat_proj=Storage_SP$dat_proj_saved %>% st_transform(st_crs(4326))
    
    # Prepare Jung habitat files
    Jung_list<-list.files(config$jung_map_path) %>% subset(., grepl(".tif.aux.xml", .)==F)
    
    # Extract habitats
    df_hab<-data.frame(Habitat=NA, Frac=NA)
    for(i in 1:length(Jung_list)){
      
      # Load raster
      jung_i<-rast(paste0(config$jung_map_path, "/", Jung_list[i]))
      
      # Extract name
      Name_raw<-Jung_list[[i]] %>% substr(43,46) %>% gsub("_", "", .)
      Name_i<-ifelse(nchar(Name_raw)==3, 
                     paste(substr(Name_raw, 1, 1), as.numeric(substr(Name_raw, 2, 3)), sep="."), 
                     paste(substr(Name_raw, 1, 2), as.numeric(substr(Name_raw, 4, 4)), sep="."))
      
      # Calculate fraction
      df_i<-data.frame(
        Habitat=Name_i,
        Frac=terra::extract(jung_i, st_coordinates(dat_proj), method="simple")[,1]
      )
      
      # Add in table
      df_hab<-rbind(df_hab, df_i)
    }
    
    df_hab<-subset(df_hab, is.na(Habitat)==F)
    df_hab$Frac<-replace(df_hab$Frac, is.na(df_hab$Frac), 0)
    
    # Group stats
    hab_stats<-ddply(df_hab, .(Habitat), function(x){data.frame(Mean=mean(x$Frac, na.rm=T))})
    
    # Prepare data frame
    hab_stats<-subset(hab_stats, Mean>0) %>% .[order(.$Mean, decreasing=T),]
    hab_stats$Habitat<-factor(hab_stats$Habitat, levels=hab_stats$Habitat)
    hab_stats$Mean<-hab_stats$Mean/sum(hab_stats$Mean, na.rm=T)*100
    
    # Barplot
    SUBTT<-ifelse(nrow(hab_stats)>0,
                  "",
                  "We were not able to extract habitat for any of the occurrence records (maybe they are all at sea)")
    G_habs<-ggplot(hab_stats)+
      geom_bar(aes(x=Habitat, y=Mean), stat="identity")+
      ylab("Percent presence")+
      labs(subtitle=SUBTT)+
      theme_minimal() + theme(plot.background=element_rect(fill="white"))
    
    # Save and return plot
    ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/gbifHabExtract.png"), G_habs, width=9, height=6) # nolint
    plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/gbifHabExtract.png"), mime = "image/png") # nolint
    
    Storage_SP<-sRL_OutLog(Storage_SP, "Gbif_Extract_Habitat", "Yes")
    sRL_StoreSave(scientific_name, username,  Storage_SP)
    
    return(list(
      plot_extract_habitat = plot
    ))
    
  }, gc=T, seed=T)
  
  return(Prom)
}  



## b: Current AOH ----------------------------------------------------------------

### Calculation ---- 
#* Calculate Area of Habitat (AOH)
#* @get species/<scientific_name>/analysis/aoh
#* @param scientific_name:string Scientific Name
#* @param habitats_pref:[str] habitats_pref
#* @param habitats_pref_MARGINAL:[str] habitats_pref_MARGINAL
#* @param altitudes_pref:[int] altitudes_pref
#* @param density_pref:string density_pref
#* @param isGbifDistribution:boolean isGbifDistribution
#* @param Dist_path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username, habitats_pref= list(), habitats_pref_MARGINAL=list(), altitudes_pref= c("0","9000"), density_pref= '-1', isGbifDistribution = FALSE, Dist_path = "") { # nolint    

# If no habitat preference or habitats not in crosswalk, return error
if(length(habitats_pref)==0){no_habitat_pref()}
if(!"TRUE" %in% (habitats_pref %in% crosswalk_to_use$code)){no_habitats_crosswalk()}

# Check elevation
if(length(altitudes_pref)==0){altitudes_pref<-c("0","9000")}
TestALT1<-altitudes_pref[1] %>% strsplit(., "-") %>% unlist(.) %>% as.numeric(.)
TestALT2<-altitudes_pref[2] %>% strsplit(., "-") %>% unlist(.) %>% as.numeric(.)
if(is.na(sum(TestALT1)) | is.na(sum(TestALT2)) | length(TestALT1)>2 | length(TestALT2)>2){elev_not_valid()} # Could be NA if text inside, could be length > 2 if two hyphens
if(max(TestALT1)>min(TestALT2) | is.unsorted(TestALT1) | is.unsorted(TestALT2)){elev_decreasing()}

# Check density
if(density_pref!='-1' & is.na(sum(as.numeric(unlist(strsplit(density_pref, "-")))))){wrong_density()}

# Clean memory
Prom_clean<-future({
  sRL_loginfo("START - Cleaning memory", scientific_name)
  sRL_cleaningMemory(Time_limit=180)
  sRL_loginfo("END - Cleaning memory", scientific_name)
}, gc=T, seed=T)
Prom_clean %...>% print(.)


Prom<-future({
  sf::sf_use_s2(FALSE)
  sRL_loginfo("START - AOH API", scientific_name)
  TIC<-Sys.time()
  
  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
  distSP=Storage_SP$distSP_selected ; if(nrow(distSP)==0){empty_distrib()}

  # Habitat table (for aoh analysis and for SIS Connect)
  habitats_pref_DF<-sRL_PrepareHabitatFile(scientific_name, habitats_pref, habitats_pref_MARGINAL)
  print(habitats_pref_DF)
  Storage_SP$habitats_SIS=habitats_pref_DF

  # Altitude table (for aoh analysis and for SIS Connect)
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
  names(distSP)<-replace(names(distSP), names(distSP)=="terrestial", "terrestrial") ; distSP$terrestrial<-"true" # I have to make it manually so that it does not exclude pure freshwater species (it will be users choice in any case)
  
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
  output_dir<-paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username))
  dir.create(paste0(output_dir, "/Current"), recursive=T)
  dir.create(paste0(output_dir, "/Current_optimistic"), recursive=T)
  dir.create(paste0(output_dir, "/Temporary"))
  do.call(file.remove, list.files(output_dir, full.names = TRUE, recursive=T) %>% .[! (grepl("Storage_SP", .) | grepl("Plots/", .))] %>% list(.)) # Remove all files but Storage_SP and plots (needed for Rmarkdown)
  terraOptions(tempdir=paste0(output_dir, "/Temporary"), memmax=config$RAMmax_GB)
  rasterOptions(tmpdir=paste0(output_dir, "/Temporary"), maxmemory=config$RAMmax_GB)

  ### SMALL-RANGES ----- (if range is smaller than Size_LargeRange and the box around it is not more than 10 times bigger)
  Range_size<-as.numeric(st_area(rangeSP_clean))/10^6 ; print(Range_size)
  Range_bbox<-as.numeric(st_area(st_as_sfc(st_bbox(rangeSP_clean))))/10^6
  AOH_type<-ifelse(((Range_size < as.numeric(config$Size_LargeRange)) & (Range_bbox < (10*as.numeric(config$Size_LargeRange)))), "Small", "Large") ; print(AOH_type)
  if(AOH_type=="Small"){

    sRL_loginfo("START - Small: Cropping rasters", scientific_name)
    alt_raw<-sRL_ChargeAltRaster()
    alt_crop=crop(alt_raw, extent(distSP), snap="out")
    writeRaster(alt_crop, paste0(output_dir, "/alt_crop.tif"))
    cci2<-sRL_ChargeCci2Raster()
    cci2_crop<-crop(cci2, extent(distSP), snap="out")
    writeRaster(cci2_crop, paste0(output_dir, "/cci2_crop.tif"), overwrite=T)

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
            terra::writeRaster(rast(AOH2_opt), paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Current_optimistic/Optimistic_identical.tif"))
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
        scale_fill_manual(values=c("#FBCB3C", "#25BC5A", NA), labels=c("Unsuitable", "Suitable", ""), name="", na.translate=F, drop=F) +
        ggtitle("Area of Habitat in 2020") +
        sRLTheme_maps
    }

    # Both scenario
    if(Uncertain=="Uncertain_yes"){

      plot1<-gplot(AOH2[[1]]+AOH2_opt[[1]])+
        coord_fixed()+
        geom_tile(aes(fill = factor(value, levels=c("0", "1", "2")))) +
        scale_fill_manual(values=c("#FBCB3C", "#ADEFBA", "#25BC5A", NA), labels=c("Unsuitable", "Unknown", "Suitable", ""), name="", na.translate=F, drop=F) +
        ggtitle(paste0("Area of Habitat in ", config$YearAOH2)) +
        sRLTheme_maps
    }


    ### LARGE-RANGES --------
  } else {

    # Prepare altitude raster
    sRL_loginfo("START - Large altitude crop", scientific_name)
    alt_large<-raster(paste0(config$cciStack2_path, "/ElevationAgg30.tif"))
    alt_crop<-crop(alt_large, extent(rangeSP_clean), snap="out")
    writeRaster(alt_crop, paste0(output_dir, "/alt_crop.tif"), overwrite=T)
    sRL_loginfo("END - Large altitude crop", scientific_name)

    # Calculate AOH
    AOH2<-sRL_largeAOH(alt_crop, habitats_pref, altitudes_pref_DF[, c("elevation_lower", "elevation_upper")], rangeSP_clean, config$YearAOH2, paste0(output_dir, "/Current/aoh.tif"))

    if(Uncertain=="Uncertain_no"){
      plot1 <- gplot((AOH2[[1]]/9)) + # Divide by 9 to get percents
        coord_fixed()+
        geom_tile(aes(fill = value)) +
        scale_fill_gradient(low="#FBCB3C", high="#25BC5A", name="Suitability (%)", limits=c(0,100), na.value=NA)+
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
          terra::writeRaster(AOH2_opt, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Current_optimistic/Optimistic_identical.tif"))
          
        } else{

          AOH2_opt<-sRL_largeAOH(alt_crop, c(habitats_pref, habitats_pref_MARGINAL), alt_pref_extreme, rangeSP_clean, config$YearAOH2, FILENAME=paste0(output_dir, "/Current_optimistic/aoh.tif"))
          sRL_loginfo("Maximum AOH calculated", scientific_name)
      }

      plot1 <- cowplot::plot_grid(
        gplot((AOH2[[1]]/9)) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient(low="#FBCB3C", high="#25BC5A", name="Suitability (%)", limits=c(0,100), na.value=NA)+
          ggtitle(paste0("Minimum AOH in ", config$YearAOH2)) +
          labs(subtitle= "(marginal or unknown habitats / extreme elevations excluded)") +
          sRLTheme_maps,

        gplot((AOH2_opt[[1]]/9)) + # Divide by 9 to get percents
          coord_fixed()+
          geom_tile(aes(fill = value)) +
          scale_fill_gradient(low="#FBCB3C", high="#25BC5A", name="Suitability (%)", limits=c(0,100), na.value=NA)+
          ggtitle(paste0("Maximum AOH in ", config$YearAOH2)) +
          labs(subtitle= "(marginal or unknown habitats / extreme elevations included)") +
          sRLTheme_maps,

        ncol=1)
    }

  }

  Storage_SP$AOH_type<-AOH_type
  Storage_SP$Range_size<-Range_size

  # Plot AOH and calculate area
  sRL_loginfo("START - Plot AOH", scientific_name)

  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/aoh.png"), plot = plot1, width=9, height=ifelse(Uncertain=="Uncertain_no" | AOH_type=="Small", 9, 15))
  plot1 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/aoh.png"), mime = "image/png", encoding = "base64") # nolint
  sRL_loginfo("END - Plot AOH", scientific_name)

  AOH_km2 <-  sRL_areaAOH(AOH2[[1]], "cci") # Same scale in small or large AOH because the unit is always 1 cell of the fine raster
  if(Uncertain=="Uncertain_yes"){AOH_km2_opt <-  sRL_areaAOH(AOH2_opt[[1]], "cci") ;  Storage_SP$AOHkm2OPT_saved<-AOH_km2_opt}
  Storage_SP$AOHkm2_saved<-AOH_km2
  
  ### Upper AOO ----
  sRL_loginfo("START - Calculate Upper AOO", scientific_name)
  grid22<-sRL_ChargeGrid22Raster()
  grid22_crop<-crop(grid22, AOH2[[1]], snap="out")
  aoh_22<-terra::resample(AOH2[[1]], grid22_crop, method="max")>0
  sRL_loginfo("END - Calculate Upper AOO", scientific_name)

  if(Uncertain=="Uncertain_no"){
    plot2 <- cowplot::plot_grid(gplot(aoh_22[[1]]>0) +
      coord_fixed()+
      geom_tile(aes(fill = factor(as.character(as.numeric(value)), levels=c("0", "1")))) +
      scale_fill_manual(values=c("#FBCB3C", "#25BC5A"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
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
        scale_fill_manual(values=c("#FBCB3C", "#ADEFBA", "#25BC5A", NA), labels=c("Unsuitable", "Unknown", "Suitable", ""), name="", na.translate=F, drop=F) +
        ggtitle("") +
        sRLTheme_maps

    } else{
    plot2 <- cowplot::plot_grid(
      gplot(aoh_22[[1]]>0) +
        coord_fixed()+
        geom_tile(aes(fill = factor(as.character(as.numeric(value)), c("0", "1")))) +
        scale_fill_manual(values=c("#FBCB3C", "#25BC5A"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
        labs(title="Likely slightly overestimated (using a 10x10km aggregate raster) \n\n Minimum AOH at 2x2km scale", subtitle= "(marginal or unknown habitats / extreme elevations excluded)")+
        sRLTheme_maps,

      gplot(aoh_22_opt[[1]]>0) +
        coord_fixed()+
        geom_tile(aes(fill = factor(as.character(as.numeric(value)), c("0", "1")))) +
        scale_fill_manual(values=c("#FBCB3C", "#25BC5A"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F, drop=F) +
        labs(title="Maximum AOH at 2x2km scale", subtitle= "(marginal or unknown habitats / extreme elevations included)")+
        sRLTheme_maps,

      ncol=1)
  }
  }
  
  sRL_loginfo("START - Saving plot AOO", scientific_name)
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/aoo.png"), plot = plot2, width=9, height=ifelse(Uncertain=="Uncertain_no" | AOH_type=="Small", 9, 15))

  plot2 <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/aoo.png"), mime = "image/png", encoding = "base64") # nolint

  AOO_km2<- sRL_areaAOH(aoh_22[[1]], SCALE="2x2") ; if(AOH_km2==0){AOO_km2<-0}
  if(Uncertain=="Uncertain_yes"){AOO_km2_opt<- sRL_areaAOH(aoh_22_opt[[1]], SCALE="2x2") ; if(AOH_km2_opt==0){AOO_km2_opt<-0} ; Storage_SP$aoo_km2_opt<-AOO_km2_opt}
  if (density_pref[1] != '-1') {Storage_SP$density_saved<-density_pref}
  Storage_SP$aoo_km2<-AOO_km2
  sRL_loginfo("END - Saving plot AOO", scientific_name)
  

  ### Save parameters and results
  density_default<-ifelse(scientific_name %in% density_data$Species, base::mean(round(as.numeric(density_data$Density[density_data$Species == scientific_name]), 2), na.rm=T), NA) ; print(density_default)
  Storage_SP<-sRL_OutLog(Storage_SP, c("AOH_HabitatPreference", "AOH_MarginalHabitatPreference", "AOH_ElevationPreference", "AOH_Density", "Original_density", "AOH_RangeType", "Estimated_AOH22"), c(paste0(habitats_pref, collapse=","), paste0(habitats_pref_MARGINAL, collapse=","), paste0(altitudes_pref, collapse=", "), ifelse(density_pref[1]=='-1', NA, density_pref[1]), density_default, AOH_type, AOO_km2))
  
  terraOptions(tempdir=tempdir())
  rasterOptions(tmpdir=tempdir())
  gc()

  ### Population size (with uncertainty to due uncertainty in AOH and in density estimate) -------
  sRL_loginfo("START - Calcualte population size", scientific_name)
  if (density_pref[1] != '-1') {
    density_pref <- unlist(strsplit(as.character(density_pref), "-")) %>% as.numeric(.) ; print(density_pref) # Density_pref has one value if certain, 2 values otherwise

    if(Uncertain=="Uncertain_no"){pop_size <- paste(round(AOH_km2 * density_pref), collapse="-")} # Multiplies AOH by both density estimates (or one if only one available)
    if(Uncertain=="Uncertain_yes"){pop_size <- paste(c(round(AOH_km2 * min(density_pref)), round(AOH_km2_opt * max(density_pref))), collapse="-")} # Multiplies pessimistic aoh by pessimistic density, optimistic AOH by optimistic density (or a single density if only one provided)
    print(pop_size)
    Storage_SP$pop_size<-pop_size
  }
  sRL_loginfo("END - Calcualte population size", scientific_name)
  
  
  ### AOO based on occurrence records -------
  if("dat_proj_saved" %in% names(Storage_SP)){
    sRL_loginfo("START - AOO calculation from points", scientific_name)
    
    # Prepare grid 22
    dat_proj=Storage_SP$dat_proj_saved
    grid22<-raster("resources/EmptyGrid2x2/Empty.grid.2x2.Mollweide.tif") # Keep as raster (not terra)
    grid_crop<-crop(grid22, (extent(dat_proj)+c(-1,1,-1,1)), snap="out") # I expand a little bit the extent, otherwise it bugs with single points
    
    # Map AOO
    pts<-dat_proj %>% as_Spatial() %>% as(., 'SpatialPoints')
    AOO_pts <- terra::rasterize(pts, grid_crop, fun='count')>=1
    writeRaster(AOO_pts, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/AOO_known.tif"), overwrite=T)
    
    # Calculate AOO
    aoo_pts_km2<-sum(as.vector(AOO_pts), na.rm=T)*4
    print(paste0("AOO from points: ", aoo_pts_km2))
    Storage_SP$aoo_points<-aoo_pts_km2
    
    # Plot (if a single cell, I transform to vector, otherwise the cell appears empty)
    aooDF<-as.data.frame(AOO_pts, xy = TRUE); names(aooDF)[3]<-"lyr1"
    
    if(nrow(aooDF)==1){
      Plot_AOOpoints<-ggplot() + 
        geom_sf(data = st_as_sf(as.polygons(rast(AOO_pts))), aes(fill = as.character(layer)), col=NA)+
        scale_fill_manual(values=c("#25BC5A"), labels="Occupied cell", name="", na.translate=F)
    } else {
      Plot_AOOpoints<-ggplot() + 
        geom_tile(data = aooDF, aes(x = x, y = y, fill = as.character(lyr1)), col=NA)+
        scale_fill_manual(values=c("#25BC5A"), labels="Occupied cell", name="", na.translate=F)
    }
    
    Plot_AOOpoints<-Plot_AOOpoints+
      geom_sf(data=distSP, aes(col=as.character("Distribution")), fill=NA, lwd=2, show.legend = "line")+
      geom_sf(data=dat_proj, aes(size="1"), fill=NA, col="black", shape=1)+
      scale_colour_manual(values=c("#EF9884"), name="")+
      scale_size_manual(values=1.6, name="", labels="Occurrences")+
      labs(caption=ifelse(Range_size<5000, "", "\n You might not see the green cells because they are too small, \n but the calculation worked correctly"))+
      sRLTheme_maps+guides(fill = guide_legend(override.aes = list(col="#25BC5A")), size=guide_legend(override.aes = list(linetype=0, shape=1, size=1.6)))+
      theme(legend.position = "bottom", plot.caption=element_text(size=8.5))
    ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/aoo_from_points.png"), plot = Plot_AOOpoints, width=6, height=6)
    plot3<-base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/aoo_from_points.png"), mime = "image/png")

    sRL_loginfo("END - AOO calculation from points", scientific_name)
  } else {pts<-NULL}
  
  # Calculate model and point prevalence (AOH validation)
  if(Uncertain=="Uncertain_no"){
    Validation_Prevalence <- sRL_CalcAohPrevalence(aoh=AOH2[[1]], aoh_opt=NULL, type=AOH_type, points=pts)
  } else {
    Validation_Prevalence <- sRL_CalcAohPrevalence(aoh=AOH2[[1]], aoh_opt=AOH2_opt[[1]], type=AOH_type, points=pts)
  }
  
  ### Save Storage_SP ----
  Storage_SP<-sRL_OutLog(Storage_SP, "AOH_time", as.numeric(format(Sys.time(), "%s"))-as.numeric(format(TIC, "%s")))
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  
  ### Return list of arguments + calculate population size
  LIST=list(
    aoh_km2 = ifelse(Uncertain=="Uncertain_no", AOH_km2, paste(AOH_km2, AOH_km2_opt, sep="-")), 
    aoo_km2 = ifelse(Uncertain=="Uncertain_no", AOO_km2, paste(AOO_km2, AOO_km2_opt, sep="-")),
    plot_aoh = plot1,
    plot_aoh_2x2 = plot2,
    validation_model = Validation_Prevalence$Model_prevalence
  )
  
  if("Point_prevalence" %in% names(Validation_Prevalence)){LIST$validation_points <- Validation_Prevalence$Point_prevalence}
  
  if("dat_proj_saved" %in% names(Storage_SP)){
    LIST$plot_aoopts<-plot3
    LIST$aoo_pts_km2<-aoo_pts_km2
  }

  if (density_pref[1] != '-1') {
    LIST$pop_size <- pop_size
  }
  
  return(LIST)
}, gc=T, seed=T)

return(Prom)
}


### Leaflet ----

#### AOH Leaflet ----
#* AOH Leaflet
#* @get species/<scientific_name>/analysis/aoh_leaflet
#* @param scientific_name:string Scientific Name
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, username) { # nolint
  
  Prom<-future({
    sf::sf_use_s2(FALSE)
    
    #Filter param
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
    aohPROJ<-raster(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Current/", list.files(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Current"))[1])) ; crs(aohPROJ)<-CRSMOLL
    distSP<-Storage_SP$distSP_selected
    distPROJ<-st_transform(distSP, st_crs(4326))
    
    ### Plot
    AOH_leaflet<-leaflet() %>%
     addTiles(group="OpenStreetMap") %>%
     addEsriBasemapLayer(esriBasemapLayers$Imagery, group = "Satellite") %>%
     addEsriBasemapLayer(esriBasemapLayers$Topographic, group = "Topography") %>%
     addPolygons(data=distPROJ, color="#D69F32", fillOpacity=0, group="Distribution") %>% 
     addMouseCoordinates()
    
    # Add occurrence records if available
    if("dat_proj_saved" %in% names(Storage_SP)){
      dat_proj<-Storage_SP$dat_proj_saved %>% st_transform(., st_crs(4326))
      COORDS<-st_coordinates(dat_proj)
      dat_proj$lon<-COORDS[,1] ; dat_proj$lat<-COORDS[,2]
      
      AOH_leaflet <- AOH_leaflet %>%
        addCircleMarkers(lng=dat_proj$lon,
                         lat=dat_proj$lat,
                         color="black",
                         fillOpacity=0.5,
                         stroke=F,
                         radius=2,
                         group="Records")
      Overlay_records <- "Records"
    } else {Overlay_records <- NULL}

    ### Color palette
    ColPal<-colorNumeric(colorRamp(c("#FBCB3C", "#25BC5A"), interpolate = "spline"), c(0,1), na.color = NA)

    ### Divide by factor (1 if small-range, 900 if large-range) so that the range is always 0-1
    FACT<-ifelse(Storage_SP$AOH_type=="Large", 900, 1)

    ### Add uncertainty if available
    opt_path<-paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Current_optimistic/") %>% paste0(., list.files(.)[1])
    if(file.exists(opt_path)){
      AOH_opt<-raster(opt_path) ; crs(AOH_opt)<-CRSMOLL

      AOH_leaflet<-AOH_leaflet %>%
        addRasterImage(aohPROJ/FACT, method="ngb", group="Minimum AOH", opacity=0.5, colors=ColPal) %>%
        addRasterImage(AOH_opt/FACT, method="ngb", group="Maximum AOH", opacity=0.5, colors=ColPal) %>%
        addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), overlayGroups=c(Overlay_records, "Distribution", "Minimum AOH", "Maximum AOH"), position="topleft", options=layersControlOptions(collapsed = FALSE))

    } else {
      AOH_leaflet<-AOH_leaflet %>%
        addRasterImage(aohPROJ/FACT, method="ngb", group="AOH", opacity=0.5, colors=ColPal) %>%
        addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), overlayGroups=c(Overlay_records, "Distribution", "AOH"), position="topleft", options=layersControlOptions(collapsed = FALSE))
    }


    ### Store usage
    Storage_SP<-sRL_OutLog(Storage_SP, "AOH_leaflet", "Used")
    Storage_SP$AOH_leaflet<-AOH_leaflet
    sRL_StoreSave(scientific_name, username,  Storage_SP)

    
    return(AOH_leaflet)
    
  }, gc=T, seed=T)
  
  return(Prom)
}



#### AOO Leaflet ----
#* AOO Leaflet
#* @get species/<scientific_name>/analysis/aoo_leaflet
#* @param scientific_name:string Scientific Name
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, username) { # nolint
  
  Prom<-future({
    sf::sf_use_s2(FALSE)
    
    #Filter param
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
    dat_proj<-Storage_SP$dat_proj_saved %>% st_transform(., st_crs(4326))
    COORDS<-st_coordinates(dat_proj)
    dat_proj$lon<-COORDS[,1] ; dat_proj$lat<-COORDS[,2]
    aoo<-rast(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/AOO_known.tif")) %>% as.polygons(.) %>% st_as_sf(.) %>% st_transform(., st_crs(4326))
    distPROJ<-st_transform(Storage_SP$distSP_selected, st_crs(4326))
    
    ### Plot AOO
    AOO_leaflet<-leaflet() %>%
      addTiles(group="OpenStreetMap") %>%
      addEsriBasemapLayer(esriBasemapLayers$Imagery, group = "Satellite") %>%
      addEsriBasemapLayer(esriBasemapLayers$Topographic, group = "Topography") %>%
      addPolygons(data=aoo, group="AOO", opacity=0.7, color="#25BC5A", stroke=F) %>%
      addPolygons(data=distPROJ, color="#D69F32", fillOpacity=0, group="Distribution") %>% 
      addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), overlayGroups=c("Distribution", "AOO"), position="topleft") %>% 
      addCircleMarkers(lng=dat_proj$lon,
                       lat=dat_proj$lat,
                       color="black",
                       fillOpacity=0.5,
                       stroke=F,
                       radius=3) %>%
      addMouseCoordinates()
    
      
    ### Store usage
    Storage_SP<-sRL_OutLog(Storage_SP, "AOO_leaflet", "Used")
    Storage_SP$AOO_leaflet<-AOO_leaflet
    sRL_StoreSave(scientific_name, username,  Storage_SP)
    
    return(AOO_leaflet)
    
  }, gc=T, seed=T)
  
  return(Prom)
}


# Step 5: Trends in AOH ----------------------------------------------------------------

### Calculation -----
#* Estimate trends in AOH as a proxy of population trends (Criterion A2)
#* @get species/<scientific_name>/analysis/trends-aoh
#* @param scientific_name:string Scientific Name
#* @param GL_species:string GL_species
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username, GL_species="1") { # nolint
  
Prom<-future({
  sf::sf_use_s2(FALSE)
  
  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1)
  GL_species<-GL_species %>% gsub(" ", "", .) %>% sub(",", ".", .) %>% as.numeric(.) ; print(GL_species) ; if(is.na(GL_species)){incorrect_GL()}

  distSP=Storage_SP$distSP_selected ; if(nrow(distSP)==0){empty_distrib()}
  output_dir<-paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username))
  alt_crop=rast(paste0(output_dir, "/alt_crop.tif")) ; crs(alt_crop)<-CRSMOLL
  rangeSP_clean=Storage_SP$RangeClean_saved
  habitats_pref_DF=Storage_SP$habitats_SIS
  altitude_pref_DF=Storage_SP$AltPref_saved
  AOH_km2<-Storage_SP$AOHkm2_saved
  AOH_type<-Storage_SP$AOH_type
  rangeSP_cleanOPT=Storage_SP$RangeCleanOPT_saved
  
  # AOH charged directly
  AOH2<-rast(paste0(output_dir, "/Current/", list.files(paste0(output_dir, "/Current"))[1])) ; crs(AOH2[[1]])<-CRSMOLL
  if(Storage_SP$Uncertain=="Uncertain_yes"){AOH2_opt<-rast(paste0(output_dir, "/Current_optimistic/", list.files(paste0(output_dir, "/Current_optimistic"))[1])) ; crs(AOH2_opt[[1]])<-CRSMOLL}
  
  # Charge distribution
  distSP$binomial<-as.character(distSP$binomial)
  
  # Output directory + options
  dir.create(paste0(output_dir, "/Initial"));   dir.create(paste0(output_dir, "/Initial_optimistic"))
  terraOptions(tempdir=paste0(output_dir, "/Temporary"), memmax=config$RAMmax_GB)
  rasterOptions(tmpdir=paste0(output_dir, "/Temporary"), maxmemory=config$RAMmax_GB)
  
  # Choose year to use 
  Year1_theo<-config$YearAOH2-max(10, round(3*GL_species))
  Year1<-max(Year1_theo, 1992) ; print(Year1)
  
  if(AOH_type=="Small"){
    ### SMALL-RANGES ----
    
    # Charge and crop CCI1
    sRL_loginfo("START - Cropping rasters", scientific_name)
    cci1<-rast(sub("XXXX", Year1, config$cci1_raster_path)) ; crs(cci1)<-CRSMOLL # I ensure the CRS is correctly assigned
    cci1_crop<-crop(cci1, extent(distSP), snap="out")
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
        ncol=1, hjust=0)
    }
  
   
  } else {
  ### LARGE-RANGES ----

    # Calculate AOH
    AOH1<-sRL_largeAOH(alt_crop, habitats_pref_DF$code[habitats_pref_DF$suitability=="Suitable"], altitude_pref_DF[, c("elevation_lower", "elevation_upper")], rangeSP_clean, Year1, FILENAME=paste0(output_dir, "/Initial/aoh.tif"))
    
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

      AOH1_opt<-sRL_largeAOH(alt_crop, habitats_pref_DF$code, alt_pref_extreme, rangeSP_clean, Year1, FILENAME=paste0(output_dir, "/Initial_optimistic/aoh.tif"))

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
        ncol=1, hjust=0)
    }

    
  }  
  
  # Plot
  ggsave(filename = paste0(output_dir, "/Plots/trends_aoh.png"), plot = plot1, width=8, height=ifelse(Storage_SP$Uncertain=="Uncertain_no", 8, 13))
  plot1 <- base64enc::dataURI(file = paste0(output_dir, "/Plots/trends_aoh.png"), mime = "image/png", encoding = "base64") # nolint
  
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
  Storage_SP<-sRL_OutLog(Storage_SP, c("AOH_GenerationLength", "Original_GL"), c(GL_species, ifelse(scientific_name %in% GL_file$internal_taxon_name, GL_file$GL_estimate[GL_file$internal_taxon_name==scientific_name][1], NA)))
  

  # Output to return
  Out_area<-ifelse(Storage_SP$Uncertain=="Uncertain_no", 
                   AOH_old_km2,
                   paste(AOH_old_km2, AOH_old_km2OPT, sep="-"))
  
  Out_loss<-ifelse(Storage_SP$Uncertain=="Uncertain_no", 
                   paste0(Year1, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of "), warn_missing = F), abs(AOH_lost), "%"), # Give trend in AOH rather than loss,
                   paste0(Year1, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of "), warn_missing = F), abs(as.numeric(AOH_lost)), "% (Min AOH) or ", revalue(as.factor(sign(AOH_lostOPT)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of "), warn_missing = F), abs(AOH_lostOPT), "% (Max AOH)")
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
              paste0(Year1_theo, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of "), warn_missing = F), AOH_lost_extrap, "%"), # Give trend in AOH rather than loss,
              paste0(Year1_theo, "-", config$YearAOH2, ": ", revalue(as.factor(sign(AOH_lost)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of "), warn_missing = F), AOH_lost_extrap, "% (Min AOH) or ", revalue(as.factor(sign(AOH_lostOPT)), c("-1"="AOH gain of ", "1"="AOH loss of ", "0"="AOH loss of "), warn_missing = F), AOH_lostOPT_extrap, "% (Max AOH)")
  ))
  Storage_SP$aoh_lost_extrap=AOH_lost_extrap
  
  
  ### Return
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  
  LIST<-list(
    aoh_lost_km2 = Out_area,
    aoh_lost = Out_loss,
    plot_trends_aoh = plot1
  )
  if(Out_loss_extrap != ""){LIST$aoh_lost_extrap = Out_loss_extrap}
  
  return(LIST)
  
}, gc=T, seed=T)

return(Prom)
}



### Leaflet -----
#* Trends Leaflet
#* @get species/<scientific_name>/analysis/trends_leaflet
#* @param scientific_name:string Scientific Name
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, username) { # nolint
  
  Prom<-future({
    sf::sf_use_s2(FALSE)
    
    #Filter param
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
    output_dir<-paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username))
    aoh2PROJ<-raster(paste0(output_dir, "/Current/", list.files(paste0(output_dir, "/Current"))[1])) ; crs(aoh2PROJ)<-CRSMOLL
    aoh1PROJ<-raster(paste0(output_dir, "/Initial/", list.files(paste0(output_dir, "/Initial"))[1])) ; crs(aoh1PROJ)<-CRSMOLL
    distSP<-Storage_SP$distSP_selected
    distPROJ<-st_transform(distSP, st_crs(4326))
    
    ### Charge optimistic if exists
    Uncert<-Storage_SP$Uncertain
    if(Uncert=="Uncertain_yes"){
      AOH2_opt<-paste0(output_dir, "/Current_optimistic/") %>% paste0(., list.files(.)[1]) %>% raster(.) ; crs(AOH2_opt)<-CRSMOLL
      AOH1_opt<-paste0(output_dir, "/Initial_optimistic/") %>% paste0(., list.files(.)[1]) %>% raster(.) ; crs(AOH1_opt)<-CRSMOLL
    }
    
    ### Basic plot
    Trends_leaflet<-leaflet() %>%
      addTiles(group="OpenStreetMap") %>%
      addEsriBasemapLayer(esriBasemapLayers$Imagery, group = "Satellite") %>%
      addEsriBasemapLayer(esriBasemapLayers$Topographic, group = "Topography") %>%
      addPolygons(data=distPROJ, color="#D69F32", fillOpacity=0, group="Distribution") %>% 
      addMouseCoordinates()
    
    
    ### Create Trends and palettes (depend on AOH_type)
    if(Storage_SP$AOH_type=="Small"){
      
      # Calculate trends
      Trends<-(aoh2PROJ*2+3)-aoh1PROJ
      if(Uncert=="Uncertain_yes"){Trends_opt<-(AOH2_opt*2+3)-AOH1_opt}
      
      # Fix color palette
      ColPal<-colorFactor(c("#a6611a", "gray90", "#80cdc1", "#018571"), domain=c("2", "3", "4", "5"), na.color = NA)
      
      
    } else {
    ### Large range  
      
      # Calculate trends
      Trends<-(aoh2PROJ-aoh1PROJ)/9
      if(Uncert=="Uncertain_yes"){Trends_opt<-(AOH2_opt-AOH1_opt)/9}
      
      # Fix color palette
      ColPal<-colorNumeric(c("#8c510a", "azure2", "#018571"), domain=c(-100, 0, 100), na.color = NA)
      
    
    }
      
    ### Add trends in plots
    # No uncertainty
    if(Uncert=="Uncertain_no"){
      Trends_leaflet<-Trends_leaflet %>%
        addRasterImage(Trends, method="ngb", group="Trends in AOH", opacity=0.7, colors=ColPal) %>%
        addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), overlayGroups=c("Trends in AOH"), position="topleft", options=layersControlOptions(collapsed = FALSE))
      
      # With uncertainty
    } else {
      Trends_leaflet<-Trends_leaflet %>%
        addRasterImage(Trends, method="ngb", group="Trends in AOH (min AOH)", opacity=0.7, colors=ColPal) %>%
        addRasterImage(Trends_opt, method="ngb", group="Trends in AOH (max AOH)", opacity=0.7, colors=ColPal) %>% 
        addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), overlayGroups=c("Distribution", "Trends in AOH (min AOH)", "Trends in AOH (max AOH)"), position="topleft", options=layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Trends in AOH (max AOH)") # Trends max is hidden by default
    }
    
    
    ### Store usage
    Storage_SP<-sRL_OutLog(Storage_SP, "Trends_leaflet", "Used")
    Storage_SP$Trends_leaflet<-Trends_leaflet
    sRL_StoreSave(scientific_name, username,  Storage_SP)
    
    return(Trends_leaflet)
    
  }, gc=T, seed=T)
  
  return(Prom)
}


# Step 6: Fragmentation ----------------------------------------------------------------

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
#* @param density_pref:string density_pref
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username, dispersion="-1", density_pref= '-1') {
  
  
  # Prepare dispersion and density
  dispersion <- dispersion %>% gsub(" ", "", .) %>% gsub(",", ".", .) %>% as.character(.) %>% strsplit(., "-") %>% unlist() %>% as.numeric(.)*1000 ; print(dispersion)
  density_raw<-density_pref ; density_pref <- density_pref %>% gsub(" ", "", .) %>% gsub(",", ".", .) %>% as.character(.)  %>% strsplit(., "-") %>% unlist(.) %>% as.numeric(.) ; print(density_pref)
  if(is.na(sum(dispersion)) | density_raw =="-1" | density_raw=="" | is.null(density_raw) | is.na(sum(density_pref)) | length(dispersion)>2 | length(density_pref)>2 | is.unsorted(density_pref) | is.unsorted(dispersion)){wrong_density()} # The error does not matter here, the error text is in the client

  
Prom<-future({
    sf::sf_use_s2(FALSE)
  
    ### Filter param
    scientific_name<-sRL_decode(scientific_name)
    Storage_SP<-sRL_StoreRead(scientific_name,  username, MANDAT=1)
    aoh<-rast(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Current/", list.files(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Current"))[1]))[[1]]  ; crs(aoh)<-CRSMOLL
    aoh_type<-Storage_SP$AOH_type
    
    ### If large range, I have to binarize the suitable habitat
    if(aoh_type=="Large"){aoh<-ifel(aoh<=90, 0, 1)}
    
    ### Calculate fragmentation
    res<-sRL_fragmentation(aoh, aoh_type, min(dispersion), density_pref)
    if(length(dispersion)>1){
      res2<-sRL_fragmentation(aoh, aoh_type, max(dispersion), density_pref) 
    } else {res2<-res} # I create a res2 even if no uncertainty in isolation distance to avoid having to code 2 plots
    
    ### Subcriteria C2
    Max_pops<-c(max(res$clusters$pop, na.rm=T), max(res2$clusters$pop, na.rm=T), max(res$clusters$pop2, na.rm=T), max(res2$clusters$pop2, na.rm=T)) %>% subset(., abs(.) != Inf) %>% unique(.) %>% round(.)
    Pop_max<-c(min(Max_pops, na.rm=T), max(Max_pops, na.rm=T)) %>% unique(.)
    Pop_prop<-c((max(res$clusters$pop, na.rm=T)/sum(res$clusters$pop, na.rm=T)), (max(res2$clusters$pop, na.rm=T)/sum(res2$clusters$pop, na.rm=T))) %>% unique(.) %>% as.numeric(.) %>% round(.,2)*100 # Density uncertainty does not make any difference there
    Pop_prop <- Pop_prop %>% as.character(.) %>% replace(., .=="NaN", "") # Used to transform NaN in empty text to avoid bugs in Final Estimates
    
    ### Plots
    ### AOH and clusters
    aohDF<-as.data.frame(aoh, xy = TRUE); names(aohDF)[3]<-"lyr1"
    
    NClust<-unique(c(nrow(res2$clusters), nrow(res$clusters)))
    
    G1<-ggplot() + 
      geom_tile(data = aohDF, aes(x = x, y = y, fill = factor(lyr1, levels=c("0", "1"))), alpha = 0.5, col=NA)+
      geom_sf(data=st_transform(res2$clusters, crs(aoh)), aes(col="disp2"), fill=NA, lwd=2, show.legend = "line")+
      geom_sf(data=st_transform(res$clusters, crs(aoh)), aes(col="disp1"), fill=NA, lwd=2, show.legend = "line")+
      ggtitle(paste0("Population fragmentation in ", paste(NClust, collapse="-"), " clusters"))+
      scale_fill_manual(values=c("#FBCB3C", "#25BC5A", NA), labels=c("Unsuitable", "Suitable", ""), name="", na.translate=F, drop=F) +
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
    
    SubTitle<-paste0("Fragmented if you consider that a subpopulation lower \n than ", paste(VTOT, collapse="-"), " mature individuals is 'small'")
    
    G2<-ggplot()+
      geom_step(data=res$prop.fragm, aes(x=pop, y=CumSum, col="ShortMin"), linewidth=2.8)+
      geom_vline(xintercept=min(res$prop.fragm$pop[res$prop.fragm$CumSum>0.5], na.rm=T), linetype="dashed")+
      geom_hline(yintercept=0.5, linetype="dashed")+
      xlab("How many individuals constitute a 'small' population?")+ylab("Proportion of the population that is fragmented")+
      ylim(c(0,1.01))+
      labs(subtitle=SubTitle)+
      theme_minimal() %+replace%   theme(text=element_text(size=15), plot.subtitle=element_text(hjust=0.5), legend.position="bottom", plot.background=element_rect(fill="white", colour="white"))
    
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
    ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/fragmentation.png"), plot = cowplot::plot_grid(G1, G2, ncol=2), width=15, height=8)
    Frag_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/fragmentation.png"), mime = "image/png", encoding = "base64") # nolint
    
    ### Save in output log
    Storage_SP<-sRL_OutLog(Storage_SP, c("Fragmentation_Isolation", "Fragmentation_Density"), c(paste(dispersion/1000, collapse="-"), paste(density_pref, collapse="-")))
    Storage_SP$Pop_max<-Pop_max
    Storage_SP$Pop_prop<-Pop_prop
    Storage_SP$Frag_result<-paste(VTOT, collapse="-")
    sRL_StoreSave(scientific_name, username,  Storage_SP)
    
    
    ### Return
    LIST<-list(
      Frag_plot=Frag_plot,
      Frag_result=SubTitle,
      Frag_PopMax=paste(Pop_max, collapse="-"),
      Frag_PopProp=paste(Pop_prop, collapse="-")
    )
    
    return(LIST)
    
  }, gc=T, seed=T)
  
  ### Return list
  return(Prom)
}




# Step 7: Remote sensing products ----------------------------------------------------------------

#* Calculate trends in RS products
#* @get species/<scientific_name>/analysis/RS_analysis
#* @param scientific_name:string Scientific Name
#* @param RSproduct:string RSproduct
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username, GL_species="1", RSproduct = "") { # nolint    
  
Prom<-future({
  sf::sf_use_s2(FALSE)
  
  # Charge parameters
  scientific_name<-sRL_decode(scientific_name)
  Storage_SP<-sRL_StoreRead(scientific_name,  username, MANDAT=1)
  distSP<-Storage_SP$distSP_selected %>% dplyr::group_by() %>% dplyr::summarise(N= n())
  GL_species<-GL_species %>% gsub(" ", "", .) %>% sub(",", ".", .) %>% as.numeric(.) ; print(GL_species) ; if(is.na(GL_species)){incorrect_GL()}
  Storage_SP$GL_saved<-GL_species
  print(RSproduct)
  
  # Run functions to calculate trends
  if(RSproduct=="Human_density"){List_trendsRS<-sRL_CalcHumandensity(scientific_name, username, distSP, GL_species)}
  if(RSproduct=="Forest_cover"){List_trendsRS<-sRL_CalcForestchange(scientific_name, username, distSP, GL_species)}
  if(RSproduct=="Human_modification"){List_trendsRS<-sRL_CalcModification(scientific_name, username, distSP)}

  # Save usage
  RS_stored<-Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Usage_RS"]
  Storage_SP<-sRL_OutLog(Storage_SP, "Usage_RS", paste(RS_stored, RSproduct, sep="."))
  Storage_SP<-sRL_OutLog(Storage_SP, paste0("RS_result_", RSproduct), paste0(List_trendsRS[3:6], collapse=" & "))
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  
  
  # Return
  return(List_trendsRS)

}, gc=T, seed=T)

return(Prom)
}



#### Leaflet ----
#* Trends RS Leaflet
#* @get species/<scientific_name>/analysis/RS_leaflet
#* @param scientific_name:string Scientific Name
#* @param RSproduct:string RSproduct
#* @serializer htmlwidget
#* @tag sRedList
function(scientific_name, username, RSproduct) { # nolint
  
  Prom<-future({
    sf::sf_use_s2(FALSE)
    
    print(RSproduct)
    
    #Filter param
    scientific_name <- sRL_decode(scientific_name)
    Storage_SP=sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
    RSPROJ_current<-raster(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/", RSproduct, "_Current.tif"))
    RSPROJ_trends<-raster(paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/", RSproduct, "_Change.tif"))
    distSP<-Storage_SP$distSP_selected
    distPROJ<-st_transform(distSP, st_crs(4326))

    ### Plot
    RS_leaflet<-leaflet() %>%
      addTiles(group="OpenStreetMap") %>%
      addEsriBasemapLayer(esriBasemapLayers$Imagery, group = "Satellite") %>%
      addEsriBasemapLayer(esriBasemapLayers$Topographic, group = "Topography") %>%
      addPolygons(data=distPROJ, color="#D69F32", fillOpacity=0, group="Distribution") %>% 
      addMouseCoordinates()

    
    ### Color palette
    LIM<-max(abs(summary(RSPROJ_trends)[1]), abs(summary(RSPROJ_trends)[5]))
    
    if(RSproduct=="Forest_cover"){
      ColPal1<-colorNumeric("viridis", c(0,100), na.color = NA)
      ColPal2<-colorNumeric(c("#8c510a", "azure2", "#018571"), domain=c(-LIM, 0, LIM), na.color = NA)
    } else {
      ColPal1<-colorNumeric("viridis", c(summary(RSPROJ_current)[1],summary(RSPROJ_current)[5]), na.color = NA)
      ColPal2<-colorNumeric(c("#018571", "azure2", "#8c510a"), domain=c(-LIM, 0, LIM), na.color = NA)
    }

    RS_leaflet<-RS_leaflet %>%
        addRasterImage(RSPROJ_current, method="ngb", group="Current", opacity=1, colors=ColPal1) %>%
        addRasterImage(RSPROJ_trends, method="ngb", group="Change", opacity=1, colors=ColPal2) %>%
        addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), overlayGroups=c("Distribution", "Current", "Change"), position="topleft", options=layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Change") # Change is hidden by default

    ### Store usage
    Storage_SP<-sRL_OutLog(Storage_SP, "RS_leaflet", "Used")
    Storage_SP[which(names(Storage_SP)==paste0("RS_leaflet_", RSproduct))]<-NULL # Remove the previous leaflet of the same RSproduct
    Storage_SP$RS_leaflet<-RS_leaflet # Save the new one
    names(Storage_SP)[which(names(Storage_SP)=="RS_leaflet")]<-paste0("RS_leaflet_", RSproduct) # Rename it
    sRL_StoreSave(scientific_name, username,  Storage_SP)
    
    return(RS_leaflet)
    
  }, gc=T, seed=T)
  
  return(Prom)
}







# Step 8: Summary ----------------------------------------------------------------

## Final estimates -------
#* Upload all parameters
#* @get species/<scientific_name>/final-estimates
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, username){
  
  scientific_name<-sRL_decode(scientific_name)
  Storage_SP<-sRL_StoreRead(scientific_name,  username, MANDAT=1) ; print(names(Storage_SP))
  
  ### MANAGE TAXONOMY ###
  sRL_loginfo("START - Extract taxonomy", scientific_name)
  kingdom<-phylum<-classname<-ordername<-family<-taxonomicAuthority<-NA
  
  ### Extract existing taxonomy
  if(scientific_name %in% speciesRL$scientific_name){
    # If species already in the Red List, we use its information
    Official<-speciesRL[speciesRL$scientific_name == scientific_name,][1,]
    sRL_loginfo("Using RL from same species", scientific_name)
    } else {
    # Otherwise we look if there is another species of the same genus (except for authority)
      Genus<-scientific_name %>% strsplit(., " ") %>% unlist(.) %>% .[1]
      if(Genus %in% speciesRL$genus){Official<-speciesRL[speciesRL$genus==Genus,][1,] ; Official$authority<-NA ; sRL_loginfo("Using RL from same genus", scientific_name)}
    }
  
  ### If genus not in RL, I use taxize
  tryCatch({
    if(exists("Official")==F){
      sRL_loginfo("Using taxize", scientific_name)
      classif<-classification(scientific_name, db="gbif", rows=1)[[1]]
      AuthorityExtract<-gnr_resolve(scientific_name, data_source_ids=11, canonical=FALSE, best_match_only = T)$matched_name %>% strsplit(., " ") %>% unlist(.) %>% .[c(3:length(.))] %>% gsub("[(),]", "", .) %>% paste0(., collapse=", ")
    }
    
    ### Assign to that species
    kingdom   <-  ifelse(exists("Official"), Official$kingdom[1], toupper(classif$name[classif$rank=="kingdom"]))
    phylum    <-  ifelse(exists("Official"), Official$phylum[1], toupper(classif$name[classif$rank=="phylum"]))
    classname <-  ifelse(exists("Official"), Official$class[1], toupper(classif$name[classif$rank=="class"]))
    ordername <-  ifelse(exists("Official"), Official$order[1], toupper(classif$name[classif$rank=="order"]))
    family    <-  ifelse(exists("Official"), Official$family[1], toupper(classif$name[classif$rank=="family"]))
    taxonomicAuthority <- ifelse(exists("Official"), Official$authority[1], AuthorityExtract)
  }, error=function(e){"Taxonomic extract with taxize was not complete"})
  sRL_loginfo("END - Extract taxonomy", scientific_name)
  

  
  ### MANAGE ESTIMATES ###
  
  ### EOO
  EOO_val <- ifelse("eoo_km2" %in% names(Storage_SP), Storage_SP$eoo_km2, NA)
  EOO_justif <- ifelse("eoo_km2" %in% names(Storage_SP), "The EOO has been estimated as the Minimum Convex Polygon around the distribution on the sRedList platform.", NA)
  Storage_SP<-sRL_OutLog(Storage_SP, "Estimated_EOO_raw", EOO_val)
  
  
  ### AOO
  tryCatch({
  # If no estimates of AOO
  if(! "aoo_km2" %in% names(Storage_SP)){
    AOO_val<-AOO_justif<-NA
  }
  
  # If only AOO from habitat
  if("aoo_km2" %in% names(Storage_SP) & (! "aoo_points" %in% names(Storage_SP))){
    AOO_val <- ifelse((Storage_SP$Uncertain=="Uncertain_no"), 
                      round(Storage_SP$aoo_km2), 
                      paste(round(Storage_SP$aoo_km2), round(Storage_SP$aoo_km2_opt), sep="-")
    )
    AOO_justif <- "The area of occupancy has been estimated on the sRedList Platform by rescaling the map of Area of Habitat to a 2x2km2 grid. This estimates corresponds to the upper bound of area of occupancy, as it assumes that all suitable habitat is occupied (at a 2x2km scale)."
  }
  
  # If AOO from habitat and occurrences
  if("aoo_points" %in% names(Storage_SP)){
    aooA<-round(Storage_SP$aoo_points)
    Nrec<-nrow(Storage_SP$dat_proj_saved)
    
    # Extract source from points
    SRCdf<-table(Storage_SP$dat_proj_saved$Source_type) %>% as.data.frame(.)
    SRCdf$Name<-revalue(SRCdf$Var1, c("GBIF sample"="a geographically representative sample of data downloaded from GBIF", "OBIS sample"="a sample of data downloaded from OBIS", "Red List"="Red List occurrence records", "Red List sample"="a sample of Red List occurrence records", "Uploaded"="occurrence records uploaded by sRedList user", "Uploaded sample"="a sample of occurrence records uploaded by sRedList user"), warn_missing = F)
    SRCdf$paste<-paste0(SRCdf$Name, " (N=", SRCdf$Freq, ")")
    SRC<-paste(SRCdf$paste, collapse=", ")
    
    # Extract AOO estimates from habitat
    aoo_hab <- ifelse(Storage_SP$Uncertain=="Uncertain_no", Storage_SP$aoo_km2, paste(Storage_SP$aoo_km2, Storage_SP$aoo_km2_opt, sep="-"))%>% as.character(.) %>% strsplit(., "-") %>% unlist(.) %>% as.vector(.) %>% as.numeric(.)
    
    # If AOO from occurrence records is bigger, I keep only this one. Otherwise, I take the range
    if(aooA > max(aoo_hab, na.rm=T)){
      AOO_val<-aooA
      AOO_justif<-paste0("The area of occupancy was estimated on the sRedList platform as the area of 2x2km grid cells intersecting with the ", Nrec, " occurrence records retrieved from ", SRC, ". This estimate assumes that the species range has been extensively surveyed at a 2x2km scale. The upper bound of area of occupancy estimated from suitable habitat was lower than this estimate (", paste(aoo_hab, collapse="-"), "km2) and was thus dismissed.")
    } else {
      AOO_val <- paste(aooA, max(aoo_hab, na.rm=T), sep="-")
      AOO_justif <- paste0("The area of occupancy was estimated on the sRedList platform. ",
                            "Its lower bound (", aooA, "km2) was estimated as the area of 2x2km grid cells intersecting with occurrence records (", Nrec, " occurrence records were retrieved from ", SRC, "); this estimate assumes that the species range has been extensively surveyed at a 2x2km scale. ",
                            "The upper bound of area of occupancy (", paste(unique(aoo_hab), collapse="-"), "km2) has been estimated by rescaling the map of Area of Habitat to a 2x2km grid; this estimate assumes that all suitable habitat is occupied by the species (at a 2x2km scale).")
    }
  }
  
  Storage_SP<-sRL_OutLog(Storage_SP, "Estimated_AOO_raw", AOO_val)
  }, error=function(e){"Error in AOO estimates"})
  
  
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
      Trends_dir<-revalue(as.character(signP), c("-1"="+", "1"="-"), warn_missing = F)
      Trends_val<-aohP
      
      # If there is uncertainty it depends if optimistic and pessimistic are in the same direction (both decreasing or both increasing)
    } else {
      signO<-sign(Storage_SP$aoh_lostOPT_saved) %>% replace(., .==0, -1)
      aohO<-abs(Storage_SP$aoh_lostOPT_extrap)
      
      # If optimistic and pessimistic have the same sign, I use this
      if(signP==signO){
        Trends_dir<-revalue(as.character(signP), c("-1"="+", "1"="-"), warn_missing = F)
        Trends_val<-paste(min(c(aohP, aohO)), max(c(aohP, aohO)), sep="-")
        # If they have different sign, I put the negative and suggest it can go down to 0
      } else {
        Trends_dir<-"-"
        Trends_val<-paste(0, aohP, sep="-")
      }
    }
    
    Trends_justif<-paste0("This trend has been measured from the sRedList platform as the trend in Area of Habitat between ", 
                          paste0(Storage_SP$Year1theo_saved, " and ",config$YearAOH2), # Give years if no extrapolation
                          ifelse(Storage_SP$Year1theo_saved==Storage_SP$Year1_saved, "", " (using exponential extrapolation for the period before 1992)."),
                          " Trends in Area of Habitat are often very conservative and should be used only if no better data on population trends are available."
                          ) # Specify extrapolation if needed
    
    Storage_SP<-sRL_OutLog(Storage_SP, "Estimated_PopTrends_raw", Trends_val)
    Storage_SP<-sRL_OutLog(Storage_SP, "Estimated_PopTrendsDir_raw", Trends_dir)
  }
  
  
  ### Pop max and prop
  if(!"Pop_prop" %in% names(Storage_SP)){Pop_max<-Pop_prop<- NA} else {
    Pop_max<-Storage_SP$Pop_max %>% paste(., collapse="-")
    Pop_prop<-Storage_SP$Pop_prop %>% paste(., collapse="-")
  }
  
  
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
    Trends_justif=Trends_justif,
    Pop_max=Pop_max,
    Pop_prop=Pop_prop
    )
  
  ### Save Storage_SP
  Storage_SP$Estimates_saved<-Estimates_df
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  
  return(list(Estimates=Estimates_df))
}


## Assign category -------
#* Plot Red List category
#* @post species/<scientific_name>/assessment/red-list-criteria
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name,
         username,
         Estimates, 
         pastTrends_dir, pastTrends_qual, pastTrends_basis, pastTrends_reversible, pastTrends_understood, pastTrends_ceased, fragmentBin, Fragment_justif,
         Extreme_EOO, Extreme_AOO, Extreme_Pop, Extreme_NLoc, Extreme_NSub, Extreme_EOO_justif, Extreme_AOO_justif, Extreme_Pop_justif, Extreme_NLoc_justif, Extreme_NSub_justif,
         Continuing_EOO, Continuing_AOO, Continuing_Hab, Continuing_Pop, Continuing_NLoc, Continuing_NSub, Continuing_EOO_justif, Continuing_AOO_justif, Continuing_Hab_justif, Continuing_Pop_justif, Continuing_NLoc_justif, Continuing_NSub_justif,
         locationNumber,	locationNumber_justif, SubNumber,	SubNumber_justif, OneSubpop,	VeryRestricted,	VeryRestricted_justif,
         populationTrend, currentTrends_basis, currentPop_years, futureTrends_quality, futureTrends_basis, futureTrends, futureTrends_dir, futureTrends_justif, ongoingTrends_NY, ongoingTrends_quality, ongoingTrends_basis, ongoingTrends_reversible, ongoingTrends_understood, ongoingTrends_ceased, ongoingTrends, ongoingTrends_dir, ongoingTrends_justif,
         C_igen_value, C_igen_qual, C_igen_justif, C_iigen_value, C_iigen_qual, C_iigen_justif, C_iiigen_value, C_iiigen_qual, C_iiigen_justif
         ) {

Prom<-future({
  
  Estimates<-replace(Estimates, Estimates %in% c("undefined", " "), NA)

  #Filter param
  sRL_loginfo("Start Criteria calculation", scientific_name)
  scientific_name <- sRL_decode(scientific_name)
  Storage_SP<-sRL_StoreRead(scientific_name,  username, MANDAT=1)
  Storage_SP$Output$Count[Storage_SP$Output$Parameter=="Col_allfields"]<-as.numeric(Storage_SP$Output$Count[Storage_SP$Output$Parameter=="Col_allfields"])+1 # Count number of times Assign categories is run
  sRL_StoreSave(scientific_name, username,  Storage_SP)
  print(names(Storage_SP))

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
    allfields$ElevationLower.limit<-min(c(AltPref_saved$elevation_lowerEXTREME[1], AltPref_saved$elevation_lower[1]), na.rm=T) # SIS Connect does not allow uncertainty, so I keep the most extreme (ie min lower limit and max upper limit)
    allfields$ElevationUpper.limit<-max(c(AltPref_saved$elevation_upper[1], AltPref_saved$elevation_upperEXTREME[1]), na.rm=T)
  }
  
  ### Save parameters from analyses
  # Generation length
  allfields$GenerationLength.range<-Storage_SP$GL_saved
  
  # EOO
  allfields$EOO.range<-sRL_CheckNumeric(Estimates[7], "[EOO value]", "NotPerc")
  allfields$EOO.justification<-Estimates[8]
  
  # AOO
  allfields$AOO.range<-sRL_CheckNumeric(Estimates[9], "[AOO value]", "NotPerc")
  allfields$AOO.justification<-Estimates[10]
  
  # Population size
  allfields$PopulationSize.range<-sRL_CheckNumeric(Estimates[11], "[Population size value]", "NotPerc")
  
  # Decline for A2
  if(!(Estimates[12] %in% c("+", "-")) & is.na(Estimates[13])==F){questionnaire_sign()}
  if(Estimates[12] %in% c("+", "-")){allfields$PopulationReductionPast.direction<-revalue(Estimates[12], c("-"="Reduction", "+"="Increase"), warn_missing = F)} # Replace by Increase or Reduction (if users wrote something else, we don't report it in allfields)
  allfields$PopulationReductionPast.range<-sRL_CheckNumeric(Estimates[13], "[Past trends value]", ifelse(allfields$PopulationReductionPast.direction == "Reduction", "Percentage_yes", "NotPerc"))
  allfields$PopulationReductionPast.justification<-Estimates[14]

  # Population trends details
  allfields$PopulationReductionPast.qualifier<-pastTrends_qual
  allfields$PopulationReductionPastBasis.value<-pastTrends_basis %>% subset(., nchar(.)>1 & . != "Unknown") %>% paste(., collapse="|") %>% ifelse(.=="", "Unknown", .) # Remove single letters, remove Unknown if with something else, paste with | as in SIS Connect Sample set
  allfields$PopulationReductionPastReversible.value<-pastTrends_reversible
  allfields$PopulationReductionPastUnderstood.value<-pastTrends_understood
  allfields$PopulationReductionPastCeased.value<-pastTrends_ceased
  
  # Fragmentation
  allfields$SevereFragmentation.isFragmented<-ifelse(fragmentBin %in% c("true", "TRUE", "T"), "Yes", "No")
  allfields$SevereFragmentation.justification<-Fragment_justif
  
  # Population details
  allfields$LocationsNumber.range<-sRL_CheckNumeric(locationNumber, "[Number of locations]", "NotPerc")
  allfields$LocationsNumber.justification<-locationNumber_justif
  allfields$YearOfPopulationEstimate.value<-sRL_CheckNumeric(currentPop_years, "[Year of population estimate]", "NotPerc")
  allfields$AreaRestricted.isRestricted<-VeryRestricted
  allfields$AreaRestricted.justification<-VeryRestricted_justif
  allfields$SubpopulationNumber.range<-sRL_CheckNumeric(SubNumber, "[Number of subpopulations]", "NotPerc")
  allfields$SubpopulationNumber.justification<-SubNumber_justif
  allfields$MaxSubpopulationSize.range<-sRL_CheckNumeric(Estimates[15], "[Maximum number of mature individuals in a subpopulation]", "NotPerc")
  allfields$MatureIndividualsSubpopulation.value<-sRL_CheckNumeric(Estimates[16], "[% of mature individuals in one subpopulation]", "Percentage_yes")
  allfields$SubpopulationSingle.value<-OneSubpop
  
  # Population trends
  allfields$CurrentTrendDataDerivation.value<-currentTrends_basis

  allfields$PopulationReductionFuture.direction<-futureTrends_dir
  allfields$PopulationReductionFuture.range<-sRL_CheckNumeric(futureTrends, "[Future trends value]", ifelse(futureTrends_dir == "Reduction", "Percentage_yes", "NotPerc"))
  allfields$PopulationReductionFuture.justification<-futureTrends_justif
  allfields$PopulationReductionFuture.qualifier<-futureTrends_quality
  allfields$PopulationReductionFutureBasis.value<-futureTrends_basis %>% subset(., nchar(.)>1 & . != "Unknown") %>% paste(., collapse="|") %>% ifelse(.=="", "Unknown", .) # Remove single letters, remove Unknown if with something else, paste with | as in SIS Connect Sample set

  allfields$PopulationReductionPastandFuture.direction<-ongoingTrends_dir
  allfields$PopulationReductionPastandFuture.range<-sRL_CheckNumeric(ongoingTrends, "[Ongoing trends value]", ifelse(ongoingTrends_dir == "Reduction", "Percentage_yes", "NotPerc"))
  allfields$PopulationReductionPastandFuture.justification<-ongoingTrends_justif
  allfields$PopulationReductionPastandFuture.qualifier<-ongoingTrends_quality
  allfields$PopulationReductionPastandFutureBasis.value<-ongoingTrends_basis %>% subset(., nchar(.)>1 & . != "Unknown") %>% paste(., collapse="|") %>% ifelse(.=="", "Unknown", .) # Remove single letters, remove Unknown if with something else, paste with | as in SIS Connect Sample set
  allfields$PopulationReductionPastandFuture.numYears<-sRL_CheckNumeric(ongoingTrends_NY, "[Number of years]", "NotPerc")
  allfields$PopulationReductionPastandFutureCeased.value<-ongoingTrends_ceased
  allfields$PopulationReductionPastandFutureReversible.value<-ongoingTrends_reversible
  allfields$PopulationReductionPastandFutureUnderstood.value<-ongoingTrends_understood

  # Population C1
  allfields$PopulationDeclineGenerations1.range<-sRL_CheckNumeric(C_igen_value, "[Decline in 1 generation]", "Percentage_yes")
  allfields$PopulationDeclineGenerations1.qualifier<-C_igen_qual
  allfields$PopulationDeclineGenerations1.justification<-C_igen_justif

  allfields$PopulationDeclineGenerations2.range<-sRL_CheckNumeric(C_iigen_value, "[Decline in 2 generations]", "Percentage_yes")
  allfields$PopulationDeclineGenerations2.qualifier<-C_iigen_qual
  allfields$PopulationDeclineGenerations2.justification<-C_iigen_justif

  allfields$PopulationDeclineGenerations3.range<-sRL_CheckNumeric(C_iiigen_value, "[Decline in 3 generations]", "Percentage_yes")
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
  output_dir<-paste0(sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "_sRedList")
  dir.create(output_dir)
  
  # Countries (but enabling skipping step) + prepare assessments.csv
  if("coo_occ" %in% names(Storage_SP)){
    tryCatch({
      countries_SIS<-sRL_OutputCountries(scientific_name, Storage_SP$coo_occ) %>% subset(., grepl("Absent_SIS", .$CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceName)==F)
      write.csv(countries_SIS, paste0(output_dir, "/countries.csv"), row.names = F)
    }, error=function(e){"Bug in exporting countries of occurrence for SIS"})
    assessments_SIS<-sRL_OutputAssessments(scientific_name, Storage_SP$Realms_saved, Storage_SP$Output$Value[Storage_SP$Output$Parameter=="System_pref"][1], populationTrend)
  } else {assessments_SIS<-sRL_OutputAssessments(scientific_name, NA, NA, populationTrend)}
  
  # Habitats (if AOH not skipped)
  if("habitats_SIS" %in% names(Storage_SP)){
    habitats_SIS<-Storage_SP$habitats_SIS[,6:13]
    habitats_SIS$internal_taxon_id<-sRL_CalcIdno(scientific_name)
    habitats_SIS<-replace(habitats_SIS, is.na(habitats_SIS), "")
    write.csv(habitats_SIS, paste0(output_dir, "/habitats.csv"), row.names = F)
  }
  
  ref_SIS<-sRL_OutputRef(scientific_name, Storage_SP) 
  taxo_SIS<-sRL_OutputTaxo(scientific_name, Estimates)
  out_save<-Storage_SP$Output[Storage_SP$Output$Definition !="Only used to track usage",] %>% subset(., select=names(.)[names(.) != "Count"])

  # Save csv files in a folder
  write.csv(replace(allfields_to_save, is.na(allfields_to_save), ""), paste0(output_dir, "/allfields.csv"), row.names = F)
  write.csv(taxo_SIS, paste0(output_dir, "/taxonomy.csv"), row.names = F)
  write.csv(ref_SIS, paste0(output_dir, "/references.csv"), row.names = F)
  write.csv(out_save, paste0(output_dir, "/00.Output_log.csv"), row.names = F)
  write.csv(assessments_SIS, paste0(output_dir, "/assessments.csv"), row.names = F)
  
  # Download tracking files
  print(username)
  if(grepl("victor.cazalis", username)){
    # List files stored in Stored_outputs to add to zip and copy
    if(scientific_name==config$Name_tracker){
      filesOut<-list.files("Species/Stored_outputs") %>% paste0("Species/Stored_outputs/", .)
      file.copy(filesOut, paste0(output_dir, "/Outputs_", filesOut))
      }
    # or list all zip files not removed yet
    if(scientific_name==sub("3","4",config$Name_tracker)){
      filesOut<-list.files() %>% .[grepl(".zip", .)]
      file.copy(filesOut, paste0(output_dir, "/Outputs_", filesOut))
      }
  }
  
  # Save distribution and occurrences if from GBIF
  if(is.null(Storage_SP$gbif_number_saved)==F | is.na(Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Gbif_EditPoly"])==F){
    sRL_loginfo("Start saving distribution", scientific_name)
    
    distSIS<-sRL_OutputDistribution(scientific_name, Storage_SP)
    if("hybas_id" %in% names(Storage_SP$distSP_saved)){
      hydroSIS<-sRL_OutputHydrobasins(distSIS, Storage_SP)
      write.csv(hydroSIS, paste0(output_dir, "/sRedList_", gsub(" ", ".", scientific_name), "_Hydrobasins.csv"), row.names=F)
    }
    distSIS <- distSIS %>% dplyr::group_by(across(.cols=-"geometry")) %>% dplyr::summarise()
   st_write(distSIS, paste0(output_dir, "/sRedList_", gsub(" ", ".", scientific_name), "_Distribution.shp"), append=F)
   
   ### Save occurrence records
   if(is.null(Storage_SP$gbif_number_saved)==F){
     write.csv(sRL_OutputOccurrences(scientific_name, Storage_SP, distSIS), paste0(output_dir, "/sRedList_", gsub(" ", ".", scientific_name), "_Occurrences.csv"), row.names=F)
   }
  }

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
  
  # Prepare Tag_taxo in case taxonomy not complete
  Tag_taxo<-""
  if(NA %in% Estimates[1:6]){Tag_taxo<-paste0("\n\n WARNING: The taxonomy information is incomplete (missing: ", 
                                         paste0(c("Kingdom", "Phylum", "Class", "Order", "Family", "Authority")[which(is.na(Estimates[1:6]))], collapse=", "), 
                                         "), \n this will cause issue if you want to push the output ZIP file to SIS Connect")
  } else {
    if((! toupper(Estimates[1]) %in% speciesRL$kingdom) |
       (! toupper(Estimates[2]) %in% speciesRL$phylum) |
       (! toupper(Estimates[3]) %in% speciesRL$class) |
       (! toupper(Estimates[4]) %in% speciesRL$order) |
       (! toupper(Estimates[5]) %in% speciesRL$family)){
      Tag_taxo<-paste0("\n\n WARNING: Some of the entered taxonomy (", 
                  c(ifelse(toupper(Estimates[1]) %in% speciesRL$kingdom, NA, "kingdom"),
                    ifelse(toupper(Estimates[2]) %in% speciesRL$phylum, NA, "phylum"),
                    ifelse(toupper(Estimates[3]) %in% speciesRL$class, NA, "class"),
                    ifelse(toupper(Estimates[4]) %in% speciesRL$order, NA, "order"),
                    ifelse(toupper(Estimates[5]) %in% speciesRL$family, NA, "family")) %>% 
                    .[is.na(.)==F] %>% paste0(., collapse=", "), 
                  ") does not correspond to any of the published species. \n Make sure it fits with the Red List taxonomic backbone before pushing to SIS Connect.")}
  }
  
  # Prepare Tag_national in case national assessment not yet in the Red List
  Tag_national<-ifelse(
    (Storage_SP$Output$Value[Storage_SP$Output$Parameter=="Crop_Country"]!= "" & (! scientific_name %in% speciesRL$scientific_name)), 
    "You are doing a national / regional assessment of a species that is not in the global Red List yet. If the species is endemic, please consider contributing it as a global assessment too!",
    ""
  )
  
  # Prepare plot
  GG_assign<-ggplot(criteria, aes(y = criterion)) +
    geom_linerange(aes(xmin=Cat_ThresholdMIN, xmax=Cat_ThresholdMAX), linewidth=10, colour="gray75")+
    geom_point(aes(x = Cat_ThresholdMIN, col = ColMin), size = 20, stroke=6, show.legend=F) +
    geom_text(aes(x=Cat_ThresholdMIN, label=Cat_ThresholdMIN), col="white", size=5)+
    geom_point(aes(x = Cat_ThresholdMAX, col = ColMax), size = 20, stroke=6, show.legend=F) +
    geom_text(aes(x=Cat_ThresholdMAX, label=Cat_ThresholdMAX), col="white", size=5)+
    scale_x_discrete(drop = F, na.translate = FALSE) + scale_y_discrete(drop=F, na.translate = FALSE) +
    xlab("Red List Category triggered") + ylab("Criteria")+
    scale_colour_manual(drop = F, values=c("#006666ff", "#cc9900ff", "#cc6633ff", "#cc3333ff", "#b3d1d1ff", "#f0e1b3ff", "#f0d1c2ff", "#f0c2c2ff"))+
    labs(title=TITLE, subtitle=SUBTITLE, tag=Tag_taxo)+
    theme_bw()  %+replace% theme(text=element_text(size=18), plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size=15), plot.tag=element_text(hjust=0.5, size=14, colour="darkred"), plot.tag.position = "bottom")
  
  # Save
  ggsave(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/Plot_assign.png"), GG_assign, width=10, height=8)
  plot_assign <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/Plot_assign.png"), mime = "image/png") # nolint
  
  
  # Call RMarkDown
  sRL_loginfo("Start RMarkDown", scientific_name)
  WD<-getwd()
  tryCatch({
    render("sRL_markdown_scripts/General_RMarkDown_script.Rmd",
           output_format="all",
           output_file=paste0("sRedList_report_", sub(" ", "_", scientific_name), ".html"),
           output_dir=paste0(sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "_sRedList"),
           knit_root_dir=WD
    )
  }, error=function(e){"Error in creating Markdown report"})
  
  # ZIP folder
  #zip(zipfile = paste0(sub(" ", "_", scientific_name), "_sRedList"), files = paste0(sub(" ", "_", scientific_name), "_sRedList"),  zip = "C:/Program Files/7-Zip/7z", flags="a -tzip")
  sRL_loginfo("Start ZIP", scientific_name)
  zip(zipfile = paste0(sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "_sRedList.zip"), files = paste0(sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "_sRedList"), extras = '-j')
  
  
  # Return
  return(
    list(
      plot=plot_assign,
      warning_taxo=Tag_taxo,
      warning_national=Tag_national
    )
  )
  
}, gc=T, seed=T)

return(Prom)

}


## Download zip -------
#* Download .zip assesment file of the species
#* @get species/<scientific_name>/assessment/red-list-criteria/zip
#* @param scientific_name:string Scientific Name
#* @serializer contentType list(type="application/octet-stream")
#* @tag sRedList
function(scientific_name, username) {
  scientific_name <- sRL_decode(scientific_name)
  
  sRL_loginfo("Start Zipping", scientific_name)

  
  # Charge ZIP file
  zip_to_extract<-readBin(paste0(gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "_sRedList.zip"), "raw", n = file.info(paste0(gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "_sRedList.zip"))$size)
  
  
  # Prepare Outputs (remove definitions, empty fields, those at default)
  output_species<-sRL_StoreRead(scientific_name, username, MANDAT=1)$Output
  output_species$Definition<-NULL
  output_species$Value<-replace(output_species$Value, is.na(output_species$Value), "") # I replace NA to make the next line work and then remove all empty fields
  if(output_species$Value[output_species$Parameter=="Gbif_Extent"]=="-180,180,-90,90"){output_species<-output_species[-which(output_species$Parameter=="Gbif_Extent"),]}
  if(output_species$Value[output_species$Parameter=="Gbif_yearBin"]=="FALSE"){output_species<-output_species[-which(output_species$Parameter=="Gbif_yearBin"),]}
  if(output_species$Value[output_species$Parameter=="Gbif_uncertainBin"]=="FALSE"){output_species<-output_species[-which(output_species$Parameter=="Gbif_uncertainBin"),]}
  if(output_species$Value[output_species$Parameter=="Mapping_Buffer"]=="0"){output_species<-output_species[-which(output_species$Parameter=="Mapping_Buffer"),]}
  if(output_species$Value[output_species$Parameter=="Mapping_Altitude"]=="0, 9000"){output_species<-output_species[-which(output_species$Parameter=="Mapping_Altitude"),]}
  if(output_species$Value[output_species$Parameter=="Mapping_Merge"]=="FALSE"){output_species<-output_species[-which(output_species$Parameter=="Mapping_Merge"),]}
  if(output_species$Value[output_species$Parameter=="Mapping_Smooth"]=="0"){output_species<-output_species[-which(output_species$Parameter=="Mapping_Smooth"),]}
  if(output_species$Value[output_species$Parameter=="Distribution_Presence"]=="1,2"){output_species<-output_species[-which(output_species$Parameter=="Distribution_Presence"),]}
  if(output_species$Value[output_species$Parameter=="Distribution_Origin"]=="1,2"){output_species<-output_species[-which(output_species$Parameter=="Distribution_Origin"),]}
  if(output_species$Value[output_species$Parameter=="Distribution_Seasonal"]=="1,2"){output_species<-output_species[-which(output_species$Parameter=="Distribution_Seasonal"),]}
  output_species$Value[output_species$Parameter=="Col_allfields"]<-paste(names(read.csv(paste0(sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "_sRedList/allfields.csv"))), collapse=",")
  output_species<-subset(output_species, is.na(output_species$Value)==F & output_species$Value != "")
  output_species$Date<-Sys.time() %>% as.character(.)
  
  # Save Outputs
  tryCatch({
    print(paste0("Species_to_record (zip function): ", scientific_name, " by ", username, "_close"))
    
    FileStored<-paste0("Species/Stored_outputs/Stored_", substr(Sys.Date(), 1, 7), ".rds")
    if(file.exists(FileStored)){
      Saved_output<-readRDS(FileStored)
      } else {Saved_output<-read.csv("Species/Output_save_empty.csv")}
    Saved_output<-rbind.fill(Saved_output, output_species)
    saveRDS(Saved_output, FileStored)
  }, error=function(e){cat("TryCatch save output while zipping")})

  # Remove the local files
  unlink(paste0(gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "_sRedList"), recursive=T)
  unlink(paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username)), recursive=T)
  unlink(paste0(gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "_sRedList.zip"), recursive=T)

  # Return
  return(zip_to_extract)
}









# MX: Distribution managing functions (platform buidling) ----------------------------------------------------------------



#* All distributions of paginated species on the platform
#* @get distributions
#* @param start:int start
#* @param end:int end
#* @param filter:string filter element by scientific_name
#* @serializer unboxedJSON
#* @tag sRedList
function(start = 0, end = 10, filter = "") {

Prom<-future({
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
      if (grepl("_Uploaded", fileName)) edit <- TRUE;  # nolint
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

}, gc=T, seed=T)

return(Prom)
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

Prom<-future({
  if (nchar(scientific_name) < 3) {
    invalid_params("You must enter at least 3 characters.")
  }
  species_distribution <- as.data.frame(list.files(config$distribution_path))
  colnames(species_distribution) <- "scientific_name"
  indices <- grep(tolower(scientific_name), tolower(species_distribution$scientific_name))  # nolint

  return(species_distribution[indices, ])

}, gc=T, seed=T)

return(Prom)
}

#* Delete distribution from sRedList platform
#* @delete species/<scientific_name>/distribution
#* @param scientific_name:string Scientific Name
#* @param file_name:string file_name
#* @param Dist_path:string path
#* @param type:string type
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, file_name, Dist_path, type) {

Prom<-future({
  scientific_name <- sRL_decode(scientific_name)
  file_name <- url_decode(file_name)
  Dist_path <- url_decode(Dist_path) %>% paste0(config$distribution_path, .)
  
  if ((scientific_name %in% list.files(config$distribution_path)) && !grepl("_RL", file_name) && !grepl("_RL", Dist_path)) { # nolint
    if (file_name %in% list.files(Dist_path)) {
      sRL_loginfo(paste0("Delete distribution:", Dist_path, '/', file_name), scientific_name) # nolint
      unlink(paste0(Dist_path, '/', file_name), recursive = TRUE)
      if(length(list.files(Dist_path, include.dirs=T))==0){unlink(Dist_path, recursive=T)}
      return(list(response = 0)) # nolint
    }
    not_found("Species distribution does not exist!") # nolint Runs only if the previous return did not work
  }else {
    not_found("Species distribution does not exist!") # nolint
  }
  
}, gc=T, seed=T)

return(Prom)
}

#* Get distributions species from sRedList platform excluding GBIF distributions
#* @get species/<scientific_name>/distributions
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name) {
Prom<-future({
    
  #Filter param
  scientific_name <- sRL_decode(scientific_name)
  
  if (scientific_name %in% list.files(config$distribution_path)) { # nolint
    
    # Create empty list and order distributions with RL first
    distributions <- list()
    DistList<-list.files(paste0(config$distribution_path, scientific_name)) %>% .[order(grepl("_RL", .), decreasing=T)]
    
    # Fill in for each distribution
    for(distributionFolder in DistList) { # nolint
      # GBIF distributions cannot be selected
      #if (!grepl("_GBIF", distributionFolder)){
        files <- list();
        directorySize <- 0;
        for(fileName in list.files(paste0(config$distribution_path, scientific_name, "/", distributionFolder))) {  # nolint
          fileSize <- (file.info(paste0(config$distribution_path, scientific_name, "/", distributionFolder, "/", fileName))$size) / 1024 # nolint 
          fileCreated <- (file.info(paste0(config$distribution_path, scientific_name, "/", distributionFolder, "/", fileName))$ctime) # nolint 
          metadata <- NULL
          if (file_ext(fileName) == "json") {
            metadata = jsonlite::read_json(paste0(config$distribution_path, scientific_name, "/", distributionFolder, "/", fileName), simplifyVector = FALSE)  # nolint 
          }
          files <- append(files, list(
            list(
              data = list(
                name = fileName,
                size = fileSize,
                created = fileCreated,
                path = distributionFolder,
                type = "file",
                metadata=metadata)
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
      #}
    }
    
    # Return
    return(distributions)
    
  }else {return(list())} # nolint
  
}, gc=T, seed=T)
  
return(Prom)
}




#* Merge ZIP files ----
#* @post assessment/merge-zip
#* @param Uploaded_Zips:[file] A zip files
#* @serializer  contentType list(type="application/octet-stream")
#* @tag sRedList
function(Uploaded_Zips=list(), res) {

Prom<-future({
  print(names(Uploaded_Zips))

  # Error if not all zips
  extensions<-substr(names(Uploaded_Zips), nchar(names(Uploaded_Zips))-3, nchar(names(Uploaded_Zips))) %>% unique(.)
  if(length(extensions)>1 | !".zip" %in% extensions){wrong_zip_extension()}
  
  # Create a folder to store results
  sRL_loginfo("Unzip files", "Merge ZIP API")
  Zip_Path<-paste0("Unzipped", sample(1:1000,1))
  unlink(Zip_Path, recursive=T)
  dir.create(Zip_Path)
  
  # Unzip loop
  for(i in 1:length(Uploaded_Zips)){
    writeBin(object=Uploaded_Zips[[i]], con=paste0(Zip_Path, "/Zip", i, ".zip"))
    unzip(paste0(Zip_Path, "/Zip", i, ".zip"), exdir=paste0(Zip_Path, "/ZIP", i))
    unlink(paste0(Zip_Path, "/Zip", i, ".zip"))
  }
  
  ### MERGE
  # Merge allfields
  sRL_loginfo("Merge allfields", "Merge ZIP API")
  All_files<-list.files(Zip_Path, recursive = T)[grepl('allfields.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  if(length(All_files[grepl("allfields", All_files)])<length(list.files(Zip_Path, recursive=F))){missing_allfields()}
  
  eval(parse(text=
               paste0("allfieldsM<-rbind.fill(",
                      paste0("read.csv(All_files[", 1:length(All_files), "])", collapse=','),")"
               )))
  if("TRUE" %in% duplicated(allfieldsM$internal_taxon_name)){duplicate_species()}
  
  # Merge habitats
  sRL_loginfo("Merge habitats", "Merge ZIP API")
  Hab_files<-list.files(Zip_Path, recursive = T)[grepl('habitats.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .) %>% subset(., .!= paste0(Zip_Path, "/"))
  
  if(length(Hab_files)>0){
    eval(parse(text=
               paste0("habitatsM<-rbind.fill(",
                      paste0("read.csv(Hab_files[", 1:length(Hab_files), "])", collapse=','),")"
               )))
  }
  
  # Merge countries
  sRL_loginfo("Merge countries", "Merge ZIP API")
  Coun_files<-list.files(Zip_Path, recursive = T)[grepl('countries.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .) %>% subset(., .!= paste0(Zip_Path, "/"))
  
  if(length(Coun_files)>0){
    eval(parse(text=
               paste0("countriesM<-rbind.fill(",
                      paste0("read.csv(Coun_files[", 1:length(Coun_files), "])", collapse=','),")"
               )))
  }
  
  # Merge references
  sRL_loginfo("Merge references", "Merge ZIP API")
  Ref_files<-list.files(Zip_Path, recursive = T)[grepl('references.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("referencesM<-rbind.fill(",
                      paste0("read.csv(Ref_files[", 1:length(Ref_files), "])", collapse=','),")"
               )))
  
  # Merge assessments
  sRL_loginfo("Merge assessments", "Merge ZIP API")
  Ass_files<-list.files(Zip_Path, recursive = T)[grepl('assessments.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("assessmentsM<-rbind.fill(",
                      paste0("read.csv(Ass_files[", 1:length(Ass_files), "])", collapse=','),")"
               )))
  
  # Merge log
  sRL_loginfo("Merge logs", "Merge ZIP API")
  Log_files<-list.files(Zip_Path, recursive = T)[grepl('00.Output_log.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
  
  eval(parse(text=
               paste0("LogM<-rbind.fill(",
                      paste0("read.csv(Log_files[", 1:length(Log_files), "])", collapse=','),")"
               )))
  
  
  # Merge Distributions
  sRL_loginfo("Merge distributions", "Merge ZIP API")
  if(TRUE %in% grepl('_Distribution.shp', list.files(Zip_Path, recursive = T))){
    
    Dist_files<-list.files(Zip_Path, recursive = T)[grepl('_Distribution.shp', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
    
    eval(parse(text=
                 paste0("DistM<-dplyr::bind_rows(",
                        paste0("st_read(Dist_files[", 1:length(Dist_files), "])", collapse=','),")"
                 )))
  }
  
  # Merge Occurrences
  sRL_loginfo("Merge occurrences", "Merge ZIP API")
  if(TRUE %in% grepl('_Occurrences.csv', list.files(Zip_Path, recursive = T))){
    
    Occ_files<-list.files(Zip_Path, recursive = T)[grepl('_Occurrences.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
    
    eval(parse(text=
                 paste0("OccM<-rbind.fill(",
                        paste0("read.csv(Occ_files[", 1:length(Occ_files), "])", collapse=','),")"
                 )))
  }
  
  # Merge Hydrobasins
  sRL_loginfo("Merge hydrobasins", "Merge ZIP API")
  if(TRUE %in% grepl('_Hydrobasins.csv', list.files(Zip_Path, recursive = T))){
    
    Hydro_files<-list.files(Zip_Path, recursive = T)[grepl('_Hydrobasins.csv', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .)
    
    eval(parse(text=
                 paste0("HydroM<-rbind.fill(",
                        paste0("read.csv(Hydro_files[", 1:length(Hydro_files), "])", collapse=','),")"
                 )))
  }
  
  # If duplicate internal_taxon_id, I replace the second by 1
  sRL_loginfo("Deal with duplicates in taxon_id", "Merge ZIP API")
  while("TRUE" %in% duplicated(allfieldsM$internal_taxon_id)){
    
    N_it<-ifelse(exists("N_it"), N_it+1, 1)
    Species_to_fix<-allfieldsM$internal_taxon_name[duplicated(allfieldsM$internal_taxon_id)][1]
    sRL_loginfo("START - Fixing duplicated internal taxon id ", Species_to_fix)
    
    allfieldsM$internal_taxon_id[allfieldsM$internal_taxon_name==Species_to_fix]<-N_it
    referencesM$internal_taxon_id[referencesM$internal_taxon_name==Species_to_fix]<-N_it
    if(exists("habitatsM")){habitatsM$internal_taxon_id[habitatsM$internal_taxon_name==Species_to_fix]<-N_it}
    if(exists("countriesM")){countriesM$internal_taxon_id[countriesM$internal_taxon_name==Species_to_fix]<-N_it}
    if(exists("assessmentsM")){assessmentsM$internal_taxon_id[assessmentsM$internal_taxon_name==Species_to_fix]<-N_it}
    if(exists("DistM")){DistM$id_no[DistM$sci_name==Species_to_fix]<-N_it}
    if(exists("OccM")){OccM$id_no[OccM$sci_name==Species_to_fix]<-N_it}
    if(exists("HydroM")){HydroM$id_no[HydroM$sci_name==Species_to_fix]<-N_it}
    
    sRL_loginfo("END - Fixing duplicated internal taxon id ", Species_to_fix)
    
  }
  
  # Copy reports
  sRL_loginfo("Copy reports", "Merge ZIP API")
  Reports<-list.files(Zip_Path, recursive = T)[grepl('sRedList_report_', list.files(Zip_Path, recursive = T))] %>% paste0(Zip_Path, "/", .) %>% subset(., .!= paste0(Zip_Path, "/"))
  file.copy(from=Reports, to=sapply(strsplit(Reports, "/"), function(x){paste(x[1], x[length(x)], sep="/")}))
  
  
  ### Save in a merged ZIP file
  sRL_loginfo("Save ZIP", "Merge ZIP API")
  # Remove initial files
  unlink(paste0(Zip_Path, "/", list.files(Zip_Path)[!grepl("sRedList_report", list.files(Zip_Path))]), recursive=T)
  write.csv(replace(allfieldsM, is.na(allfieldsM), ""), paste0(Zip_Path, "/allfields.csv"), row.names = F)
  write.csv(replace(assessmentsM, is.na(assessmentsM), ""), paste0(Zip_Path, "/assessments.csv"), row.names = F)
  if(exists("countriesM")){write.csv(replace(countriesM, is.na(countriesM), ""), paste0(Zip_Path, "/countries.csv"), row.names = F)}
  write.csv(replace(referencesM, is.na(referencesM), ""), paste0(Zip_Path, "/references.csv"), row.names = F)
  if(exists("habitatsM")){write.csv(replace(habitatsM, is.na(habitatsM), ""), paste0(Zip_Path, "/habitats.csv"), row.names = F)}
  write.csv(LogM, paste0(Zip_Path, "/00.Output_log.csv"), row.names = F)
  
  # Save distribution and occurrences if from GBIF
  if("DistM" %in% ls()){
    st_write(DistM, paste0(Zip_Path, "/sRedList_Distribution.shp"), append=F)
    write.csv(OccM, paste0(Zip_Path, "/sRedList_Occurrences.csv"), row.names=F)
    if(exists("HydroM")){write.csv(HydroM, paste0(Zip_Path, "/sRedList_Hydrobasins.csv"), row.names=F)}
  }
  
  
  # Zip that folder and delete it
  Zip_name<-Sys.time() %>% gsub("-", "_", .) %>% gsub(" ", "_", .) %>% gsub (":", "_", .) %>% paste0(Zip_Path, "/sRedList_mergedZIP_", ., ".zip")
  zip(zipfile = Zip_name, files = paste0(Zip_Path, "/", list.files(Zip_Path)), extras='-j')
  zip_to_extract<-readBin(Zip_name, "raw", n = file.info(Zip_name)$size)
  unlink(Zip_Path, recursive=T)
  print(Zip_name)
  
  # Track Merge ZIP usage
  sRL_loginfo("Track usage", "Merge ZIP API")
  tryCatch({
    File_track<-"Species/Stored_outputs/Stored_ZIP.csv"
    if(! file.exists(File_track)){write.csv(data.frame(Date=NA, Nspc=NA, DupliID=NA), File_track, row.names=F)}
    track<-read.csv(File_track)
    track[(nrow(track)+1),]<-c(as.character(Sys.Date()), length(All_files), exists("N_it"))
    write.csv(track, File_track, row.names=F)
  }, error=function(e){cat("TryCatch track mergeZIP failed")})
  
  return(zip_to_extract)

}, gc=T, seed=T)  %>% then(onRejected=function(err){

                              res$setHeader("Access-Control-Expose-Headers","Content-Disposition")
                              res$setHeader("Content-Disposition", "attachment; error.txt")
                              

                              print(paste0("ERROR TO RETURN: ", err))
                              NAM<-paste0("ERROR", sample(1:50, size=1),".txt")
                              writeLines(as.character(err), NAM)
                              ERR_TO_RET<-readBin(NAM, "raw", n=file.info(NAM)$size)
                              unlink(NAM)
                              return(ERR_TO_RET)
                              })
                              
return(Prom) 
}



