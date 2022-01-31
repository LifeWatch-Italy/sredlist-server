#Extensions File Distribution
extensions <- list("shp", "shx", "prj", "dbf", "cpg")

#Load ESA land cover data (in 2010 and 2020) and altitude data
alt <- raster(config$alt_raster_path)
cci1 <- raster(config$cci1_raster_path)
cci2 <- raster(config$cci2_raster_path)

#Load Crosswalk CSV and Density CSV
density<-read.csv("Species/Density.table.csv", sep=",") ; names(density)[1]<-"Species" ; # nolint
crosswalk <- read.csv("Species/Crosswalk_CCI_IUCN.csv")
cci_classes <- data.frame(Classes = c(levels(as.factor(crosswalk$esa_code)), 220)) # nolint


#* Upload Distribution species
#* @post species/<scientific_name>/distribution
#* @param req:file Distribution file (shp,.shx,.prj,.dbf,.cpg)
#* @param scientific_name:str Insert Species Name
#* @serializer unboxedJSON
#* @parser multi
#* @tag sRedList
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
#* @tag sRedList
function(scientific_name, path = "") {
  scientific_name <- url_decode(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  distributions <- read_distribution(scientific_name, path)
  distSP <- subset(distributions, distributions$binomial == scientific_name) # nolint
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
#* @tag sRedList
function(scientific_name, presences = list(), seasons = list() , origins = list(), path = "") { # nolint
  #Filter param
  scientific_name <- url_decode(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  if (length(presences) != 0) presences <- as.character(presences);
  if (length(seasons) != 0) seasons <- as.character(seasons);
  if (length(origins) != 0) origins <- as.character(origins);
  
  #Load Map countries
  distCountries <- read_map_countries()
  #Load Distribution Species
  distributions <- read_distribution(scientific_name, path)
  distSP_full <- subset(distributions, distributions$binomial == scientific_name) # nolint 
  choice_presence <- c(presences)
  choice_season <- c(seasons)
  choice_origin <- c(origins)

  distSP <- subset(distSP_full, # nolint
    distSP_full$presence %in% choice_presence &
    distSP_full$seasonal %in% choice_season &
    distSP_full$origin %in% choice_origin)

  if (nrow(distSP) > 0) { 
    distSP$cols <- NA
    distSP$cols <- revalue(as.character(distSP$presence), c("1"=NA, "2"=NA, "3"=NA, "4"="mistyrose1", "5"="brown4", "6"="gray70")) # nolint    if (nrow(distSP) > 0) {
    for (i in which(is.na(distSP$cols))) {
        if (distSP$origin[i] == "1") {
          if(distSP$seasonal[i] == "1"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#d95f02", "2"="#fc8d62", "3"="#fc8d62"))} # nolint
          if(distSP$seasonal[i] == "2"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#1b9e77", "2"="#66c2a5", "3"="#66c2a5"))} # nolint
          if(distSP$seasonal[i] == "3"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#7570b3", "2"="#8da0cb", "3"="#8da0cb"))} # nolint
          if(distSP$seasonal[i] == "4"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="yellowgreen", "2"="yellow2", "3"="yellow2"))} # nolint
          if(distSP$seasonal[i] == "5"){ distSP$cols[i]<-"gray70"}
        } else{
          distSP$cols[i]<-revalue(as.character(distSP$origin[i]), c("2"="darkorchid1", "3"="darkorchid4", "4"="darkseagreen3", "5"="gray70", "6"="darkorchid1")) # nolint
        }   # nolint
    }
  }
  sf::sf_use_s2(FALSE)
  if (nrow(distSP) > 0) {
  return(plot(ggplot() +
              geom_sf(data = st_buffer(st_crop(distCountries, extent(distSP)),0), fill="white", col="gray50") + # nolint
              geom_sf(data = distSP, fill = distSP$cols) +
              theme_void() +
              ggtitle("Distribution")))
  } else{
    sf::sf_use_s2(FALSE)
    return(plot(ggplot() +
                geom_sf(data = st_buffer(st_crop(distCountries, extent(distSP_full)),0), fill="white", col="gray50") + # nolint
                theme_void() +
                ggtitle("The distribution is empty")))
  }
}


#* Global Biodiversity Information Facility
#* @get species/<scientific_name>/gbif
#* @param scientific_name:string Scientific Name
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name) {
    #Clean-string from user
    scientific_name <- url_decode(scientific_name)
    scientific_name <- R.utils::capitalize(trim(gsub("[[:punct:]]", " ", scientific_name))) # nolint
    print(scientific_name)
    #GBIF procedure createDataGBIF, cleanDataGBIF in utils.R
    log_info("START - Create data")
    dat <- createDataGBIF(scientific_name)
    wm <- borders("world", colour = "gray50", fill = "gray50")
    ggsave("plot_data.png", plot(ggplot() + coord_fixed() + wm + geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 0.5) + theme_bw())) # nolint
    plot1 <- base64enc::dataURI(file = "plot_data.png", mime = "image/png")
    log_info("END - Create data")
    log_info("START - Clean coordinates")

    #Convert country code from ISO2c to ISO3c
    dat$countryCode <-  countrycode::countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c') # nolint
    #Flag observations to remove and subset: in utils.R
    dat <- data.frame(dat)
    flags <- cleanDataGBIF(dat)
    ggsave("clean_coordinates.png", plot(plot(flags, lon = "decimalLongitude", lat = "decimalLatitude"))) # nolint
    plot2 <- base64enc::dataURI(file = "clean_coordinates.png", mime = "image/png") # nolint
    log_info("END - Clean coordinates")

    # Calculates EOO and assess Red List criterion B1
    log_info("START - Calculates EOO and assess Red List criterion B1")
    dat_cl <- dat[flags$.summary,]
    gbif_data_number <- nrow(dat_cl)
    dat_proj <- SpatialPoints(cbind(dat_cl$decimalLongitude, dat_cl$decimalLatitude), proj4string=CRS("+proj=longlat +datum=WGS84")) %>% spTransform(., CRSobj = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")) #nolint
    ll <- data.frame(y = coordinates(dat_proj)[,2], x = coordinates(dat_proj)[,1]) # nolint
    EOO_km2 <- round(EOOarea(ll)/1000000) # nolint
    EOO_rating <- EOORating(EOO_km2) # nolint

    log_info("END - Calculates EOO and assess Red List criterion B1")

    #Plot EOO
    log_info("START - Plot EOO")
    EOO <- st_convex_hull(st_combine(st_as_sf(dat_proj))) # nolint
    EOO<-st_as_sf(st_convex_hull(st_combine(st_as_sf(dat_proj))))
    #EOO$binomial <- "Papilio sosia"
    EOO$binomial <- as.character(scientific_name)
    #Save EOO distribution: Utils.R
    gbif_path <- saveEooDistribution(scientific_name, EOO, gbif_data_number)
    ggsave("eoo.png", plot(ggplot() + geom_sf(data = EOO) + geom_sf(data = st_as_sf(dat_proj)) + ggtitle(paste0("EOO of ", scientific_name)))) # nolint
    plot3 <- base64enc::dataURI(file = "eoo.png", mime = "image/png") # nolint
    log_info("END - Plot EOO")

    file.remove("plot_data.png")
    file.remove("clean_coordinates.png")
    file.remove("eoo.png")
    return(list(
        plot_data = plot1,
        plot_clean_coordinates = plot2,
        eoo_km2 = EOO_km2,
        eoo_rating = EOO_rating,
        plot_eoo = plot3,
        gbif_data_number  = gbif_data_number,
        gbif_path = gbif_path
        ));

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
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  if (length(presences) != 0) presences <- as.character(presences);
  if (length(seasons) != 0) seasons <- as.character(seasons);
  if (length(origins) != 0) origins <- as.character(origins);
  
  #Load Distribution Species
  distributions <- read_distribution(scientific_name, path)

  distSP_full <- subset(distributions, distributions$binomial == scientific_name) # nolint    
  choice_presence <- c(presences)
  choice_season <- c(seasons)
  choice_origin <- c(origins)

  distSP <- subset(distSP_full, # nolint
    distSP_full$presence %in% choice_presence &
    distSP_full$seasonal %in% choice_season &
    distSP_full$origin %in% choice_origin)

  # Combine polygons in a single shapefile, that will be used in analyses
  distSP <- distSP %>% dplyr::group_by(binomial) %>% dplyr::summarise(N = n())
  range <- st_transform(distSP, crs(alt))

  #Plot EOO
  log_info("START - Plot EOO")
  EOO <- st_convex_hull(range)
  ggsave(paste0(scientific_name, ".png"), plot(ggplot() + geom_sf(data=EOO, fill="darkred", col=NA) + geom_sf(data=range, fill="coral2", col=NA) + ggtitle(paste0("EOO of ", scientific_name)))) # nolint
  plot3 <- base64enc::dataURI(file = paste0(scientific_name, ".png"), mime = "image/png") # nolint
  log_info("END - Plot EOO")
  file.remove(paste0(scientific_name, ".png"))
  EOO_km2 <- round(as.numeric(st_area(EOO))/1000000)
  return(list(
        eoo_km2 = EOO_km2,
        plot_eoo = plot3
        ));

}


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

#* Estimate the Area of Habitat (AOH)
#* @get species/<scientific_name>/analysis/aoh
#* @param scientific_name:string Scientific Name
#* @param presences:[int] presences (1, 2)
#* @param seasons:[int] seasons (1, 2)
#* @param origins:[int] origins (1, 2)
#* @param habitats_pref:[str] habitats_pref
#* @param altitudes_pref:[int] altitudes_pref
#* @param density_pref:int density_pref
#* @param density_pref:int density_pref
#* @param isGbifDistribution:boolean isGbifDistribution
#* @param path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, presences = list(), seasons = list() , origins = list(), habitats_pref= list(), altitudes_pref= list(), density_pref= -1, isGbifDistribution = FALSE, path = "") { # nolint    
  #Filter param
  scientific_name <- url_decode(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  if (length(presences) != 0) presences <- as.character(presences);
  if (length(seasons) != 0) seasons <- as.character(seasons);
  if (length(origins) != 0) origins <- as.character(origins);

  habitats_pref <- as.character(habitats_pref)
  altitudes_pref <- as.integer(altitudes_pref)
  density_pref <- as.integer(density_pref)

  print(presences)
  print(seasons)
  print(origins)
  print(habitats_pref)
  print(altitudes_pref)
  print(density_pref)
  print(isGbifDistribution)
  
  #Load Distribution Species
  distributions <- read_distribution(scientific_name, path)

  distSP_full <- subset(distributions, distributions$binomial == scientific_name) # nolint    
  choice_presence <- c(presences)
  choice_season <- c(seasons)
  choice_origin <- c(origins)

  # isGbifDistribution <- TRUE
  if (isGbifDistribution == FALSE) {
  distSP <- subset(distSP_full, # nolint
    distSP_full$presence %in% choice_presence &
    distSP_full$seasonal %in% choice_season &
    distSP_full$origin %in% choice_origin)
  }else {
    distSP <- st_transform(distSP_full, crs(cci2))
  }
  # Combine polygons in a single shapefile, that will be used in analyses
  distSP <- distSP %>% dplyr::group_by(binomial) %>% dplyr::summarise(N = n())
  range <- st_transform(distSP, crs(alt))

  #species habitat preferences
  crosswalk$species_pref <- as.numeric(crosswalk$iucn_code %in% habitats_pref)

  print("ok1")

  #Restrict to land cover categories that are in species habitat preferences (takes all ESA categories that are named against the habitat preferences of the species)
  cci_classes$SP <- cci_classes$Classes %in% crosswalk$esa_code[crosswalk$species_pref == 1] %>% as.numeric(.) # nolint 

  print("ok1.1")
  cci_classes$Classes <- as.numeric(as.character(cci_classes$Classes))
  print("ok1.2")
  cci_classes$SP <- as.numeric(as.character(cci_classes$SP))
  print("ok1.3")
  cci2_sp <- crop(cci2, extent(distSP)) # nolint 
  print("ok1.4")
  cci2_sp <- reclassify(cci2_sp, as.matrix(cci_classes)) # nolint

  print("ok2")

  #Restrict by species altitude preferences (if they are known). We downscale altitude data to fit with the resolution of Land Cover. # nolint

  if (is.na(altitudes_pref[1])) {
    altitudes_pref[1] <- (-10)
    } # If there are no minimal value provided, we consider -10 so that every altitude is higher # nolint
  if (is.na(altitudes_pref[2])) {
    altitudes_pref[2] <- (10000)
    } # If there are no maximal value provided, we consider 10000 so that every altitude is higher # nolint

  print("ok3")

  alt_mat <- matrix(c(-11, altitudes_pref[1], 0, altitudes_pref[1], altitudes_pref[2], 1, altitudes_pref[2], 10001, 0), byrow=T, ncol=3)

  print("ok4")

  alt_sp <- crop(alt, extent(range))

  alt_sp <- reclassify(alt_sp, alt_mat)

  print("ok5")

  alt_spPROJ <- projectRaster(alt_sp, cci2_sp) > 0 # Becomes a 1 wherever there was suitable altitude

  #Map AOH (reproject altitude raster, overlap with distribution and sum)

  AOH0 <- (cci2_sp + alt_spPROJ) == 2

  AOH <- mask(AOH0, distSP)

  log_info("START - Plot EOO")

  plot1 <- gplot(AOH) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values=c("#dfc27d", "#018571"), labels=c("Unsuitable", "Suitable", ""), name="", na.translate=F) +
  ggtitle("Area of Habitat") +
  theme_void() %+replace%   theme(plot.title=element_text(hjust=0.5, size=14, face="bold"))

  ggsave(filename = "aoh.png", plot = plot(plot1))
  
  plot1 <- base64enc::dataURI(file = "aoh.png", mime = "image/png", encoding = "base64") # nolint
  file.remove("aoh.png")
  log_info("END - Plot EOO")

  AOH_km2 <- 0.09*exactextractr::exact_extract(AOH, range, fun="sum") # This is an approximation (resolution in degree), will have to be improved at some point
  
  #Calculate Area of Habitat in a resolution of 2x2km (as is the map of altitude provided), in order to use it as an upper bound of species Area of Occupancy under criterion B2 (each cell covers 4km2)

  AOO <- projectRaster(AOH, alt_sp) > 0

  plot2 <- gplot(AOO) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values=c("#dfc27d", "#018571"), labels=c("Unsuitable", "Suitable"), name="", na.translate=F) +
  ggtitle("Area of Habitat (2x2km)") +
  theme_void() %+replace%   theme(plot.title=element_text(hjust=0.5, size=14, face="bold"))

  ggsave(filename = "aoo.png", plot = plot(plot2))
  plot2 <- base64enc::dataURI(file = "aoo.png", mime = "image/png", encoding = "base64") # nolint
  file.remove("aoo.png")

  AOO_km2<-4*exactextractr::exact_extract(AOO, range, fun="sum")

  #Calculate population size
  if (density_pref != -1) {
    #TODO: If not present in CSV calculate populution size
    #density_sp <- density$Density[density$Species == scientific_name]
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
#* @param isGbifDistribution:boolean isGbifDistribution
#* @param path:string Distribution Folder default RedList
#* @serializer unboxedJSON
#* @tag sRedList
function(scientific_name, presences = list(), seasons = list() , origins = list(), habitats_pref= list(), altitudes_pref= list(), isGbifDistribution = FALSE, path = "") { # nolint
  #Filter param
  scientific_name <- url_decode(scientific_name)
  path <- ifelse(path == "", paste0(R.utils::capitalize(trim(gsub(" ", "_", scientific_name))), '_RL'), path ) # nolint
  if (length(presences) != 0) presences <- as.character(presences);
  if (length(seasons) != 0) seasons <- as.character(seasons);
  if (length(origins) != 0) origins <- as.character(origins);

  habitats_pref <- as.character(habitats_pref)
  altitudes_pref <- as.integer(altitudes_pref)

  print(presences)
  print(seasons)
  print(origins)
  print(habitats_pref)
  print(altitudes_pref)
  
  #Load Distribution Species
  distributions <- read_distribution(scientific_name, path)

  distSP_full <- subset(distributions, distributions$binomial == scientific_name) # nolint    
  choice_presence <- c(presences)
  choice_season <- c(seasons)
  choice_origin <- c(origins)

  #isGbifDistribution <- TRUE
  if (isGbifDistribution == FALSE) {
  distSP <- subset(distSP_full, # nolint
    distSP_full$presence %in% choice_presence &
    distSP_full$seasonal %in% choice_season &
    distSP_full$origin %in% choice_origin)
  }else {
    distSP <- st_transform(distSP_full, crs(cci2))
  }

  # Combine polygons in a single shapefile, that will be used in analyses
  distSP <- distSP %>% dplyr::group_by(binomial) %>% dplyr::summarise(N = n())
  range <- st_transform(distSP, crs(alt))

  #species habitat preferences
  crosswalk$species_pref <- as.numeric(crosswalk$iucn_code %in% habitats_pref)

  #Restrict to land cover categories that are in species habitat preferences (takes all ESA categories that are named against the habitat preferences of the species)
  cci_classes$SP <- cci_classes$Classes %in% crosswalk$esa_code[crosswalk$species_pref == 1] %>% as.numeric(.) # nolint 

  cci_classes$Classes <- as.numeric(as.character(cci_classes$Classes))
  cci_classes$SP <- as.numeric(as.character(cci_classes$SP))

  cci2_sp <- crop(cci2, extent(distSP)) # nolint 

  cci2_sp <- reclassify(cci2_sp, as.matrix(cci_classes)) # nolint


  #Restrict by species altitude preferences (if they are known). We downscale altitude data to fit with the resolution of Land Cover. # nolint

  if (is.na(altitudes_pref[1])) {
    altitudes_pref[1] <- (-10)
    } # If there are no minimal value provided, we consider -10 so that every altitude is higher # nolint
  if (is.na(altitudes_pref[2])) {
    altitudes_pref[2] <- (10000)
    } # If there are no maximal value provided, we consider 10000 so that every altitude is higher # nolint

  alt_mat <- matrix(c(-11, altitudes_pref[1], 0, altitudes_pref[1], altitudes_pref[2], 1, altitudes_pref[2], 10001, 0), byrow=T, ncol=3)

  alt_sp <- crop(alt, extent(range))

  alt_sp <- reclassify(alt_sp, alt_mat)

  alt_spPROJ <- projectRaster(alt_sp, cci2_sp) > 0 # Becomes a 1 wherever there was suitable altitude

  #Map AOH (reproject altitude raster, overlap with distribution and sum)

  AOH0 <- (cci2_sp + alt_spPROJ) == 2

  AOH <- mask(AOH0, distSP)

  AOH_km2 <- 0.09*exactextractr::exact_extract(AOH, range, fun="sum") # This is an approximation (resolution in degree), will have to be improved at some point

  #Map old AOH and overlap with distribution
  cci1_sp<-crop(cci1, extent(distSP))
  cci1_sp<-reclassify(cci1_sp, as.matrix(cci_classes))

  AOH0_old<-(cci1_sp+alt_spPROJ)==2
  AOH_old<-mask(AOH0_old, distSP)
  AOH_old_km2<-0.09*exactextractr::exact_extract(AOH_old, range, fun="sum")

  #Calculate trends in AOH and plot
  AOH_lost<- (1-(AOH_km2/AOH_old_km2))

  plot1 <- gplot((AOH*2+3)-AOH_old) + 
  geom_tile(aes(fill = as.factor(value)))+
  scale_fill_manual(values=c("#a6611a", "gray90", "#80cdc1", "#018571"), labels=c("Newly unsuitable", "Unsuitable", "Suitable", "Newly suitable"), name="", na.translate=F)+
  ggtitle("Trends in Area of Habitat")+
  theme_void() %+replace%   theme(plot.title=element_text(hjust=0.5, size=14, face="bold"))

  ggsave(filename = "trends-aoh.png", plot = plot(plot1))
  plot1 <- base64enc::dataURI(file = "trends-aoh.png", mime = "image/png", encoding = "base64") # nolint
  file.remove("trends-aoh.png")

  return(list(
        aoh_lost_km2 = round(AOH_old_km2),
        aoh_lost = AOH_lost * 100,
        plot_trends_aoh = plot1
        ));

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
function(scientific_name, aoh_lost, eoo_km2, aoo_km2, pop_size) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  AOH_lost <- round(as.numeric(aoh_lost), 3)
  EOO_km2 <-  round(as.numeric(eoo_km2))
  AOO_km2 <-  round(as.numeric(aoo_km2))
  Pop_size <-  round(as.numeric(pop_size))
  if (Pop_size == -1) {
    Pop_size <- as.numeric(NaN)
  }
  log_info("AOH_lost", AOH_lost, "EOO_km2", EOO_km2, "AOO_km2", AOO_km2, "Pop_size", Pop_size)

  criteria <- data.frame(Crit=c("A2", "B1", "B2", "C1", "D"), Value=NA) ; criteria$Value=factor(criteria$Value, c("LC/NT", "VU", "EN", "CR")) # nolint
  criteria$Value[criteria$Crit=="A2"] <- cut(AOH_lost, breaks=c(-Inf, 0.3, 0.5, 0.8, 1), labels=c("LC/NT", "VU", "EN", "CR")) # nolint
  criteria$Value[criteria$Crit=="B1"] <- cut(EOO_km2, breaks=c(0, 100, 5000, 20000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR"))) # nolint
  criteria$Value[criteria$Crit=="B2"] <- cut(AOO_km2, breaks=c(0, 10, 500, 2000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR"))) # nolint
  criteria$Value[criteria$Crit=="C1"] <- min(
    as.numeric(cut(Pop_size, breaks=c(0, 250, 2500, 10000, Inf), labels=rev(c(1,2,3,4)))),  # nolint
    as.numeric(cut(AOH_lost, breaks=c(-Inf, 0.1, 0.2, 0.25, 1), labels=rev(c(1,2,3,4))))) %>% as.character(.) %>% revalue(., c("1"="LC/NT", "2"="VU", "3"="EN", "4"="CR")) # Note for later, the timeframe applied here for CR and EN species is not in line with guidelines)  # nolint
  criteria$Value[criteria$Crit=="D"]<-cut(Pop_size, breaks=c(0, 50, 250, 1000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR")))  # nolint

  return(plot(ggplot(criteria) +
    geom_point(aes(x = Value, y = Crit, col = Value), size = 10, show.legend=F) +
    scale_x_discrete(drop = F) + scale_y_discrete(drop=F) +
    scale_colour_manual(drop = F, values=c("#60c659ff", "#f9e814ff", "#fc7f3fff", "#d81e05ff"))))

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
function(scientific_name, aoh_lost, eoo_km2, aoo_km2, pop_size, res) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  AOH_lost <- round(as.numeric(aoh_lost), 3)
  EOO_km2 <-  round(as.numeric(eoo_km2))
  AOO_km2 <-  round(as.numeric(aoo_km2))

  criteria <- data.frame(Crit=c("A2", "B1", "B2", "C1", "D"), Value=NA) ; criteria$Value=factor(criteria$Value, c("LC/NT", "VU", "EN", "CR")) # nolint
  criteria$Value[criteria$Crit=="A2"] <- cut(AOH_lost, breaks=c(-Inf, 0.3, 0.5, 0.8, 1), labels=c("LC/NT", "VU", "EN", "CR")) # nolint
  criteria$Value[criteria$Crit=="B1"] <- cut(EOO_km2, breaks=c(0, 100, 5000, 20000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR"))) # nolint
  criteria$Value[criteria$Crit=="B2"] <- cut(AOO_km2, breaks=c(0, 10, 500, 2000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR"))) # nolint


  Pop_size <-  round(as.numeric(pop_size))

  if (Pop_size == -1) {
    Pop_size <- as.numeric(NaN)
    criteria$Value[criteria$Crit=="C1"] <- NA # Note for later, the timeframe applied here for CR and EN species is not in line with guidelines)  # nolint
    criteria$Value[criteria$Crit=="D"]<- NA  # nolint
  } else{
  criteria$Value[criteria$Crit=="C1"] <- min(
    as.numeric(cut(Pop_size, breaks=c(0, 250, 2500, 10000, Inf), labels=rev(c(1,2,3,4)))),  # nolint
    as.numeric(cut(AOH_lost, breaks=c(-Inf, 0.1, 0.2, 0.25, 1), labels=rev(c(1,2,3,4))))) %>% as.character(.) %>% revalue(., c("1"="LC/NT", "2"="VU", "3"="EN", "4"="CR")) # Note for later, the timeframe applied here for CR and EN species is not in line with guidelines)  # nolint
  criteria$Value[criteria$Crit=="D"]<-cut(Pop_size, breaks=c(0, 50, 250, 1000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR")))  # nolint  #TODO: Error when Pop_size == NaN
  }

  df <- data.frame(
    Species = scientific_name,
    Date_processed = Sys.time(),
    EOO = EOO_km2,
    AOO = AOO_km2,
    "Percentage_of_habitat_lost" = AOH_lost,
    Pop.size = Pop_size,
    Criterias = paste0(criteria$Value, " (", criteria$Crit, ")") %>% paste(., collapse="; "),
    Highest_category = criteria$Value[which(as.numeric(criteria$Value)==max(as.numeric(criteria$Value), na.rm=T))] %>% unique()
  )

  filename <- paste0('assessment-', scientific_name, '-', Sys.Date(), '.csv') 
  pathToSaveAssessment <- paste0("Assessments/", filename)

  write.csv(df, pathToSaveAssessment, row.names = F)
  return(df)
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
function(scientific_name, aoh_lost, eoo_km2, aoo_km2, pop_size) {
  #Filter param
  scientific_name <- url_decode(scientific_name)
  AOH_lost <- round(as.numeric(aoh_lost), 3)
  EOO_km2 <-  round(as.numeric(eoo_km2))
  AOO_km2 <-  round(as.numeric(aoo_km2))

  criteria <- data.frame(Crit=c("A2", "B1", "B2", "C1", "D"), Value=NA) ; criteria$Value=factor(criteria$Value, c("LC/NT", "VU", "EN", "CR")) # nolint
  criteria$Value[criteria$Crit=="A2"] <- cut(AOH_lost, breaks=c(-Inf, 0.3, 0.5, 0.8, 1), labels=c("LC/NT", "VU", "EN", "CR")) # nolint
  criteria$Value[criteria$Crit=="B1"] <- cut(EOO_km2, breaks=c(0, 100, 5000, 20000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR"))) # nolint
  criteria$Value[criteria$Crit=="B2"] <- cut(AOO_km2, breaks=c(0, 10, 500, 2000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR"))) # nolint


  Pop_size <-  round(as.numeric(pop_size))

  if (Pop_size == -1) {
    Pop_size <- as.numeric(NaN)
    criteria$Value[criteria$Crit=="C1"] <- NA # Note for later, the timeframe applied here for CR and EN species is not in line with guidelines)  # nolint
    criteria$Value[criteria$Crit=="D"]<- NA  # nolint
  } else{
  criteria$Value[criteria$Crit=="C1"] <- min(
    as.numeric(cut(Pop_size, breaks=c(0, 250, 2500, 10000, Inf), labels=rev(c(1,2,3,4)))),  # nolint
    as.numeric(cut(AOH_lost, breaks=c(-Inf, 0.1, 0.2, 0.25, 1), labels=rev(c(1,2,3,4))))) %>% as.character(.) %>% revalue(., c("1"="LC/NT", "2"="VU", "3"="EN", "4"="CR")) # Note for later, the timeframe applied here for CR and EN species is not in line with guidelines)  # nolint
  criteria$Value[criteria$Crit=="D"]<-cut(Pop_size, breaks=c(0, 50, 250, 1000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR")))  # nolint  #TODO: Error when Pop_size == NaN
  }
  json <- list(
    Species = scientific_name,
    Date_processed = Sys.time(),
    EOO = EOO_km2,
    AOO = AOO_km2,
    "Percentage_of_habitat_lost" = AOH_lost,
    Pop.size = Pop_size,
    Criterias = paste0(criteria$Value, " (", criteria$Crit, ")") %>% paste(., collapse="; "),
    Highest_category = criteria$Value[which(as.numeric(criteria$Value)==max(as.numeric(criteria$Value), na.rm=T))] %>% unique()
  )

  return(json);
}


#* All species distributions on the platform
#* @get distributions
#* @serializer unboxedJSON
#* @tag sRedList
function() {
  # File size in bytes
  distributions <- list()
  for (directoryName in list.files(config$distribution_path)) {
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