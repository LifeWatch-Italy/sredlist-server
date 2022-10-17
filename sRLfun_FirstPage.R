
###################################################
### Function to identify levels not in a vector ###
###################################################
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) # nolint


##############################################
### PERSONALISED THEMES FOR PLATFORM PLOTS ###
##############################################
sRLTheme_maps<-theme_void() %+replace%   theme(
  plot.title=element_text(hjust=0.5, size=14, face="bold"),
  plot.background=element_rect(fill="white", colour=NA)
  )




##############################################
### Function to capitalise scientific name ###
##############################################

sRL_firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}




###################################################
### Function to plot the history of assessments ###
###################################################

sRL_PlotHistory <- function(sciname_fun){
  historic <- rl_history(sciname_fun, key = config$red_list_token)$result
  
  if(nrow(historic) == 0){ # nolint
    not_found("Scientific name not found.")
  } else{
    historic$Cat<-revalue(historic$category, c("Least Concern"="LC", "Near Threatened"="NT", "Vulnerable"="VU", "Endangered"="EN", "Critically Endangered"="CR", "Extinct in the Wild"="EW", "Extinct"="EX", "Data Deficient"="DD")) # nolint
    historic$Cat[historic$Cat %not in% c("LC", "NT", "DD", "VU", "EN", "CR", "EW", "EX")]<-"old" # nolint
    historic$Cat <- factor(historic$Cat, c("LC", "NT", "DD", "old", "VU", "EN", "CR", "EW", "EX")) # nolint
    historic$year <- as.numeric(historic$year)
    
    return(plot(ggplot()+
                  geom_line(data=historic[historic$Cat != "old",], aes(x = year, y = as.numeric(Cat)), size = 0.5) +
                  geom_point(data=historic, aes(x = year, col = Cat, y = Cat), shape = 15, size = 5, show.legend = F) + # nolint
                  scale_colour_manual(values=rev(c("#000000ff", "#542344ff", "#d81e05ff", "#fc7f3fff", "#f9e814ff", "#bcbddc", "#d1d1c6ff", "#cce226ff", "#60c659ff")), name="", drop=FALSE) + # nolint
                  scale_y_discrete(rev(levels(historic$Cat)), drop=FALSE, name="Extinction risk") + # nolint
                  theme_minimal() %+replace% theme(axis.text=element_text(size=15), axis.title=element_text(size=20)) +
                  xlab("") +
                  ggtitle("")))
  }
}



##########################################
### Functions to plot the distribution ###
##########################################

# Generated distribution from File by scientific_name of species and folder path
sRL_ReadDistribution <- function(scientific_name, path) {
  speciesPath <- paste0(config$distribution_path, scientific_name, "/", path) # nolint
  files <- base::list.files(path = speciesPath, pattern = "\\.shp$")

  if (length(files) == 0) {
    distributions<-"No dist"
    print("Shapefile of the species does not exist!") # nolint
  } else {
    distributionPath <- paste0(speciesPath, "/", files[1]) # nolint
    distributions <- sf::st_read(distributionPath)
  }
  
  #distributions<-st_read("Distributions/Chameleons/CHAMELEONS.shp") # nolint
  return(distributions)
}

### Prepare distribution
sRL_PrepareDistrib <- function(distributions, scientific_name){
  names(distributions)[which(names(distributions) %in% c("SCINAME", "binomial", "BINOMIAL", "binomil"))] <- "binomial" # nolint
  names(distributions)[which(names(distributions) %in% c("PRESENC", "PRESENCE", "presence", "presenc"))] <- "presence" # nolint
  names(distributions)[which(names(distributions) %in% c("ORIGIN", "origin"))] <- "origin" # nolint
  names(distributions)[which(names(distributions) %in% c("SEASONA", "SEASONAL", "seasonal", "seasonl"))] <- "seasonal" # nolint
  distSP <- subset(distributions, distributions$binomial == scientific_name) # nolint 
  
  distSP<-st_transform(distSP, st_crs(CRSMOLL))
  return(distSP)
}


### Colour the distribution
sRL_ColourDistrib <- function(distSP){
  if(nrow(distSP) > 0){ 
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
  
  return(distSP)
}


### Prepare the cropped map of countries
sRL_PrepareCountries <- function(LIMS){
  
  # Crop countries
  CountrySP<-st_crop(distCountries, LIMS)
  
  # Calculate the tolerance parameter for simplification (depending on the longitudinal extent of the range)
  TOL=(extent(CountrySP)[2]-extent(CountrySP)[1])/1000
  
  if(TOL>0.001){
    CountrySP<-st_simplify(CountrySP, dTolerance=TOL)}
  
  # Remove empty countries
  CountrySP<-CountrySP[as.numeric(st_area(CountrySP))>0,]
  
  return(CountrySP)
}



### Function to reuse past calculated and stored values
sRL_reuse=function(scientific_name){
  eval(parse(text=paste0("Storage_SP_", sub(" ", "_", url_decode(scientific_name)))))
}

### Functions to store parameters in output
sRL_InitLog<-function(scientific_name, DisSource){
  output_to_save<-output
  output_to_save$Value[output_to_save$Parameter=="Distribution_Source"]<-DisSource
  output_to_save$Date<-Sys.Date()
  output_to_save$Species<-url_decode(scientific_name)
  
  return(output_to_save)
}

sRL_OutLog=function(STOR, Par, Val){
  for(i in 1:length(Par)){STOR$Output$Value[STOR$Output$Parameter==Par[i]]<-Val[i]}
  return(STOR)
}



### sRedList cleaning function
sRL_cleaningMemory<-function(Time_limit){
  
  # Current time
  Time_now<-Sys.time()

  
  # Remove temporary files in temporary folder
  tryCatch({
    # Remove ZIP files
    list_tempdir<-paste0(tempdir(), "\\", list.files(tempdir(), recursive=T))
    if(length(list_tempdir)>0){
      Time_diff_tempdir<-difftime(Time_now, file.info(list_tempdir)$ctime, units="mins") %>% as.numeric(.)
      toremove_tempdir<-list_tempdir[Time_diff_tempdir>Time_limit]
      unlink(toremove_tempdir, recursive=T)
      tempdir_prop_removed<- (length(list_tempdir)-length(list.files(tempdir(), recursive=T)))
      cat(paste0(length(toremove_tempdir), " / ", length(list_tempdir), " Temporary files in tempdir should be removed, (", tempdir_prop_removed, " were correctly removed)", "\n"))
      
    }
  } ,error=function(e){cat("Problem removing temporary files from tempdir()")}) 
  
  tryCatch({
  # Remove ZIP files
  list_zips<-list.files()[grepl(".zip", list.files())]
  if(length(list_zips)>0){
    Time_diff_zips<-difftime(Time_now, file.info(list_zips)$ctime, units="mins") %>% as.numeric(.)
    toremove_zips<-list_zips[Time_diff_zips>Time_limit]
    unlink(toremove_zips, recursive=T)
    zips_prop_removed<- (length(list_zips)-length(list.files()[grepl(".zip", list.files())]))
    cat(paste0(length(toremove_zips), " / ", length(list_zips), " zip files should be removed, (", zips_prop_removed, " were correctly removed)", "\n"))
    
  }
  } ,error=function(e){cat("Problem removing Zip files")}) 
  
  # Remove temporary folders
  tryCatch({
    list_temp<-paste0("resources/AOH_stored/", list.files("resources/AOH_stored"))
  if(length(list.files("resources/AOH_stored"))>0){
    Time_diff_temp<-difftime(Time_now, file.info(list_temp)$ctime, units="mins") %>% as.numeric(.)
    toremove_temp<-list_temp[Time_diff_temp>Time_limit]
    unlink(toremove_temp, recursive=T)
    temp_prop_removed<- (length(list_temp)-length(list.files("resources/AOH_stored")))
    cat(paste0(length(toremove_temp), " / ", length(list_temp), " temporary folders should be removed, (", temp_prop_removed, " were correctly removed)", "\n"))
  }
} ,error=function(e){cat("Problem removing temporary files")}) 
  
  # Remove Storage_SP
  tryCatch({
    list_storage<-ls(name=".GlobalEnv")[grepl("Storage_SP", ls(name=".GlobalEnv"))]
    toremove_storage<-0
  if(length(list_storage)>1){ # >1 because there will always be the Storage of the species currently running
    for(i in 1:length(list_storage)){
      Time_storage<-eval(parse(text= paste0(list_storage[i], "$Creation")))
      Time_diff_stor<-difftime(Time_now, Time_storage, units="mins") %>% as.numeric(.)
      if(Time_diff_stor>Time_limit){rm(list=list_storage[i], pos=".GlobalEnv")
        toremove_storage<-toremove_storage+1}
    }
    cat(paste0(toremove_storage, " / ", length(list_storage), " Storage_SP should be removed, (", (length(list_storage)-length(ls(name=".GlobalEnv")[grepl("Storage_SP", ls(name=".GlobalEnv"))])), " were correctly removed)", "\n"))
   }
  } ,error=function(e){cat("Problem removing Storage_SP")}) 
}




