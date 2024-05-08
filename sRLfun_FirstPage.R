

### Print log info inside future promise
sRL_loginfo<-function(x, scientific_name){
  print(paste0(x, " [", Sys.time(), " - ", scientific_name, "]"))
}



### Function to identify levels not in a vector
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) # nolint




### PERSONALISED THEMES FOR PLATFORM PLOTS ###
sRLTheme_maps<-theme_void() %+replace%   theme(
  plot.title=element_text(hjust=0.5, size=14, face="bold"),
  plot.subtitle=element_text(hjust=0.5),
  plot.background=element_rect(fill="white", colour=NA),
  plot.caption=element_text(hjust=0.5, size=11.5)
  )




### Function to capitalise scientific name ###
sRL_firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


### Function to transform a character value that can have uncertainty (e.g. "2.0-4.5") into a vector of 1 or 2 numeric values
sRL_UncertToVector <- function(x) {
  
  estimate <- x %>%
    gsub(" ", "", .) %>% # Remove spaces
    gsub(",", ".", .)%>% # Replace comma by dots (wrong decimals)
    as.character(.) %>%
    strsplit(., "-") %>% unlist(.) %>%# Split in vector
    as.numeric(.) %>%
    sort(.) # Sort by increasing order
  
  estimate
}


### Function to transform scientific name into a random number to use as id_no (so that it remains different between species and easy to calculate from just scientific name)
sRL_CalcIdno <- function(scientific_name){
  scientific_name<-sRL_decode(scientific_name)
  
  # If species already in RL I can use that number
  if(scientific_name %in% speciesRL$scientific_name){

    ID<-speciesRL$taxonid[speciesRL$scientific_name==scientific_name]

  } else {
    
    # Using the example of speciesRL names, there is a 10-5 probability of picking two species that have the same id_no
    # Number of characters
    SP <- scientific_name %>% strsplit(., " ") %>% unlist(.)
    N1 <- SP %>% nchar(.) %>% paste0(., collapse="")
    
    # First letter of genus as numbers
    N2<-which(letters == tolower(substr(scientific_name,1,1)))
    
    # First and last letters of species as numbers
    N3<-paste0(
      which(letters == tolower(substr(SP[2],1,1))),
      which(letters == tolower(substr(SP[2],nchar(SP[2]),nchar(SP[2]))))
    )
    
    # Paste and limit to 8 characters to get id_no 
    ID=paste0(N1, N2, N3)
    if(nchar(ID)>8){ID<-substr(ID, 1, 8)}
    ID=as.numeric(ID)
    
  }
  
  return(ID)
}


### Function to plot the history of assessments ###
sRL_PlotHistory <- function(sciname_fun){
  historic <- rl_history(sciname_fun, key = config$red_list_token)$result
  
  if(nrow(historic) == 0){ # nolint
    not_found("Scientific name not found.")
  } else{
    historic$Cat<-revalue(historic$category, c("Least Concern"="LC", "Near Threatened"="NT", "Vulnerable"="VU", "Endangered"="EN", "Critically Endangered"="CR", "Extinct in the Wild"="EW", "Extinct"="EX", "Data Deficient"="DD")) # nolint
    historic$Cat[historic$Cat %not in% c("LC", "NT", "DD", "VU", "EN", "CR", "EW", "EX")]<-"Former" # nolint
    historic$Cat <- factor(historic$Cat, c("Former", "DD", "LC", "NT", "VU", "EN", "CR", "EW", "EX")) # nolint
    historic$year <- as.numeric(historic$year)
    
    return(ggplot()+
                  geom_point(data=historic, aes(x = year, col = Cat, y = Cat), size = 6, show.legend = F) + # nolint
                  scale_colour_manual(values=rev(c("#000000ff", "#542344ff", "#d81e05ff", "#fc7f3fff", "#f9e814ff", "#cce226ff", "#60c659ff", "#d1d1c6ff", "#bcbddc")), name="", drop=FALSE) + # nolint
                  scale_y_discrete(rev(levels(historic$Cat)), drop=FALSE, name="Extinction risk") + # nolint
                  scale_x_continuous(limits=c(NA, 2023))+
                  theme_minimal() %+replace% theme(axis.text=element_text(size=15), axis.title=element_text(size=20)) +
                  xlab("") +
                  ggtitle(""))
  }
}



### Functions to plot the distribution ###

# Generated distribution from File by scientific_name of species and folder path
sRL_ReadDistribution <- function(scientific_name, path) {
  speciesPath <- paste0(config$distribution_path, scientific_name, "/", path) # nolint
  print(speciesPath)
  files <- base::list.files(path = speciesPath, pattern = "\\.shp$")

  if (length(files) == 0) {
    distributions<-"No dist"
    print("Shapefile of the species does not exist!") # nolint
  } else {
    distributionPath <- paste0(speciesPath, "/", files[1]) # nolint
    distributions <- sf::st_read(distributionPath)
  }
  
  return(distributions)
}




### Prepare distribution
sRL_PrepareDistrib <- function(distributions, scientific_name){
  
  # Replace column names
  names(distributions)[which(tolower(names(distributions)) %in% c("sciname", "sci_name", "scientific_name", "binomial", "binomil"))] <- "binomial" # nolint
  names(distributions)[which(tolower(names(distributions)) %in% c("presence", "presenc"))] <- "presence" # nolint
  names(distributions)[which(tolower(names(distributions)) %in% c("origin"))] <- "origin" # nolint
  names(distributions)[which(tolower(names(distributions)) %in% c("seasonal", "seasonl", "seasona"))] <- "seasonal" # nolint
  names(distributions)[which(tolower(names(distributions)) %in% c("taxonid", "sisid", "id_no"))] <- "id_no" # nolint
  
  # If no column, add them
  if(!"binomial" %in% names(distributions)){distributions$binomial<-scientific_name}
  if(!"presence" %in% names(distributions)){distributions$presence<-1}
  if(!"origin" %in% names(distributions)){distributions$origin<-1}
  if(!"seasonal" %in% names(distributions)){distributions$seasonal<-1}
  
  # Normalise species name
  distributions$binomial<-distributions$binomial %>% sRL_decode(.)
  
  # If empty values, I replace by 1
  distributions$presence <- replace(distributions$presence, ! distributions$presence %in% c(1:6), 1) %>% as.numeric(.)
  distributions$origin <- replace(distributions$origin, ! distributions$origin %in% c(1:6), 1) %>% as.numeric(.)
  distributions$seasonal <- replace(distributions$seasonal, ! distributions$seasonal %in% c(1:5), 1) %>% as.numeric(.)
  
  # If no id_no
  distributions$id_no<-sRL_CalcIdno(scientific_name)
  
  # Subset
  if(! scientific_name %in% distributions$binomial){species_not_in_distrib()}
  distSP <- subset(distributions, distributions$binomial == scientific_name) # nolint 
  if(nrow(distSP)==0){empty_distrib()}
  
  # Transform CRS
  distSP<-st_transform(distSP, st_crs(4326)) # Needed in case uploaded distribution has a different CRS and because GBIF distributions are stored in WGS
  sf::sf_use_s2(FALSE)
  EXT<-extent(distSP)
  if(EXT[1]<=(-180) | EXT[2]>=180 | EXT[3]<=(-90) | EXT[4]>=90){distSP<-st_crop(distSP, xmin=-180, xmax=180, ymin=-90, ymax=90)}
  distSP<-st_transform(distSP, st_crs(CRSMOLL))
  
  # Repair if needed
  if("FALSE" %in% st_is_valid(distSP)){
    distSP<-aoh::st_repair_geometry(distSP)
  }
  
  return(distSP)
}




### Colour the distribution
sRL_ColourDistrib <- function(distSP){
  
  ### Deal with 1|4 (at least useful when colouring COO)
  for(R in which(grepl("[|]", distSP$presence))){distSP$presence[R] <- distSP$presence[R] %>% strsplit(., "[|]") %>% unlist(.) %>% as.numeric(.) %>% min(.) %>% as.character(.)}
  for(R in which(grepl("[|]", distSP$origin))){distSP$origin[R] <- distSP$origin[R] %>% strsplit(., "[|]") %>% unlist(.) %>% as.numeric(.) %>% min(.) %>% as.character(.)}
  for(R in which(grepl("[|]", distSP$seasonal))){distSP$seasonal[R] <- distSP$seasonal[R] %>% strsplit(., "[|]") %>% unlist(.) %>% as.numeric(.) %>% min(.) %>% as.character(.)}
  
  ### Colourise
  if(nrow(distSP) > 0){ 
    distSP$cols <- NA
    distSP$cols <- revalue(as.character(distSP$presence), c("1"=NA, "2"=NA, "3"=NA, "4"="#FFE4E1", "5"="#8B2323", "6"="#B3B3B3"), warn_missing=F) # nolint    if (nrow(distSP) > 0) {
    
    for (i in which(is.na(distSP$cols) & is.na(distSP$presence)==F)) {
      if (distSP$origin[i] == "1") {
        if(distSP$seasonal[i] == "1"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#d95f02", "2"="#fc8d62", "3"="#fc8d62"), warn_missing=F)} # nolint
        if(distSP$seasonal[i] == "2"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#1b9e77", "2"="#66c2a5", "3"="#66c2a5"), warn_missing=F)} # nolint
        if(distSP$seasonal[i] == "3"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#7570b3", "2"="#8da0cb", "3"="#8da0cb"), warn_missing=F)} # nolint
        if(distSP$seasonal[i] == "4"){ distSP$cols[i] <- revalue(as.character(distSP$presence[i]), c("1"="#9ACD32", "2"="#EEEE00", "3"="#8B8B00"), warn_missing=F)} # nolint
        if(distSP$seasonal[i] == "5"){ distSP$cols[i]<-"#B3B3B3"}
      } else{
        distSP$cols[i]<-revalue(as.character(distSP$origin[i]), c("2"="#BF3EFF", "3"="#68228B", "4"="#9BCD9B", "5"="#B3B3B3", "6"="#BF3EFF"), warn_missing=F) # nolint
      }   # nolint
    }
  }
  
  # Fill with white
  distSP$cols[is.na(distSP$cols)==T] <- "white"
  
  return(distSP)
}




### Prepare the cropped map of countries
sRL_PrepareCountries <- function(LIMS){
  
  # Crop countries
  CountrySP<-st_crop(distCountries, LIMS)
  
  # Calculate the tolerance parameter for simplification (depending on the longitudinal extent of the range)
  TOL=(extent(CountrySP)[2]-extent(CountrySP)[1])/1000
  
  if(is.na(TOL)==F & TOL>0.001){
    CountrySP<-st_simplify(CountrySP, dTolerance=TOL)}
  
  # Remove empty countries
  CountrySP<-CountrySP[as.numeric(st_area(CountrySP))>0,]
  
  return(CountrySP)
}





### Function to reuse past calculated and stored values
sRL_StoreSave<-function(scientific_name, username, Storage_SP){
  SCI<-sRL_decode(scientific_name)
  FOLDER<-paste0("resources/AOH_stored/", gsub(" ", "_", SCI), "_", sRL_userdecode(username))
  FILE=paste0(FOLDER, "/Storage_SP.rds")
  dir.create(paste0(FOLDER, "/Plots"), recursive=T, showWarnings=F)
  saveRDS(Storage_SP, file=FILE)
}

sRL_StoreRead<-function(scientific_name, username, MANDAT){
  SCI<-sRL_decode(scientific_name)
  
  # Read if it exists or charge it
  FILE<-paste0("resources/AOH_stored/", gsub(" ", "_", SCI), "_", sRL_userdecode(username), "/Storage_SP.rds")
  if(file.exists(FILE)){
    Storage<-readRDS(FILE)
  } else { # Create a folder for the first functions, if it's with MANDAT=1 (ie after COO), then return an error that Storage_SP was removed
    if(MANDAT==1){no_storage()}
    Storage<-list(Creation=Sys.time())
  }
  
  return(Storage)
}






### Functions to store parameters in output
sRL_InitLog<-function(scientific_name, username, DisSource){
  output_to_save<-output
  output_to_save$Value[output_to_save$Parameter=="Distribution_Source"]<-DisSource
  output_to_save$Date<-Sys.Date()
  output_to_save$Species<-sRL_decode(scientific_name)
  output_to_save$Username<-sRL_userdecode(username)
  
  return(output_to_save)
}

sRL_OutLog=function(STOR, Par, Val){
  for(i in 1:length(Par)){
    STOR$Output$Value[STOR$Output$Parameter==Par[i]]<-Val[i]
    STOR$Output$Count[STOR$Output$Parameter==Par[i]]<-STOR$Output$Count[STOR$Output$Parameter==Par[i]]+1
    }
  return(STOR)
}





### sRedList cleaning function
sRL_cleaningMemory<-function(Time_limit){
  
  gc()
  
  # Current time
  Time_now<-Sys.time()

  
  # Remove temporary files in temporary folder
  tryCatch({
    list_tempdir<-paste0(tempdir(), "\\", list.files(tempdir(), recursive=T))
    if(length(list_tempdir)>0){
      Time_diff_tempdir<-difftime(Time_now, file.info(list_tempdir)$atime, units="mins") %>% as.numeric(.)
      toremove_tempdir<-list_tempdir[Time_diff_tempdir>Time_limit]
      unlink(toremove_tempdir, recursive=T)
      tempdir_prop_removed<- (length(list_tempdir)-length(list.files(tempdir(), recursive=T)))
      cat(paste0(length(toremove_tempdir), " / ", length(list_tempdir), " Temporary files in tempdir should be removed, (", tempdir_prop_removed, " were correctly removed)", "\n"))
      
    }
  } ,error=function(e){cat("Problem removing temporary files from tempdir()")}) 
  
  tryCatch({
  # Remove ZIP files + pre-zip folders + merged zip
  list_zips<-c(list.files()[grepl("_sRedList", list.files())], list.files()[grepl("Unzipped", list.files())])
  if(length(list_zips)>0){
    Time_diff_zips<-difftime(Time_now, file.info(list_zips)$atime, units="mins") %>% as.numeric(.)
    toremove_zips<-list_zips[Time_diff_zips>Time_limit]
    unlink(toremove_zips, recursive=T)
    zips_prop_removed<- (length(list_zips)-length(list.files()[grepl(".zip", list.files())]))
    cat(paste0(length(toremove_zips), " / ", length(list_zips), " zip files should be removed, (", zips_prop_removed, " were correctly removed)", "\n"))
    
  }
  } ,error=function(e){cat("Problem removing Zip files")}) 
  
  
  
  # Remove AOH_stored
  tryCatch({
    list_temp<-paste0("resources/AOH_stored/", list.files("resources/AOH_stored"))
    
    if(length(list.files("resources/AOH_stored"))>0){
      # List files to remove
      Time_diff_temp<-difftime(Time_now, file.info(paste(list_temp, "Storage_SP.rds", sep="/"))$mtime, units="mins") %>% as.numeric(.) #I use the modification time of Storage_SP because the folder modification time is not updated when Storage_SP is updated (which made problems when you started working on the same species after more than 45 minutes)
      toremove_temp<-list_temp[Time_diff_temp>Time_limit | is.na(Time_diff_temp)] # If time higher than Time_limit or if time is NA (which can happen if Storage_SP does not exist) I remove them
      
      # Keep track in Stored_Output
      if(length(toremove_temp)>0){
      
      tryCatch({
        print(paste0("Species_to_record (clean function): ", paste(toremove_temp, collapse=" + "), "_close"))
        
        FileStored<-paste0("Species/Stored_outputs/Stored_", substr(Sys.Date(), 1, 7), ".rds")

        # Determine if I have to run distributions cleaning function (only once at the start of the month)        
        if(file.exists(FileStored)){
          Saved_output<-readRDS(FileStored)
          RMDIST<-ifelse(("NotCompleted" %in% Saved_output$Parameter | "AOH_HabitatPreference" %in% Saved_output$Parameter), 0, 1) # If AOH has never been called (ie if there is no NotCompleted saved this month, AND no complete assessment with AOH performed)n then I clean distributions
        } else {
          Saved_output<-read.csv("Species/Output_save_empty.csv")
          RMDIST<-1 # If it's the first Output_save run of the month, clean distributions (but later so that it does not delay saving output_save)
        }
        
        # Extract NotCompleted assessments
        for(SP in 1:length(toremove_temp)){
          tryCatch({
            # Create empty line
            N_line<-nrow(Saved_output)+1
            Saved_output[N_line,]<-NA
            # Charge Storage SP
            St_SP<-readRDS(paste0(toremove_temp[SP], "/Storage_SP.rds"))
            # Assign values
            Saved_output$Species[N_line]<-St_SP$Output$Species[1]
            Saved_output$Date[N_line] <- St_SP$Creation  %>% as.character(.)
            Saved_output$Username[N_line]<-St_SP$Output$Username[1]
            Saved_output$Parameter[N_line]<-"NotCompleted"
            Saved_output$Value[N_line]<-sRL_LastStep(St_SP)

          }, error=function(e){cat(paste0("Problem tracking SP: ", SP_name))})
        }
        
        # Save output_save
        saveRDS(Saved_output, FileStored)
        
      }, error=function(e){cat("Problem tracking incomplete assessments")})
      
      # Remove
      unlink(toremove_temp, recursive=T)
      
      # Print
      temp_prop_removed<- (length(list_temp)-length(list.files("resources/AOH_stored")))
      cat(paste0(length(toremove_temp), " / ", length(list_temp), " stored folders should be removed, (", temp_prop_removed, " were correctly removed)", "\n"))
      }
  }
}, error=function(e){cat("Problem removing Stored files")})
  
  ### Clean distributions (should be called once per month)
  tryCatch({
    if(RMDIST==1){
      sRL_RemoveDistrib(15)
    }
  }, error=function(e){cat("Problem cleaning distributions")})

}



### Function sRL_LastStep: Extract last step run in incomplete assessment
sRL_LastStep <- function(St_SP){
  Out<-St_SP$Output
  Out<-subset(Out, is.na(Out$Value)==F)
  
  # At least step 1 or not
  if(! "Distribution_Source" %in% Out$Parameter){Steps<-"0"} else {
    Steps<-c(
      ifelse(Out$Value[Out$Parameter=="Distribution_Source"]=="Created", "1b", "1a"),
      ifelse("System_pref" %in% Out$Parameter, "2", NA),
      ifelse("eoo_km2" %in% names(St_SP), "3", NA),
      ifelse("AOH_HabitatPreference" %in% Out$Parameter, "4", NA),
      ifelse("AOH_GenerationLength" %in% Out$Parameter, "5", NA),
      ifelse("Fragmentation_Isolation" %in% Out$Parameter, "6a", NA),
      ifelse("Usage_RS" %in% Out$Parameter, "6b", NA),
      ifelse("Estimates_saved" %in% names(St_SP), "7", NA)
    )
    Steps<-Steps[is.na(Steps)==F] %>% paste0(., collapse="+")
  }
  
  return(as.character(Steps))
}


### Function sRL_decode : it combines url_decode and control of the case of scientific name
sRL_decode<-function(scientific_name){
  scientific_name<-url_decode(scientific_name)
  scientific_name<-paste0(toupper(substr(scientific_name, 1, 1)), tolower(substr(scientific_name, 2, 200)))
  return(scientific_name)
}


### Function sRL_userdecode : it combines url_decode and control of the case of scientific name
sRL_userdecode<-function(username){
  if(grepl("@", username)){username <- username %>% strsplit(., "@") %>% unlist(.) %>% .[1]}
  username<-url_decode(username) %>% tolower(.) %>% gsub(" ", ".", .) %>% iconv(., to="ASCII//TRANSLIT")
  return(username)
}



### Function sRL_RemoveDistrib: removes distributions created with GBIF or uploaded in >15 days
sRL_RemoveDistrib<-function(Max_Days){
  
  cat("START - Cleaning distributions \n")
  
  
  # List all distribution files
  list_dist<-list.files(config$distribution_path, recursive = T)
  
  # Extract non-RL distributions (and remove config$distribution_path which occurs if nothing to remove)
  list_remove<-list_dist[! grepl("_RL", list_dist)] %>% paste0(config$distribution_path, .) %>% subset(., . != config$distribution_path)
  
  # Restrict to distributions from > 15 days
  Recent_dates <- (Sys.Date()-c(0:Max_Days)) %>% as.character(.) %>% gsub("-", "", .)
  list_remove_filtered<-subset(list_remove, ! grepl(paste(Recent_dates, collapse="|"), list_remove))
  
  # Remove them
  unlink(list_remove_filtered, recursive=T)
  
  # Remove empty directories in distribution folders
  COUNT<-0
  for(FIL in list_remove_filtered){
    Dir0<-FIL %>% sub(config$distribution_path, "", .) %>% strsplit(., "/") %>% unlist(.) %>% .[1] %>% paste0(config$distribution_path, .)
    TAF::rmdir(Dir0, recursive=T)
    COUNT<-COUNT+1
  }

  
  # Return result of the cleaning
  cat(paste0("There are ", length(list_remove_filtered), " files and ", COUNT, " empty folders to remove", "\n"))
  
  cat("END - Cleaning distributions \n")
}
