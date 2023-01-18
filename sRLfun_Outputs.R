

### Calculate Criteria
sRL_CalculateCriteria<- function(aoh_lost, eoo_km2, aoo_km2, pop_size){
  

  criteria <- data.frame(Crit=c("A2", 'A2', "B1", "B1", "B2", "B2", "C1", "C1", "D", "D"), Scenario=rep(c("Pessimistic", "Optimistic"), 5), Value=NA) ; criteria$Value=factor(criteria$Value, c("LC/NT", "VU", "EN", "CR"), ordered=T) # nolint

  # B1
  criteria$Value[criteria$Crit=="B1"] <- cut(as.numeric(eoo_km2), breaks=c(0, 100, 5000, 20000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR")), include.lowest=T) # nolint
  
  if(is.na(aoh_lost)==F){
    # A2
    aoh_lost_processed<-unlist(strsplit(as.character(aoh_lost), "/")) %>% as.numeric(.)
    criteria$Value[criteria$Crit=="A2"] <- cut(as.numeric(aoh_lost_processed), breaks=c(-Inf, 30, 50, 80, 100), labels=c("LC/NT", "VU", "EN", "CR")) # nolint

    # B2
    aoo_processed<-unlist(strsplit(as.character(aoo_km2), "-")) %>% as.numeric(.)
    criteria$Value[criteria$Crit=="B2"] <- cut(as.numeric(aoo_processed), breaks=c(0, 10, 500, 2000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR")), include.lowest=T) # nolint
  }
  
  # C1 (only for VU for now)
  # if(aoh_lost>=10){criteria$Value[criteria$Crit=="C1"]<-cut(as.numeric(Pop_size), breaks=c(0, 10000, Inf), labels=rev(c("LC/NT", "VU")))} else {criteria$Value[criteria$Crit=="C1"]<-"LC/NT"}
  
  # D
  if(pop_size != '-1'){
    pop_processed<-unlist(strsplit(as.character(pop_size), "-")) %>% as.numeric(.)
    criteria$Value[criteria$Crit=="D"]<-cut(pop_processed, breaks=c(0, 50, 250, 1000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR")), include.lowest=T)  # nolint
  }
  
  criteria<-subset(criteria, is.na(criteria$Value)==F)
  
  return(criteria)
}




### Create the allfields file for SIS Connect
sRL_CreateALLFIELDS <- function(scientific_name, aoh_lost, eoo_km2, aoo_km2, pop_size, AltPref_saved){
  
  # Charge empty allfields
  allfields<-read.csv("Species/SIS_allfields_empty.csv")[1,]
  allfields$X<-NULL
  
  # Take data from saved prepared dataset
  allfields$internal_taxon_id<-AltPref_saved$taxonid[1]
  allfields$internal_taxon_name<-AltPref_saved$scientific_name[1]
  allfields$assessment_id<-NA
  
  allfields$ElevationLower.limit<-paste0(c(AltPref_saved$elevation_lowerEXTREME[1], AltPref_saved$elevation_lower[1]), collapse="-")
  allfields$ElevationUpper.limit<-paste0(c(AltPref_saved$elevation_upper[1], AltPref_saved$elevation_upperEXTREME[1]), collapse="-")
  
  ### Save parameters from analyses
  
  # Generation length
  allfields$GenerationLength.range<-Storage_SP$GL_saved

  # EOO
  allfields$EOO.range<-eoo_km2
  allfields$EOO.justification<-"The EOO has been estimated on the sRedList Platform"
  
  if(is.na(aoh_lost)==F){
  # AOO
  allfields$AOO.range<-aoo_km2
  allfields$AOO.justification<-"The AOO has been estimated on the sRedList Platform by rescaling the Area of Habitat to a 2x2km2 grid"

  # Decline for A2
  aoh_lost_processed<-unlist(strsplit(as.character(aoh_lost), "/")) %>% as.numeric(.)
  
  if(length(aoh_lost_processed)==1 | sign(aoh_lost_processed[1])==sign(aoh_lost_processed[2])){ # If only one estimate or two of the same sign, keep uncertainty
    allfields$PopulationReductionPast.range<-paste(sort(abs(aoh_lost_processed)), collapse="-")
    allfields$PopulationReductionPast.direction<-revalue(as.character(sign(aoh_lost_processed[1])), c("1"="Reduction", "-1"="Increase", "0"=NA))
    } else{ # If two estimates with different signs, I keep the minimum and put 0 as maximum
    allfields$PopulationReductionPast.range<-paste(c(0, abs(min(aoh_lost_processed))), collapse="-")
    allfields$PopulationReductionPast.direction<-"Reduction"
    
  }
  
  Justif.3gen<-ifelse(Storage_SP$Year1_saved>Storage_SP$Year1theo_saved, paste0(" (which is ", (Storage_SP$Year1_saved-Storage_SP$Year1theo_saved), " years less than 3 generations)"),  " (which corresponds to the maximum between 10 years / 3 generations)")
  allfields$PopulationReductionPast.justification<-allfields$PopulationDeclineGenerations3.justification<-paste0("The decline has been measured from the sRedList platform as the decline in Area of Habitat between ", Storage_SP$Year1_saved, " and ",  config$YearAOH2, Justif.3gen)
  }
  
  # Decline for C1
  #allfields$PopulationDeclineGenerations3.range<-ifelse(aoh_lost>=0, aoh_lost, 0)
 
    
  # Population size (only if positive, i.e. if an estimate of density has been provided)
  if(pop_size>=0){allfields$PopulationSize.range<-pop_size}

  return(allfields)
  
}




### Prepare countries output csv
sRL_OutputCountries<-function(scientific_name, countries, AltPref_saved){
  
  if(nrow(countries)==0){CO_SIS<-data.frame()} else{
  # Assign the name to provide (SIS_name1 if available, SIS_name2 otherwise)
  countries$Name<-ifelse(is.na(countries$SIS_name1), countries$SIS_name0, countries$SIS_name1)
  
  # Prepare file
  CO_SIS<-data.frame(CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceName=countries$Name)
    
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceLookup=NA
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.formerlyBred=NA
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.origin=NA # "Native"
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.presence=NA # "Extant"
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.seasonality=NA # "Resident"
  CO_SIS$assessment_id=NA
  CO_SIS$internal_taxon_id=AltPref_saved$taxonid[1]
  CO_SIS$internal_taxon_name=scientific_name
  }
  
  return(CO_SIS)
}




### Prepare references output csv
sRL_OutputRef<-function(scientific_name, AltPref_saved){

  # Charge the file with sRedList reference
  ref_SIS<-read.csv("Species/SIS_references_empty.csv")
  
  if("dat_proj_saved" %in% names(Storage_SP)){
    
    # Add rgbif if used
    if("GBIF" %in% Storage_SP$dat_proj_saved$Source){
      ROW<-(nrow(ref_SIS)+1)
      ref_SIS[ROW,]<-NA
      ref_SIS$author[ROW]<-"Chamberlain S, Barve V, Mcglinn D, Oldoni D, Desmet P, Geffert L, Ram K"
      ref_SIS$title[ROW]<-"rgbif: Interface to the Global Biodiversity Information Facility API_. R package version 3.7.2, <https://CRAN.R-project.org/package=rgbif>"
      ref_SIS$type[ROW]<-"R package"
      ref_SIS$year[ROW]<-2017
    }

  # Add robis if used
    if("OBIS" %in% Storage_SP$dat_proj_saved$Source){
      ROW<-(nrow(ref_SIS)+1)
      ref_SIS[ROW,]<-NA
      ref_SIS$author[ROW]<-"Provoost, P., Bosch, S."
      ref_SIS$title[ROW]<-"robis: Ocean Biodiversity Information System (OBIS) Client. R package version 2.8.2. url: https://CRAN.R-project.org/package=robis"
      ref_SIS$type[ROW]<-"R package"
      ref_SIS$year[ROW]<-2021
    }  
  }
  
  # Add species name / taxonid
  ref_SIS$internal_taxon_name<-scientific_name
  ref_SIS$internal_taxon_id<-AltPref_saved$taxonid[1]
    
  return(ref_SIS)
}





### Prepare distribution output shapefile
sRL_OutputDistribution<-function(scientific_name){
  
  distSP<-Storage_SP$distSP_saved
  
  # Create template
  distSP$OBJECTID<-1:nrow(distSP)
  distSP$sci_name<-scientific_name
  distSIS<-distSP[, c("OBJECTID", "sci_name")]
  distSIS[,c("presence", "origin", "seasonal", "compiler", "yrcomplied", "citation", "subspecies", "subpop", "data_sens", "sens_comm", "source", "dist_comm", "island", "tax_comm", "generalisd", "id_no", "Shape_Leng", "Shape_Area")]<-NA
  
  # Fill in some information
  distSIS$yrcomplied<-Sys.time() %>% format(., "%Y")
  distSIS$citation<-"Citation to add" # To fill
  distSIS$source<-"sRedList platform"
  
  # Send geometry column at the end of the table
  distSIS<-distSIS[, c(names(distSIS)[names(distSIS) != "geometry"], "geometry")]
  
  return(distSIS)
}



### Save occurrences shapefile from the GBIF procedure
sRL_OutputOccurrences <- function(scientific_name) {
  
  # Transform in lat/lon
  dat<-Storage_SP$dat_proj_saved %>% st_transform(., "+init=epsg:4326")
  names(dat)<-replace(names(dat), names(dat)=="objectid", "OBJECTID")
  
  # Create template shape
  if(!"OBJECTID" %in% names(dat)){dat$OBJECTID<-1:nrow(dat)}
  dat$sci_name<-scientific_name
  dat_SIS<-dat[, c("OBJECTID", "sci_name")]
  dat_SIS[,c("presence", "origin", "seasonal", "compiler", "yrcomplied", "citation", "dec_lat", "dec_long", "spatialref", "subspecies", "subpop", "data_sens", "sens_comm", "event_year", "source", "basisofrec", "catalog_no", "dist_comm", "island", "tax_comm", "id_no")]<-NA
  
  # Fill in some information
  dat_SIS$yrcomplied<-Sys.time() %>% format(., "%Y")
  dat_SIS$dec_long<-st_coordinates(dat_SIS)[,1]
  dat_SIS$dec_lat<-st_coordinates(dat_SIS)[,2]
  dat_SIS$spatialref<-"WGS84"
  dat_SIS$event_year<-dat$year
  dat_SIS$citation<-dat$citation


  # If data from the Red List, copy the information previously saved
  if("RL" %in% dat$Source){
    RL<-read.csv(paste0(config$POINTdistribution_path, scientific_name, ".csv"))
  
    for(COL in names(dat_SIS)[!names(dat_SIS) %in% c("OBJECTID", "sci_name", "geometry", "yrcomplied", "citation", "dec_lat", "dec_long", "spatialref")]){
      if(COL %in% names(RL)){
      dat_SIS[,COL]<-RL[,COL][match(dat_SIS$OBJECTID, RL$objectid)]
    }}
    
  }
  
  # Source information for GBIF / OBIS
  dat_SIS$source[dat$Source=="GBIF"]<-paste0("Extracted from GBIF; gbif_ID= ", dat$gbifID[dat$Source=="GBIF"])
  dat_SIS$source[dat$Source=="OBIS"]<-paste0("Extracted from OBIS; obis_ID= ", dat$id[dat$Source=="OBIS"])
  
  # Send geometry column at the end of the table
  dat_SIS<-dat_SIS[, c(names(dat_SIS)[names(dat_SIS) != "geometry"], "geometry")]
  
  return(dat_SIS)
}



