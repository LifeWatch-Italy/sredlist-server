

### Calculate Criteria
sRL_CalculateCriteria<- function(aoh_lost, eoo_km2, aoo_km2, pop_size){
  

  Pop_size <-  round(as.numeric(pop_size))
  if (Pop_size == -1) {
    Pop_size <- as.numeric(NaN)
  }

  criteria <- data.frame(Crit=c("A2", "B1", "B2", "C1", "D"), Value=NA) ; criteria$Value=factor(criteria$Value, c("LC/NT", "VU", "EN", "CR"), ordered=T) # nolint

  criteria$Value[criteria$Crit=="A2"] <- cut(as.numeric(aoh_lost), breaks=c(-Inf, 30, 50, 80, 100), labels=c("LC/NT", "VU", "EN", "CR")) # nolint

  criteria$Value[criteria$Crit=="B1"] <- cut(as.numeric(eoo_km2), breaks=c(0, 100, 5000, 20000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR"))) # nolint

  criteria$Value[criteria$Crit=="B2"] <- cut(as.numeric(aoo_km2), breaks=c(0, 10, 500, 2000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR"))) # nolint

  # C1 (only for VU for now)
  if(aoh_lost>=10){criteria$Value[criteria$Crit=="C1"]<-cut(as.numeric(Pop_size), breaks=c(0, 10000, Inf), labels=rev(c("LC/NT", "VU")))} else {criteria$Value[criteria$Crit=="C1"]<-"LC/NT"}
  criteria$Value[criteria$Crit=="D"]<-cut(Pop_size, breaks=c(0, 50, 250, 1000, Inf), labels=rev(c("LC/NT", "VU", "EN", "CR")))  # nolint
  
  criteria<-subset(criteria, is.na(criteria$Value)==F)
  
  return(criteria)
}




### Create the allfields file for SIS Connect
sRL_CreateALLFIELDS <- function(aoh_lost, eoo_km2, aoo_km2, pop_size){
  
  # Charge empty allfields
  allfields<-read.csv("Species/SIS_allfields_empty.csv")[1,]
  
  # Take data from saved prepared dataset
  allfields$internal_taxon_id<-AltPref_saved$taxonid[1]
  allfields$internal_taxon_name<-AltPref_saved$scientific_name[1]
  allfields$assessment_id<-99999999999
  
  allfields$ElevationLower.limit<-AltPref_saved$elevation_lower[1]
  allfields$ElevationUpper.limit<-AltPref_saved$elevation_upper[1]
  
  # Save parameters from analyses
  
  # EOO
  allfields$EOO.range<-eoo_km2
  allfields$EOO.justification<-"The EOO has been estimated on the sRedList Platform"
  
  # AOO
  allfields$AOO.range<-aoo_km2
  allfields$AOO.justification<-"The AOO has been estimated on the sRedList Platform by rescaling the Area of Habitat to a 2x2km2 grid"

  # Decline
  allfields$PopulationDeclineGenerations3.range<-aoh_lost
  allfields$PopulationReductionPast.range<-aoh_lost
  allfields$PopulationReductionPast.justification<-allfields$PopulationDeclineGenerations3.justification<-"The decline has been measured from the sRedList platform as the decline in Area of Habitat over the last 10 years / 3 generations"
  
  # Population size
  allfields$PopulationSize.range<-pop_size

  return(allfields)
  
}




### Prepare countries output csv
sRL_OutputCountries<-function(scientific_name){
  
  # Charge the file with 2 sRedList references
  countries_inters<-st_join(distSP_saved, CountrySP_saved, join=st_intersects)$FIRST_NA2_ %>% unique(.)
  
  # Prepare file
  CO_SIS<-data.frame(CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceName=countries_inters)
    
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceLookup=NA
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.formerlyBred=NA
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.origin="Native"
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.presence="Extant"
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.seasonality="Resident"
  CO_SIS$assessment_id=99999999999
  CO_SIS$internal_taxon_id=AltPref_saved$taxonid[1]
  CO_SIS$internal_taxon_name=scientific_name
  
  return(CO_SIS)
}




### Prepare references output csv
sRL_OutputRef<-function(scientific_name){

  # Charge the file with 2 sRedList references
  ref_SIS<-read.csv("Species/SIS_references_empty.csv")
  ref_SIS$internal_taxon_name<-scientific_name
  ref_SIS$internal_taxon_id<-AltPref_saved$taxonid[1]
  
  return(ref_SIS)
}





### Prepare distribution output shapefile
sRL_OutputDistribution<-function(scientific_name){
  
  # Charge the file with 2 sRedList references
  distSP_SIS<-distSP_saved[, names(distSP_saved) %not in% c("cols")] # Write column names to remove from the file to download
  
  return(distSP_SIS)
}



