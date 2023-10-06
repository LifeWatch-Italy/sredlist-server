



### Prepare countries output csv
sRL_OutputCountries<-function(scientific_name, countries){
  
  if(nrow(countries)==0){CO_SIS<-data.frame()} else{
  # Assign the name to provide (SIS_name1 if available, SIS_name2 otherwise)
  countries$Name<-ifelse(is.na(countries$SIS_name1), countries$SIS_name0, countries$SIS_name1)
  
  # Prepare file (including level 0 when level 1 is subnational)
  CO_SIS<-data.frame(CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceName=c(countries$Name, countries$SIS_name0),
                     CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceLookup=c(countries$lookup, countries$lookup_SIS0)) %>%
    .[order(grepl("<i>", .[,1])),] %>%
    distinct(., CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceLookup, .keep_all = T)
    
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.formerlyBred=NA
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.origin="Native"
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.presence <- ifelse(grepl("<i>", CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceName), "Possibly Extant", "Extant") # Possibly extant if no occurrence records within country, extant otherwise
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceName <- CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceName %>% gsub("<i>", "", .) %>% gsub("</i>", "", .)
  CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.seasonality="Resident"

  CO_SIS$internal_taxon_name=scientific_name
  CO_SIS$internal_taxon_id<-sRL_CalcIdno(scientific_name)
  
  # Remove countries that appear twice (in EEZ and COO), and keep "Extant" if both present
  CO_SIS<-CO_SIS %>% 
    arrange(CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceLookup, desc(factor(CountryOccurrence.CountryOccurrenceSubfield.presence, c("Possibly Extant", "Extant")))) %>% # First arrange to get lines with Extant before so that it keeps Extant if both Extant and Possibly Extant
    distinct(., CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceLookup, .keep_all=T)
  
  }
  
  # Remove subnational entities that are missing in SIS to avoid bugs
  CO_SIS<-subset(CO_SIS, grepl("Absent_SIS", CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceName)==F)
  
  # Remove lines with no countries name if it happens
  CO_SIS<-subset(CO_SIS, is.na(CO_SIS$CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceName)==F)
  
  # Transform NA in "" to match SIS
  CO_SIS<-replace(CO_SIS, is.na(CO_SIS), "")
  
  return(CO_SIS)
}




### Prepare references output csv
sRL_OutputRef<-function(scientific_name, Storage_SP){

  # Charge the file with sRedList reference
  ref_SIS<-read.csv("Species/SIS_references_empty.csv")
  
  if("dat_proj_saved" %in% names(Storage_SP)){
    
    # Add primary sources from occurrence data and number of records used
    citation_table<-as.data.frame(table(Storage_SP$dat_proj_saved$source))
    citation_table$Ref_with_N <- citation_table$Var1 %>% sub("[.]$", "", .) %>% paste0(., " (", citation_table$Freq, ifelse(citation_table$Freq==1, " record was used).", " records were used)."))
    citations<-unique(citation_table$Ref_with_N)
    ROW<-(nrow(ref_SIS))
    ref_SIS[(ROW+1):(ROW+length(citations)),]<-NA
    ref_SIS$title[(ROW+1):(ROW+length(citations))]<-citations
    ref_SIS$type[(ROW+1):(ROW+length(citations))]<-"electronic source"
    
    # Add rgbif if used
    if("GBIF" %in% Storage_SP$dat_proj_saved$Source | "Synonyms_GBIF" %in% Storage_SP$dat_proj_saved$Source){
      ROW<-(nrow(ref_SIS)+1)
      ref_SIS[ROW,]<-NA
      ref_SIS$author[ROW]<-"Chamberlain S, Barve V, Mcglinn D, Oldoni D, Desmet P, Geffert L, Ram K"
      ref_SIS$title[ROW]<-"rgbif: Interface to the Global Biodiversity Information Facility API_. R package version 3.7.2, <https://CRAN.R-project.org/package=rgbif>"
      ref_SIS$type[ROW]<-"R package"
      ref_SIS$year[ROW]<-2017
    }

  # Add robis if used
    if("OBIS" %in% Storage_SP$dat_proj_saved$Source | "Synonyms_OBIS" %in% Storage_SP$dat_proj_saved$Source){
      ROW<-(nrow(ref_SIS)+1)
      ref_SIS[ROW,]<-NA
      ref_SIS$author[ROW]<-"Provoost, P., Bosch, S."
      ref_SIS$title[ROW]<-"robis: Ocean Biodiversity Information System (OBIS) Client. R package version 2.8.2. url: https://CRAN.R-project.org/package=robis"
      ref_SIS$type[ROW]<-"R package"
      ref_SIS$year[ROW]<-2021
    }  
  }
  
  # Add species name
  ref_SIS$internal_taxon_name<-scientific_name
  ref_SIS$internal_taxon_id<-sRL_CalcIdno(scientific_name)
  ref_SIS$Reference_type<-"Assessment"
  
  ### TRY SHAPING REFERENCES
  tryCatch({
    # Put references that are ready aside
    ref_ready<-subset(ref_SIS, is.na(ref_SIS$author)==F & ref_SIS$author !="")
    ref<-ref_SIS[! ref_SIS$title %in% ref_ready$title,]
    
    # Keep entire reference
    ref$Original_reference<-ref$title
    
    # Look for year in brackets
    ref$titleYEAR<-gsub("[0-9]", "$", ref$title)
    ref$GREPL<-grepl("($$$$)", ref$titleYEAR, fixed=T)
    
    for(i in which(ref$GREPL==TRUE)){
      
      # Split the title in 3
      char<-unlist(gregexpr('($$$$)', ref$titleYEAR[i], fixed=T))
      AUTHOR<-substr(ref$title[i], 1, (char-1))
      YEAR<-substr(ref$title[i], (char+1), (char+4))
      TITLE<-substr(ref$title[i], (char+6), 10000)
      
      if(is.na(as.numeric(YEAR))==F & as.numeric(YEAR)>1500){
        # Save Author if valid
        ref$author[i]<-AUTHOR
        # Save Year if valid
        ref$year[i]<-as.numeric(YEAR)
        # Save title if valid and remove dots or spaces at the beginning
        while(substr(TITLE,1,1) %in% c(".", " ", ",", "/", ")")){
          TITLE<-substr(TITLE, 2, 10000)
        }
        ref$title[i]<-TITLE
        
      }
    }
    
    ### Remove working columns
    ref$titleYEAR<-ref$GREPL<-NULL
    
    ### Merge with original references
    ref_SIS<-rbind.fill(ref_ready, ref)
  })
  
  # Transform NA in "" to match SIS
  ref_SIS<-replace(ref_SIS, is.na(ref_SIS), "") %>% subset(., (is.empty(.$title)==F | is.empty(.$Original_reference)==F))
  ref_SIS$author[ref_SIS$author==""]<-"AUTHOR TO ADD MANUALLY"
  
  return(ref_SIS)
}



### Prepare references output csv
sRL_OutputTaxo<-function(scientific_name, Estimates){

  # Load empty file
  taxo<-read.csv("Species/SIS_taxonomy_empty.csv")
  
  # Add scientific name
  taxo$genus<-scientific_name %>% strsplit(., " ") %>% unlist(.) %>% .[1]
  taxo$species<-scientific_name %>% strsplit(., " ") %>% unlist(.) %>% .[2]
  
  # Add taxonomy
  taxo$kingdom<-toupper(Estimates[1])
  taxo$phylum<-toupper(Estimates[2])
  taxo$classname<-toupper(Estimates[3])
  taxo$ordername<-toupper(Estimates[4])
  taxo$family<-toupper(Estimates[5])
  taxo$taxonomicAuthority<-Estimates[6]
  
  # Add internal_taxon_name
  taxo$internal_taxon_id<-sRL_CalcIdno(scientific_name)
  taxo$Redlist_id<-sRL_CalcIdno(scientific_name)
  
  # Remove NAs
  taxo[1,]<-replace(taxo[1,], is.na(taxo[1,]), "")
  
  # Return
  return(taxo)
  
}


### Prepare assessments output csv
sRL_OutputAssessments<-function(scientific_name, Realms, Systems, Trends){
  
  assessments<-data.frame(
    BiogeographicRealm.realm=Realms,
    PopulationTrend.value=Trends,
    RedListCriteria.critVersion="3.1",
    System.value=Systems %>% sub("Freshwater", "Freshwater (=Inland waters)", .),
    internal_taxon_id=sRL_CalcIdno(scientific_name),
    internal_taxon_name=scientific_name
  )
  
  assessments<-replace(assessments, is.na(assessments), "")
  
  return(assessments)
}




### Prepare distribution output shapefile
sRL_OutputDistribution<-function(scientific_name, Storage_SP, username){
  
  distSP<-Storage_SP$distSP_saved
  
  # Create template
  distSP$sci_name<-scientific_name
  distSIS<-distSP[, c("sci_name")]
  distSIS[,c("presence", "origin", "seasonal", "compiler", "yrcompiled", "citation", "spatialref", "subspecies", "subpop", "data_sens", "sens_comm", "source", "dist_comm", "island", "tax_comm", "id_no", "Shape_Leng", "Shape_Area")]<-NA
  
  # Fill in some information
  distSIS$presence<-1
  distSIS$origin<-1
  distSIS$seasonal<-1
  distSIS$spatialref<-"WGS84"
  distSIS$yrcompiled<-Sys.time() %>% format(., "%Y")
  distSIS$citation<-"sRedList Working Group 2023" # To fill
  distSIS$source<-"sRedList platform"
  distSIS$id_no<-sRL_CalcIdno(scientific_name)
  distSIS$compiler<-username
  distSIS$data_sens<-0
  
  # Send geometry column at the end of the table
  distSIS<-distSIS[, c(names(distSIS)[names(distSIS) != "geometry"], "geometry")]
  
  # Transform NA in "" to match SIS
  distSIS<-replace(distSIS, is.na(distSIS), "")
  
  return(distSIS)
}


sRL_OutputHydrobasins<-function(distSIS, Storage_SP){
  
  # Extract hyrobasin list
  Hydro_list<-Storage_SP$distSP_saved$hybas_concat %>% strsplit(., ",") %>% unlist(.)
  
  # distSIS to data frame
  distSIS<-as.data.frame(distSIS)
  distSIS$geometry<-NULL
  
  # Export hydrobasins with as many lines as hydrobasins
  HydroSIS<-distSIS[1,]
  HydroSIS[1:length(Hydro_list),]<-distSIS[1,]
  HydroSIS$hybas_id<-Hydro_list
  
  return(HydroSIS)
}

### Save occurrences shapefile from the GBIF procedure
sRL_OutputOccurrences <- function(scientific_name, Storage_SP, username) {
  
  # Transform in lat/lon
  dat<-Storage_SP$dat_proj_saved %>% st_transform(., "+init=epsg:4326")

  # Create template shape
  dat$sci_name<-scientific_name
  dat_SIS<-dat[, c("sci_name")]
  dat_SIS[,c("presence", "origin", "seasonal", "compiler", "yrcompiled", "citation", "dec_lat", "dec_long", "spatialref", "subspecies", "subpop", "data_sens", "sens_comm", "event_year", "source", "basisofrec", "catalog_no", "dist_comm", "island", "tax_comm", "id_no")]<-NA

  # Fill in some information
  dat_SIS$presence<-1
  dat_SIS$origin<-1
  dat_SIS$seasonal<-1
  dat_SIS$yrcompiled<-Sys.time() %>% format(., "%Y")
  dat_SIS$dec_long<-st_coordinates(dat_SIS)[,1]
  dat_SIS$dec_lat<-st_coordinates(dat_SIS)[,2]
  dat_SIS$spatialref<-"WGS84"
  dat_SIS$event_year<-dat$event_year
  dat_SIS$citation<-"IUCN (International Union for Conservation of Nature)"
  dat_SIS$source<-dat$source
  dat_SIS$id_no<-sRL_CalcIdno(scientific_name)
  dat_SIS$compiler<-username
  dat_SIS$data_sens<-0
  
  if("" %in% names(dat)){
    dat_SIS$basisofrec<-revalue(dat$basisOfRecord, c(
      "FOSSIL_SPECIMEN"="FossilSpecimen",
      "HUMAN_OBSERVATION"="HumanObservation",
      "MATERIAL_CITATION"="PreservedSpecimen",
      "MATERIAL_SAMPLE"="PreservedSpecimen",
      "MaterialSample"="PreservedSpecimen", # This one is in OBIS
      "LIVING_SPECIMEN"="LivingSpecimen",
      "MACHINE_OBSERVATION"="MachineObservation",
      "OBSERVATION"="HumanObservation",
      "PRESERVED_SPECIMEN"="PreservedSpecimen",
      "OCCURRENCE"="HumanObservation"
    ))
  }
  
  # If data from the Red List, copy the information previously saved
  if("RL" %in% dat$Source){
    RL<-read.csv(paste0(config$POINTdistribution_path, scientific_name, ".csv"))
  
    for(COL in names(dat_SIS)[!names(dat_SIS) %in% c("sci_name", "geometry", "yrcompiled", "citation", "dec_lat", "dec_long", "spatialref")]){
      if(COL %in% names(RL)){
      dat_SIS[,COL]<-RL[,COL][match(dat_SIS$OBJECTID, RL$objectid)]
    }}
    
  }

  # Remove geometry and make a csv file
  dat_SIS$geometry<-NULL
  dat_SIS<-as.data.frame(dat_SIS)
  
  # Transform NA in "" to match SIS
  dat_SIS<-replace(dat_SIS, is.na(dat_SIS), "")
  
  return(dat_SIS)
}


### Check numeric values in questionnaire
sRL_CheckNumeric<-function(PAR, PAR_NAM, PERC){
  
  ### Replace commas and remove spaces in PAR to ensure it can be read as numeric
  PAR<-PAR %>% gsub(",", ".", .) %>% gsub(" ", "", .)
  
  ### Only if the parameter has a value (ie not NA)
  if(is.na(PAR)==F & PAR!="NA" & PAR !=""){
    
    # Check if the parameter is positive (if the first parameter is "-" it means it's not)
    if(substr(PAR, 1, 1)=="-"){questionnaire_negative(PAR_NAM)}
    # If uncertainty, I transform to a vector
    PAR_check<-PAR %>% strsplit(., "-") %>% unlist(.)
    # Check if the parameter is numeric
    if("TRUE" %in% is.na(as.numeric(PAR_check))){questionnaire_numeric(PAR_NAM)}
    # Check if the parameter is a percentage
    if(PERC=="Percentage_yes" & "TRUE" %in% (as.numeric(PAR_check)>100)){questionnaire_percentage(PAR_NAM)}
    
  }
  return(PAR)
}






extract_range<-function(carac){
  strsplit(as.character(carac), "-") %>% unlist(.) %>% as.numeric(.)
}

crit_apply<-function(crit, CRIT, Val){
  
  if(length(Val)!=0){ # If Val is empty (i.e. no value for this parameter in allfields), I leave empty
    Val_num<-revalue(Val, c("LC"="1", "VU"="2", "EN"="3", "CR"="4")) %>% as.numeric(.)
    crit$Cat_ThresholdMIN[crit$criterion==CRIT]<-min(Val_num) %>% as.character(.) %>% revalue(., c("1"="LC", "2"="VU", "3"="EN", "4"="CR"))
    crit$Cat_ThresholdMAX[crit$criterion==CRIT]<-max(Val_num) %>% as.character(.) %>% revalue(., c("1"="LC", "2"="VU", "3"="EN", "4"="CR"))
  }
  return(crit)
}



sRL_CriteriaCalculator <- function(allfields){
  
  crit <- data.frame(criterion=c("A1", "A2", "A3", "A4", "B1", "B2", "C1", "C2", "D1", "D2"), Cat_ThresholdMIN=NA, Cat_ThresholdMAX=NA, Subcrit=NA)
  

  
  ### CRITERIA A ------------------------------

  
  ### Criteron A1 
  
  # Thresholds
  if(is.na(allfields$PopulationReductionPast.direction)==F){
  if(allfields$PopulationReductionPast.direction == "Increase"){crit_A1<-"LC"} else{
    crit_A1<-cut(extract_range(allfields$PopulationReductionPast.range), breaks=c(0,50,70,90,101), labels=c("LC", "VU", "EN", "CR"), include.lowest=T, right=F) %>% as.character(.)
  }
  crit<-crit_apply(crit, "A1", crit_A1)
  }
  
  # Subcriteria
  crit$Subcrit[crit$criterion=="A1"]<-ifelse(allfields$PopulationReductionPast.qualifier %in% c("Observed","Estimated","Inferred","Suspected") &
                                               allfields$PopulationReductionPastUnderstood.value=="Yes" &
                                               allfields$PopulationReductionPastCeased.value=="Yes" &
                                               allfields$PopulationReductionPastReversible.value=="Yes",
                                             1, 0)
  
  
  
  
  ### Criteron A2
  
  # Thresholds
  if(is.na(allfields$PopulationReductionPast.direction)==F){
  if(allfields$PopulationReductionPast.direction == "Increase"){crit_A2<-"LC"} else{
    crit_A2<-cut(extract_range(allfields$PopulationReductionPast.range), breaks=c(0,30,50,80,101), labels=c("LC", "VU", "EN", "CR"), include.lowest=T, right=F) %>% as.character(.)
  }
  crit<-crit_apply(crit, "A2", crit_A2)
  }
  
  # Subcriteria
  crit$Subcrit[crit$criterion=="A2"]<-ifelse(allfields$PopulationReductionPast.qualifier %in% c("Observed","Estimated","Inferred","Suspected") &
                                               (allfields$PopulationReductionPastUnderstood.value=="No" |
                                                allfields$PopulationReductionPastCeased.value=="No" |
                                                allfields$PopulationReductionPastReversible.value=="No"),
                                             1, 0)
  
  
  
  
  ### Criteron A3
  
  # Thresholds
  if(is.na(allfields$PopulationReductionFuture.direction)==F){
  if(allfields$PopulationReductionFuture.direction == "Increase"){crit_A3<-"LC"} else{
    crit_A3<-cut(extract_range(allfields$PopulationReductionFuture.range), breaks=c(0,30,50,80,101), labels=c("LC", "VU", "EN", "CR"), include.lowest=T, right=F)
  }
  crit<-crit_apply(crit, "A3", crit_A3)
  }
  
  # Subcriteria
  crit$Subcrit[crit$criterion=="A3"]<-ifelse(allfields$PopulationReductionFuture.qualifier %in% c("Projected","Inferred","Suspected"),
                                             1, 0)
  
  
  
  ### Criteron A4
  
  # Thresholds
  if(is.na(allfields$PopulationReductionPastandFuture.direction)==F){
  if(allfields$PopulationReductionPastandFuture.direction == "Increase"){crit_A4<-"LC"} else{
    crit_A4<-cut(extract_range(allfields$PopulationReductionPastandFuture.range), breaks=c(0,30,50,80,101), labels=c("LC", "VU", "EN", "CR"), include.lowest=T, right=F)# %>% as.character(.)
  }
  crit<-crit_apply(crit, "A4", crit_A4)
  }
  
  # Subcriteria
  crit$Subcrit[crit$criterion=="A4"]<-ifelse(allfields$PopulationReductionPastandFuture.qualifier %in% c("Observed", "Estimated", "Projected", "Inferred", "Suspected") &
                                               (allfields$PopulationReductionPastandFutureUnderstood.value=="No" |
                                                allfields$PopulationReductionPastandFutureCeased.value=="No" |
                                                allfields$PopulationReductionPastandFutureReversible.value=="No"),
                                             1, 0)

  
  
  
  
  
  
  
  
  
  
  ### CRITERIA B ----------------

  ### Criterion B1
  crit_B1<-cut(extract_range(allfields$EOO.range), breaks=c(0,100,5000,20000,Inf), labels=c("CR", "EN", "VU", "LC"), include.lowest=T, right=F) %>% as.character(.)
  crit<-crit_apply(crit, "B1", crit_B1)
  
  
  ### Criterion B2
  crit_B2<-cut(extract_range(allfields$AOO.range), breaks=c(0,10,500,2000,Inf), labels=c("CR", "EN", "VU", "LC"), include.lowest=T, right=F) %>% as.character(.)
  crit<-crit_apply(crit, "B2", crit_B2)
  
  
  ### Subcriteria
  
  # Severely fragmented
  sev.fragm<-allfields$SevereFragmentation.isFragmented=="Yes" 
  if(is.na(sev.fragm)){sev.fragm<-FALSE}
  
  # Number of locations
  N.loc<-cut(extract_range(allfields$LocationsNumber.range), breaks=c(0,1,5,10,Inf), labels=c("CR", "EN", "VU", "LC"), include.lowest=T, right=T) %>% as.character(.)
  if(is.na(N.loc)[1]){N.loc<-"LC"} # If NA I consider unmet
  
  # Continuing decline
  cont.decl<-allfields$EOOContinuingDecline.isContinuingDecline=="Yes" |
    allfields$AOOContinuingDecline.isInContinuingDecline=="Yes" |
    allfields$HabitatContinuingDecline.isDeclining=="Yes" |
    allfields$SubpopulationContinuingDecline.isDeclining=="Yes" |
    allfields$LocationContinuingDecline.inDecline=="Yes" |
    allfields$PopulationContinuingDecline.isDeclining=="Yes"
  if(is.na(cont.decl)){cont.decl<-FALSE}
  
  # Extreme fluctuations
  extr.fluc<-allfields$EOOExtremeFluctuation.isFluctuating=="Yes" |
    allfields$AOOExtremeFluctuation.isFluctuating=="Yes" |
    allfields$SubpopulationExtremeFluctuation.isFluctuating=="Yes" |
    allfields$LocationExtremeFluctuation.isFluctuating=="Yes" |
    allfields$PopulationExtremeFluctuation.isFluctuating=="Yes"
  if(is.na(extr.fluc)){extr.fluc<-FALSE}
  
  # Apply subcriteria (first without N.loc as it's more complicated an category specific, we only look for it if only one of b or c)
  if(sum(sev.fragm, cont.decl, extr.fluc, na.rm=T) >= 2){crit$Subcrit[crit$criterion %in% c("B1", "B2")]<-1}
  if(sum(sev.fragm, cont.decl, extr.fluc, na.rm=T) == 0){crit$Subcrit[crit$criterion %in% c("B1", "B2")]<-0}
  if(sum(sev.fragm, cont.decl, extr.fluc, na.rm=T) == 1){
    # If the only one is severe fragmentation, subcriteria not met
    if(sev.fragm==TRUE){crit$Subcrit[crit$criterion %in% c("B1", "B2")]<-0} else {
      # If fluctuations or decline met, I check the highest category met by number of locations
      crit$Subcrit[crit$criterion %in% c("B1", "B2")]<-revalue(sort(factor(N.loc, levels=c("LC", "VU", "EN", "CR")), decreasing=T)[1], c("CR"="1", "EN"="EN", "VU"="VU", "LC"="0")) %>% as.character(.)
    }
    
    ## If I used N.loc with EN or VU, I can adapt the category met
    if(crit$Subcrit[crit$criterion=="B1"] == "VU"){
      # Change category for B1
      if(crit$Cat_ThresholdMAX[crit$criterion=="B1"] %in% c("CR", "EN")){ # If the threshold category is higher than VU
        crit$Cat_ThresholdMAX[crit$criterion=="B1"]<-"VU" # Then the B1 max category is brought to VU
        if(crit$Cat_ThresholdMIN[crit$criterion=="B1"] %in% c("EN", "CR")){crit$Cat_ThresholdMIN[crit$criterion=="B1"]<-"VU"} # And also the min category (unless it's already VU or LC)
      }
      # Change category for B2
      if(crit$Cat_ThresholdMAX[crit$criterion=="B2"] %in% c("CR", "EN")){ # If the threshold category is higher than VU
        crit$Cat_ThresholdMAX[crit$criterion=="B2"]<-"VU" # Then the B2 max category is brought to VU
        if(crit$Cat_ThresholdMIN[crit$criterion=="B2"] %in% c("EN", "CR")){crit$Cat_ThresholdMIN[crit$criterion=="B2"]<-"VU"} # And also the min category (unless it's already VU or LC)
      }
      # Validate subcriteria
      crit$Subcrit[crit$criterion %in% c("B1", "B2")]<-1
    }
    
    if(crit$Subcrit[crit$criterion=="B1"] == "EN"){
      # Change category for B1
      if(crit$Cat_ThresholdMAX[crit$criterion=="B1"] %in% c("CR")){ # If the threshold category is higher than EN
        crit$Cat_ThresholdMAX[crit$criterion=="B1"]<-"EN" # Then the B1 max category is brought to EN
        if(crit$Cat_ThresholdMIN[crit$criterion=="B1"] %in% c("CR")){crit$Cat_ThresholdMIN[crit$criterion=="B1"]<-"EN"} # And also the min category (unless it's already VU or LC)
      }
      # Change category for B2
      if(crit$Cat_ThresholdMAX[crit$criterion=="B2"] %in% c("CR")){ # If the threshold category is higher than EN
        crit$Cat_ThresholdMAX[crit$criterion=="B2"]<-"EN" # Then the B2 max category is brought to EN
        if(crit$Cat_ThresholdMIN[crit$criterion=="B2"] %in% c("CR")){crit$Cat_ThresholdMIN[crit$criterion=="B2"]<-"EN"} # And also the min category (unless it's already VU or LC)
      }
      # Validate subcriteria
      crit$Subcrit[crit$criterion %in% c("B1", "B2")]<-1
    }
  }
  
  
  
  
  
  
  
  ### CRITERIA C -----------------------------

  ### Population size
  crit_CPOP<-cut(extract_range(allfields$PopulationSize.range), breaks=c(0,250,2500,10000,Inf), labels=c("CR", "EN", "VU", "LC"), include.lowest=T, right=F) %>% as.character(.)
  
  
  ### Criterion C1
  if(is.na(crit_CPOP[1])==F){
    # Extract declines and keep the one that applies
    crit_C1b_1gen<-revalue(as.character(extract_range(allfields$PopulationDeclineGenerations1.range)>=25), c("TRUE"="CR", "FALSE"="LC"))
    crit_C1b_2gen<-revalue(as.character(extract_range(allfields$PopulationDeclineGenerations2.range)>=20), c("TRUE"="EN", "FALSE"="LC"))
    crit_C1b_3gen<-revalue(as.character(extract_range(allfields$PopulationDeclineGenerations3.range)>=10), c("TRUE"="VU", "FALSE"="LC"))
    
    if("CR" %in% crit_C1b_1gen){crit_C1b<-crit_C1b_1gen} else {
      if("EN" %in% crit_C1b_2gen){crit_C1b<-crit_C1b_2gen} else {
        if("VU" %in% crit_C1b_3gen){crit_C1b<-crit_C1b_3gen} else {
          crit_C1b<-"LC"
        }
      }
    } 
    
    # Calculate minimum and maximum common category
    Cat_min<-sort(factor(c(crit_C1b, crit_CPOP), levels=c("LC", "VU", "EN", "CR")))[1] # Minimum category is the first one when you order categories
    Cat_max<-sort(factor(c(
      sort(factor(crit_CPOP, levels=c("LC", "VU", "EN", "CR")), decreasing=T)[1], # keeps the highest category met according to population size
      sort(factor(crit_C1b, levels=c("LC", "VU", "EN", "CR")), decreasing=T)[1] # Keeps the highest category met according to decline
    ), levels=c("LC", "VU", "EN", "CR")))[1] # Keeps the lowest between both
    
    
    # Assign threshold
    crit<-crit_apply(crit, "C1", c(Cat_min, Cat_max))
    
    
    # Subcriterion (only for the highest met category). In case of intermediate category I also look at qualifier for higher category, this is important eg if pop size triggers EN but decline triggers CR, then the qualifier is only given for 1 generation decline
    COND1<-allfields$PopulationDeclineGenerations1.qualifier %in% c("Observed", "Estimated", "Projected")
    COND2<-allfields$PopulationDeclineGenerations2.qualifier %in% c("Observed", "Estimated", "Projected")
    COND3<-allfields$PopulationDeclineGenerations3.qualifier %in% c("Observed", "Estimated", "Projected")
    if(Cat_max=="CR"){crit$Subcrit[crit$criterion=="C1"]<-ifelse(COND1, 1, 0)}
    if(Cat_max=="EN"){crit$Subcrit[crit$criterion=="C1"]<-ifelse(COND1 | COND2, 1, 0)}
    if(Cat_max=="VU"){crit$Subcrit[crit$criterion=="C1"]<-ifelse(COND1 | COND2 | COND3, 1, 0)}
    if(Cat_max=="LC"){crit$Subcrit[crit$criterion=="C1"]<-1}
  }
  
  
  ### Criterion C2
  Crit_C2<-sort(factor(crit_CPOP, levels=c("LC", "VU", "EN", "CR")), decreasing=T)[1]
  
  if(is.na(Crit_C2)==F){
    ## If no decline, then it's not met
    if(is.na(allfields$PopulationContinuingDecline.isDeclining) | allfields$PopulationContinuingDecline.isDeclining %in% c("Unknown", "No")){crit$Subcrit[crit$criterion=="C2"]<-0} else {
      
      ## If fluctuations, then it's automatically met
      if(is.na(allfields$PopulationExtremeFluctuation.isFluctuating)==F & allfields$PopulationExtremeFluctuation.isFluctuating == "Yes"){crit$Subcrit[crit$criterion=="C2"]<-1} else {
        
        ## If decline but no fluctuations, I have to look at C2a thresholds
        # C2ai threshold (using the minimum value)
        C2ai_cat<-cut(min(extract_range(allfields$MaxSubpopulationSize.range)), breaks=c(0,50,250,1000,Inf), labels=c("CR", "EN", "VU", "LC"), include.lowest=T, right=T) %>% as.character(.)
        if(is.na(C2ai_cat)[1]){C2ai_cat<-"LC"} # If NA, it's not met
        # C2aii threshold (using the maximum value; if a single subpopulation we consider 100% in the same subpopulation)
        if(allfields$SubpopulationSingle.value=="Yes"){allfields$MatureIndividualsSubpopulation.value<-100}
        C2aii_value<-max(extract_range(allfields$MatureIndividualsSubpopulation.value))
        C2aii_cat<-cut(C2aii_value, breaks=c(0,90,95,100,101), labels=c(0, "CR", "EN", 1), include.lowest=T, right=F) %>% as.character(.) %>% replace(., is.na(.), 0)
        if(C2aii_cat == 0){crit$Subcrit[crit$criterion=="C2"]<-0} # Will be overwritten if this does not apply
        # Apply category by category
        if(Crit_C2=="LC"){crit$Subcrit[crit$criterion=="C2"]<-1} # Always met
        if(Crit_C2=="VU"){crit$Subcrit[crit$criterion=="C2"]<-ifelse(C2aii_cat=="1" | C2ai_cat %in% c("CR", "EN", "VU"),1,0)} # Met only if C2ai_cat is threatened or if C2aii_cat is 1
        if(Crit_C2=="EN"){
          if(C2aii_cat %in% c("1", "EN")){crit$Subcrit[crit$criterion=="C2"]<-1} else { # If C2aii trigerred, the subcriterion is met
            if(C2ai_cat %in% c("CR", "EN")){crit$Subcrit[crit$criterion=="C2"]<-1} # If the CR/EN of C2ai is met, it's all fine
            if(C2ai_cat == "VU"){crit$Subcrit[crit$criterion=="C2"]<-1; Crit_C2<-"VU"} # If only VU is met for C2ai, I bring the category to VU
            if(C2ai_cat == "LC"){crit$Subcrit[crit$criterion=="C2"]<-0} # If only LC is met for C2ai, subcriteria not met
          }}
        if(Crit_C2=="CR"){
          if(C2aii_cat %in% c("1", "EN", "CR")){crit$Subcrit[crit$criterion=="C2"]<-1} else { # If C2aii trigerred, the subcriterion is met
            if(C2ai_cat == "CR"){crit$Subcrit[crit$criterion=="C2"]<-1} # If the CR/EN of C2ai is met, it's all fine
            if(C2ai_cat == "EN"){crit$Subcrit[crit$criterion=="C2"]<-1; Crit_C2<-"EN"} # If only EN is met for C2ai, I bring the category to EN
            if(C2ai_cat == "VU"){crit$Subcrit[crit$criterion=="C2"]<-1; Crit_C2<-"VU"} # If only VU is met for C2ai, I bring the category to VU
            if(C2ai_cat == "LC"){crit$Subcrit[crit$criterion=="C2"]<-0} # If only LC is met for C2ai, subcriteria not met
          }}
      }
    }}
  
  ## Assign values of C2
  crit<-crit_apply(crit, "C2", c(
    sort(factor(c(crit_CPOP, Crit_C2), levels=c("LC", "VU", "EN", "CR")))[1], # The min value is the minimum value of Crit_CPOP unless it has to be lowered because of subcriterion C2ai
    Crit_C2 # The max value is Crit_C2 (i.e., the maximum category but maybe lowered by subcriterion C2ai)
  ))
  
  
  
  
  
  
  ### CRITERIA D ----------------

  ### criterion D1
  crit_D1<-cut(extract_range(allfields$PopulationSize.range), breaks=c(0,50,250,1000,Inf), labels=c("CR", "EN", "VU", "LC"), include.lowest=T, right=F) %>% as.character(.)
  crit<-crit_apply(crit, "D1", crit_D1)
  crit$Subcrit[crit$criterion=="D1"]<-1
  
  ### Criterion D2
  if(is.na(allfields$AreaRestricted.isRestricted)==F){
    crit_D2<-revalue(allfields$AreaRestricted.isRestricted, c("Yes"="VU", "No"="LC"))
    crit<-crit_apply(crit, "D2", crit_D2)
    crit$Subcrit[crit$criterion=="D2"]<-1
  }
  
  
  
  ### RETURN THE DATA FRAME -----------------------

  return(crit)
}






