### Create leaflet map

sRLCountry_CreateLeaflet <- function(coo, Storage_SP, Include_Buttons){
  
  # Load distribution
  distSP <- Storage_SP$distSP_saved %>% st_transform(., "+init=epsg:4326") %>% dplyr::group_by(origin, presence, seasonal) %>% dplyr::summarise(N= n())
  if((extent(distSP)@xmax-extent(distSP)@xmin)>50){distSP<-st_simplify(distSP, dTolerance=0.05)}
  
  # Edit popup (include buttons for the output leaflet but not the one stored for RMD)
  coo$Popup <- paste0(
    "<b> National entity: ","</b>", coo$SIS_name0, ifelse(coo$Level0_occupied==T, " (Present)", " (Absent)"), "<br>", "<br>",
    "<b> Subnational entity: ","</b>", coo$SIS_name1, ifelse(is.na(coo$SIS_name1)==T, "", ifelse(coo$Level1_occupied==T, " (Present)", " (Absent)")), "<br>", 
    ifelse(is.na(coo$presence), "", paste0("<br><b><center> Presence: </b>", coo$presence, "<b>  Origin: </b>", coo$origin, "<b>  Seasonal: </b>", coo$seasonal, "</center>"))
    )
  
  if(Include_Buttons==T){coo$Popup <- paste0(coo$Popup, "<center>", ifelse(is.na(coo$presence), "", "(attributes can be edited in the table)<br>"), "<br>", shinyInputCountry(actionButton, coo$lookup, Occupied = coo$Level1_occupied, 100), "</center>")}
  
  # Define colour
  coo$colour <- sRL_ColourDistrib(coo)$cols
  
  # Create leaflet from sRLfun_Mapping function (I provide empty df for realms because I don't want them for this leaflet)
  Leaflet_COO <- sRL_LeafCountry(coo, distSP, data.frame(), Storage_SP)
  
  
  return(Leaflet_COO)
  
} 


sRLCountry_CreateTable <- function(COO){
  
  # Add action button
  COO$Action <- shinyInputCountry(actionButton, COO$lookup, Occupied = COO$Level1_occupied, 90)
  
  # Subset column
  COO <- COO[, !names(COO) %in% c("geometry", "Popup", "colour", "Domain", "Level0_occupied", "Level1_occupied", "lookup_SIS0")]
  
  # Rename columns
  names(COO) <- revalue(names(COO), c("SIS_name0"="Country", "SIS_name1"="Sub-country"))
  
  # Make a pretty datatable
  datatable(
    COO,
    escape = FALSE,
    options = list(pageLength = 15,
                   dom="ftp",
                   autoWidth = T
                   ),
    editable = list(target = "cell", disable = list(columns = c(0,1,2,6))),
    selection="none",
    rownames=FALSE)
}


shinyInputCountry <- function(FUN, Country, Occupied, SizePerc) {
  inputs <- character(length(Country))
  for (i in 1:length(Country)) {
    inputs[i] <- as.character(
      FUN(
        paste0(
          ifelse(Occupied[i]==T, "makeempty_", "makeoccupied_"), 
          Country[i]
        ),
        label=ifelse(Occupied[i]==T, "Make Empty", "Make Occupied"), 
        style=paste0("color: ", ifelse(Occupied[i]==T, "#000", "#fff"), "; background-color: ", ifelse(Occupied[i]==T, "#D2D2D2", "#8C2316"), "; border-color: ", ifelse(Occupied[i]==T, "#D2D2D2", "#8C2316"), "; font-size:", SizePerc, "%"), 
        onclick="Shiny.onInputChange( \"button_occupied\" , this.id)"
      )
    )
  }
  inputs
}