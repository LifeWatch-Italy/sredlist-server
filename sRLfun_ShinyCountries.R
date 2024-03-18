### Create leaflet map

sRLCountry_CreateLeaflet <- function(coo, Storage_SP){
  
  # Load distribution
  distSP <- Storage_SP$distSP_saved %>% st_transform(., "+init=epsg:4326") %>% dplyr::group_by(origin, presence, seasonal) %>% dplyr::summarise(N= n())
  if((extent(distSP)@xmax-extent(distSP)@xmin)>50){distSP<-st_simplify(distSP, dTolerance=0.05)}
  
  # Edit popup
  coo$Popup <- paste0(
    "<b> National entity: ","</b>", coo$SIS_name0, ifelse(coo$Level0_occupied==T, " (Present)", " (Absent)"), "<br>", "<br>",
    "<b> Subnational entity: ","</b>", coo$SIS_name1, ifelse(is.na(coo$SIS_name1)==T, "", ifelse(coo$Level1_occupied==T, " (Present)", " (Absent)")),
    "<br>", 
    '<br>', 
    shinyInputCountry(actionButton, coo$lookup, Occupied = coo$Level1_occupied)
  )
  
  # Define colour
  coo$colour<-paste0(coo$Domain, coo$Level0_occupied, coo$Level1_occupied) 
  coo$colour<-sRL_COOColours$Col[match(coo$colour, sRL_COOColours$Code)]
  
  # Create leaflet from sRLfun_Mapping function
  Leaflet_COO <- sRL_LeafCountry(coo, distSP, realms_raw, Storage_SP)
  
  
  return(Leaflet_COO)
  
} 


sRLCountry_CreateTable <- function(COO){
  
  # Add action button
  COO$Action <- shinyInputCountry(actionButton, COO$lookup, Occupied = COO$Level1_occupied)
  
  # Subset column
  COO <- COO[, !names(COO) %in% c("geometry", "Popup", "colour", "Domain", "Level0_occupied", "Level1_occupied", "lookup_SIS0")]
  
  # Make a pretty datatable
  datatable(
    COO,
    escape = FALSE,
    filter="top",
    options = list(pageLength = 30,
                   dom="t",
                   columnDefs=list(list(targets=6, searchable = FALSE))
                   ),
    editable = list(target = "cell", disable = list(columns = c(0,1,2,6))),
    selection="none",
    rownames=FALSE)
}


shinyInputCountry <- function(FUN, Country, Occupied) {
  inputs <- character(length(Country))
  for (i in 1:length(Country)) {
    inputs[i] <- as.character(
      FUN(
        paste0(
          ifelse(Occupied[i]==T, "makeempty_", "makeoccupied_"), 
          Country[i]
        ),
        label=ifelse(Occupied[i]==T, "Make Empty", "Make Occupied"), 
        style=paste0("color: ", ifelse(Occupied[i]==T, "#000", "#fff"), "; background-color: ", ifelse(Occupied[i]==T, "#D2D2D2", "#8C2316"), "; border-color: ", ifelse(Occupied[i]==T, "#D2D2D2", "#8C2316")), 
        onclick="Shiny.onInputChange( \"button_occupied\" , this.id)"
      )
    )
  }
  inputs
}