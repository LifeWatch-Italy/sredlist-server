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
    shinyInputCountry(actionButton, paste(coo$SIS_name0, coo$SIS_name1, sep="/"), Occupied = coo$Level1_occupied)
  )
  
  # Define colour
  coo$colour<-paste0(coo$Domain, coo$Level0_occupied, coo$Level1_occupied) 
  coo$colour<-sRL_COOColours$Col[match(coo$colour, sRL_COOColours$Code)]
  
  # Create leaflet from sRLfun_Mapping function
  Leaflet_COO <- sRL_LeafCountry(coo, distSP, realms_raw, Storage_SP)
  
  
  return(Leaflet_COO)
  
} 



shinyInputCountry <- function(FUN, Country, Occupied) {
  inputs <- character(length(Country))
  for (i in 1:length(Country)) {
    inputs[i] <- as.character(
      FUN(
        paste0(
          ifelse(Occupied[i]==T, "makeempty_", "makeoccupied_"), 
          gsub(" ", "_", Country[i])
        ),
        label=ifelse(Occupied[i]==T, "Make Empty", "Make Occupied"), 
        style=paste0("color: ", ifelse(Occupied[i]==T, "#000", "#fff"), "; background-color: ", ifelse(Occupied[i]==T, "#D2D2D2", "#8C2316"), "; border-color: ", ifelse(Occupied[i]==T, "#D2D2D2", "#8C2316")), 
        onclick="Shiny.onInputChange( \"button_occupied\" , this.id)"
      )
    )
  }
  inputs
}