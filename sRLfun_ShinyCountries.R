### Create leaflet map

sRLCountry_CreateLeaflet <- function(coo, Storage_SP){
  
  # Prepare extent
  EXT<-1.2*extent(coo[coo$Level0_occupied==T,])
  if(is.na(EXT[1]) | is.na(EXT[2]) | is.na(EXT[3]) | is.na(EXT[4])){EXT<-1.2*extent(distSP_WGS)} # In case there is no overlap with countries (e.g., distribution at sea because of simplification)
  
  # Load distribution
  distSP <- Storage_SP$distSP_saved %>% st_transform(., "+init=epsg:4326") %>% dplyr::group_by(origin, presence, seasonal) %>% dplyr::summarise(N= n())
  
  # Prepare colours
  col.df<-data.frame(
    Code=c("MarineFALSEFALSE", "MarineTRUEFALSE", "MarineTRUETRUE", "TerrestrialFALSEFALSE", "TerrestrialTRUEFALSE", "TerrestrialTRUETRUE"),
    Col=c("#D2D2D2", "#9595C3", "#5757A9", "white", "#F17777", "#8C2316"),
    Label=c("Absent (marine)", "Absent from subnational (marine)", "Present (marine)", "Absent (terrestrial)", "Absent from subnational (terrestrial)", "Present (terrestrial)")
  )
  
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
  coo$colour<-col.df$Col[match(coo$colour, col.df$Code)]
  
  # Create leaflet
  Leaflet_COO<-leaflet() %>%
    fitBounds(lng1=EXT[1], lng2=EXT[2], lat1=EXT[3], lat2=EXT[4]) %>%
    addPolygons(data=realms_raw, group="Realms", fillOpacity=0.5) %>%
    addPolygons(data=coo,
                color=ifelse(coo$Level0_occupied==T, "black", "grey"),
                fillColor=coo$colour,
                popup=coo$Popup,
                stroke=T, weight=2, fillOpacity=1) %>%
    addPolygons(data=distSP, color="#D69F32", fillOpacity=0.4, group="Range map") %>%
    addLegend(position="bottomleft", colors=c(col.df$Col[col.df$Col %in% coo$colour], "#D69F32"), labels=c(col.df$Label[col.df$Col %in% coo$colour], "Distribution"), opacity=1)
  
  # Add points if we had occurrences and add layer control
  if("dat_proj_saved" %in% names(Storage_SP)){
    
    Coords<-Storage_SP$dat_proj_saved %>% st_transform(., st_crs(4326)) %>% st_coordinates(.) %>% as.data.frame()
    Leaflet_COO<-Leaflet_COO %>%
      addCircleMarkers(lng=Coords[,1], lat=Coords[,2], color="black", fillOpacity=0.3, stroke=F, radius=2, group="Occurrence records") %>%
      addLayersControl(overlayGroups=c("Range map", "Occurrence records", "Realms"), position="topleft", options=layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup("Realms")
    
  } else {
    Leaflet_COO<-Leaflet_COO %>% addLayersControl(overlayGroups=c("Range map", "Realms"), position="topleft", options=layersControlOptions(collapsed = FALSE)) %>% hideGroup("Realms")
  }
  
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