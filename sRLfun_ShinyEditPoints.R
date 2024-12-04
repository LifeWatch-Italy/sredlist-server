### Create leaflet map

sRLMan_CreateLeaflet <- function(){
  
  leaflet() %>%
    addTiles()  %>%
    addLegend(position="bottomleft", colors=c('#fde725ff', '#440154ff'), labels=c("Valid", "Not valid")) %>%
    addMouseCoordinates() %>%
    addScaleBar(position="bottomright") %>%
    leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE, position="topright")) %>%
    addDrawToolbar(
      polylineOptions = FALSE,
      circleOptions = FALSE,
      markerOptions = drawMarkerOptions(repeatMode = TRUE),
      circleMarkerOptions = FALSE,
      editOptions=editToolbarOptions(edit=FALSE, remove=FALSE)
    ) %>%
    onRender("function(el, x){
               var map = this;
               var polygons = [];
               map.on('layeradd', function(e) {
                 polygons.push(e.layer);
               });
               $('#save').on('click', function() {
                 var features = [];
                 for(var polygon of polygons) {
                   if(polygon.feature) {
                     features.push(polygon);
                     map.removeLayer(polygon);
                   }
                 }
                 polygons = [];
                 var group = L.layerGroup(features);
                 var collection = group.toGeoJSON(false);
                 var n = collection.features.length;
                 var i = 0;
                 var interval = setInterval(function() {
                    if(i === n) {
                      clearInterval(interval);
                    } else {
                      var f = collection.features[i];
                      var deleted = {type: 'FeatureCollection', features: [f]};
                      Shiny.setInputValue(el.id + '_draw_deleted_features', deleted);
                      i = i + 1;
                    }
                 });
               });
             document.querySelector('.leaflet-draw-draw-polygon') .setAttribute('title', 'Draw a polygon around records to exclude');
             document.querySelector('.leaflet-draw-draw-rectangle') .setAttribute('title', 'Draw a rectangle around records to exclude');
             document.querySelector('.leaflet-draw-draw-marker') .setAttribute('title', 'Draw new records');
             }")
  
} 




sRLMan_UpdateLeaflet <- function(flagsSF, frame, Drag){
  
  # Update Popup
  flagsSF <- sRL_PopRecords(flagsSF)
  
  # Update map
  leafletProxy("map-map") %>% 
    removeMarker(layerId = paste0("CircleFlags", 1:nrow(flagsSF))) %>%
    addCircleMarkers(lng=flagsSF$Lon_jitt,
                     lat=flagsSF$Lat_jitt,
                     color=ifelse(is.na(flagsSF$Reason)==T, "#fde725ff", "#440154ff"),
                     layerId=paste0("CircleFlags", 1:nrow(flagsSF)),
                     fillOpacity=0.5,
                     stroke=F,
                     popup=paste0(
                       flagsSF$PopText, '<br>', 
                       shinyInputPts(actionButton, flagsSF$gbifID, Valid = is.na(flagsSF$Reason))
                     ),
                     radius=8,
                     options = markerOptions(draggable = Drag))
  
  # Update framing if frame==1
  if(frame==1){
    # Get extent
    EXT <- ext(flagsSF)*1.1 %>% as.numeric()
    
    # Update map
    leafletProxy("map-map") %>% 
      fitBounds(lng1 = as.numeric(EXT[1]), lng2 = as.numeric(EXT[2]), lat1 = as.numeric(EXT[3]), lat2 = as.numeric(EXT[4]))
  }
  
}




sRLMan_EditPoints <- function(EditsGeom, flagsSF, Pts_year, Pts_source){
  
  
  ### Remove points overlapping with polygon
  if("rectangle" %in% EditsGeom$feature_type | "polygon" %in% EditsGeom$feature_type){
    Points_to_rm<-st_intersection(flagsSF, EditsGeom[EditsGeom$feature_type %in% c("polygon", "rectangle"),])
    flagsSF$Manu_removed<-flagsSF$gbifID %in% Points_to_rm$gbifID
    flagsSF$Reason[flagsSF$Manu_removed==T]<-"Manually_removed"
    flagsSF$Manu_removed<-NULL
  }

  ### Add points added by user
  if("marker" %in% EditsGeom$feature_type){
    
    ## Check if we already have manual points to adapt the numerotation
    NUM <- flagsSF$gbifID %>% .[grepl("Manual_", .)] %>% sub("Manual_", "", .) %>% as.numeric(.) %>% max(c(., 0), na.rm=T)

    ## Populate Points to add
    Points_to_add<-EditsGeom[EditsGeom$feature_type=="marker",]
    Points_to_add$gbifID<-paste0("Manual_", (NUM+1):(NUM+nrow(Points_to_add)))
    Points_to_add$Reason<-NA
    Points_to_add$Source_type<-"Manually added"
    Points_to_add$decimalLongitude<-st_coordinates(Points_to_add)[,1]
    Points_to_add$decimalLatitude<-st_coordinates(Points_to_add)[,2]
    Points_to_add$leaflet_id<-Points_to_add$feature_type<-NULL
    Points_to_add$Lon_jitt <- Points_to_add$decimalLongitude
    Points_to_add$Lat_jitt <- Points_to_add$decimalLatitude
    
    Points_to_add$year <- Pts_year
    Points_to_add$source <- Pts_source
    
    ## Merge
    flagsMerged <- rbind.fill(as.data.frame(flagsSF), as.data.frame(Points_to_add))
    flagsMerged$geometry <- NULL
    flagsSF <- st_as_sf(flagsMerged, coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84", remove=F)
  }

  # Edit popups
  flagsSF <- sRL_PopRecords(flagsSF)
  
  # Rerun jitter function
  flagsSF <- sRLMan_JitterPoints(flagsSF)
  
  return(flagsSF)
}




sRLMan_JitterPoints <- function(flagsSF){
  
  # Jitter coordinates if they are not unique
  print("JITTER")
  flags_jitt <- flagsSF %>% st_jitter(., amount=0.0002) %>% st_coordinates(.)
  flagsSF$Coords_merged <- paste0(flagsSF$decimalLongitude, flagsSF$decimalLatitude) 
  flagsSF$unique <- !(flagsSF$Coords_merged %in% flagsSF$Coords_merged[duplicated(flagsSF$Coords_merged)])
  flagsSF$Lon_jitt <- flagsSF$decimalLongitude ; flagsSF$Lat_jitt <- flagsSF$decimalLatitude
  flagsSF$Lon_jitt[flagsSF$unique==F]<-flags_jitt[flagsSF$unique==F, 1]
  flagsSF$Lat_jitt[flagsSF$unique==F]<-flags_jitt[flagsSF$unique==F, 2]
  
  return(flagsSF)
  
}



sRLMan_MovePoints <- function(flagsFUN, Pts_dragged){
  
# Apply changes in coordinates
flagsFUN$ID <- paste0("CircleFlags", 1:nrow(flagsFUN))

flagsFUN$decimalLongitude[flagsFUN$ID==Pts_dragged$id] <- flagsFUN$Lon_jitt[flagsFUN$ID==Pts_dragged$id] <- Pts_dragged$lng
flagsFUN$decimalLatitude[flagsFUN$ID==Pts_dragged$id] <- flagsFUN$Lat_jitt[flagsFUN$ID==Pts_dragged$id] <- Pts_dragged$lat

# Add note that it was moved
flagsFUN$Source_type[flagsFUN$ID==Pts_dragged$id] <- flagsFUN$Source_type[flagsFUN$ID==Pts_dragged$id] %>% sub("_Dragged", "", .) %>% paste0(., "_Dragged")
flagsFUN <- sRL_PopRecords(flagsFUN)

return(flagsFUN)
}


shinyInputPts <- function(FUN, SPs, Valid) {
  inputs <- character(length(SPs))
  for (i in 1:length(SPs)) {
    inputs[i] <- as.character(
      FUN(
        paste0(
          ifelse(Valid[i]==T, "makeinvalid_", "makevalid_"), 
          sub(" ", "_", SPs[i])
        ),
        label=ifelse(Valid[i]==T, "Make Invalid", "Make Valid"), 
        style=paste0("color: ", ifelse(Valid[i]==T, "#fff", "#000"), "; background-color: ", ifelse(Valid[i]==T, "#440154ff", "#fde725ff"), "; border-color: ", ifelse(Valid[i]==T, "#440154ff", "#fde725ff")), 
        onclick="Shiny.onInputChange( \"button_valid\" , this.id)"
      )
    )
  }
  inputs
}