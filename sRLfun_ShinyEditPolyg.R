
Pres2_error <- "Presence 'Probably Extant' is deprecated, please select another code."


sRLPolyg_InitDistri <- function(distSP, CRS){
  
  # Transform and split each polygon
  distSP <- distSP %>% st_transform(., CRS) %>% st_cast(., "MULTIPOLYGON", warn=F) %>% st_cast(., "POLYGON", warn=F) # Transform and split in multiple polygons

  # Adapt ID column (and make sure we don't have 2 polygons with same name, which can happen eg when split by line)
  if(! "ID" %in% names(distSP)){
    distSP$ID <- paste0("distSP", 1:nrow(distSP))
  } else {
    distSP$ID <- make.unique(distSP$ID, sep=".")
  }
  
  # Add popup
  distSP$Popup <- sRLPolyg_CreatePopup(distSP)
  distSP$N<-NULL
  
  return(distSP)
}



sRLPolyg_PrepareHydro <- function(distSP, hydro_raw, HydroLev, SRC_created){
  
  # Crop hydro (take all hydrobasins intersecting a 50km buffer)
  hydroSP_HQ <-  st_filter(hydro_raw, st_buffer(st_as_sfc(st_bbox(distSP)), 50000), .predicate = st_intersects)
  if(nrow(hydroSP_HQ)==0){return(data.frame())}
  
  # If level 10 or 12, charge the layers
  if(HydroLev %in% c("hydro10", "hydro12")){
    distSP$Grid_cells<-NA ; distSP$Grid_cells[1]<-paste0(unique(hydroSP_HQ$Grid_cells), collapse="")
    
    # List files to load
    Cells <- hydroSP_HQ$Grid_cells %>% unique(.) %>% paste0(., collapse="") %>% strsplit(., ",") %>% unlist(.) %>% unique(.) %>% subset(., . !="") %>% as.numeric(.)
    
    # Load shapefile of LEVEL
    Path_cells<-paste0("Hydro_cut_", sub("hydro", "", HydroLev), "/Hydrobasins_level", sub("hydro", "", HydroLev), "_cut_cell", Cells, ".shp") %>% paste0(sub("Hydrobasins_level8_ready.shp", "", config$hydrobasins_path), .)
    hydroLEV_raw<-st_read(Path_cells[1])
    if(length(Path_cells)>1){
      for(PATH in Path_cells[2:length(Path_cells)]){
        hydroLEV_toadd<- st_read(PATH)
        hydroLEV_raw<-rbind(hydroLEV_raw, hydroLEV_toadd)
      }
    }
    st_crs(hydroLEV_raw)<-CRSMOLL
    
    # Crop new hydro in the 50km buffer
    hydroSP_HQ <-  st_filter(hydroLEV_raw, st_buffer(st_as_sfc(st_bbox(distSP)), 50000), .predicate = st_intersects)
    
  }
  
  # Add binomial
  hydroSP_HQ$binomial <- distSP$binomial[1]
  
  # Simplify
  hydroSP <- hydroSP_HQ
  if(npts(hydroSP_HQ)>(50*nrow(hydroSP_HQ))){hydroSP <- ms_simplify(hydroSP, keep=(50*nrow(hydroSP))/npts(hydroSP), keep_shapes=T)}
  
  # Attributes (depends if hydrobasins from created distribution or from "Edit as hydrobasins" button)
  if("dist_comm" %in% names(distSP)){hydroSP$dist_comm <- distSP$dist_comm[1]}
  if(SRC_created=="yes"){
    # Use hybas_id column to know which hydrobasins are in the hydrobasins
    hydroSP$InDistri <- hydroSP$hybas_id %in% distSP$hybas_id
    hydroSP$presence <- distSP$presence[match(hydroSP$hybas_id, distSP$hybas_id)]
    hydroSP$origin <- distSP$origin[match(hydroSP$hybas_id, distSP$hybas_id)]
    hydroSP$seasonal <- distSP$seasonal[match(hydroSP$hybas_id, distSP$hybas_id)]

  } else {
    
    # Create centroids of hydrobasins
    hydro_centro <- st_centroid(hydroSP)
    # Intersects centroids with distSP
    inter <- st_intersection(hydro_centro, distSP)
    hydroSP$InDistri <- hydroSP$hybas_id %in% inter$hybas_id
    # Take attributes
    hydroSP$presence <- inter$presence[match(hydroSP$hybas_id, inter$hybas_id)]
    hydroSP$origin <- inter$origin[match(hydroSP$hybas_id, inter$hybas_id)]
    hydroSP$seasonal <- inter$seasonal[match(hydroSP$hybas_id, inter$hybas_id)]
  }
  
  # Unique ID
  hydroSP$ID <- paste0("distSP", 1:nrow(hydroSP))
  
  # Popup
  hydroSP$Popup <- sRLPolyg_CreatePopup(hydroSP)
  
  # Transform
  hydroSP_HQ <- hydroSP_HQ %>% st_transform(., 4326)
  hydroSP <- hydroSP %>% st_transform(., 4326)
  hydroSP$N<-NULL
  
  return(list(hydroSP_HQ=hydroSP_HQ, hydroSP=hydroSP))
  
}



### Function to prepare tooltips for leafpm toolbar to include in onRender (it's important that only those used in the leaflet are listed here)
sRLPolyg_PrepareRender <- function(param){
  
  CMD <- "function(el, x){"
  
  if("Polygon" %in% param){CMD <- paste0(CMD, "document.querySelector('.control-icon.leaflet-pm-icon-polygon') .setAttribute('title', 'Draw a new polygon');")}
  if("Polyline" %in% param){CMD <- paste0(CMD, "document.querySelector('.control-icon.leaflet-pm-icon-polyline') .setAttribute('title', 'Split polygons');")}
  if("Edit" %in% param){CMD <- paste0(CMD, "document.querySelector('.control-icon.leaflet-pm-icon-edit') .setAttribute('title', 'Edit polygons');")}
  if("Drag" %in% param){CMD <- paste0(CMD, "document.querySelector('.control-icon.leaflet-pm-icon-drag') .setAttribute('title', 'Drag polygons');")}
  if("Marker" %in% param){CMD <- paste0(CMD, "document.querySelector('.control-icon.leaflet-pm-icon-marker') .setAttribute('title', 'Place markers in hydrobasins to edit');")}
  
  CMD <- paste0(CMD, "}")
  
  return(CMD)
}

### Create leaflet map
sRLPolyg_CreateLeaflet <- function(AllowEdit, hydro3_stored=data.frame()){

  ### Create map
  LEAF <- leaflet(options = leafletOptions(doubleClickZoom= FALSE)) %>%
    addTiles()  %>%
    addEsriBasemapLayer(esriBasemapLayers$Imagery, group = "Satellite") %>%
    addEsriBasemapLayer(esriBasemapLayers$Topographic, group = "Topography") %>%
    addMouseCoordinates() %>%
    addScaleBar(position="bottomright") %>%
    addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), position="bottomleft")
  
  # Add hydro3 for freshwater
  if(nrow(hydro3_stored)>0){
    LEAF <- LEAF %>%
      addPolygons(data=hydro3_stored, color="darkblue", stroke=T, fill=F, group="Hydrobasins-3") %>%
      hideGroup("Hydrobasins-3")
  }
  
  ### Set toolbar options: if users decided not to simplify a complex distribution, keep the original distribution but remove option to edit manually; if hydrobasins then only markers
  if(AllowEdit=="yes"){OPTs_pm <- list(targetGroup="editable", toolbarOptions = pmToolbarOptions(drawMarker=F, drawPolygon=T, drawPolyline=T, drawCircle=F, drawRectangle=F, removalMode=F, cutPolygon=F)) ; Render_TXT <- sRLPolyg_PrepareRender(param=c("Polygon", "Polyline", "Edit", "Drag"))}
  if(AllowEdit=="no"){OPTs_pm <- list(targetGroup="editable", toolbarOptions = pmToolbarOptions(drawMarker=F, drawPolygon=T, drawPolyline=T, drawCircle=F, drawRectangle=F, removalMode=F, cutPolygon=F, editMode=F)) ; Render_TXT <- sRLPolyg_PrepareRender(param=c("Polygon", "Polyline", "Drag"))}
  if(AllowEdit=="hydro"){OPTs_pm <- list(editOptions=pmEditOptions(draggable=F, snappable=F, snapDistance=1), drawOptions=pmDrawOptions(snappable=F), toolbarOptions = pmToolbarOptions(drawMarker=T, drawPolygon=T, drawPolyline=F, drawCircle=F, drawRectangle=F, removalMode=F, cutPolygon=F, editMode=F)) ; Render_TXT <- sRLPolyg_PrepareRender(param=c("Marker", "Drag"))}
  
  ### Create module
  EDIT <- callModule(
    editMod,
    leafmap = LEAF %>% htmlwidgets::onRender(Render_TXT),
    id = "map",
    record = FALSE,
    sf = TRUE,
    targetLayerId = "editable",
    editor="leafpm",
    editorOptions=OPTs_pm
  )
  
  return(EDIT)
  
}

STYLE="label.control-label, .selectize-control.single {
         display: table-cell;
         text-align: center;
         vertical-align: middle;
      }
      label.control-label {
        padding-right: 10px;
      }
      .form-group {
        display: table-row;
      }
      .selectize-control.single div.item {
        padding-right: 15px;
      }"



sRLPolyg_CreatePopup <- function(distSP){
  
  if("hybas_id" %in% names(distSP)){
    distSP$ConditionHydro <- ifelse(is.na(distSP$presence), "hydroEmpty", "hydroOccupied")
    Popup <- paste0("<b>Hydrobasin ID: ", distSP$hybas_id, " </b><br><br>")
  } else {
    distSP$ConditionHydro <- "no"
    Popup <- rep("", nrow(distSP))
  }
  

  Popup <- paste0(
    Popup,
    "<b> Edit Polygons: ","</b><br>",
    shinyInputRmPolygon(actionButton, distSP$ID, distSP$ConditionHydro),
    "<br><br>")
  
  ### Add individual smooth / simplify options (except for hydrobasins)
  distSub <- subset(distSP, distSP$ConditionHydro != "hydroEmpty")
  
  if("no" %in% distSP$ConditionHydro){
    Popup[distSP$ConditionHydro != "hydroEmpty"] <- paste0(
      Popup[distSP$ConditionHydro != "hydroEmpty"],
      shinyInputActionPolygon(distSub, "Smooth"),
      shinyInputActionPolygon(distSub, "Simplify"),
      "</center><br><br>"
    )
  } else {
    Popup[distSP$ConditionHydro != "hydroEmpty"] <- paste0(
      Popup[distSP$ConditionHydro != "hydroEmpty"],
      "<b> Complete hydrobasins: ","</b><br>",
      "<center>",
      shinyInputActionPolygon(distSub, "Upstream"), "    ",
      shinyInputActionPolygon(distSub, "Downstream"), "<br>",
      shinyInputActionPolygon(distSub, "Catchment"),
      "</center><br>"
    )
  }
  
  ### Add attributes selectors (except if empty hydrobasin)
  Popup[distSP$ConditionHydro != "hydroEmpty"] <- paste0(
    Popup[distSP$ConditionHydro != "hydroEmpty"],
    
    "<b> Edit Presence: ","</b><br>",
    shinyInputEditAttribute(distSub, "presence", 1),
    shinyInputEditAttribute(distSub, "presence", 2),
    shinyInputEditAttribute(distSub, "presence", 3),
    shinyInputEditAttribute(distSub, "presence", 4),
    shinyInputEditAttribute(distSub, "presence", 5),
    shinyInputEditAttribute(distSub, "presence", 6),
    shinyInputEditAttribute(distSub, "presence", 7),
    "<br>",
    
    "<b> Edit Origin: ","</b><br>",
    shinyInputEditAttribute(distSub, "origin", 1),
    shinyInputEditAttribute(distSub, "origin", 2),
    shinyInputEditAttribute(distSub, "origin", 3),
    shinyInputEditAttribute(distSub, "origin", 4),
    shinyInputEditAttribute(distSub, "origin", 5),
    shinyInputEditAttribute(distSub, "origin", 6),
    "<br>",
    
    "<b> Edit Seasonal: ","</b><br>",
    shinyInputEditAttribute(distSub, "seasonal", 1),
    shinyInputEditAttribute(distSub, "seasonal", 2),
    shinyInputEditAttribute(distSub, "seasonal", 3),
    shinyInputEditAttribute(distSub, "seasonal", 4),
    shinyInputEditAttribute(distSub, "seasonal", 5),
    "<br><br>"
  )
  
  return(Popup)
}



sRLPolyg_UpdateLeaflet <- function(distSP, dat_pts, frame, PolygRemove="", AllowEdit="yes", dat_comparison){
  
  # Update map 
  distSP$cols <- sRL_ColourDistrib(distSP)$cols %>% replace(., .=="white", "black") %>% replace(., .=="#FFE4E1", "darksalmon")
  
  leafletProxy("map-map") %>% 
    removeShape(layerId = c(distSP$ID, paste0("distSP", 1:10000), 1:10000, PolygRemove)) %>% # To make sure we remove all polygons (even if some were removed and thus are not in distSP$ID anymore)
    addPolygons(data=distSP, color=distSP$cols, stroke=T, weight=2, fillOpacity=ifelse(distSP$cols=="black", 0.1, 0.5), group="editable", layerId=distSP$ID, popup=distSP$Popup, popupOptions = popupOptions(autoPan=T, keepInView = T, closeOnClick=T))
  
  # Add data points (has to be done each time to be on top of polygons)
  if(is.null(nrow(dat_pts))==F){
    if(dat_pts$Pts_type[1]=="1b"){
      leafletProxy("map-map") %>% 
        addCircleMarkers(lng = dat_pts$lon, lat = dat_pts$lat, radius=5, stroke=F, fillOpacity=0.8, color="black", popup=dat_pts$PopText, group="Occurrences")
    } else {
      leafletProxy("map-map") %>% 
        addCircleMarkers(lng = dat_pts$lon, lat = dat_pts$lat, radius=5, stroke=F, fillOpacity=0.8, color=revalue(dat_pts$ValidDistri, c("In"="#fdcb25ff", "Out"="#EA5F94", "Invalid"="#440154ff")), popup=dat_pts$PopText, group="Occurrences") %>%
        clearControls() %>%
        addLegend(position="bottomleft", colors=c('#fdcb25ff', '#EA5F94', '#440154ff'), labels=c("Inside distribution", "Outside distribution", "Invalid record"))
      }
  }
  
  # Update control layer
  CTRL_var <- c(ifelse(is.null(nrow(dat_pts)), NA, "Occurrences"), ifelse("hybas_id" %in% names(distSP), "Hydrobasins-3", NA)) %>% .[is.na(.)==F]
  if(length(CTRL_var)>0){
    leafletProxy("map-map") %>% 
      addLayersControl(baseGroups=c("OpenStreetMap", "Satellite", "Topography"), overlayGroups=CTRL_var, position="bottomleft") 
  }
  
  # Update framing if frame==1
  if(frame==1){
    # Get extent
    EXT <- ext(distSP)*1.1 %>% as.numeric()
    
    # Update map
    leafletProxy("map-map") %>% 
      fitBounds(lng1 = as.numeric(EXT[1]), lng2 = as.numeric(EXT[2]), lat1 = as.numeric(EXT[3]), lat2 = as.numeric(EXT[4]))
  }
}


### Smooth function
sRLPolyg_Smooth <- function(distSP, Smooth_par){
  
  if(npts(distSP) < 20){distSP <- smoothr::densify(distSP, n=5)}
  
  distSP_smoothed <- distSP %>% 
    smoothr::smooth(., method = "ksmooth", smoothness=(exp(as.numeric(Smooth_par)/20)-1), max_distance=10000) %>%
    st_make_valid(.) %>% 
    .[which(grepl("POLYGON", st_geometry_type(.))),] %>% 
    st_cast(., "MULTIPOLYGON", warn=F) %>% 
    st_cast(., "POLYGON", warn=F) # For unknown reasons I need to first cast to multipolygons (github topic)
  
  return(distSP_smoothed)
}



### Create buttons to add or remove polygons
shinyInputRmPolygon <- function(FUN, ID, Hydro_par) {
  inputs <- character(length(ID))
  for (i in 1:length(ID)) {
    inputs[i] <- as.character(
      FUN(paste0(revalue(Hydro_par[i], c("no"="RmPolygon_", "hydroOccupied"="RmHydro_", "hydroEmpty"="AddHydro_"), warn_missing=F), ID[i]), #
          label=revalue(Hydro_par[i], c("no"="Remove polygon", "hydroOccupied"="Remove hydrobasin", "hydroEmpty"="Add hydrobasin"), warn_missing=F),
          icon=icon(name=as.character(ifelse(Hydro_par[i]=="hydroEmpty", "plus", "trash"))),#as.character(ifelse(Hydro_par[i]=="no", "trash", "plus")), 
          style=ifelse(Hydro_par[i]=="hydroEmpty", "color: #fff; background-color: #009138ff; border-color: #009138ff", "color: #fff; background-color: #dea218ff; border-color: #dea218ff"),
          onclick="Shiny.onInputChange( \"button_RmPolygon\" , this.id)"
      )
    )
  }
  inputs
}


### Button to smooth or simplify individual polygons
shinyInputActionPolygon <- function(distSP, Action_type) {
  
  # Adapt BUTT_raw with information fitting all polygons ; I'm adding TIME to allow users to click twice on the same button after changing the parameter value
  BUTT_raw <- as.character(
    tooltip(trigger=actionButton(paste0(Action_type, "Polygon_#ID_#TIME"),
                                 label=Action_type, 
                                 style="color: #fff; background-color: #009138ff; border-color: #009138ff; padding:6px",
                                 onclick=paste0("Shiny.onInputChange( \"", paste0(Action_type, "Button"), "\" , this.id)")
    ), revalue(Action_type, c("Smooth"="Smooth a single polygon", "Simplify"="Simplify a single polygon", "Downstream"="Add downstream hydrobasins", "Upstream"="Add upstream hydrobasins", "Catchment"="Add all hydrobasins within the watershed"), warn_missing=F))
  )
  
  BUTT <- BUTT_raw %>% strsplit(., "#ID") %>% unlist(.) %>% strsplit(.,"#TIME") %>% unlist(.)
  Buttons <- paste0(BUTT[1], distSP$ID, BUTT[2], "#TIME", round(as.numeric(Sys.time())), BUTT[3])
  
  return(Buttons)
}



### Create the buttons to edit attributes
shinyInputEditAttribute <- function(distSP, Attribute, NewVal) {
  
  # Adapt BUTT_raw with information fitting all polygons
  BUTT_raw <- as.character(
    tooltip(trigger=actionButton(paste0("EditAtt_Attribute=", Attribute, "_SiteID=#ID_NewVal=", NewVal),
                                 label=NewVal, 
                                 style="color: #fff; background-color: #COL; border-color: #COL; padding:6px; font-size:110%",
                                 onclick="Shiny.onInputChange( \"button_EditAtt\" , this.id)"
    ), ifelse((Attribute=="presence" & NewVal==2), "Probably Extant - DISABLED", sRL_CountriesAttributes(NewVal, Attribute, "num2char")))
  )
  
  # Select the column of current attribute
  if(Attribute=="presence"){distSP$CurrentVal<-distSP$presence}
  if(Attribute=="origin"){distSP$CurrentVal<-distSP$origin}
  if(Attribute=="seasonal"){distSP$CurrentVal<-distSP$seasonal}
  
  # Identify colour for each polygon (dark for the button with the value of NewVal)
  COLs <- ifelse(distSP$CurrentVal == NewVal, "#66C656", "#B0F49F")
  
  # Cut BUTT_raw in 3 parts
  BUTT <- BUTT_raw %>% strsplit(., "#ID") %>% unlist(.) %>% strsplit(., "#COL") %>% unlist(.)
  
  # Merge for each polygon
  Buttons <- paste0(BUTT[1], distSP$ID, BUTT[2], COLs, BUTT[3], COLs, BUTT[4])
  
  return(Buttons)
}

