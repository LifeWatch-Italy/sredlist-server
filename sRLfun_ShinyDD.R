
### Prepare the plot to display
DDfun_PlotIsoc<-function(GR, Tab_subset, Sel_SP){
  print(paste0("START - Plot ", GR))
  
  DD_GR<-subset(DD, Group==GR & is.na(PrioDS_raw)==F)
  QUANT<-quantile(DD_GR$PrioDS_raw, probs=c(0,0.5,0.75,0.9,1))
  DD_GR$Priority<-cut(DD_GR$PrioDS_raw, breaks=QUANT, include.lowest=T, labels=c("Low", "Medium", "High", "Top")) %>% as.character(.)
  DD_GR$Priority[DD_GR$AOHlost<=(-0.2) | DD_GR$Forestloss<=(-0.2)]<-"Top (Habitat loss: NT)"
  DD_GR$Priority[DD_GR$AOHlost<=(-0.3) | DD_GR$Forestloss<=(-0.3)]<-"Top (Habitat loss: threatened)"
  DD_GR$Priority <- DD_GR$Priority %>% as.factor(.) %>% factor(., levels=c("Top", "Top (Habitat loss: NT)", "Top (Habitat loss: threatened)", "High", "Medium", "Low"))
  
  Isoclines<-data.frame(X=seq(0,1,0.01))
  suppressWarnings(Isoclines$Y1<- 1-sqrt(2*(1-QUANT[2])^2- (1-Isoclines$X)^2))
  suppressWarnings(Isoclines$Y2<- 1-sqrt(2*(1-QUANT[3])^2- (1-Isoclines$X)^2))
  suppressWarnings(Isoclines$Y3<- 1-sqrt(2*(1-QUANT[4])^2- (1-Isoclines$X)^2))
  Isoclines[, c("Y1", "Y2", "Y3")]<-replace(Isoclines[, c("Y1", "Y2", "Y3")], Isoclines[, c("Y1", "Y2", "Y3")]<0, NA)
  
  LAB<-paste0("<b>", DD_GR$scientific_name, "</b><br>PrioDS=", round(100*DD_GR$PrioDS), "%")
  
  DD_GR$Alpha<-ifelse(DD_GR$scientific_name %in% Tab_subset$SP, 1, 0.2)

  suppressWarnings(
    G<-ggplot()+
      geom_line(data=Isoclines, aes(x=X, y=Y1), col="#de77ae", linetype="dashed")+
      geom_line(data=Isoclines, aes(x=X, y=Y2), col="#c51b7d", linetype="dashed")+
      geom_line(data=Isoclines, aes(x=X, y=Y3), col="#8e0152", linetype="dashed")+
      geom_point(data=DD_GR, aes(x=pDS, y=dpDS, col=Priority, fill=Priority, text=LAB, size=Priority, customdata=scientific_name), alpha=DD_GR$Alpha, shape=21)+
      scale_colour_manual(values=c("#8e0152", "black", "black", "#c51b7d", "#de77ae", "gray75"), drop=FALSE)+
      scale_fill_manual(values=c("#8e0152", "white", "black", "#c51b7d", "#de77ae", "gray75"), drop=FALSE)+
      scale_size_manual(values=c(1,2,2,1,1,1), drop=FALSE)+
      ylim(c(min(DD_GR$dpDS), max(DD_GR$dpDS)))+
      xlab("Probability to become data sufficient")+ylab("Increase in probability since last assessment")+
      theme_minimal() %+replace% theme(plot.title=element_text(hjust=0, size=12, face="bold"), legend.position='bottom')+ 
      ggtitle(paste0(GR, " (N=", nrow(DD_GR), ")"))
  )

  # Add selected points
  if(length(Sel_SP)>0){
    G<-G+geom_point(data=DD_GR[DD_GR$scientific_name %in% Sel_SP,], aes(x=pDS, y=dpDS), shape=21, size=7, col="#0d6efdff", fill=NA, stroke=2)
  }

  # Transform into ggplotly
  suppressWarnings(
    G %>%
      ggplotly(., tooltip="LAB") %>% 
      plotly::config(displayModeBar = "static", displaylogo = FALSE, modeBarButtonsToRemove = list("toImage", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
  )

}





### Prepare the table to display
DDfun_Table<-function(GR){
  print(paste0("START - Table ", GR))
  
  ### For init, I need a 1 line table, so I change the group of the first line
  if(GR=="Init"){DD$Group[1] <- "Init"}
  
  ### Keep only species from the group
  DD_sub<-subset(DD, Group==GR & DD==TRUE)
  DD_sub$List<-NA
  
  ### Add increase in GBIF and OBIS (quite weird calculation but I'm happy with the order it gives, important to prioritise a species that went from 0 to 50 over a species that went from 0 to 1)
  DD_sub$inc.GBIF<-round((100*(DD_sub$nb_GBIFgeo - DD_sub$nb_GBIFgeoASS)/(1+DD_sub$nb_GBIFgeoASS))) %>% as.integer(.)
  DD_sub$inc.WOS<-round((100*(DD_sub$WOS - DD_sub$WOSASS)/(1+DD_sub$WOSASS))) %>% as.integer(.)
  
  ### Recreate last assessment date
  DD_sub$Last_assessment <- as.integer(2022-(DD_sub$time_desc-DD_sub$time_descASS))
  
  ### Remove columns that should not be displayed + order columns
  LIST_Vars<-c("List", "scientific_name", "taxo_valid", "RLAuthority", "Realm", "Last_assessment", "PrioDS", "pDS", "dpDS", "AOHlost", "Forestloss", "nb_GBIFgeo", "nb_GBIFgeoASS", "inc.GBIF", "WOS", "WOSASS", "inc.WOS", "site", "taxonid")
  if(GR %in% c("Odonata", "Amphibian")){LIST_Vars<-LIST_Vars[LIST_Vars != "RLAuthority"]}
  DD_sub<-subset(DD_sub, select=LIST_Vars)
  
  ### Round variables
  DD_sub[,names(DD_sub) %in% c("pDS", "dpDS", "AOHlost", "Forestloss", "PrioDS")] <- round(DD_sub[,names(DD_sub) %in% c("pDS", "dpDS", "AOHlost", "Forestloss", "PrioDS")], 3)
  
  ### Habitat variables in %
  DD_sub$AOHlost<-round(100*DD_sub$AOHlost)
  DD_sub$Forestloss<-round(100*DD_sub$Forestloss)
  
  ### Transform fields into factor to choose among modalities
  if("RLAuthority" %in% names(DD_sub)){DD_sub$RLAuthority<-as.factor(DD_sub$RLAuthority)}
  DD_sub$Realm<-as.character(DD_sub$Realm) # It has to stay a character to filter Realm1|Realm2
  DD_sub$taxo_valid<-as.factor(DD_sub$taxo_valid) ; names(DD_sub)[names(DD_sub)=="taxo_valid"]<-"Family" ; DD_sub$taxo_valid<-NULL

  ### Add link to assessment on species name
  DD_sub$SP<-DD_sub$scientific_name # Keep species name without links (for Action buttons)
  # Add new names when the name changed
  DD_sub$current_name <- speciesRL$scientific_name[match(DD_sub$taxonid, speciesRL$taxonid)]
  DD_sub$scientific_name<-ifelse((DD_sub$current_name==DD_sub$scientific_name | is.na(DD_sub$current_name)), DD_sub$scientific_name, paste0(DD_sub$current_name, " (", DD_sub$scientific_name, ")"))
  DD_sub$scientific_name<-paste0("<a href='", DD_sub$site, "' target='_blank'>", DD_sub$scientific_name,"</a>")
  DD_sub$site <-  DD_sub$taxonid <- DD_sub$current_name <- NULL
  
  ### PrioDS in %
  DD_sub$PrioDS<-round(100*DD_sub$PrioDS, 1)
  
  ### Add action buttons to get more information on species
  DD_sub$More<-NA
  DD_sub$More<-shinyInput(actionButton, DD_sub$SP, 'button_', label = "More", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
  DD_sub$List<-shinyInput(actionButton, DD_sub$SP, 'buttonList_', label = "+", onclick = 'Shiny.onInputChange(\"list_button\",  this.id)' )
  
  ### Rename some variables
  names(DD_sub)<-revalue(names(DD_sub), c("nb_GBIFgeo"="GBIF_now", "nb_GBIFgeoASS"="GBIF_assess", "WOS"="WOS_now", "WOSASS"="WOS_assess"))
  
  ### Add group (just to control we are on the correct group when we click on "More information")
  DD_sub$Group <- GR
  
  ### Return
  return(DD_sub)
  
}




### Subset Table by brushing
DDfun_SelectTable <- function(Tab){
  print(paste0("START - Select table"))
  
  # Extract selection
  LimitSel<-event_data("plotly_selected")

  # Subset lines (only if LimitSel is not NULL, otherwise keep the full table)
  if(is.null(LimitSel)==F){
    Tab<-subset(Tab, SP %in% LimitSel$customdata)
  }

  # Return
  return(Tab)
  
}





### Prepare table tooltips
DDfun_Tooltips<-function(NAMES){
  
  tooltips<<-revalue(NAMES, c(
    "scientific_name"="Name of the species and link to Red List assessment",
    "Family"="Taxonomic family (as classified in the Red List)",
    "Last_assessment"="Year of last assessment",
    "Realm"="Realms of species occurrence",
    "RLAuthority"="Name of the Red List Authority",
    "PrioDS"="Ranked priority for reassessment (0-100), considering pDS, dpDS and habitat loss",
    "pDS"="Probability of being reclassified in a data sufficient category",
    "dpDS"="Increase in probability of being reclassified in a data sufficient category since last assessment",
    "AOHlost"="Percentage habitat loss measured from the ESA-CCI land-cover maps. Species with values below -20% are considered top priority for reassessment as it suggests the species might be reclassified as NT ]-30;-20], VU ]-50;-30], EN ]-80;-50] or CR [-100;-80] under criterion A2",
    "Forestloss"="Percentage habitat loss measured from the Global Forest Change data (only calculated for forest specialists). Species with values below -20% are considered top priority for reassessment as it suggests the species might be reclassified as NT ]-30;-20], VU ]-50;-30], EN ]-80;-50] or CR [-100;-80] under criterion A2",
    "GBIF_now"="Number of georeferrenced GBIF occurrence records currently (at last update)",
    "GBIF_assess"="Number of georeferrenced GBIF occurrence records before last assessment",
    "inc.GBIF"="Increase in GBIF occurrence records since last assessment; calculated as 100*(GBIF_now - GBIF_assess)/(1+GBIF_assess)",
    "WOS_now"="Number of published articles in the Web of Science currently (at last update)",
    "WOS_assess"="Number of published articles in the Web of Science before last assessment",
    "inc.WOS"="Increase in published articles in the Web of Science since last assessment; calculated as 100*(WOS_now - WOS_assess)/(1+WOS_assess)",
    "More"="Click to visualise additional information on that species (e.g., map of GBIF records, list of published articles)"
  ))
  
  return(tooltips)
}



### Prepare GBIF interactive map
DDfun_GBIFLeaf<-function(SP){
  print(paste0("START - Leaflet ", SP))
  
  tryCatch({
    distSP<-data.frame()
    distSP<-st_read(paste0(config$distribution_path, SP, "/", gsub(" ", "_", SP), "_RL/", SP, ".shp")) %>% st_transform(., "+init=epsg:4326")
  }, error=function(e){cat(paste0("No distribution for ", SP, ". \n"))})
  
  # Download and remove records with no spatial coordinates
  GBIF <- rgbif::occ_search(scientificName = SP, hasCoordinate = T, limit=1000)$data 
  
  # Prepare GBIF data
  if(is.null(nrow(GBIF))==F){
    GBIF <- GBIF %>% filter(!is.na(decimalLongitude)) %>% filter(!is.na(decimalLatitude))
    
    # Year column
    if(! "year" %in% names(GBIF)){GBIF$year<-NA}
    GBIF$New_data<- GBIF$year > as.numeric(DD$Last_assessment[DD$scientific_name==SP])
    GBIF$colour<-revalue(as.character(GBIF$New_data), c("TRUE"="#C93149", "FALSE"="#2316E1", "NA"="gray60"))
    
    # Popup text
    GBIF$PopText<- paste0("<b>", ifelse(is.na(GBIF$New_data), "No observation date", revalue(as.character(GBIF$New_data), c("TRUE"="New observation since last assessment", "FALSE"="Observation made before last assessment"))),"</b>", "<br>", "<br>",
                          "<b>","Observation ID: ","</b>", paste0("<a href='", "https://gbif.org/occurrence/", GBIF$gbifID, "' target='_blank'>", GBIF$gbifID, "</a>"), "<br>",
                          "<b>","Year: ","</b>", GBIF$year, "<br>",
                          "<b>","Uncertainty (km): ","</b>", as.numeric(as.character(GBIF$coordinateUncertaintyInMeters))/1000, "<br>")
  }
  
  
  # Create map
  GBIF_leaflet <- leaflet() %>%
    addTiles(group="OpenStreetMap") %>%
    addMouseCoordinates() %>%   addScaleBar(position="bottomright")
  
  
  if(nrow(distSP)>0){GBIF_leaflet<-GBIF_leaflet %>% addPolygons(data=distSP, color="#D69F32", fillOpacity=0.5, group="Distribution")}
  
  if(is.null(nrow(GBIF))){
    GBIF_leaflet<-GBIF_leaflet %>%
      addControl("We did not find GBIF occurrence records with coordinates", position = "bottomleft")
  } else {
    GBIF_leaflet<-GBIF_leaflet %>%
      addCircleMarkers(lng=GBIF$decimalLongitude,
                       lat=GBIF$decimalLatitude,
                       color=GBIF$colour,
                       fillOpacity=0.5,
                       stroke=F,
                       group="GBIF records",
                       popup=GBIF$PopText,
                       radius=8) %>%
      addLegend(position="bottomleft", colors=c('#C93149', '#2316E1', 'black', '#D69F32'), labels=c("New", "Old", "Unknown", "Published distribution"))
  }
    
  # Add habitat loss if needed
  Hab_SP<-DD$Hab_file[DD$scientific_name==SP]
  if(Hab_SP != "None"){
    
    ### Load forest
    if(Hab_SP == "Forest"){
      Files_forest<-list.files(paste0("resources/resources_Shiny_DD/1.GFC_final/")) %>% .[grepl(sub(" ", "_", SP), .)]
      
      if(grepl("RANGEsmall", Files_forest[1])){
        ras<-raster(paste0("resources/resources_Shiny_DD/1.GFC_final/", sub(" ", "_", SP), "_RANGEsmall.tif"))
        ColPal<-colorNumeric(c("#d01c8b", "#a1d76a"), c(1, 100), na.color = NA)
        
        GBIF_leaflet <- GBIF_leaflet %>%
          addRasterImage(ras, method="ngb", group="Forest loss", opacity=0.9, colors=ColPal, project=FALSE) %>%
          addLayersControl(overlayGroups=c("GBIF records", "Distribution", "Forest loss"), position="topleft", options=layersControlOptions(collapsed = FALSE))
        
      } else {
        ras_cover<-raster(paste0("resources/resources_Shiny_DD/1.GFC_final/", sub(" ", "_", SP), "_coverRANGElarge.tif"))
        ras_loss<-raster(paste0("resources/resources_Shiny_DD/1.GFC_final/", sub(" ", "_", SP), "_lossRANGElarge.tif"))
        ColPal1<-colorNumeric(c("white", "darkgreen"), domain=c(0,100), na.color=NA)
        ColPal2<-colorNumeric(c("white", "#d01c8b"), domain=c(0, 100), na.color = NA)
        
        GBIF_leaflet <- GBIF_leaflet %>%
          addRasterImage(ras_cover, method="bilinear", group="Forest cover", opacity=0.9, colors=ColPal1, project=FALSE) %>%
          addRasterImage(ras_loss, method="auto", group="Forest loss", opacity=0.9, colors=ColPal2, project=FALSE) %>%
          addLayersControl(overlayGroups=c("GBIF records", "Distribution", "Forest cover", "Forest loss"), position="topleft", options=layersControlOptions(collapsed = FALSE))
        
      }
            
    }
      
    ### Load CCI
    if(Hab_SP == "CCI"){
      
      Files_CCI<-list.files(paste0("resources/resources_Shiny_DD/1.CCI_final/")) %>% .[grepl(sub(" ", "_", SP), .)] %>% paste0("resources/resources_Shiny_DD/1.CCI_final/", .)
      
      ras<-raster(Files_CCI)
      ColPal<-colorNumeric(c("#a6611a", "gray90", "#80cdc1", "#018571"), c(2,3,4,5), na.color = NA)

      GBIF_leaflet <- GBIF_leaflet %>%
        addRasterImage(ras, method="ngb", group="AOH loss", opacity=0.9, colors=ColPal, project=FALSE) %>%
        addLayersControl(overlayGroups=c("GBIF records", "Distribution", "AOH loss"), position="topleft", options=layersControlOptions(collapsed = FALSE)) %>%
        addLegend(position="topright", colors=c('#a6611a', 'gray90', '#80cdc1', '#018571'), labels=c("Newly unsuitable", "Unsuitable", "Suitable", "Newly suitable"))
      
      
      
    }


  }
  
  
  return(GBIF_leaflet)
  
}


DDfun_RefWos <- function(WOS_extract){
  
  WOS_extract$Ref<-paste0(
    WOS_extract$Author %>% gsub('["]', '', .) %>% gsub("[c(]", "", .) %>% gsub("[)]", "", .),
    " (", WOS_extract$Year, "). ",
    "<b>", WOS_extract$Title, "</b>",
    ". ",
    "<i>", WOS_extract$Journal, "</i>",
    ": ",
    ifelse(is.na(WOS_extract$DOI),
           paste0("<a href='https://scholar.google.com/scholar?lr=lang_en&q=", gsub(" ", "+", WOS_extract$Title), "&btnG=' target='_blank'>Link</a>"),
           paste0("<a href='http://doi.org/", WOS_extract$DOI, "' target='_blank'>", WOS_extract$DOI,"</a>")
    ),
    ".<br><br>"
  )
  
  return(WOS_extract)
}

DDfun_GetWOS<-function(SP){
  print("START - WOS extract")
  WOS_SP<-subset(WOS_extract, WOS_extract$Species==SP)$Ref
  print(paste0(ifelse(is.null(nrow(WOS_SP)), 0, nrow(WOS_SP)), " WOS references found"))
  return(WOS_SP)
}


DDfun_AddList <- function(ListButton, NROW, DDTable){
  
  print("START - Add species to list")
  
  # If Init create an empty dataframe
  if(ListButton=="Init"){
    SP_list <- cbind(DDTable, ListOrder=NA)[0,]

    } else {
      
    # Extract species name
    SP_name <- ListButton %>% strsplit(., "_") %>% unlist(.) %>% subset(., . != "buttonList") %>% paste(., collapse=" ")
    # Extract what we want to keep for that species
    SP_list <- subset(DDTable, SP==SP_name)
    SP_list$ListOrder <- (NROW+1)
    }
  
  # Remove some columns and reorder with priority and SP first
  Col_names <- names(SP_list) %>% .[!. %in% c("List", "scientific_name", "More")] %>% .[order(! . %in% c("SP", "ListOrder"))]
  SP_list <- SP_list[, Col_names]
  names(SP_list) <- replace(names(SP_list), names(SP_list)=="SP", "Species")

  return(SP_list)
}


shinyInput <- function(FUN, SPs, id, ...) {
  inputs <- character(length(SPs))
  for (i in 1:length(SPs)) {
    inputs[i] <- as.character(FUN(paste0(id, sub(" ", "_", SPs[i])), ...))
  }
  inputs
}



### Function to record usage for tracker
DDfun_Track <- function(tracker_df, ACT, Nrows){

  # Count
  if(ACT %in% c("Create_priority", "More", "Count_table")){tracker_df$Val[tracker_df$Action==ACT] <- as.numeric(tracker_df$Val[tracker_df$Action==ACT])+1}
  
  # Or report nrow of priority list
  if(ACT=="Download_priority"){tracker_df$Val[tracker_df$Action==ACT] <- Nrows}

  return(tracker_df)
  
  
}

