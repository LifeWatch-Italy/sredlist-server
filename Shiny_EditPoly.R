
# Set working directory (Victor path if we are on his laptop, LifeWatch path otherwise)
setwd(dir=ifelse(file.exists("C:/Users/Victor"),"C:/Users/Victor/Documents/sRedList/Platform/InProgress/sredlist-server-develop", "/media/docker/sRedList/sredlist-server"))


library(shiny)
library(mapedit)
library(rmapshaper)
library(htmlwidgets)
library(leafpm)
library(bslib)
library(bsicons)
library(lwgeom)
library(shinyWidgets)
library(spsComps) # For addLoader in server


source(textConnection(readLines("server.R")[15:123]))


### Load functions
source("sRLfun_ShinyEditPolyg.R")





### Specify url
options(shiny.host = '127.0.0.1')
options(shiny.port = 6211)
# http://127.0.0.1:6211/?sci_name=Lophornis_brachylophus&user=victor.cazalis


### UI ----------
ui <- page_fillable(
  
  # Used to make attributes labels and values on the same line in popups
  tags$head(
    tags$style(type="text/css",
               "#Attributes_box{
               label.control-label, .selectize-control.single {
         display: table-cell;
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
      }}
    ")
  ),

  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(10%);
             left: calc(-150%);
             }")
    )),
  
  ### Make everything 20% smaller (better included in iframe like that). It works but creates bugs with sliders and draw polygons (the mouse is not correctly rescaled)
  # tags$style("
  #               body {
  #     -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
  #     zoom: 0.8; /* Other non-webkit browsers */
  #     zoom: 80%; /* Webkit browsers */
  # }
  #               "),
  
  # add button for reassessment with hydrobasins
  conditionalPanel(condition='output.Suggest_hydro=="yes"',
                   column(6, style="margin-top: 35px;",
                          actionButton('EditAsHydroButt', 'Edit as hydrobasins', style="color: #fff; background-color: #009138ff; border-color: #009138ff", icon=icon("arrow-right", lib="font-awesome")),
                          tooltip(trigger=bs_icon("info-circle"), "Click here to edit the distribution of this species as hydrobasins of level 8.")
                   )
  ),
  
  # Add main panel and sidebar
  sidebarLayout(
    position="left",
    
    mainPanel(
      conditionalPanel(condition='false', 
                       textInput("sci_name", "Scientific name:"),
                       textInput("user", "User name:")
      ),
      accordion_filters <- accordion(open="Map",
        accordion_panel(
          # Cheatsheet (different if hydrobasins or not)
          "How does it work?", icon = bsicons::bs_icon("question-circle"),
          conditionalPanel(condition='output.AllowEdit!="hydro"', includeMarkdown("resources/Cheatsheet_editpoly/home.md")),
          conditionalPanel(condition='output.AllowEdit=="hydro"', includeMarkdown("resources/Cheatsheet_editpoly/home_hydrobasins.md"))
        ),
        accordion_panel(
          "Map", icon = bsicons::bs_icon("map"),
          editModUI("map", height=600, width = "100%"),
        ),
        accordion_panel(
          "Fill text attributes", icon = bsicons::bs_icon("type"),
          fluidRow(textInput("Field_source", label = "Source"),
                   textInput("Field_yrcompiled", label="Year compiled"),
                   textInput("Field_citation", label="Citation"),
                   textInput("Field_compiler", label="Compiler")
          ),
          fluidRow(checkboxInput("Field_datasens", label="Data sensitive", value=0),
                   conditionalPanel(condition='input.Field_datasens==true', textInput("Field_senscomm", label="Data Sensitive comment")),
                   textInput("Field_island", label="Island")
          ),
          textInput("Field_distcomm", label="Distribution comment", width="100%")
        )
      ),
      
      # Version number (just for Victor to ensure the correct version is deployed)
      conditionalPanel(condition='input.user=="victor.cazalis"', "version 1.3_online1")
      
    ),
    
    sidebarPanel(

      ### Panel to show when normal editing (ie not hydrobasins)
      conditionalPanel(condition='output.AllowEdit!="hydro"',
                       
                       ## Simplify
                       div(align="center",
                           titlePanel("Edit distribution"),
                           fluidRow(
                             column(8, sliderInput("Simplify_par", label="Simplify (%)", min=0, max=100, value=0)),
                             column(4, style="margin-top: 35px;",
                                    actionButton('SimplifyButton', 'Simplify', style="color: #fff; background-color: #009138ff; border-color: #009138ff", icon=icon("arrow-right", lib="font-awesome")),
                                    tooltip(trigger=bs_icon("info-circle"), "Simplify polygon geometries (eg. when you want to edit the distribution manually but there are hundreds of nodes to move around). Set percentage, then click 'Simplify' button."),),
                           )),
                       
                       ## Smooth
                       div(class="vertical-center", align="center",
                           fluidRow(
                             column(8, sliderInput("Smooth_par", label="Smooth (%)", min=0, max=100, value=0)),
                             column(4, style="margin-top: 35px;",
                                    actionButton('SmoothButton', 'Smooth', style="color: #fff; background-color: #009138ff; border-color: #009138ff", icon=icon("arrow-right", lib="font-awesome")),
                                    tooltip(trigger=bs_icon("info-circle"), "Smooth polygon borders (eg. when your distribution has sharp angles and you want it to be smoother). Set percentage, then click 'Smooth' button."),
                             ),
                           )),
                       
                       ## Split
                       HTML("<br><br>"),
                       div(align="center",
                           titlePanel("Split polygons"),
                           actionButton('SplitButton', 'Split polygons by lines', style="color: #fff; background-color: #009138ff; border-color: #009138ff", icon=icon("scissors", lib="font-awesome")),
                           tooltip(trigger=bs_icon("info-circle"), "Cut polygons after drawing lines (eg. when you want to split a polygon to assign different attributes, or when you want to remove part of a polygon). Draw lines crossing your polygons, then click 'Split polygons by lines' button.")
                       ),
                       
                       ## Crop
                       conditionalPanel(condition='output.CropLand!="" | output.CropCountry!=""',
                                        HTML("<br><br>"),
                                        div(align="center",
                                            titlePanel("Crop distribution"),
                                            conditionalPanel(condition='output.CropLand!=""', 
                                                             actionButton("CroplandButton", textOutput("CropLand"), style="color: #fff; background-color: #009138ff; border-color: #009138ff"),
                                                             tooltip(trigger=bs_icon("info-circle"), textOutput("TooltipCropland"))
                                                             ),
                                            conditionalPanel(condition='output.CropCountry!=""', 
                                                             actionButton("CropcountryButton", textOutput("CropCountry"), style="color: #fff; background-color: #009138ff; border-color: #009138ff"),
                                                             tooltip(trigger=bs_icon("info-circle"), "The distribution was previously cropped by country for National Red Listing, you can crop again with this button after editing the distribution if needed.")
                                                             ),
                                        )
                       )
      ),
      
      ### Panel to show when editing hydrobasins
      conditionalPanel(condition='output.AllowEdit=="hydro"',
                       
                       titlePanel("Edit hydrobasins"),
                       HTML("<br>"),
                       fluidRow(
                         column(4,
                                fluidRow(
                                  div(align="center", 
                                      HTML("Presence &nbsp"),
                                      tooltip(trigger=bs_icon("info-circle"), HTML("1=Extant <br>2=Probably Extant (deprecated) <br>3=Possibly Extant <br>4=Possibly Extinct <br>5=Extinct Post-1500 <br>6=Presence Uncertain <br>7=Expected Additional Range")))),
                                numericInput(inputId="Pres_batch", label="", value=1, min=1, max=7, step=1, width="95%")),
                         column(4,
                                fluidRow(
                                  div(align="center", 
                                      HTML("Origin &nbsp"), 
                                      tooltip(trigger=bs_icon("info-circle"), HTML("1=Native <br>2=Reintroduced <br>3=Introduced <br>4=Vagrant <br>5=Origin Uncertain <br>6=Assisted Colonisation")))),
                                numericInput(inputId="Orig_batch", label="", value=1, min=1, max=6, step=1, width="95%")),
                         column(4,
                                fluidRow(
                                  div(align="center", 
                                      HTML("Seasonal &nbsp"),
                                      tooltip(trigger=bs_icon("info-circle"), HTML("1=Resident <br>2=Breeding Season <br>3=Non-Breeding Season <br>4=Passage <br>5=Seasonal Occurrence Uncertain")))),
                                numericInput(inputId="Seas_batch", label="", value=1, min=1, max=5, step=1, width="95%"))
                       ),
                       
                       HTML("<br><br>"),
                       fluidRow(div(align="center",
                                    actionButton('Rm_batch', 'Remove hydrobasins', style="color: #fff; background-color: #dea218ff; border-color: #dea218ff", icon=icon("trash", lib="font-awesome"), width="30%"),
                                    actionButton('Add_batch', 'Add / edit hydrobasins', style="color: #fff; background-color: #009138ff; border-color: #009138ff", icon=icon("plus", lib="font-awesome"), width="30%"),
                                    tooltip(trigger=bs_icon("info-circle"), "Set up your attributes, place markers within the hydrobasins you want to edit on the map and click on 'Remove' or 'Add/edit'")
                       ))
      ),
      
      ## Save
      HTML("<br><br>"),
      div(
        titlePanel("Save changes"),
        conditionalPanel(condition='output.Unsaved_changes=="yes"', HTML('<i><font color="#C8BAB3">You have unsaved changes</i></font><br>')),
        actionButton('discard', 'Discard changes', style="color: #fff; background-color: #dea218ff; border-color: #dea218ff", icon=icon("trash", lib="font-awesome")),
        actionButton('save', 'Save changes', style="color: #fff; background-color: #009138ff; border-color: #009138ff", icon=icon("floppy-disk", lib="font-awesome")),
        align="center")          
    )
  )
)






### Server ----------
server <- function(input, output, session) {
  
  ### Create objects ---------

  ### Create base map (create with hydro to prevent "editable" to be associated to the map by default)
  edits <- sRLPolyg_CreateLeaflet("hydro")
  
  ### CREATE REACTIVE VALUES
  distSP <- reactiveVal() # distribution
  Storage_SP <- reactiveVal() # Species storage file from sRedList
  dat_pts <- reactiveVal() # Occurrence records gathered in 1b OR in 1a by comparing distribution with GBIF
  drawn_storage <- reactiveVal() # Polygons manually drawn or edited
  lines_storage <- reactiveVal() # Lines manually drawn or edited
  track_storage <- reactiveValues(L=list(Hydro_init=0, Hydro_editas=0, Simplify_init=0, Simplify=0, Smooth=0, Split=0, CropLand=0, CropCountry=0, ManuDrawEdit=0, Attributes=0, RmPoly=0, AddHydro=0, RmHydro=0, BatchAddHydro=0, BatchRmHydro=0)) # List of counters used for usage tracking
  markers_storage <- reactiveVal() # Markers manually drawn or edited
  AllowEdit <- reactiveVal() # Factor used to inform if we propose the default or the hydrobasin version + if we allow editing manually (not allowed if distribution too complex and users clicked 'No' in the popup)
  Run_discard <- reactiveVal(0) # Reactive value that calls Discard button (either from the discard button itself or from other functions, eg if users click on drag with hydrobasins)
  Suggest_hydro <- reactiveVal("no") # Reactive value that becomes T if we should add the button "Edit as hydrobasins", ie if this is a reassessment of a freshwater species
  Unsaved_changes <- reactiveVal("no") # Reactive value that becomes T when there are unsaved changes
  Warning_drawned <- reactiveVal("no") # Reactive value that becomes T when the warning that we need to save after drawing has been shown (to avoid it to be repeated)
  
  ### Events ---------

  ### Read the URL parameter from session$clientData$url_search
  observe({
    query <- parseQueryString(session$clientData$url_search) ; req(nchar(query)>0)
    updateTextInput(session, "sci_name", value = query[['sci_name']] %>% gsub("_" , " ", .) %>% gsub("%20%" , " ", .) %>% sRL_firstup(.))
    updateTextInput(session, "user", value = sRL_userdecode(query[['user']]))
  })
  
  
  #### Load species ------
  observeEvent(input$sci_name, {
    
    req(input$sci_name) ; req(input$sci_name != "")
    
    sRL_loginfo("START - Load distribution", input$sci_name)
    
    ### Loader
    loader_load <- addLoader$new("sci_name", color = "#009138ff", method = "full_screen", height = "30rem", opacity=0.4) ; loader_load$show()
    
    ### Load StorageSP and distSP
    Stor_tempo <- sRL_StoreRead(input$sci_name,  input$user, MANDAT=1)
    if(length(names(Stor_tempo))<=1){loader_load$hide() ; req(F)}
    
    # Load distribution
    if(T %in% grepl("hydro", Stor_tempo$Output$Value) & Stor_tempo$Output$Value[Stor_tempo$Output$Parameter=="Distribution_Source"]=="Created"){
      dist_loaded0 <- Stor_tempo$distSP3_BeforeCrop
      # Extract hydrobasins, will return a list with hydroSP to use and hydro_HQ with hydrobasins in the original quality
      track_storage$L$Hydro_init <- track_storage$L$Hydro_init+1
      hydro_ready <- sRLPolyg_PrepareHydro(dist_loaded0, hydro_raw, Stor_tempo$Output$Value[Stor_tempo$Output$Parameter=="Mapping_Start"], SRC_created="yes")
      dist_loaded <- hydro_ready[["hydroSP"]] ; Stor_tempo$hydroSP_HQ <- hydro_ready[["hydroSP_HQ"]]
      if(nrow(dist_loaded)==0){showNotification("The distribution does not overlap with hydrobasins, we cannot load the distribution", type="error", duration=3); req(F)}
    } else {
      dist_loaded <- Stor_tempo$distSP_saved
      dist_loaded <- sRLPolyg_InitDistri(dist_loaded, 4326)
    }
    
    # Extract crop options
    Stor_tempo$CropCountry_option <- Stor_tempo$Output$Value[Stor_tempo$Output$Parameter=="Crop_Country"] ; if(is.na(Stor_tempo$CropCountry_option)==F & Stor_tempo$CropCountry_option != ""){Stor_tempo$CropCountry_option <- paste0("Crop country: ", Stor_tempo$CropCountry_option)}
    if(Stor_tempo$Output$Value[Stor_tempo$Output$Parameter=="Distribution_Source"]=="Created"){
      Stor_tempo$CropLand_option <- substr(Stor_tempo$Output$Value[Stor_tempo$Output$Parameter=="Mapping_Crop"], 5, 10) ; if(is.na(Stor_tempo$CropLand_option)==F & Stor_tempo$CropLand_option != ""){Stor_tempo$CropLand_option <- paste0("Crop by ", Stor_tempo$CropLand_option)} 
      Stor_tempo$CropLand_tooltip <- ifelse(Stor_tempo$CropLand_option=="Crop by land", "The distribution was previously cropped by land, you can crop again with this button after editing the distribution if needed.", "The distribution was previously cropped by sea, you can crop again with this button after editing the distribution if needed.")
    } else {
      Domain_Centroid <- st_point_on_surface(dist_loaded[1,]) %>% st_transform(., st_crs(distCountries_mapping)) %>% st_intersects(., distCountries_mapping) # Take a central point within the distribution and check if it's at sea or at land
      Stor_tempo$CropLand_option <- ifelse(length(Domain_Centroid[[1]])>0, "Crop by land", "Crop by sea")
      Stor_tempo$CropLand_tooltip <- ifelse(length(Domain_Centroid[[1]])>0, "You can crop the distribution by land to keep only the terrestrial part", "You can crop the distribution by sea to keep only the marine part")
    }
    
    # Save Storage_SP
    Storage_SP(Stor_tempo)
    
    ### Prepare textInput initial values
    tryCatch({
      if(! "compiler" %in% names(dist_loaded)){dist_loaded$compiler <- NA}
      if(! "citation" %in% names(dist_loaded)){dist_loaded$citation <- NA}
      if(! "source" %in% names(dist_loaded)){dist_loaded$source <- NA}
      updateTextInput(session, "Field_source", value = ifelse((is.na(dist_loaded$source[1])==F), dist_loaded$source[1], "sRedList platform"))
      updateNumericInput(session, "Field_yrcompiled", value = format(Sys.time(), "%Y"))
      updateTextInput(session, "Field_citation", value = ifelse((is.na(dist_loaded$citation[1])==F), dist_loaded$citation[1], "IUCN (International Union for Conservation of Nature)"))
      updateTextInput(session, "Field_compiler", value = ifelse((is.na(dist_loaded$compiler[1])==F), dist_loaded$compiler[1], sRL_userformatted(input$user)))
      updateTextInput(session, "Field_island", value = ifelse(("island" %in% names(dist_loaded) & is.na(dist_loaded$island[1])==F), dist_loaded$island[1], "")) # Ideally I could automatically extract if on an island?
      updateCheckboxInput(session, "Field_datasens", value = ifelse(("TRUE" %in% dist_loaded$data_sens | "true" %in% dist_loaded$data_sens | "1" %in% dist_loaded$data_sens), TRUE, FALSE))
      updateTextInput(session, "Field_senscomm", value = ifelse(("sens_comm" %in% names(dist_loaded) & is.na(dist_loaded$sens_comm[1])==F), dist_loaded$sens_comm[1], ""))
      # Prepare distcomm
      New_DistComm <- ifelse(Stor_tempo$Output$Value[Stor_tempo$Output$Parameter=="Distribution_Source"] == "Created", 
                             dist_loaded$dist_comm[1], 
                             ifelse(is.na(dist_loaded$dist_comm[1]), 
                                    paste0("The distribution was taken from the sRedList platform (", Stor_tempo$Output$Value[Stor_tempo$Output$Parameter=="Distribution_Source"], ") and was "), 
                                    paste0(dist_loaded$dist_comm[1], " It was ")) %>% paste0(., "manually edited on the sRedList platform on the ", Sys.Date(), ".")
      )
      updateTextInput(session, "Field_distcomm", value = substr(New_DistComm, 1, 254))
    }, error=function(e){"Error in field extraction"})
    
    ### Process distribution
    distSP(dist_loaded)
    
    ### Extract AllowEdit status (important that the leaflet is not created with "editable" as a group for hydrobasins, otherwise hydrobasins are draggable)
    AllowEdit(ifelse("hybas_id" %in% names(dist_loaded), "hydro", "yes"))
    
    # Edit Suggest_hydro to determine if we display the 'Edit as hydrobasins' button
    speciesRL_sub <- subset(speciesRL, scientific_name == input$sci_name)
    if(AllowEdit()!="hydro" & nrow(speciesRL_sub)>0 & TRUE %in% grepl("Freshwater", Stor_tempo$SpeciesAssessment$systems$description) & Stor_tempo$Output$Value[Stor_tempo$Output$Parameter=="Distribution_Source"] != "Created"){Suggest_hydro("yes")}
    
    ### Reload edits and storages (needed to make sure we start from zero; bugs otherwise)
    edits <- sRLPolyg_CreateLeaflet(AllowEdit())
    lines <- lines_storage() ; if(is.null(nrow(lines))==F){lines$Applied <- T ; lines_storage(lines)}
    drawn <- drawn_storage() ; if(is.null(nrow(drawn))==F){drawn$Applied <- T ; drawn_storage(drawn)}
    markers <- markers_storage() ; if(is.null(nrow(markers))==F){markers$Applied <- T ; markers_storage(markers)}
    
    ### Add points if distribution created
    if("dat_proj_saved" %in% names(Storage_SP())){
      pts_raw <- Storage_SP()$dat_proj_saved %>% st_transform(., 4326)
      dat_coords <- st_coordinates(pts_raw)
      pts_raw$lon <- dat_coords[,1] ; pts_raw$lat <- dat_coords[,2]
      pts_raw$Pts_type <- "1b"
      dat_pts(pts_raw)
    }
    
    ### Add points if distribution compared with GBIF
    if("data_comparison" %in% names(Storage_SP())){
      pts_comparison <- st_as_sf(Storage_SP()$data_comparison, coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84")
      dat_coords <- st_coordinates(pts_comparison)
      pts_comparison$lon <- dat_coords[,1] ; pts_comparison$lat <- dat_coords[,2]
      pts_comparison$Pts_type <- "comparison"
      dat_pts(pts_comparison)
    }

    # Message if distribution loaded is too big
    if(npts(dist_loaded)>5000 & AllowEdit()!="hydro"){
      print(npts(dist_loaded))
      ask_confirmation(inputId = "SimplifyLoading", 
                       type = "warning", 
                       title = "Distribution too heavy", 
                       text="The distribution is too complex to be efficiently edited here. You can choose to simplify it, which will allow you to edit it manually. If you choose not to simplify, you won't be able to edit manually (only adding polygons and changing attributes will be possible)",
                       btn_labels = c("Do not simplify", "Simplify"),
                       btn_colors = c("#dea218ff", "#009138ff"))
    } else {
      
      ### Update leaflet map
      sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=1)
      Unsaved_changes("no")
    }
    
    ### End loader
    loader_load$hide()
  })
  
  
  #### Load as hydrobasins -----
  observeEvent(input$EditAsHydroButt, {
    sRL_loginfo("START - Load as hydrobasins", input$sci_name)
    
    ### Loader
    loader_loadhydro <- addLoader$new("EditAsHydroButt", color = "#009138ff", method = "full_screen", height = "30rem", opacity=0.4) ; loader_loadhydro$show()
    track_storage$L$Hydro_editas <- track_storage$L$Hydro_editas+1
    AllowEdit("hydro")
    Suggest_hydro("no") # No need to see the button anymore
    
    ### Subset hydrobasins
    hydro_ready <- sRLPolyg_PrepareHydro(st_transform(distSP(), CRSMOLL), hydro_raw, "hydro8", SRC_created="no")
    dist_hydro <- hydro_ready[["hydroSP"]] ; Stor_tempo <- Storage_SP() ; Stor_tempo$hydroSP_HQ <- hydro_ready[["hydroSP_HQ"]] ; Storage_SP(Stor_tempo)
    if(nrow(dist_hydro)==0){showNotification("The distribution does not overlap with hydrobasins, we cannot load the distribution", type="error", duration=3); loader_loadhydro$hide(); req(F)}
    distSP(dist_hydro)

    ### Update leaflet map (necessary to call CreateLeaflet in case no simplification was selected to have the correct text for onRender for toolbar tooltips)
    req(distSP())
    edits <- sRLPolyg_CreateLeaflet("hydro")
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=1, AllowEdit=AllowEdit())
    Unsaved_changes("no")
    
    ### End loader
    loader_loadhydro$hide()
    
  })
  
  
  #### Simplify while loading -----
  observeEvent(input$SimplifyLoading, {
    sRL_loginfo("START - Load simplified distribution", input$sci_name)
    
    ### Loader
    loader_simplifload <- addLoader$new("SimplifyLoading", color = "#009138ff", method = "full_screen", height = "30rem", opacity=0.4) ; loader_simplifload$show()
    track_storage$L$Simplify_init <- track_storage$L$Simplify_init+1
    
    ### Simplify
    if(input$SimplifyLoading==T){
      
      Simplif_par <- 3000 / npts(distSP()) # Keep = number of points wanted / total points

      dist_simpl <- ms_simplify(distSP(), keep=Simplif_par, keep_shapes=T) %>% st_make_valid(.) %>% .[which(grepl("POLYGON", st_geometry_type(.))),]
      showNotification(ui=HTML(paste0("The distribution was successfully reduced by ", round(1-npts(dist_simpl)/npts(distSP()),3)*100, "% (", npts(dist_simpl), " points remaining)")), type="message", duration=2)
      if(nrow(dist_simpl) != nrow(distSP())){dist_simpl$Popup <- sRLPolyg_CreatePopup(dist_simpl)} # Recreate popups if some polygons were created by simplifying
      distSP(dist_simpl)
      
      # Save (in case discard is used)
      Storage_SPNEW <- Storage_SP()
      Storage_SPNEW$distSP_saved <- dist_simpl
      sRL_StoreSave(input$sci_name, input$user,  Storage_SPNEW)
    }
    
    # Update AllowEdit (to ensure that editing remains impossible even if users click Save or Discard)
    if(AllowEdit() != "hydro"){AllowEdit(ifelse(input$SimplifyLoading==T, "yes", "no"))}
    
    ### Update leaflet map (necessary to call CreateLeaflet in case no simplification was selected to have the correct text for onRender for toolbar tooltips)
    req(distSP())
    if(AllowEdit()=="no"){edits <- sRLPolyg_CreateLeaflet("no")}
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=1, AllowEdit=AllowEdit())
    
    ### End loader
    loader_simplifload$hide()
  })
  
  
  #### Simplify --------------
  observeEvent(input$SimplifyButton, {
    sRL_loginfo("START - Simplify button", input$sci_name)
    SimplifyButtVal <- input$SimplifyButton %>% as.character(.) %>% strsplit(., "_#TIME") %>% unlist(.) %>% .[1]
    
    ### Loader
    loader_simplif <- addLoader$new("SimplifyButton", color = "white", method = "inline") ; loader_simplif$show()
    track_storage$L$Simplify <- track_storage$L$Simplify+1
    
    ### Check parameter > 0
    if(input$Simplify_par==0){
      showNotification(ui=HTML("The simplify parameter is null, please set a positive value with the slider on the right"), type="error", duration=6)
      dist<-distSP() ; dist$Popup <- sRLPolyg_CreatePopup(dist) ; distSP(dist) # Rerun the popup to ensure the button can be re-clicked if users changed smooth_par
      sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=0)
      loader_simplif$hide()
      req(F)
    }
    
    ### Exacerbate a bit the simplify parameter with a logit function (to help big or minimal simplifications)
    SimpPar <- 1 - (1 / (1+exp(0.1*(50-input$Simplify_par))))
    
    ### Simplify
    dist_to_simplify <- distSP()
    
    if(grepl("SimplifyPolygon_", as.character(SimplifyButtVal))){
      ### Smooth a single polygon if actionned from popup button
      ID_to_simplify <- sub("SimplifyPolygon_", "", as.character(SimplifyButtVal))
      Polyg_simplified <- ms_simplify(dist_to_simplify[dist_to_simplify$ID==ID_to_simplify,], keep=SimpPar, keep_shapes=T)
      distSimpli <- sRL_rbindfillSF(dist_to_simplify[dist_to_simplify$ID != ID_to_simplify,], Polyg_simplified)
    } else {
      ### Smooth all polygons if actionned from the sidebar button
      distSimpli <- ms_simplify(dist_to_simplify, keep=SimpPar, keep_shapes=T)
    }
    
    distSimpli <- distSimpli %>% st_make_valid(.)
    if(nrow(distSimpli) != nrow(dist_to_simplify)){distSimpli$Popup <- sRLPolyg_CreatePopup(distSimpli)} # Recreate popups if some polygons were created by smoothing
    print(paste0("Distribution was reduced by ", round(1-npts(distSimpli)/npts(distSP()),3)*100, "% (", npts(distSimpli), " points remaining)"))
    
    # Save only if a polygon remains
    if(as.numeric(st_area(st_combine(distSimpli)))==0){
      showNotification(ui=HTML("Please reduce the simplify parameter, it is currently too high"), type="error", duration=3)
    } else{
      showNotification(ui=HTML(paste0("The distribution was successfully simplified (", npts(distSimpli), " points are remaining)")), type="message", duration=2)
      distSP(distSimpli)
      sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=0)
      Unsaved_changes("yes")
    }
    
    ### End loader
    loader_simplif$hide()

  })
  
  
  #### Smooth --------
  observeEvent(input$SmoothButton, {
    
    sRL_loginfo("START - Smooth button", input$sci_name)
    SmoothButtVal <- input$SmoothButton %>% as.character(.) %>% strsplit(., "_#TIME") %>% unlist(.) %>% .[1]
    
    ### Loader
    loader_smooth <- addLoader$new("SmoothButton", color = "white", method = "inline") ; loader_smooth$show()
    track_storage$L$Smooth <- track_storage$L$Smooth+1
    
    ### Check parameter > 0
    if(input$Smooth_par==0){
     showNotification(ui=HTML("The smooth parameter is null, please set a positive value with the slider on the right"), type="error", duration=2)
     dist<-distSP() ; dist$Popup <- sRLPolyg_CreatePopup(dist) ; distSP(dist) # Rerun the popup to ensure the button can be re-clicked if users changed smooth_par
     sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=0)
     loader_smooth$hide()
     req(F)
    }
    
    ### Run smooth
    tryCatch({
      dist_to_smooth <- st_transform(distSP(), CRSMOLL)

      if(grepl("SmoothPolygon_", as.character(SmoothButtVal))){
        ### Smooth a single polygon if actionned from popup button
        ID_to_smooth <- sub("SmoothPolygon_", "", as.character(SmoothButtVal))
        Polyg_smoothed <- sRLPolyg_Smooth(dist_to_smooth[dist_to_smooth$ID==ID_to_smooth,], input$Smooth_par)
        distSmoothed <- sRL_rbindfillSF(dist_to_smooth[dist_to_smooth$ID != ID_to_smooth,], Polyg_smoothed)
      } else {
        ### Smooth all polygons if actionned from the sidebar button
        distSmoothed <- sRLPolyg_Smooth(dist_to_smooth, input$Smooth_par)
      }

      # Remove too small polygons
      distSmoothed$Area <- as.numeric(st_area(distSmoothed))
      distSmoothed <- subset(distSmoothed, distSmoothed$Area > sum(distSmoothed$Area)*10^-6) # Keep only polygons that are bigger than total_area*10-6
      distSmoothed <- st_buffer(distSmoothed, 10) # Buffer by 10m to avoid small holes in the distribution

      # Make valid and unique polygons
      distSmoothed <- distSmoothed %>% sRLPolyg_InitDistri(., 4326) %>% st_make_valid()

      # Save only if a polygon remains
      if(as.numeric(st_area(distSmoothed))[1]==0){
        showNotification(ui=HTML("Please reduce the smooth parameter, it is currently too high"), type="error", duration=3)
      } else{
        showNotification(ui=HTML("The distribution was successfully smoothed"), type="message", duration=2)
        distSP(st_transform(distSmoothed, 4326))
        sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=0)
        Unsaved_changes("yes")
      }

    }, error=function(e){showNotification(ui=HTML("Smoothing was not possible with this parameter. Try another value or skip smoothing."), type="error", duration=6)})

    ### End loader
    loader_smooth$hide()
  })



  #### Split distSP with line ---------------
  observeEvent(input$SplitButton, {
    sRL_loginfo("START - Split polygons with line", input$sci_name)
    
    # Prepare lines (taken from drawn, or from edited if they were manually edited)
    req(is.null(lines_storage())==F)
    drawn_line <- lines_storage() %>% subset(., .$Applied==F)
    req(nrow(drawn_line)>0)
    drawn_line$Line <-paste0("L", 1:nrow(drawn_line))
    
    ### Loader
    loader_split <- addLoader$new("SplitButton", color = "white", method = "inline") ; loader_split$show()
    track_storage$L$Split <- track_storage$L$Split+1
    
    # Cut polygon with lines (only polygons intersecting the lines to compute faster)
    dist_inter <- st_filter(distSP(), drawn_line, .predicate = st_intersects)
    dist_notinter <- subset(distSP(), ! ID %in% dist_inter$ID) # Put aside polygons not affected by the cut (no need to edit Popup for those)
    dist_split <- lwgeom::st_split(dist_inter, st_union(st_cast(drawn_line, 'MULTILINESTRING', warn=F))) %>% st_collection_extract('POLYGON')
    dist_split$ID <- make.unique(dist_split$ID, sep=".")
    
    # Recreate Popup
    dist_split$Popup <- sRLPolyg_CreatePopup(dist_split)
    
    # Update
    if(nrow(dist_split)==0){loader_split$hide()}
    req(nrow(dist_split)>0)
    dist_new <- sRL_rbindfillSF(dist_notinter, dist_split)
    distSP(dist_new)
    edits <- sRLPolyg_CreateLeaflet(AllowEdit())
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=1)
    Unsaved_changes("yes")
    
    ### End loader
    loader_split$hide()
  })
  
  
  #### Crop by land --------
  observeEvent(input$CroplandButton, {
    sRL_loginfo("START - Crop land button", input$sci_name)
    
    req(distSP())
    dist <- distSP() %>% st_transform(., CRSMOLL)
    
    # Loader
    loader_cropland <- addLoader$new("CroplandButton", color = "white", method = "inline") ; loader_cropland$show()
    track_storage$L$CropLand <- track_storage$L$CropLand+1
    
    # Crop by land/sea
    Crop_par <- ifelse(Storage_SP()$CropLand_option =="Crop by sea", "cropsea", "cropland")
    distCrop <- sRL_CropDistributionGBIF(dist, Crop_par) %>% sRLPolyg_InitDistri(., 4326) %>% st_make_valid()
    
    # Update
    distSP(distCrop)
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=0)
    Unsaved_changes("yes")
    
    # End loader
    loader_cropland$hide()
    
  })
  
  #### Crop by country ------------
  observeEvent(input$CropcountryButton, {
    sRL_loginfo("START - Crop country button", input$sci_name)
    
    req(distSP())
    dist <- distSP() %>% st_transform(., CRSMOLL)
    
    # Loader
    loader_cropcountry <- addLoader$new("CropcountryButton", color = "white", method = "inline") ; loader_cropcountry$show()
    track_storage$L$CropCountry <- track_storage$L$CropCountry+1
    
    # Crop country if National Red Listing
    Crop_Country<-Storage_SP()$Output$Value[Storage_SP()$Output$Parameter=="Crop_Country"]
    distCrop <- sRL_CropCountry(dist, Crop_Country, input$sci_name) %>% sRLPolyg_InitDistri(., 4326) %>% st_make_valid()
    
    # Update
    distSP(distCrop)
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=0)
    Unsaved_changes("yes")
    
    # End loader
    loader_cropcountry$hide()
    
  })
  
  
  #### Record edits -----------
  observeEvent(edits(), {

    req(is.null(nrow(edits()$drawn))==F | is.null(nrow(edits()$edited))==F)
    sRL_loginfo("START - Record drawing and editing", input$sci_name)
    
    ### Extract edits
    P_drawn <- edits()$drawn ; if(is.null(nrow(P_drawn))==F){P_drawn <- cbind(P_drawn, data.frame(Row=paste0("Drawn", 1:nrow(P_drawn))))}
    P_edit <- edits()$edited ; if(is.null(nrow(P_edit))==F){P_edit <- cbind(P_edit, data.frame(Row=paste0("Edited", 1:nrow(P_edit))))}
    P_tot <- sRL_rbindfillSF(P_drawn, P_edit) %>% group_by(layerId) %>% summarise_all(last)
    
    ### Lines
    lines <- P_tot %>% subset(., grepl("LINE", st_geometry_type(.)) & (! .$Row %in% lines_storage()$Row))
    if(nrow(lines)>0){
      print("Record line drawing")
      lines$Applied <- F # Create column recording if changes were applied or not

      # Store in lines_storage
      if(is.null(lines_storage())){
        lines_storage(lines)
      } else {
        lines_ST <- lines_storage() %>% subset(., ! .$layerId %in% lines$layerId) # Remove lines stored that were modified
        lines_storage(sRL_rbindfillSF(lines_ST, lines))
      }
    }
    
    ### Polygons
    drawn <- P_tot %>% subset(., grepl("POLYGON", st_geometry_type(.)) & (! .$Row %in% drawn_storage()$Row))
    if(nrow(drawn)>0){
      print("Record and apply polygon drawing")
      
      if(is.null(P_drawn)==F){if(Warning_drawned()=="no"){showNotification(ui=HTML("When you finished drawing new polygons, please click on save to be able to specify their attributes."), type="message", duration=2) ; Warning_drawned("yes")}}
      
      drawn$Applied <- F # Create column recording if changes were applied or not
      
      # Store in drawn_storage
      if(is.null(drawn_storage())){
        drawn_storage(drawn)
      } else {
        edited_ST <- drawn_storage() %>% subset(., ! .$layerId %in% drawn$layerId) # Remove polygon modified from drawn_storage
        drawn_storage(sRL_rbindfillSF(edited_ST, drawn))
      }
      
      # Send error if hydrobasins are being modified (it means they were dragged) as currently I'm not able to remove that option
      if(AllowEdit()=="hydro"){
        showNotification(ui=HTML("You should not move polygons when using hydrobasins. Your unsaved changes have been discarded."), type="error", duration=3)
        Run_discard(Run_discard()+1)
        req(F)
      }
    }
    
    
    ### Markers
    markers <- P_tot %>% subset(., grepl("POINT", st_geometry_type(.)) & (! .$Row %in% markers_storage()$Row))
    if(nrow(markers)>0){
      print("Record marker drawing")
      markers$Applied <- F # Create column recording if changes were applied or not
      
      # Store in markers_storage
      if(is.null(markers_storage())){
        markers_storage(markers)
      } else {
        markers_ST <- markers_storage() %>% subset(., ! .$layerId %in% markers$layerId) # Remove markers stored that were modified
        markers_storage(sRL_rbindfillSF(markers_ST, markers))
      }
    }
  })
  
  
  #### Apply polygon drawing and editing ------
  observeEvent(drawn_storage(), {
    
    req(F %in% drawn_storage()$Applied)
    sRL_loginfo("START - Apply polygon drawing and editing", input$sci_name)
    track_storage$L$ManuDrawEdit <- track_storage$L$ManuDrawEdit+1
    
    # Combine dist and drawn polygons
    dist <- distSP()
    drawn_ST <- drawn_storage()
    
    drawn <- drawn_ST[drawn_ST$Applied==F,]
    drawn$ID <- drawn$layerId
    drawn$presence <- dist$presence[match(drawn$ID, dist$ID)] %>% replace(., is.na(.), 1)
    drawn$origin <- dist$origin[match(drawn$ID, dist$ID)] %>% replace(., is.na(.), 1)
    drawn$seasonal <- dist$seasonal[match(drawn$ID, dist$ID)] %>% replace(., is.na(.), 1)
    
    drawn <- subset(drawn, select=names(drawn)[names(drawn) %in% names(dist)])
    drawn$Popup <- sRLPolyg_CreatePopup(drawn)
    dist <- dist %>% subset(., ! dist$ID %in% drawn$ID) %>% st_difference(., drawn[,"geometry"])
    dist <- sRL_rbindfillSF(dist, drawn)
    
    # Update drawn_storage Applied column
    drawn_ST$Applied[drawn_ST$layerId %in% drawn$ID] <- T
    drawn_storage(drawn_ST)
    
    # Update distSP
    distSP(dist)
    Unsaved_changes("yes")
  })
  
  
  #### Remove polygon (or add hydro)-----------
  observeEvent(input$button_RmPolygon, {
    sRL_loginfo("START - Remove polygon", input$sci_name)
    distSP_edit <- distSP()
    
    ### Remove polygons (not hydrobasins)
    if(substr(input$button_RmPolygon, 1, 9)=="RmPolygon"){
      track_storage$L$RmPoly <- track_storage$L$RmPoly+1
      PolygID <- input$button_RmPolygon %>% sub("RmPolygon_", "", .)
      distSP_edit <- distSP_edit[! distSP_edit$ID %in% PolygID,]
    }
    
    ### Remove hydrobasin
    if(substr(input$button_RmPolygon, 1, 7)=="RmHydro"){
      track_storage$L$RmHydro <- track_storage$L$RmHydro+1
      PolygID <- input$button_RmPolygon %>% sub("RmHydro_", "", .)
      distSP_edit[distSP_edit$ID %in% PolygID, c("presence", "origin", "seasonal")] <- NA
      distSP_edit$Popup[distSP_edit$ID %in% PolygID] <- sRLPolyg_CreatePopup(distSP_edit[distSP_edit$ID %in% PolygID,])
    }
    
    ### Add hydrobasin
    if(substr(input$button_RmPolygon, 1, 8)=="AddHydro"){
      track_storage$L$AddHydro <- track_storage$L$AddHydro+1
      if(input$Pres_batch==2){showNotification(ui=HTML("Presence 'Probably Extant' is deprecated, code 1 (extant) was used instead"), type="warning", duration=2)}
      PolygID <- input$button_RmPolygon %>% sub("AddHydro_", "", .)
      distSP_edit$presence[distSP_edit$ID %in% PolygID] <- ifelse(input$Pres_batch==2, 1, input$Pres_batch)
      distSP_edit$origin[distSP_edit$ID %in% PolygID] <- input$Orig_batch
      distSP_edit$seasonal[distSP_edit$ID %in% PolygID] <- input$Seas_batch
      distSP_edit$Popup[distSP_edit$ID %in% PolygID] <- sRLPolyg_CreatePopup(distSP_edit[distSP_edit$ID %in% PolygID,])
    }

    ### Save changes and edit leaflet (I also include PolygID to make sure removed polygons disappear from the map)
    distSP(distSP_edit)
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=0, PolygID)
    Unsaved_changes("yes")
  })
  
  
  #### Edit attributes ------------
  observeEvent(input$button_EditAtt, {
    sRL_loginfo("START - Edit attributes", input$sci_name)
    track_storage$L$Attributes <- track_storage$L$Attributes+1
    
    # Collect command line and stop if users selected presence "Probably Extant"
    CMD <- input$button_EditAtt %>% strsplit(., "_") %>% unlist(.)
    if(CMD[2]=="Attribute=presence" & CMD[4]=="NewVal=2"){showNotification(ui=HTML(Pres2_error), type="error", duration=2) ; req(FALSE)}
    
    # Edit distSP
    dist <- distSP()
    if(sub("Attribute=", "", CMD[2])=="presence"){dist$presence[dist$ID==sub("SiteID=", "", CMD[3])] <- as.numeric(sub("NewVal=","",CMD[4]))}
    if(sub("Attribute=", "", CMD[2])=="origin"){dist$origin[dist$ID==sub("SiteID=", "", CMD[3])] <- as.numeric(sub("NewVal=","",CMD[4]))}
    if(sub("Attribute=", "", CMD[2])=="seasonal"){dist$seasonal[dist$ID==sub("SiteID=", "", CMD[3])] <- as.numeric(sub("NewVal=","",CMD[4]))}
    
    # Add popup
    dist$Popup[dist$ID==sub("SiteID=", "", CMD[3])] <- sRLPolyg_CreatePopup(dist[dist$ID==sub("SiteID=", "", CMD[3]),])
    
    # Update leaflet
    distSP(dist)
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=0)
    Unsaved_changes("yes")
  })
  
  
  #### Batch add hydrobasins with markers ------------
  observeEvent(input$Add_batch, {
    sRL_loginfo("START - Batch add hydrobasins", input$sci_name)
    track_storage$L$BatchAddHydro <- track_storage$L$BatchAddHydro+1
    
    # Return error if presence 2 selected
    if(input$Pres_batch==2){showNotification(ui=HTML(Pres2_error), type="error", duration=2) ; req(FALSE)}
    
    # Prepare markers
    req(is.null(markers_storage())==F)
    markers_ST <- markers_storage()
    markers <- markers_ST %>% subset(., .$Applied==F)
    req(nrow(markers)>0)

    # Intersect markers and distribution
    dist_inter <- distSP()
    InterMark <- st_join(dist_inter, markers, join=st_intersects) %>% subset(., is.na(.$Row)==F)
    dist_inter$mark_intersect <- dist_inter$ID %in% InterMark$ID
    
    # Add/edit polygons
    dist_inter$presence[dist_inter$mark_intersect==T] <- input$Pres_batch
    dist_inter$origin[dist_inter$mark_intersect==T] <- input$Orig_batch
    dist_inter$seasonal[dist_inter$mark_intersect==T] <- input$Seas_batch
    
    # Update markers_storage Applied column
    markers_ST$Applied <- T
    markers_storage(markers_ST)
    
    # Update distribution
    dist_inter$Popup[dist_inter$mark_intersect==T] <- sRLPolyg_CreatePopup(dist_inter[dist_inter$mark_intersect==T,])
    dist_inter$mark_intersect <- NULL
    distSP(dist_inter)
    edits <- sRLPolyg_CreateLeaflet(AllowEdit())
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=1)
    Unsaved_changes("yes")
    
    showNotification("Hydrobasins correctly added or edited. Don't forget to click on 'Save changes'", type="message", duration=2)
  })
  
  
  
  #### Batch remove hydrobasins with markers -------------
  observeEvent(input$Rm_batch, {
    sRL_loginfo("START - Batch remove hydrobasins", input$sci_name)
    track_storage$L$BatchRmHydro <- track_storage$L$BatchRmHydro+1
    
    # Prepare markers
    req(is.null(markers_storage())==F)
    markers_ST <- markers_storage()
    markers <- markers_ST %>% subset(., .$Applied==F)
    req(nrow(markers)>0)
    
    # Intersect markers and distribution
    dist_inter <- distSP()
    InterMark <- st_join(dist_inter, markers, join=st_intersects) %>% subset(., is.na(.$Row)==F)
    dist_inter$mark_intersect <- dist_inter$ID %in% InterMark$ID
    
    # Remove polygons
    dist_inter$presence[dist_inter$mark_intersect==T] <- NA
    dist_inter$origin[dist_inter$mark_intersect==T] <- NA
    dist_inter$seasonal[dist_inter$mark_intersect==T] <- NA
    
    # Update markers_storage Applied column
    markers_ST$Applied <- T
    markers_storage(markers_ST)
    
    # Update distribution
    dist_inter$Popup[dist_inter$mark_intersect==T] <- sRLPolyg_CreatePopup(dist_inter[dist_inter$mark_intersect==T,])
    dist_inter$mark_intersect <- NULL
    distSP(dist_inter)
    edits <- sRLPolyg_CreateLeaflet(AllowEdit())
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=1)
    Unsaved_changes("yes")
    
    showNotification("Hydrobasins correctly removed. Don't forget to click on 'Save changes'", type="message", duration=2)
    
  })
  
  
  #### Save polygon edits --------
  observeEvent(input$save, {
    sRL_loginfo("START - Save manual edit polygon", input$sci_name)

    ### Save distribution (with edited attribute fields)
    Storage_SPNEW <- Storage_SP()
    dist_tosave <- distSP()
    
    # Crop if needed
    EXT_dist <- extent(dist_tosave)
    if(EXT_dist[1]<(-180) | EXT_dist[2]>180 | EXT_dist[3]<(-90) | EXT_dist[4]>90){dist_tosave <- st_crop(dist_tosave, xmin=-179.999,xmax=179.999,ymin=-89.999,ymax=89.999) ; distSP(dist_tosave)}
    
    # Change for HQ geometries if hydro was simplified
    if("hydroSP_HQ" %in% names(Storage_SPNEW)){
      dist_tosave$geometry <- Storage_SPNEW$hydroSP_HQ$geometry[match(dist_tosave$hybas_id, Storage_SPNEW$hydroSP_HQ$hybas_id)]
    }
    
    # Check at least one polygon remains
    if(nrow(dist_tosave)==0){showNotification(ui=HTML("You removed all polygons from your distribution and it is now empty. We discarded your last edits to bring back your former distribution."), type="error", duration=3) ; Run_discard(Run_discard()+1) ; req(F)}
    
    # Add attributes
    dist_tosave$binomial <- input$sci_name
    dist_tosave$id_no <- sRL_CalcIdno(input$sci_name)
    dist_tosave$source <- input$Field_source
    dist_tosave$yrcompiled <- input$Field_yrcompiled
    dist_tosave$citation <- input$Field_citation
    dist_tosave$compiler <- input$Field_compiler
    dist_tosave$data_sens <- ifelse(input$Field_datasens, 1, 0)
    dist_tosave$sens_comm <- input$Field_senscomm
    dist_tosave$island <- input$Field_island
    dist_tosave$dist_comm <- input$Field_distcomm
    if(nchar(input$Field_distcomm)>254){showNotification(ui=HTML(paste0("Distribution comment cannot be longer than 254 characters (currently ", nchar(input$Field_distcomm)," characters), please reduce the text.")), type="error", duration=3) ; req(F)}
    
    # Subset if hydrobasins
    if(AllowEdit()=="hydro"){
     dist_tosave <- dist_tosave %>% subset(., is.na(presence)==F)
    }
    
    # Save
    Storage_SPNEW$distSP_saved <- dist_tosave 
    
    ### Extract points attributes
    if(is.null(nrow(dat_pts()))==F){
      # Intersect
      pts_inter <- st_join(dat_pts(), dist_tosave, join=st_intersects)
      # Save in dat_proj_saved
      Storage_SPNEW$dat_proj_saved$presence <- pts_inter$presence[match(Storage_SPNEW$dat_proj_saved$gbifID, pts_inter$gbifID)]
      Storage_SPNEW$dat_proj_saved$origin <- pts_inter$origin[match(Storage_SPNEW$dat_proj_saved$gbifID, pts_inter$gbifID)]
      Storage_SPNEW$dat_proj_saved$seasonal <- pts_inter$seasonal[match(Storage_SPNEW$dat_proj_saved$gbifID, pts_inter$gbifID)]
    }
    
    ### Save plot edited distribution for report
    dist_tosave$cols <- sRL_ColourDistrib(dist_tosave)$cols
    plot_dist <- ggplot() +
     geom_sf(data = Storage_SPNEW$CountrySP_saved, fill="gray96", col="gray50") + # nolint
     geom_sf(data = dist_tosave, fill = dist_tosave$cols) +
     theme_void() +
     ggtitle("")
    ggsave(paste0("resources/AOH_stored/", sub(" ", "_", input$sci_name), "_", sRL_userdecode(input$user), "/Plots/plot_manually_edited.png"), plot_dist, width=10, height=8)
    
    # Record usage
    Storage_SPNEW$Output$Value[Storage_SPNEW$Output$Parameter=="Gbif_EditPoly"]<-"yes"
    Storage_SPNEW$Output$Count[Storage_SPNEW$Output$Parameter=="Gbif_EditPoly"]<-Storage_SPNEW$Output$Count[Storage_SPNEW$Output$Parameter=="Gbif_EditPoly"]+1
    Storage_SPNEW$Output$Value[Storage_SPNEW$Output$Parameter=="Gbif_EditPoly_details"] <- paste(names(track_storage$L), unlist(track_storage$L), sep="=") %>% paste(., collapse=", ")
    
    # Save Storage file
    sRL_StoreSave(input$sci_name, input$user,  Storage_SPNEW)
    Storage_SP(Storage_SPNEW)
    sRL_saveMapDistribution(input$sci_name, Storage_SPNEW)
    
    ### Update map
    edits <- sRLPolyg_CreateLeaflet(AllowEdit())
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=1)
    Unsaved_changes("no")
    
    showNotification(ui=HTML("Changes saved succefully"), type="message", duration=2)
    
  })
  
  
  #### Discard changes ----------
  ### Trigger discard function when discard button is clicked
  observeEvent(input$discard, {
    Run_discard(Run_discard()+1)
  })
  
  ### Apply discard effect (when button clicked or when an error was found and Run_discard was called)
  observeEvent(Run_discard(), {
    req(Run_discard()>0)
    sRL_loginfo("START - Discard changes", input$sci_name)
    
    PolygToRemove <- distSP()$ID
    
    Storage_SP(sRL_StoreRead(input$sci_name,  input$user, MANDAT=1))
    if(T %in% grepl("hydro", Storage_SP()$Output$Value)){
      if("distSP_saved" %in% names(Storage_SP())){
        dist_loaded <- Storage_SP()$distSP_saved
      } else {
          dist_loaded <- Storage_SP()$distSP3_BeforeCrop %>% sRLPolyg_PrepareHydro(., hydro_raw, paste0("hydro", substr(dist_saved$hybas_id[1],2,3)), SRC_created="yes")$hydroSP # SRC_created is yes even if we used "Edit as hydrobasins" for a RL distribution to avoid charging hydrobasins from the scratch
          }
    } else {
      if("distSP_saved" %in% names(Storage_SP())){dist_loaded <- Storage_SP()$distSP_saved} else {dist_loaded <- Storage_SP()$distSP_saved}
      dist_loaded <- sRLPolyg_InitDistri(dist_loaded, 4326)
    }
    distSP(dist_loaded)
    
    ### Edit storage reactive values saying everything was applied
    lines <- lines_storage() ; lines$Applied <- T ; lines_storage(lines)
    drawn <- drawn_storage() ; drawn$Applied <- T ; drawn_storage(drawn)
    markers <- markers_storage() ; markers$Applied <- T ; markers_storage(markers)
    
    ### Update leaflet map
    req(distSP())
    edits <- sRLPolyg_CreateLeaflet(AllowEdit())
    sRLPolyg_UpdateLeaflet(distSP(), dat_pts(), frame=1, PolygToRemove)
    Unsaved_changes("no")
  })
  
  
  ### Outputs ---------
  # Return AllowEdit to determine if we propose normal or hydrobasins options (set suspendwhenHidden is needed to make it work without returning AllowEdit in the panel)
  output$AllowEdit <- reactive({AllowEdit()}) ; outputOptions(output, 'AllowEdit', suspendWhenHidden=FALSE)
  
  # Return Suggest_hydro to determine if we propose the 'Edit as hydrobasins' button
  output$Suggest_hydro <- reactive({return(Suggest_hydro())}) ; outputOptions(output, 'Suggest_hydro', suspendWhenHidden=FALSE)
  
  # Create an output with field values, so that Unsaved_changes is turned to "yes" when activated
  output$RecordChangeFields <- reactive({Combin <- paste0(input$Field_source, input$Field_yrcompiled, input$Field_citation, input$Field_compiler, input$Field_datasens, input$Field_senscomm, input$Field_island, input$Field_distcomm) ; Unsaved_changes("yes"); return(Combin)}) ; outputOptions(output, 'RecordChangeFields', suspendWhenHidden=FALSE)
  
  # Return Unsaved_changes to determine if there are unsaved changes
  output$Unsaved_changes <- reactive({return(Unsaved_changes())}) ; outputOptions(output, 'Unsaved_changes', suspendWhenHidden=FALSE)
  
  # Extract cropping parameters to display buttons or not
  output$CropLand <- reactive({req(Storage_SP()) ; Mess <- Storage_SP()$CropLand_option ; return(Mess)}) ; outputOptions(output, 'CropLand', suspendWhenHidden=FALSE)
  output$TooltipCropland <- reactive({req(Storage_SP()) ; Mess <- Storage_SP()$CropLand_tooltip ; return(Mess)})
  output$CropCountry <- reactive({req(Storage_SP()) ; Mess <- Storage_SP()$CropCountry_option ; return(Mess)}) ; outputOptions(output, 'CropCountry', suspendWhenHidden=FALSE)
}



### Run app ----
shinyApp(ui = ui, server = server)
