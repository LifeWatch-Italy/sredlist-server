# Set working directory (Victor path if we are on his laptop, LifeWatch path otherwise)
setwd(dir=ifelse(file.exists("C:/Users/Victor"),"C:/Users/Victor/Documents/sRedList/Platform/InProgress/sredlist-server-develop", "/media/docker/sRedList/sredlist-server"))


library(shiny)
library(leaflet)
library(mapedit)
library(sf)
library(htmlwidgets)
library(bslib)
library(rgbif)
library(DT)
library(spsComps) # For addLoader in server

source(textConnection(readLines("server.R")[1:73]))

### Charge results from step 1b2
# http://127.0.0.1:7839/?sci_name=Azteca_xanthochroa&user=victor.cazalis



### Load functions
source("sRLfun_ShinyEditPoints.R")


### Specify url
options(shiny.host = '127.0.0.1')
options(shiny.port = 7839)





### UI ----------
ui <- page_fillable(
  
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(10%);
             left: calc(-150%);
             }")
    )),
  
  # add map
  accordion_filters <- accordion(open="Map",
      accordion_panel("Map", icon = bsicons::bs_icon("map"),              
                                   
                      sidebarLayout(
                        position="left",
                        mainPanel(
                          conditionalPanel('false', 
                                           textInput("sci_name",
                                                     "Scientific name:"),
                                           textInput("user",
                                                     "User name:")
                          ),
                          editModUI("map", height=600, width = "100%"),
                          
                          # Version number (just for Victor to ensure the correct version is deployed)
                          conditionalPanel(condition='input.user=="victor.cazalis"', paste0("version 1.5_prodSept deployed on ", as.character(Sys.Date())))
                        ),
                        sidebarPanel(
                          titlePanel("Drag existing records"),
                          actionButton('movePts', 'Allow dragging', style="color: #fff; background-color: #009138ff; border-color: #009138ff"), 
                          
                          titlePanel("Enter new records attributes"),
                          numericInput("Pts_year", label="Year", value=NA),
                          textInput("Pts_source", label="Source", value=NA),
                          titlePanel("Save changes"),
                          actionButton('save', 'Save from Map', style="color: #fff; background-color: #009138ff; border-color: #009138ff")
                        )
                      )
      ),
      
      accordion_panel("Explore georeferenced records", icon = bsicons::bs_icon("search"),
                      DTOutput("Table_points")
      ),
      
      accordion_panel("Explore non-georeferenced records", icon = bsicons::bs_icon("search"),
                      actionButton("GetNonGeo", "Get GBIF non-georeferenced records", style="color: #fff; background-color: #009138ff; border-color: #009138ff"),
                      DTOutput("Table_nongeoDT")
                      )
    )
)



### Server ----------
server <- function(input, output, session) {
  
  
  
  ### Create objects ---------

  ### Create base map
  edits <- callModule(
    editMod,
    leafmap = sRLMan_CreateLeaflet(),
    id = "map",
    record = FALSE,
    sf = TRUE
  )
  
  ### CREATE REACTIVE VALUES
  flagsSF <- reactiveVal()
  flags <- reactiveVal()
  Storage_SP <- reactiveVal()
  Table_nongeo <- reactiveVal(data.frame())
  
  
  ### Events ---------

  ### Read the URL parameter from session$clientData$url_search
  observe({
    query <- parseQueryString(session$clientData$url_search) ; req(nchar(query)>0)
    updateTextInput(session, "sci_name", value = query[['sci_name']] %>% gsub("_" , " ", .) %>% gsub("%20%" , " ", .))
    updateTextInput(session, "user", value = sRL_userdecode(query[['user']]))
  })
  
  
  ### Update base map with species information
  observe({
      req(input$sci_name)
      req(is.null(nrow(flags()))) # Does not run if flags already loaded
      sRL_loginfo("START - Load records", input$sci_name)
      
      ### Load StorageSP and flags
      Error_mess<-tryCatch({
        Storage_SP(sRL_StoreRead(input$sci_name,  input$user, MANDAT=1)) ; print(names(Storage_SP()))
        flags(Storage_SP()$flags)
      }, error=function(e){"<b>We could not find the points gathered in Step 1b of the sRedList platform. Please try again.</b>"})
      if(Error_mess != "TRUE"){showNotification(ui=HTML(Error_mess), type="error", duration=8)}
      
      
      ### Update leaflet map
      req(flags())
      flagsSF(sRLMan_JitterPoints(st_as_sf(flags(), coords = c("decimalLongitude", "decimalLatitude"), crs="+proj=longlat +datum=WGS84", remove=F)))
      sRLMan_UpdateLeaflet(flagsSF(), frame=1, Drag=F)
  })
  
  
  ### Move points
  # Action button to allow moving points
  observeEvent(input$movePts, {
    sRLMan_UpdateLeaflet(flagsSF(), frame=0, Drag=T)
  })
  
  
  # Moving points function
  observeEvent(input$"map-map_marker_dragend", {

    sRL_loginfo("START - Save dragging records", input$sci_name)
    
    flagsSF(sRLMan_MovePoints(flagsSF(), input$"map-map_marker_dragend"))
    
    sRL_loginfo("END - Save dragging records", input$sci_name)
  })
  
  
  ### Explore non-georeferenced records
  observeEvent(input$GetNonGeo, {
    sRL_loginfo("START - Explore non-georeferenced records", input$sci_name)
    
    # Loader
    loader_nongeo <- addLoader$new("sci_name", color = "#009138ff", method = "full_screen", height = "30rem", opacity=0.4) ; loader_nongeo$show()
    
    # Get records
    dat_nongeo <- occ_data(scientificName=input$sci_name, 
                                hasCoordinate = F, 
                                limit=1000
    )$data
    
    # Create table
    if(is.null(nrow(dat_nongeo))==F){
      Tab <- dat_nongeo %>%
        mutate(Link=paste0("<a href='https://gbif.org/occurrence/", .$gbifID, "' target='_blank'>Link</a>")) %>%
        .[, names(.) %in% c("scientificName", "basisOfRecord", "eventDate", "higherGeography", "continent", "country", "locality", "institutionCode", "collectionCode", "occurrenceRemarks", "Link")]
      
      Table_nongeo(Tab)
    }
    
    # End loader
    loader_nongeo$hide()
    
  })
  
  
  ### Save points
  observeEvent(input$save, {
    sRL_loginfo("START - Save manual edit records", input$sci_name)
    
    ### Update record flagging
    flagsSF(sRLMan_EditPoints(edits()$finished, flagsSF(), input$Pts_year, input$Pts_source))

    ### Save flags and record usage
    # Save points
    Storage_SPNEW <- Storage_SP()
    Storage_SPNEW$flags <- flagsSF() %>% as.data.frame(.) %>% .[, names(.) != "geometry"]
    Storage_SPNEW$dat_proj_saved <- sRL_SubsetGbif(Storage_SPNEW$flags, input$sci_name) # I use Storage_SPNEW$flags to make sure we are not using a df with geometries
    
    # Record usage
    Storage_SPNEW$Output$Value[Storage_SPNEW$Output$Parameter=="Gbif_EditPts"]<-"yes"
    Storage_SPNEW$Output$Count[Storage_SPNEW$Output$Parameter=="Gbif_EditPts"]<-Storage_SPNEW$Output$Count[Storage_SPNEW$Output$Parameter=="Gbif_EditPts"]+1
    
    # Save table nongeo
    if(nrow(Table_nongeo())>0){Storage_SPNEW$TableNonGeo <- Table_nongeo()}

    # Save Storage file
    sRL_StoreSave(input$sci_name, input$user,  Storage_SPNEW)
    Storage_SP(Storage_SPNEW)

    ### Update map
    sRLMan_UpdateLeaflet(flagsSF(), frame=1, Drag=F)

    sRL_loginfo("END - Save manual edit records", input$sci_name)
  })
  
  
  observeEvent(input$button_valid, {
    
    # Identify GBIF id of the record to edit
    GbifID <- input$button_valid %>% sub("makeinvalid_", "", .)  %>% sub("makevalid_", "", .) 
    
    # Isolate flagsSF and edit its "reason"
    flagsSF_edit <- flagsSF()
    flagsSF_edit$Reason[flagsSF_edit$gbifID==GbifID] <- ifelse(grepl("makeinvalid", input$button_valid), "Manual_invalid", NA)
    
    # Save changes and edit leaflet
    flagsSF(flagsSF_edit)
    sRLMan_UpdateLeaflet(flagsSF(), frame=0, Drag=F)
    
  })
  
  
  ### Outputs ---------
  output$Table_points <- renderDataTable({
    
    flags_to_show <- flagsSF()[, ! names(flagsSF()) %in% c("geometry", "PopText", "Coords_merged", "unique", "Lon_jitt", "Lat_jitt", "_leaflet_id", "Only_for_syn")]
    flags_to_show[,which(substr(names(flags_to_show), 1, 1)==".")] <- NULL
    flags_to_show$geometry <- NULL

    flags_to_show$Link[is.na(flags_to_show$Link)==F] <- paste0("<a href='", flags_to_show$Link[is.na(flags_to_show$Link)==F], "' target='_blank'>Link</a>")
    flags_to_show <- dplyr::relocate(flags_to_show, "Reason", .before="countryCode")
    
    COL_names <- c(levels(as.factor(flags_to_show$Reason)), NA)
    COL_values <- c(rep("#C0B2CF", nlevels(as.factor(flags_to_show$Reason))), "#FCE89C")
    
    req(nrow(flagsSF())>0)
    datatable(
      flags_to_show,
      escape = FALSE,
      filter="top",
      options = list(pageLength = 50,
                     headerCallback = DT::JS(# Needed to reduce column name font
                       "function(thead) {",
                       "  $(thead).css('font-size', '0.75em');",
                       "}")
      ),
      rownames=FALSE) %>% 
      DT::formatStyle(columns = c(1:ncol(flags_to_show)), fontSize = '75%') %>%
      DT::formatStyle(columns=which(names(flags_to_show)=="Reason"), backgroundColor=styleEqual(levels=COL_names, values=COL_values))
  })
  
  output$Table_nongeoDT <- renderDataTable({
    req(input$GetNonGeo & nrow(Table_nongeo())>0)
    datatable(
      Table_nongeo(),
      escape = FALSE,
      filter="top",
      options = list(pageLength = 50,
                     headerCallback = DT::JS(# Needed to reduce column name font
                       "function(thead) {",
                       "  $(thead).css('font-size', '0.75em');",
                       "}")
      ),
      rownames=FALSE) %>% DT::formatStyle(columns = c(1:ncol(Table_nongeo())), fontSize = '75%')
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

