# Set working directory (Victor path if we are on his laptop, LifeWatch path otherwise)
setwd(dir=ifelse(file.exists("C:/Users/Victor"),"C:/Users/Victor/Documents/sRedList/Platform/InProgress/sredlist-server-develop", "/media/docker/sRedList/sredlist-server"))


library(shiny)
library(leaflet)
library(mapedit)
library(sf)
library(htmlwidgets)

source(textConnection(readLines("server.R")[1:73]))

### Charge results from step 1b2
# http://127.0.0.1:7839/?sci_name=Azteca_xanthochroa&user=victor.cazalis



### Load functions
source("sRLfun_ShinyEditPoints.R")


### Specify url
options(shiny.host = '127.0.0.1')
options(shiny.port = 7839)





### UI ----------
ui <- fluidPage(
  
  tags$style(type="text/css",
             ".shiny-notification {
    position:fixed;
    top: calc(25%);
    left: calc(50%);
  }"),
  
  # add map
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
  
  
  
  ### Events ---------

  ### Read the URL parameter from session$clientData$url_search
  observe({
    query <- parseQueryString(session$clientData$url_search)
    updateTextInput(session, "sci_name", value = query[['sci_name']] %>% gsub("_" , " ", .) %>% gsub("%20%" , " ", .))
    updateTextInput(session, "user", value = query[['user']] %>% sRL_userdecode(.))
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
}





# Run the application 
shinyApp(ui = ui, server = server)




