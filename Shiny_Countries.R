### TO DO
# TO TEST: What happens with SISabsent (when matching existing attributes + when exporting)
# Tracking (inc. did I change manually)
# RMD

library(shiny)
library(leaflet)
library(mapedit)
library(sf)
library(htmlwidgets)
library(shinycssloaders)
library(DT)

source(textConnection(readLines("server.R")[1:99])) # Must include coo_raw and realms_raw

### Charge results from step 1b2
# http://127.0.0.1:3965/?sci_name=Azteca_xanthochroa&user=victor.cazalis



### Load functions
source("sRLfun_ShinyCountries.R")


### Specify url
options(shiny.host = '127.0.0.1')
options(shiny.port = 3965)





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
      withSpinner(
        leafletOutput('Leaf', height=600, width="100%"), 
        type=3, color.background = "#f5f5f5ff"
      ),
      width=7
    ),
    sidebarPanel(
      titlePanel("Edit Countries of occurrence"),
      actionButton('save', 'Save changes', style="color: #fff; background-color: #009138ff; border-color: #009138ff"),
      DT::DTOutput("COO_table"),
      width=5
    )
  )
)




### Server ----------
server <- function(input, output, session) {
  
  
  ### CREATE REACTIVE VALUES ---------
  COO <- reactiveVal()
  Change_tomake <- reactiveVal(data.frame(lookup=NA, Change=NA)[0,])
  Storage_SP <- reactiveVal()
  
  
  
  ### EVENTS ---------

  ### Read the URL parameter from session$clientData$url_search
  observe({
    query <- parseQueryString(session$clientData$url_search)
    updateTextInput(session, "sci_name", value = query[['sci_name']] %>% gsub("_" , " ", .) %>% gsub("%20%" , " ", .))
    updateTextInput(session, "user", value = query[['user']])
  })
  
  
  ### Update base map with species information
  observe({
      req(input$sci_name)
      req(is.null(nrow(COO()))) # Does not run if COO already loaded
      sRL_loginfo("START - Load COO", input$sci_name)
      
      ### Load StorageSP and COO
      Error_mess<-tryCatch({
        Storage_SP(sRL_StoreRead(input$sci_name,  input$user, MANDAT=1))
        # Load COO and add lookups for subnational entities not in SIS
        COO_touse <- Storage_SP()$coo %>% .[order(.$presence),]
        COO_touse$lookup[is.na(COO_touse$lookup)] <- revalue(COO_touse$SIS_name1[is.na(COO_touse$lookup)], c("Arica y Parinacota (Absent_SIS)"="notSIS1", "Los Rios (Absent_SIS)"="notSIS2", "Nuble (Absent_SIS)"="notSSIS3", "Telangana (Absent_SIS)"="notSIS4"))
        # Add attributes from countries_SIS
        coo_occ <- Storage_SP()$coo_occ
        COO_touse$presence <- coo_occ$presence[match(COO_touse$lookup, coo_occ$lookup)] %>% as.character(.)
        COO_touse$origin <- coo_occ$origin[match(COO_touse$lookup, coo_occ$lookup)] %>% as.character(.)
        COO_touse$seasonal <- coo_occ$seasonal[match(COO_touse$lookup, coo_occ$lookup)] %>% as.character(.)
        # Update COO()
        COO(COO_touse)
      }, error=function(e){"<b>We could not find the COO extract. Please try again.</b>"})
      if(Error_mess != "TRUE"){showNotification(ui=HTML(Error_mess), type="error", duration=8)}
      
  })
  
  
  ### Record change to make to COO from button actions
  observeEvent(input$button_occupied, {

    sRL_loginfo("START - Record change to make", input$sci_name)

    Change_dir <- ifelse(grepl("makeoccupied", input$button_occupied), TRUE, FALSE)
    Change_lookup <- input$button_occupied %>% sub("makeoccupied_", "", .) %>% sub("makeempty_", "", .)
    
    Change_tomake(rbind(Change_tomake(), 
                        data.frame(lookup=Change_lookup, 
                                   Change=Change_dir)
    ))
    
    sRL_loginfo("END - Record change to make", input$sci_name)
    
  })
  
  ### Record change to make to COO from table editing
  observeEvent(input$COO_table_cell_edit, {
    
    sRL_loginfo("START - Record table edits", input$sci_name)
    
    # Extract the changes that have been made
    Change_lookup <- COO()$lookup[input$COO_table_cell_edit$row]
    Column <- names(COO())[(input$COO_table_cell_edit$col+3)]
    Change_val <- paste0(Column, "_", input$COO_table_cell_edit$value)
    
    # Check if values are valid
    STOP=0
    if(Column=="presence" & (! sub("presence_", "", Change_val) %in% c("", "1", "2", "3", "4", "5", "6"))){STOP=1 ; showNotification(ui=HTML("Presence column can only include values from 1 to 6"), type="error", duration=5)}
    if(Column=="origin" & (! sub("origin_", "", Change_val) %in% c("", "1", "2", "3", "4", "5", "6"))){STOP=1 ; showNotification(ui=HTML("Origin column can only include values from 1 to 6"), type="error", duration=5)}
    if(Column=="seasonal" & (! sub("seasonal_", "", Change_val) %in% c("", "1", "2", "3", "4", "5"))){STOP=1 ; showNotification(ui=HTML("Seasonal column can only include values from 1 to 5"), type="error", duration=5)}
    
    # Store changes in Change_tomake
    if(STOP==0){
      Change_tomake(rbind(Change_tomake(), 
                          data.frame(lookup=Change_lookup, 
                                     Change=Change_val)
      )) 
    }
    
    sRL_loginfo("END - Record table edits", input$sci_name)
    
  })
  
  
  
  ### Save
  observeEvent(input$save, {
    sRL_loginfo("START - Save manual edit countries", input$sci_name)
    
    COO_NEW <- COO()
    
    ### Edit COO from button actions or table editing
    if(nrow(Change_tomake())>0){
      for(R in 1:nrow(Change_tomake())){
        if(Change_tomake()$Change[R] %in% c(TRUE, FALSE)){
          COO_NEW$Level1_occupied[COO_NEW$lookup==Change_tomake()$lookup[R]] <- Change_tomake()$Change[R]
          COO_NEW$presence[COO_NEW$lookup==Change_tomake()$lookup[R]] <- ifelse(Change_tomake()$Change[R]==TRUE, 1, NA)
          COO_NEW$origin[COO_NEW$lookup==Change_tomake()$lookup[R]] <- ifelse(Change_tomake()$Change[R]==TRUE, 1, NA)
          COO_NEW$seasonal[COO_NEW$lookup==Change_tomake()$lookup[R]] <- ifelse(Change_tomake()$Change[R]==TRUE, 1, NA)
        } else {
          
          # Apply changes in attributes (+ if presence attribute changed from yes to no (or reverse), also change Level1_occupied)
          if(grepl("presence", Change_tomake()$Change[R])){
            COO_NEW$presence[COO_NEW$lookup==Change_tomake()$lookup[R]] <- unlist(strsplit(Change_tomake()$Change[R], "_"))[2]
            COO_NEW$Level1_occupied[COO_NEW$lookup==Change_tomake()$lookup[R]] <- ifelse(Change_tomake()$Change[R]=="presence_", FALSE, TRUE)
            if(COO_NEW$Level1_occupied[COO_NEW$lookup==Change_tomake()$lookup[R]]==""){COO_NEW$presence[COO_NEW$lookup==Change_tomake()$lookup[R]] <- NA ; COO_NEW$origin[COO_NEW$lookup==Change_tomake()$lookup[R]] <- NA ; COO_NEW$seasonal[COO_NEW$lookup==Change_tomake()$lookup[R]] <- NA}
            }
          if(grepl("origin", Change_tomake()$Change[R])){COO_NEW$origin[COO_NEW$lookup==Change_tomake()$lookup[R]] <- unlist(strsplit(Change_tomake()$Change[R], "_"))[2]}
          if(grepl("seasonal", Change_tomake()$Change[R])){COO_NEW$seasonal[COO_NEW$lookup==Change_tomake()$lookup[R]] <- unlist(strsplit(Change_tomake()$Change[R], "_"))[2]}
        }
      }
    }
    
    
    # Edit national levels when a subnational is TRUE
    COO_NEW$Level0_occupied <- COO_NEW$SIS_name0 %in% subset(COO_NEW, COO_NEW$Level1_occupied==T)$SIS_name0
    
    # Order by presence
    COO_NEW <- COO_NEW[order(COO_NEW$presence, COO_NEW$SIS_name0, COO_NEW$SIS_name1),]
    
    
    # Save changes in COO and Storage_SP
    COO(COO_NEW)
    Storage_SPNEW <- Storage_SP()
    Storage_SPNEW$coo <- COO()

    # Save change in coo list
    coo_occ<-subset(COO(), COO()$Level1_occupied==T) 
    if(nlevels(as.factor(paste0(coo_occ$SIS_name0, coo_occ$SIS_name1))) < nrow(coo_occ)){coo_occ <- coo_occ %>% dplyr::group_by(SIS_name0, SIS_name1, lookup, lookup_SIS0, presence, origin, seasonal) %>% dplyr::summarise(N= n())}
    Storage_SPNEW$coo_occ <- coo_occ
    
    # Save leaflet
    Storage_SPNEW$Leaflet_COO <- sRLCountry_CreateLeaflet(COO(), Storage_SP())
    
    # Record usage
    Storage_SPNEW$Output$Value[Storage_SPNEW$Output$Parameter=="COO_EditShiny"]<-"yes"
    Storage_SPNEW$Output$Count[Storage_SPNEW$Output$Parameter=="COO_EditShiny"]<-Storage_SPNEW$Output$Count[Storage_SPNEW$Output$Parameter=="COO_EditShiny"]+1
    
    # Save Storage file
    sRL_StoreSave(input$sci_name, input$user,  Storage_SPNEW)
    
    # Reinitiate Change_tomake
    Change_tomake(data.frame(lookup=NA, Change=NA)[0,])
    
    sRL_loginfo("END - Save manual edit countries", input$sci_name)
  })
  
  
  
  ### OUTPUTS  ---------
  
  output$Leaf <- renderLeaflet({
    req(COO())
    print("New map")
    sRLCountry_CreateLeaflet(COO(), Storage_SP())
    })
  
  output$COO_table <- renderDataTable({
    req(COO())
    print("New table")
    sRLCountry_CreateTable(COO())
  })
}



# Run the application 
shinyApp(ui = ui, server = server)




