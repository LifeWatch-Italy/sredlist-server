# Set working directory (Victor path if we are on his laptop, LifeWatch path otherwise)
setwd(dir=ifelse(file.exists("C:/Users/Victor"),"C:/Users/Victor/Documents/sRedList/Platform/InProgress/sredlist-server-develop", "/media/docker/sRedList/sredlist-server"))

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
      width=6
    ),
    sidebarPanel(
      titlePanel("Edit Countries of occurrence"),
      conditionalPanel(
        "input.button_occupied || input.COO_table_cell_edit",
        HTML('<font color="#009138"> <b>Click "Save changes" to apply changes to the map and save</b></font><br>'),
        actionButton('save', 'Save changes', style="color: #fff; background-color: #009138ff; border-color: #009138ff")
        ),
      div(DT::DTOutput("COO_table"), style = "font-size:80%"),
      width=6
    )
  )
)




### Server ----------
server <- function(input, output, session) {
  
  
  ### CREATE REACTIVE VALUES ---------
  COO <- reactiveVal()
  COO_uniq <- reactiveVal()
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
        COO_touse <- Storage_SP()$coo
        COO_touse$lookup[is.na(COO_touse$lookup)] <- revalue(COO_touse$SIS_name1[is.na(COO_touse$lookup)], c("Arica y Parinacota (Absent_SIS)"="notSIS1", "Los Rios (Absent_SIS)"="notSIS2", "Nuble (Absent_SIS)"="notSSIS3", "Telangana (Absent_SIS)"="notSIS4"))
        # Add attributes from coo_occ
        coo_occ <- Storage_SP()$coo_occ
        if("Records" %in% names(coo_occ)){coo_occ$presence <- ifelse(coo_occ$Records, 1, 3)}
        COO_touse$presence <- coo_occ$presence[match(COO_touse$lookup, coo_occ$lookup)] %>% as.character(.)
        COO_touse$origin <- coo_occ$origin[match(COO_touse$lookup, coo_occ$lookup)] %>% as.character(.)
        COO_touse$seasonal <- coo_occ$seasonal[match(COO_touse$lookup, coo_occ$lookup)] %>% as.character(.)
        # Order lines
        COO_touse <- COO_touse[order(COO_touse$presence, COO_touse$SIS_name0, COO_touse$SIS_name1),]
        # Update COO()
        COO(COO_touse)
        # Create COO_uniq for table (needs to remove marine/terrestrial duplicates)
        COO_uniq(as.data.frame(COO_touse) %>% distinct(., lookup, .keep_all=T))
        
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
    Change_lookup <- COO_uniq()$lookup[input$COO_table_cell_edit$row]
    Column <- names(COO_uniq())[(input$COO_table_cell_edit$col+3)]
    Change_val <- paste0(Column, "_", input$COO_table_cell_edit$value)
    
    # Check if values are valid
    STOP=0
    check_attribute <- function(Val, nums){Values <- Val %>% strsplit(., "[|]") %>% unlist(.) ; return(! FALSE %in% (Values %in% as.character(nums)))} # Return TRUE if attributes are ok, FALSE if a value is not in nums
    if(Column=="presence" & check_attribute(sub("presence_", "", Change_val), c(1:7))==F){STOP=1 ; showNotification(ui=HTML("Presence column can only include values from 1 to 7"), type="error", duration=5)}
    if(Column=="origin" & check_attribute(sub("origin_", "", Change_val), c(1:6))==F){STOP=1 ; showNotification(ui=HTML("Origin column can only include values from 1 to 6"), type="error", duration=5)}
    if(Column=="seasonal" & check_attribute(sub("seasonal_", "", Change_val), c(1:5))==F){STOP=1 ; showNotification(ui=HTML("Seasonal column can only include values from 1 to 5"), type="error", duration=5)}
    
    # Store changes in Change_tomake
    if(STOP==0){
      Change_tomake(rbind(Change_tomake(), 
                          data.frame(lookup=Change_lookup, 
                                     Change=Change_val)
      )) 
    }
    
    print(Change_tomake())
    
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
            if(!TRUE %in% COO_NEW$Level1_occupied[COO_NEW$lookup==Change_tomake()$lookup[R]]){COO_NEW$presence[COO_NEW$lookup==Change_tomake()$lookup[R]] <- NA ; COO_NEW$origin[COO_NEW$lookup==Change_tomake()$lookup[R]] <- NA ; COO_NEW$seasonal[COO_NEW$lookup==Change_tomake()$lookup[R]] <- NA}
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
    COO_uniq(as.data.frame(COO_NEW) %>% distinct(., lookup, .keep_all=T))
    Storage_SPNEW <- Storage_SP()
    Storage_SPNEW$coo <- COO()

    # Save change in coo list
    coo_occ<-subset(COO(), COO()$Level1_occupied==T) 
    if(nlevels(as.factor(paste0(coo_occ$SIS_name0, coo_occ$SIS_name1))) < nrow(coo_occ)){coo_occ <- coo_occ %>% dplyr::group_by(SIS_name0, SIS_name1, lookup, lookup_SIS0, presence, origin, seasonal) %>% dplyr::summarise(N= n())}
    Storage_SPNEW$coo_occ <- coo_occ
    
    # Save leaflet
    Storage_SPNEW$Leaflet_COO <- sRLCountry_CreateLeaflet(COO(), Storage_SP(), F)
    
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
    sRLCountry_CreateLeaflet(COO(), Storage_SP(), T)
    })
  
  output$COO_table <- renderDataTable({
    req(COO_uniq())
    print("New table")
    sRLCountry_CreateTable(COO_uniq())
  })
}



# Run the application 
shinyApp(ui = ui, server = server)




