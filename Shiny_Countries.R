### TO DO
# Marine ???
# What happens when multiple attributes per country (either make it work in exports or ensure people can only enter specific values)
# Tracking
# RMD

library(shiny)
library(leaflet)
library(mapedit)
library(sf)
library(htmlwidgets)
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
      leafletOutput('Leaf', height=600, width="100%"),
      width=7
      
    ),
    sidebarPanel(
      titlePanel("XX"),
      actionButton('save', 'Save from Map', style="color: #fff; background-color: #009138ff; border-color: #009138ff"),
      titlePanel("Countries of occurrence"),
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
        Storage_SP(sRL_StoreRead(input$sci_name,  input$user, MANDAT=1)) ; print(names(Storage_SP()))
        COO(Storage_SP()$coo %>% .[order(.$presence),])
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
    
    print(Change_tomake())
    
    sRL_loginfo("END - Record change to make", input$sci_name)
    
  })
  
  ### Record change to make to COO from table editing
  observeEvent(input$COO_table_cell_edit, {
    
    sRL_loginfo("START - Record table edits", input$sci_name)
    
    Change_lookup <- COO()$lookup[input$COO_table_cell_edit$row]
    Column <- names(COO())[(input$COO_table_cell_edit$col+3)]
    Change_val <- paste0(Column, "_", input$COO_table_cell_edit$value)
    
    Change_tomake(rbind(Change_tomake(), 
                        data.frame(lookup=Change_lookup, 
                                   Change=Change_val)
    ))
    
    print(Change_tomake())
    
    
    sRL_loginfo("END - Record table edits", input$sci_name)
    
  })
  
  
  
  ### Save points
  observeEvent(input$save, {
    sRL_loginfo("START - Save manual edit countries", input$sci_name)
    
    COO_NEW <- COO()
    
    ### Edit COO from button actions or table editing
    for(R in 1:nrow(Change_tomake())){
      
      if(Change_tomake()$Change[R] %in% c(TRUE, FALSE)){
        COO_NEW$Level1_occupied[COO_NEW$lookup==Change_tomake()$lookup[R]] <- Change_tomake()$Change[R]
        COO_NEW$presence[COO_NEW$lookup==Change_tomake()$lookup[R]] <- ifelse(Change_tomake()$Change[R]==TRUE, 1, NA)
        COO_NEW$origin[COO_NEW$lookup==Change_tomake()$lookup[R]] <- ifelse(Change_tomake()$Change[R]==TRUE, 1, NA)
        COO_NEW$seasonal[COO_NEW$lookup==Change_tomake()$lookup[R]] <- ifelse(Change_tomake()$Change[R]==TRUE, 1, NA)
      } else {
        if(grepl("presence", Change_tomake()$Change[R])){COO_NEW$presence[COO_NEW$lookup==Change_tomake()$lookup[R]] <- unlist(strsplit(Change_tomake()$Change[R], "_"))[2]}
        if(grepl("origin", Change_tomake()$Change[R])){COO_NEW$origin[COO_NEW$lookup==Change_tomake()$lookup[R]] <- unlist(strsplit(Change_tomake()$Change[R], "_"))[2]}
        if(grepl("seasonal", Change_tomake()$Change[R])){COO_NEW$seasonal[COO_NEW$lookup==Change_tomake()$lookup[R]] <- unlist(strsplit(Change_tomake()$Change[R], "_"))[2]}
      }
      
    }
    
    # Edit national levels when a subnational is TRUE
    COO_NEW$Level0_occupied <- COO_NEW$SIS_name0 %in% subset(COO_NEW, COO_NEW$Level1_occupied==T)$SIS_name0
    
    # Order by presence
    COO_NEW <- COO_NEW[order(COO_NEW$presence),]
    
    
    # Save changes in COO and Storage_SP
    COO(COO_NEW)
    Storage_SPNEW <- Storage_SP()
    Storage_SPNEW$coo <- COO()
    Storage_SPNEW$coo_res <- sRL_cooInfoBox_prepare(COO(), Storage_SPNEW) %>% sRL_cooInfoBox_format(.)
      
      # Save Storage file
      sRL_StoreSave(input$sci_name, input$user,  Storage_SPNEW)
    
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




