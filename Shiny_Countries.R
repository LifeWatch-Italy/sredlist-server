### TO DO
# Add option to change Extant / Possibly Extant
# Add table below
# Integrate in client
# Update national colours when subnational are changes





library(shiny)
library(leaflet)
library(mapedit)
library(sf)
library(htmlwidgets)

source(textConnection(readLines("server.R")[1:73]))

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
      leafletOutput('Leaf', height=600, width="75%")
      
    ),
    sidebarPanel(
      titlePanel("Drag existing records"),
      # actionButton('movePts', 'Allow dragging', style="color: #fff; background-color: #009138ff; border-color: #009138ff"), 
      # 
      # titlePanel("Enter new records attributes"),
      # numericInput("Pts_year", label="Year", value=NA),
      # textInput("Pts_source", label="Source", value=NA),
      # titlePanel("Save changes"),
      actionButton('save', 'Save from Map', style="color: #fff; background-color: #009138ff; border-color: #009138ff")
    )
  )
)




### Server ----------
server <- function(input, output, session) {
  
  
  ### CREATE REACTIVE VALUES ---------
  COO <- reactiveVal()
  Change_tomake <- reactiveVal(data.frame(SIS0=NA, SIS1=NA, Pres=NA)[0,])
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
        COO(Storage_SP()$coo)
      }, error=function(e){"<b>We could not find the COO extract. Please try again.</b>"})
      if(Error_mess != "TRUE"){showNotification(ui=HTML(Error_mess), type="error", duration=8)}
      
  })
  
  
  ### Record change to make to COO
  observeEvent(input$button_occupied, {

    sRL_loginfo("START - Record change to make", input$sci_name)

    Change_dir <- ifelse(grepl("makeoccupied", input$button_occupied), TRUE, FALSE)
    Change_country <- input$button_occupied %>% sub("makeoccupied_", "", .) %>% sub("makeempty_", "", .) %>% strsplit(., "/") %>% unlist() %>% gsub("_", " ", .)
    
    Change_tomake(rbind(Change_tomake(), 
                        data.frame(SIS0=Change_country[1], 
                                   SIS1=Change_country[2], 
                                   Pres=Change_dir)
    ))
    
    print(Change_tomake())
    
    sRL_loginfo("END - Record change to make", input$sci_name)
    
  })
  
  
  ### Save points
  observeEvent(input$save, {
    sRL_loginfo("START - Save manual edit countries", input$sci_name)
    
    # Edit COO
    COO_NEW <- COO()
    
    for(R in 1:nrow(Change_tomake())){
      COO_NEW$Level0_occupied[COO_NEW$SIS_name0==Change_tomake()$SIS0[R] & COO_NEW$SIS_name1==Change_tomake()$SIS1[R]] <- Change_tomake()$Pres[R]
      COO_NEW$Level1_occupied[COO_NEW$SIS_name0==Change_tomake()$SIS0[R] & COO_NEW$SIS_name1==Change_tomake()$SIS1[R]] <- Change_tomake()$Pres[R]
      COO(COO_NEW)
    }

    
    # Add COO to Storage_SP
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
}



# Run the application 
shinyApp(ui = ui, server = server)




