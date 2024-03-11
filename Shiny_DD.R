# Set working directory (Victor path if we are on his laptop, LifeWatch path otherwise)
setwd(dir=ifelse(file.exists("C:/Users/Victor"),"C:/Users/Victor/Documents/sRedList/Platform/InProgress/sredlist-server-develop", "/media/docker/sRedList/sredlist-server"))


### Charge libraries
library(ggplot2)
library(shiny)
library(dplyr)
library(plyr)
library(DT)
library(plotly)
library(sf) ; library(leaflet) ; library(leafem) ; library(rgbif)
library(shinycssloaders) ; library(shinylogs)
library(raster)


### Specify url
options(shiny.host = '127.0.0.1')
options(shiny.port = 6780)


### Load config
config <- config::get(file="../config.yml")

### Source functions
source("sRLfun_ShinyDD.R")

### Load files
speciesRL<-readRDS("Species/species-all-page.rds")

DD<-readRDS("resources/resources_Shiny_DD/DD_prepared_for_ShinyREALMS.rds")
DD$PrioDS_raw<-DD$PrioDS
DD$PrioDS<-DD$Prio_rank
DD$Last_assessment <- as.integer(2022-(DD$time_desc-DD$time_descASS))
DD$Red_List_Authority<-DD$RLA

# Remove species not DD anymore
DD$CurrentCat<-speciesRL$category[match(DD$taxonid, speciesRL$taxonid)]
DD<-subset(DD, ! CurrentCat %in% c("CR", "EN", "VU", "NT", "LC"))
DD$CurrentCat<-NULL

### Prepare WOS
WOS_extract <- readRDS("resources/resources_Shiny_DD/WOS_extract_moreinfo.rds") %>% subset(., is.na(.$Title)==F) %>% DDfun_RefWos(.)


### Prepare Habitat
DD$Hab_file<-NA
For_LIST<-paste(list.files("resources/resources_Shiny_DD/1.GFC_final"), collapse=",")
for(i in 1:nrow(DD)){
  DD$Hab_file[i]<-ifelse(grepl(sub(" ", "_", DD$scientific_name[i]), For_LIST), "Forest", 
                         ifelse(paste0("AOHtrends_CCI_", sub(" ", "_", DD$scientific_name[i]), ".tif") %in% c(list.files("resources/resources_Shiny_DD/1.CCI_final")), "CCI", 
                                "None"))
}








### User interface

ui <-fluidPage(
  
  # Define style for WOS box (especially to have the scrolldown)
  tags$head(
    tags$style(
      HTML(
        'div.scroll{
        width: 100%;
        height: 220px;
        overflow-x: hidden;
        overflow-y: auto;
        padding: 20px;
      }
      '
      )
    )
  ),
  
  fluidRow(
    column(6,
           fluidRow(
             column(6,
                    selectInput("TaxoGroup", "Group:", choices=c("Select your group", levels(as.factor(DD$Group))))),
             column(6,
                    div(tags$label(), style = "margin-bottom: 5px"),
                    conditionalPanel(condition = "input.list_button",
                                     downloadButton("downloadList", "Download your priority list", style="color: #fff; background-color: #8e0152; border-color: #8e0152")
                    ))
           ),
           fluidRow(
             withSpinner(
              plotlyOutput("plot1", width = "100%", height = "100%"), 
              type=3, 
              color.background = "#f5f5f5ff"
            )
           )
    ),
    column(6,
           wellPanel(
             htmlOutput('myTitle'),
             htmlOutput('myIntroText'),
             htmlOutput('myWOSText'),
             withSpinner(htmlOutput('myWOSList', class="scroll"), type=3, color.background = "#f5f5f5ff"),
             htmlOutput('myGBIFTitle'),
             leafletOutput('mymap', height=300, width="75%")
           )
    )
  ),
  
  # Create a new row for the table.
  DT::DTOutput("table")
)





server <- function(input, output) {
  
  mySpecies <- reactiveValues(SP = '')
  myList <- reactiveVal(DDfun_AddList("Init"))
  DD_Table <- reactiveVal(data.frame())
  
  ### Track usage
  track_usage(
    storage_mode = store_rds(path = paste0(getwd(), "/resources/resources_Shiny_DD/Logs")),
    what=c("session", "input"),
    app_name="sRL_DDapp"
  )
  
  
  
  
  ### EVENTS ###
  
  ### Observe Event when the group is selected
  observeEvent(input$TaxoGroup, {
    
    req(input$TaxoGroup != "Select your group")
    
    ## Create table
    DD_Table(DDfun_Table(GR=input$TaxoGroup))
    
  })
  
  
  ### Action when we click on More information
  observeEvent(input$select_button, {
    SP_name <- strsplit(input$select_button, "_")[[1]][c(2,3)] %>% paste(., collapse=" ")
    selectedRow <- which(DD_Table()$SP==SP_name)[1]
    
    if(DD_Table()$Group[1] == input$TaxoGroup){
      mySpecies$Title <<- paste0('<h3>More information on <i>', SP_name, '</i> (last assessed in ', DD_Table()$Last_assessment[selectedRow], ')</h3>')
      mySpecies$IntroText <- DD$DD_Prio_text[match(SP_name, DD$scientific_name)] %>% paste0("<b><p style='color:#8E2921';>", ., "</p></b><br><br>")
      mySpecies$WOSText <- paste0(DD$WOS[match(SP_name, DD$scientific_name)], " articles in Web of Science:") %>% paste0("<p style='color:#8E2921';>", ., "</p>")
      mySpecies$WOSList <- DDfun_GetWOS(SP=SP_name)# %>% paste0("<div class='shadowbox'>", ., "</div>")
      mySpecies$GBIFTitle<-"<br><p style='color:#8E2921';>GBIF occurrence records:</p>"
      mySpecies$GBIFmap <- DDfun_GBIFLeaf(SP_name)
    }
  })
  
  
  ### Add species to the list
  observeEvent(input$list_button, {
    myList(rbind(myList(), DDfun_AddList(input$list_button, nrow(myList()))))
    print(myList())
  })
  
  
  
  
  ### OUTPUTS ###
  
  ### Table
  output$table <- renderDataTable({
    req(input$TaxoGroup != "Select your group")
    DD_TableSelect <- DDfun_SelectTable(DD_Table()) %>% .[, ! names(.) %in% c("SP", "Group")]
    DD_tooltips <- DDfun_Tooltips(names(DD_Table())) %>% .[! . %in% c("SP", "Group")]
    
    datatable(
      DD_TableSelect,
      escape = FALSE,
      filter="top",
      options = list(pageLength = 50,
                     columnDefs=list(list(targets=0, searchable = FALSE), list(targets=(which(names(DD_TableSelect)=="More")-1), searchable = FALSE), list(targets=2, regex = TRUE)), # Prevent column "List" and "More" from being searchable to filter
                     dom = 't' # dom='t' is to remove the search bar
      ),
      callback = JS(paste0("var tips = ['",paste0(DD_tooltips ,collapse = "','"),"'], header = table.columns().header(); for (var i = 0; i < tips.length; i++) {$(header[i]).attr('title', tips[i]);}")),
      rownames=FALSE)
  })
  
  ### Plot
  output$plot1 <- renderPlotly({
    req(input$TaxoGroup != "Select your group")
    Tab_subset <-DDfun_SelectTable(DD_Table())
    Sel_SP = Tab_subset$SP[input$table_rows_selected]
    
    DDfun_PlotIsoc(GR=input$TaxoGroup, Tab_subset, Sel_SP) %>% 
      plotly::layout(dragmode = "select") %>% 
      plotly::event_register("plotly_selecting")
  })
  
  ### Download priority list
  output$downloadList <- downloadHandler(
    filename = function() {paste0("My_DD_Priority_List_", Sys.Date(), ".csv")},
    content = function(file) {write.csv(myList(), file, row.names = FALSE)}
  )
  
  
  ### Outputs for the extra information
  output$myTitle <- renderText({mySpecies$Title})
  output$myIntroText <- renderText({mySpecies$IntroText})
  output$myWOSText <- renderText({mySpecies$WOSText})
  output$myWOSList <- renderText({mySpecies$WOSList})
  output$myGBIFTitle <- renderText({mySpecies$GBIFTitle})
  output$mymap <- renderLeaflet({mySpecies$GBIFmap})
}



shinyApp(ui, server)

