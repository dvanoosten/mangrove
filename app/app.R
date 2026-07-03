library(shiny)
library(bslib)
library(shinyjs)
library(reticulate)
library(DT)
library(leaflet)
library(tidyverse)

source("helpers.R")

use_python("~/miniforge3/envs/name-matching/bin/python", required = T) # CHECK
source_python("search_functions.py")

ui <- navbarPage(title = div(img(src = "mangrove_logo.png", height = "100px", width = "auto",
                                 style = "background-color: transparent; position: relative; top: -40px;left: 5px;")),
                 windowTitle = "Mangrove",
                 tags$style(HTML(".navbar {padding-top:25px !important; height: 100px; font-size: 20px}")),               
  tabPanel(
    # opening tab with general information and data selection
    title = "General",
    selectInput(
      inputId = "batchID",
      label = "Version",
      choices = c("royals")),
    strong("Ancestor table structure"),
    a("See also Wikipedia", href="https://en.wikipedia.org/wiki/Ahnentafel#:~:text=Seize%20Quartiers.-,Inductive%20reckoning,-%5Bedit%5D"),
    uiOutput(outputId = "example_ped")
  ),
  tabPanel(
    # individual search tab, sidebar for query input, masterlist table and map as output
    title = "Search individuals",
    sidebarPanel(
      useShinyjs(),
      id = "side_panel",
      actionButton(
        inputId = "reset",
        label = "Reset filters"
      ),
      selectInput(
        inputId = "ogID_list",
        label = "original ID",
        choices = NULL,
        multiple = TRUE
      ),
      textInput(
        inputId = "name_query",
        label = "Last name"
      ),
      checkboxInput(
        inputId = "contains",
        label = "Name contains"
      ),
      sliderInput(
        inputId = "name_DL", 
        label = "Number of typos allowed in name",
        min = 0, max = 5, value = 1, ticks = FALSE
      ),
      textInput(
        inputId = "city_query",
        label = "Place of birth"
      ),
      sliderInput(
        inputId = "dist",
        label = "Distance to place of birth (km)",
        min = 0, max = 20, value = 10, ticks = FALSE
      ),
      dateInput(
        inputId = "dob_query",
        label = "Date of birth (yyyy-mm-dd)",
        value = "1800-01-01", weekstart = 1
      ),
      checkboxInput(
        inputId = "dob_day",
        label = "Do not use day in date of birth"
      ),
      sliderInput(
        inputId = "dob_DL", 
        label = "Number of typos allowed in date of birth",
        min = 0, max = 2, value = 1, ticks = FALSE
      ),
      dateInput(
        inputId = "dod_query",
        label = "Date of death (yyyy-mm-dd)",
        value = "1800-01-01", weekstart = 1
      ),
      checkboxInput(
        inputId = "dod_day",
        label = "Do not use day in date of death"
      ),
      sliderInput(
        inputId = "dod_DL", 
        label = "Number of typos allowed in date of death",
        min = 0, max = 2, value = 1, ticks = FALSE
      ),
    ),
    mainPanel(
      card(dataTableOutput(outputId = "masterTable"), style="font-size: 75%", height="fit-content"),
      card(leafletOutput("masterlistMap"), height="50%")
    )
  ),
  
  tabPanel(
    # superpedigree browser tab, sidebar with search input, pedigree plot and table as output
    title = "Superpedigree browser",
    sidebarPanel(
      radioButtons(
        inputId = "searchtype",
        label = "Search by",
        choices = c("Superpedigree ID (Mangrove)", "Pedigree ID (original)", "Mangrove ID", "Original ID")
      ),
      conditionalPanel("input.searchtype == 'Superpedigree ID (Mangrove)'",
        selectInput(
          inputId = "SupPEDID",
          label = "Superpedigree ID (Mangrove)",
          choices = NULL
      )),
      conditionalPanel("input.searchtype == 'Pedigree ID (original)'",
        selectInput(
          inputId = "PEDID",
          label = "Pedigree ID (original)",
          choices = NULL
      )),
      conditionalPanel("input.searchtype == 'Mangrove ID'",
        selectInput(
          inputId = "MgvID",
          label = "Mangrove ID",
          choices = NULL
      )),
      conditionalPanel("input.searchtype == 'Original ID'",
        selectInput(
          inputId = "ogID",
          label = "Original ID",
          choices = NULL
      )),
      checkboxInput(
        inputId = "trim_ped",
        label = "Trim pedigree"
      ),
    ),
    mainPanel(
      card(plotOutput(outputId = "pedPlot"), height="100%"),
      card(card_header("Individuals in pedigree", style="font-size: 150%; font-weight: bold"),
           dataTableOutput(outputId = "indTable"), height="50%", style="font-size: 75%")
    )
  )
)
 

server <- function(input, output, session) {
  # read in data and update input controls
  output$example_ped <- renderUI({img(src="kwartierstaat_plot.png", style="width:50%")})

  proband_IDs <- reactiveVal()
  ped_data <- reactiveVal()
  masterlist <- reactiveVal()
  
  observe({
    proband_IDs(read.csv(paste0("../",input$batchID,"/proband_IDs_",input$batchID,".csv"), colClasses="character") %>%
                mutate(label = paste(ogID, PEDID, sep="\n"))) # CHECK
    ped_data(read.csv(paste0("../",input$batchID,"/ped_",input$batchID,".csv")) %>%
               mutate(Sex = case_match(Sex, "M"~1, "F"~2))) # CHECK
    masterlist(read.csv(paste0("../",input$batchID,"/masterlist_",input$batchID,".csv")) %>% 
                 mutate(across(c("Date_of_birth", "Date_of_death"), ~ substr(.x, 2, nchar(.x))))) # CHECK
  
    updateSelectizeInput(session, "ogID_list",
                         choices = sort(unique(proband_IDs()$ogID)), server = TRUE
    )
    
    updateSelectizeInput(session, "SupPEDID",
                         choices = sort(unique(proband_IDs()$SupPEDID))[-1], server = TRUE
    )
    updateSelectizeInput(session, "PEDID",
                         choices = sort(unique(proband_IDs()$PEDID))[-1], server = TRUE
    )
    updateSelectizeInput(session, "MgvID",
                         choices = sort(unique(ped_data()$ID)), server = TRUE
    )
    updateSelectizeInput(session, "ogID",
                         choices = sort(unique(proband_IDs()$ogID)), server = TRUE
    )
    
  })

  observeEvent(input$reset, {tagList(
    reset("side_panel"), 
    updateActionButton(session, "contains", label = "Name contains")
  )})
  
  observe({
    req(input$name_query)
    updateActionButton(session, "contains",
                       label = paste("Name contains", dQuote(input$name_query, FALSE)))
  })
  
  # filter masterlist by queries for individual search, render output table and map
  masterlist_filt <- reactiveVal()
  observe({
    filt_ids <- c("empty")
    if (length(input$ogID_list) == 1) {
      filt_ids <- append(filt_ids[-1], search_ogID_IDs(input$ogID_list, masterlist(), multiple=FALSE))
    }
    else if (length(input$ogID_list) > 1) {
      filt_ids <- append(filt_ids[-1], search_ogID_IDs(input$ogID_list, masterlist(), multiple=TRUE))
    }
    if (input$name_query != "") {
      filt_ids <- append(filt_ids[-1], search_name_IDs(input$name_query, masterlist(), input$name_DL, input$contains))
    }
    if (input$city_query != "") {
      filt_ids <- append(filt_ids[-1], search_city_IDs(tolower(input$city_query), masterlist(), input$dist))
    }
    if (input$dob_query != as.Date("1800-01-01")) {
      dob_query <- as.character(input$dob_query)
      filt_ids <- append(filt_ids[-1], search_dob_IDs(dob_query, masterlist(), input$dob_DL, input$dob_day))
    }
    if (input$dod_query != as.Date("1800-01-01")) {
      dod_query <- as.character(input$dod_query)
      filt_ids <- append(filt_ids[-1], search_dod_IDs(dod_query, masterlist(), input$dod_DL, input$dod_day))
    }
    if (!is.na(filt_ids[1]) & filt_ids[1] == c("empty")) {
      masterlist_filt(masterlist())
    }
    else {
      masterlist_filt(filter(masterlist(), MgvID %in% filt_ids))
    }
  })
  
  output$masterTable <- renderDataTable(
    DT::datatable({
      data = masterlist_filt() %>%
        select(-c(Place_of_birth_code, Initials)) %>%
        rename("Mangrove ID"="MgvID", "All AncIDs"="All_IDs")
    },
    options = list(pageLength=15, searching=FALSE), rownames=FALSE
    )
  )
  
  output$masterlistMap <- renderLeaflet({
    if (nrow(masterlist_filt()) < nrow(masterlist()) & nrow(masterlist_filt() > 0)) {
      leaflet(filter(masterlist_filt(), Place_of_birth_code != " ") %>%
                separate_wider_delim(Place_of_birth_code, ",", names=c("lat","long")) %>%
                mutate_at(c("lat", "long"), as.numeric)) %>%
        addTiles() %>%
        addAwesomeMarkers(~long, ~lat, popup=~paste(MgvID, Place_of_birth, sep=", "), clusterOptions = markerClusterOptions())
    }
    else {
      leaflet() %>%
        addTiles()
    }
  })
  
  # get ID list and pedigree object from superpedigree query, render output plot and table
  searchtype <- reactive(input$searchtype)
  id_list <- reactive({
    if (searchtype() == "Superpedigree ID (Mangrove)") 
      get_MgvIDs("SupPEDID", input$SupPEDID, masterlist(), ped_data(), proband_IDs())
    else if (searchtype() == "Pedigree ID (original)") 
      get_MgvIDs("PEDID", input$PEDID, masterlist(), ped_data(), proband_IDs())
    else if (searchtype() == "Mangrove ID") 
      get_MgvIDs("MgvID", input$MgvID, masterlist(), ped_data(), proband_IDs())
    else if (searchtype() == "Original ID") 
      get_MgvIDs("ogID", input$ogID, masterlist(), ped_data(), proband_IDs())
  })
  
  ped_output <- reactive({
    get_ped(id_list()$MgvIDs, input$trim_ped, ped_data(), proband_IDs(), masterlist())
  })
  
  output$pedPlot <- renderPlot({
    plot_ped(ped_output()$ped_obj, id_list()$title)
  })
  
  output$indTable <- renderDataTable({
    DT::datatable(data = ped_output()$ind_table %>%
                    rename("Mangrove ID"="MgvID", "All AncIDs"="All_IDs") %>%
                    rename_with( ~gsub("_", " ", .x, fixed=TRUE)),
                  options = list(pageLength = 15),
                  rownames = FALSE)
  })
}

shinyApp(ui,server)
