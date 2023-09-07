library(tidyverse) # replace_na 
library(shiny) # fluidPage wellPanel radioButtons div HTML htmlOutput conditionalPanel hr h4 selectInput reactiveVal renderUI req includeHTML observeEvent validate need updateSelectInput renderText shinyApp 
library(leaflet) # leafletOutput renderLeaflet leaflet leafletOptions addTiles setMaxBounds setView addPolygons labelOptions highlightOptions
library(rworldmap) # joinCountryData2Map 
library(shinythemes)

## COUNTRY LISTS ----

note_map <- "Note: This map is stylized and not to scale and does not reflect 
a position by UNICEF on the legal status of any country or territory or the delimitation 
of any country or territory or the delimitation of any frontiers."
prospect_countries <- c('EGY', 'ETH', 'IRQ', 'JOR',
                        'KEN', 'LBN', 'SDN', 'UGA')
blueprint_countries <- c("BGD", "CMR", "ECU", "ETH", "HND", "IDN", 
                         "IRQ", "KEN", "LBN", "LBY", "RWA")
lostgen_countries <- c("SYR","TUR","LBN",
                       "JOR","IRQ","EGY")
aa_countries <- c("AFG","CAF","TCD","COL","ETH","IRQ",
                  "LBY","MOZ","NER","NGA","PHL","SOM",
                  "SSD","SDN","VUT","YEM")
# all_ctry <- intersect(wpp.pop |> filter(area.id < 900, year == 2019,poptotal.thsd >90) |> pull(iso3),
#                       mig.stock.orig.dest |> filter(year == 2019, dest.area.id<900) |> distinct(dest.iso3) |> pull())

#Country and region data
load('data/all_ctry.Rdata')
load('data/location.Rdata')
load('data/UNSD_M49_Aug2019.Rdata')

countries <- wpp.unicef.all |> 
  filter(iso3 %in% all_ctry & !iso3 %in% c('TWN', "SHN", "FRO", "GGY", 
                                           "IMN", "JEY", "GIB", "VAT", 
                                           "XKX", "BES", "VGB", "CYM",
                                           "MSR", "BLM", "MAF", "SXM",
                                           "TCA", "FLK", "BMU", "GRL",
                                           "SPM", "MHL", "NRU", "MNP",
                                           "PLW", "ASM", "COK", "NIU",
                                           "TKL", "TUV", "WLF"))  |> #which countries to not show
  left_join(select(m49,iso3,m49_sub = subregion)) |>
  mutate(m49_sub = replace_na(m49_sub,'Unknown'))
choices <- countries$area



# Helper function to generate conditional panels with description, title, and map

generate_conditional_panel <- function(condition, description, map_id) {
  if(condition == "input.type_of_countries == 'world'") {
    return(
      conditionalPanel(
        condition = condition,
        wellPanel(
          HTML(description),
          tags$hr(),
          h4(style = 'font-family:Arial;', 'Please select a country'),
          selectizeInput(inputId = "selected_country_world", label = "Search and choose a country:", choices = setNames(countries$iso3, countries$area), selected = 'AFG'),
          leaflet::leafletOutput(outputId = map_id),
          style = "border: 3px solid #00AEEF; opacity: 0.92;"
        )
      )
    )
  } else {
    return(
      conditionalPanel(
        condition = condition,
        wellPanel(
          HTML(description),
          tags$hr(),
          h4(style = 'font-family:Arial;', 'Please select a country'),
          leaflet::leafletOutput(outputId = map_id),
          style = "border: 3px solid #00AEEF; opacity: 0.92;"
        )
      )
    )
  }
}


## UI ----
ui <- function(req) {
  fluidPage(
    theme = shinytheme("cerulean"),
    wellPanel(
      radioButtons(
        inputId = "type_of_countries", 
        "Browse all countries or select a country group",
        choiceNames = list(
          'All countries',
          div(HTML("<em>Prospects</em> countries")),
          div(HTML("<em>Blueprint</em> countries")),
          div(HTML("<em>No Lost Generation</em>")),
          div(HTML("<em>IDP Action Agenda</em>"))
        ),
        choiceValues = c('world', 'prospects', 'blueprint', 'lostgen', 'aa'),
        selected = 'world',
        inline = TRUE
      ),
      style = "border: 3px solid #00AEEF; opacity: 0.92;"
    ),
    
    # Using the helper function to generate panels
    generate_conditional_panel(
      "input.type_of_countries == 'world'",
      "", 
      "mymap1"
    ),
    generate_conditional_panel(
      "input.type_of_countries == 'prospects'",
      "<i>The </i><b>PROSPECTS</b><i> Partnership is a programme funded by the Dutch Government focusing on improving responses to forced displacement crises for the benefit of forcibly displaced populations and host communities. The programme was launched in 2019 and it brings together the International Finance Corporation (IFC), International Labour Organization (ILO), United Nations High Commissioner for Refugees (UNHCR), United Nations Children's Fund (UNICEF), and the World Bank (WB) to find innovative solutions to this growing challenge around the world.</i>", 
      "mymap2"
    ),
    generate_conditional_panel(
      "input.type_of_countries == 'blueprint'",
      "<i>In 2020, UNHCR and UNICEF agreed on an ambitious two-year </i><b>Blueprint for Joint Action</b>. <i>The</i> Blueprint <i>represents a commitment to accelerate joint efforts in a transformational agenda, to promote and protect the rights of refugee children and the communities that host them, and to support their inclusion and access to services. In this, it will help us deliver on the pledges we made at the Global Refugee Forum in December 2019, in support of the Global Compact on Refugees.</i>", 
      "mymap3"
    ),
    generate_conditional_panel(
      "input.type_of_countries == 'lostgen'",
      "<b>No Lost Generation</b><i> brings together key partners to achieve agreed outcomes essential for the education, protection, and adolescent and youth engagement of Syrian and Iraqi refugees. The initiative is led jointly by UNICEF and World Vision. Partners include UN agencies, international and national NGOs, institutional donors, private sector companies and the startup community; governments, and individuals.</i>", 
      "mymap4"
    ),
    generate_conditional_panel(
      "input.type_of_countries == 'aa'",
      "<b>IDP Action Agenda</b><i> The United Nations Secretary-General’s Action Agenda on Internal Displacement aims to 1) help IDPs find a durable solution to their displacement; 2) better prevent new displacement crises from emerging; and 3) ensure those facing displacement receive effective protection and assistance.</i>", 
      "mymap5"
    ),
    
    wellPanel(
      htmlOutput("ctry_profile"),
      style = "border: 3px solid #00AEEF; opacity: 0.92;"
    )
  )
}




## Server ----
server <- function(input, output, session) {
  
  ctry <- reactiveVal(value = 'KEN')
  
  observeEvent(input$selected_country_world, {
    ctry(input$selected_country_world)
  }, ignoreInit = TRUE)
  
  
  
  # Helper function to render maps
  render_map <- function(selected_countries, current_country) {
    countries_selected <- selected_countries
    worldmap_spdf <- joinCountryData2Map(
      data.frame(ISO = countries_selected, is.prospect = 1),
      joinCode = "ISO3",
      nameJoinColumn = "ISO"
    )
    
    map.ctry <- worldmap_spdf |> subset( ISO3 ==ctry())
    label.map.ctry <- HTML(paste0(map.ctry$type,map.ctry$NAME))
    map.other.ctry <- worldmap_spdf |> subset(is.prospect==1 & ISO3 !=ctry())
    label.map.other.ctry <- paste0(map.other.ctry$type,map.other.ctry$NAME) |> lapply(htmltools::HTML)
    
    mymap <- leaflet::leaflet(quakes, options = leafletOptions(minZoom = 2, maxZoom = 6)) |>
      addTiles("http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") |>
      setMaxBounds(-180,-55,180,75) |>
      setView(map.ctry$LON,map.ctry$LAT,zoom = 2.5) |>
      addPolygons(data = map.ctry ,
                  fillColor = '#0083CF',
                  weight = 1,
                  opacity = 0.7,
                  color = "grey",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  label = ~NAME,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"),
                  layerId = ~ISO3) |>
      addPolygons(data = map.other.ctry,
                  fillColor = '#69DBFF',
                  weight = 1,
                  opacity = 0.7,
                  color = "grey",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 5,color = '#0083CF',dashArray = "",fillOpacity = 0.5,bringToFront = TRUE),
                  label = ~NAME,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"),
                  layerId = ~ISO3)
    
    return(mymap)
  }
  
  output$ctry_profile <- renderUI({
    req(ctry())  # Ensure 'ctry()' has a value before proceeding
    
    filename <- paste0('www/profile_v6_', ctry(), '.html')
    
    # Check if the file exists and include it. If it doesn't exist, display a message.
    if (file.exists(filename)) {
      includeHTML(filename)
    } else {
      h4('Currently not rendered.')
    }
  })
  
  output$mymap1 <- leaflet::renderLeaflet({
    render_map(all_ctry, ctry())
  })
  
  output$mymap2 <- leaflet::renderLeaflet({
    render_map(prospect_countries, ctry())
  })
  
  output$mymap3 <- leaflet::renderLeaflet({
    render_map(blueprint_countries, ctry())
  })
  
  output$mymap4 <- leaflet::renderLeaflet({
    render_map(lostgen_countries, ctry())
  })
  
  output$mymap5 <- leaflet::renderLeaflet({
    render_map(aa_countries, ctry())
  })
  
  observe_map_click <- function(map_id) {
    observeEvent(input[[paste0(map_id, "_shape_click")]], {
      clicked_country <- input[[paste0(map_id, "_shape_click")]]$id
      ctry(clicked_country)
      
    }, ignoreInit = T)
  }
  
  observe_map_click("mymap1")
  observe_map_click("mymap2")
  observe_map_click("mymap3")
  observe_map_click("mymap4")
  observe_map_click("mymap5")
  
  
  
  #changes when changing type of countries
  observeEvent(input$type_of_countries,{
    if(input$type_of_countries == 'prospects'){
      ctry('KEN')
    } 
    if(input$type_of_countries == 'blueprint'){
      ctry('BGD') 
    } 
    if(input$type_of_countries == 'lostgen'){
      ctry('TUR') 
    }
    if(input$type_of_countries == 'aa'){
      ctry('COL') 
    }
    if(input$type_of_countries == 'world'){
      ctry('AFG')
    } 
  })
}


shinyApp(ui,server)
