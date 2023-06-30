library(shiny) # fluidPage wellPanel radioButtons div HTML htmlOutput conditionalPanel hr h4 selectInput reactiveVal renderUI req includeHTML observeEvent validate need updateSelectInput renderText shinyApp 
library(leaflet) # leafletOutput renderLeaflet leaflet leafletOptions addTiles setMaxBounds setView addPolygons labelOptions highlightOptions
library(dplyr) # filter left_join select mutate pull 
library(tidyr) # replace_na 
library(rworldmap) # joinCountryData2Map 

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

countries <- wpp.unicef.all |> filter(iso3 %in% all_ctry & !iso3 %in% c('TWN'))  |>
  left_join(select(m49,iso3,m49_sub = subregion)) |>
  mutate(m49_sub = replace_na(m49_sub,'Unknown'))
choices <- countries$area

## UI ----

ui <- fluidPage(
  wellPanel(radioButtons(inputId="type_of_countries", "Browse all countries or select a country group",
                         choiceNames = list('All countries',
                                            div(HTML("<em>Prospects</em> countries")),  
                                            div(HTML("<em>Blueprint</em> countries")),
                                            div(HTML("<em>No Lost Generation</em>")),
                                            div(HTML("<em>IDP Action Agenda</em>"))),
                         choiceValues = c('world','prospects','blueprint','lostgen','aa'),
                         selected = 'prospects',
                         inline = T),
            htmlOutput(outputId = 'country_help_text'),
            tags$head(tags$style("#country_help_text{color: grey; font-size: 15px;}")),
            style = "border: 3px solid #00AEEF; opacity: 0.92;"),
  conditionalPanel(condition = "input.type_of_countries == 'world'",
                   wellPanel(tags$head(tags$style('.selectize-dropdown {z-index: 100000000 !important;}')),
                             tags$hr(),
                             h4(style = 'font-family:Arial;','Please select a country'),
                             selectInput(inputId = "world_country", label = NULL,
                                         choices = choices,
                                         selected = choices[1],
                                         multiple = F,
                                         width = '400px'),
                             leaflet::leafletOutput(outputId =  "mymap4"),
                             style = "border: 3px solid #00AEEF; opacity: 0.92;")),
  conditionalPanel(condition = "input.type_of_countries == 'prospects'",
                   wellPanel(tags$hr(),
                             h4(style = 'font-family:Arial;','Please select a country'),
                             leaflet::leafletOutput(outputId =  "mymap1"),
                             style = "border: 3px solid #00AEEF; opacity: 0.92;")),
  conditionalPanel(condition = "input.type_of_countries == 'blueprint'",
                   wellPanel(tags$hr(),
                             h4(style = 'font-family:Arial;','Please select a country'),
                             leaflet::leafletOutput(outputId =  "mymap2"),
                             style = "border: 3px solid #00AEEF; opacity: 0.92;")),
  conditionalPanel(condition = "input.type_of_countries == 'lostgen'",
                   wellPanel(tags$hr(),
                             h4(style = 'font-family:Arial;','Please select a country'),
                             leaflet::leafletOutput(outputId =  "mymap3"),
                             style = "border: 3px solid #00AEEF; opacity: 0.92;")),
  conditionalPanel(condition = "input.type_of_countries == 'aa'",
                   wellPanel(tags$hr(),
                             h4(style = 'font-family:Arial;','Please select a country'),
                             leaflet::leafletOutput(outputId =  "mymap5"),
                             style = "border: 3px solid #00AEEF; opacity: 0.92;")),
  
  wellPanel(htmlOutput("ctry_profile"),
            style = "border: 3px solid #00AEEF; opacity: 0.92;")
)

## SERVER ----
server <-  function(input, output,session) {
  
  ctry <- reactiveVal('AFG')
  
  output$ctry_profile <- renderUI({
    req(ctry())
    includeHTML('www/profile_v6_AFG.html')
  })
  
  output$mymap1 <- leaflet::renderLeaflet({
    req(ctry())
    countries_selected <- prospect_countries
    worldmap_spdf <-   joinCountryData2Map(data.frame(ISO = countries_selected, is.prospect = 1) |>
                                             mutate(type = '<em>Prospects</em> Country: '),
                                           joinCode = "ISO3",
                                           nameJoinColumn = "ISO")
    
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
  })
  
  output$mymap2 <- leaflet::renderLeaflet({
    req(ctry())
    countries_selected <- blueprint_countries
    worldmap_spdf <-   joinCountryData2Map(data.frame(ISO = countries_selected, is.prospect = 1) |>
                                             mutate(type = '<em>Blueprint</em> Country: '),
                                           joinCode = "ISO3",
                                           nameJoinColumn = "ISO")
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
  })
  
  output$mymap3 <- leaflet::renderLeaflet({
    req(ctry())
    countries_selected <- lostgen_countries
    worldmap_spdf <-   joinCountryData2Map(data.frame(ISO = countries_selected, is.prospect = 1) |> 
                                              mutate(type = '<em>No Lost Generation</em> Country: ')
                                            , joinCode = "ISO3"
                                            , nameJoinColumn = "ISO")
    
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
                  highlight = highlightOptions(weight = 5, color = '#0083CF', dashArray = "", fillOpacity = 0.5, bringToFront = TRUE),
                  label = ~NAME,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"),
                  layerId = ~ISO3)
    return(mymap)
  })
  
  output$mymap5 <- leaflet::renderLeaflet({
    req(ctry())
    countries_selected <- aa_countries
    worldmap_spdf <-   joinCountryData2Map( data.frame(ISO = countries_selected, is.prospect = 1) |>
                                              mutate(type = '<em>IDP Action Agenda</em> Country: ')
                                            , joinCode = "ISO3"
                                            , nameJoinColumn = "ISO")
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
  })
  
  output$mymap4 <- leaflet::renderLeaflet({
    req(ctry())
    countries_selected <- countries |> pull(iso3)
    worldmap_spdf <-   joinCountryData2Map( data.frame(ISO = countries_selected, is.prospect = 1) |> mutate(type = 'Country: ')
                                            , joinCode = "ISO3"
                                            , nameJoinColumn = "ISO")
    
    if(!ctry() %in% c("MYT", "REU", "GLP", "MTQ", "CHI")){
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
                    highlight = highlightOptions(weight = 5,
                                                 color = '#0083CF',
                                                 dashArray = "",
                                                 fillOpacity = 0.5,
                                                 bringToFront = TRUE),
                    label = ~NAME,
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                textsize = "15px",
                                                direction = "auto"),
                    layerId = ~ISO3)
    } else {
      map.other.ctry <- worldmap_spdf |> subset(is.prospect==1 )
      label.map.other.ctry <- paste0(map.other.ctry$type,map.other.ctry$NAME) |> lapply(htmltools::HTML)
      
      mymap <- leaflet::leaflet(quakes, options = leafletOptions(minZoom = 2, maxZoom = 6)) |>
        addTiles("http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") |>
        setMaxBounds(-180,-55,180,75) |>
        addPolygons(data = map.other.ctry,
                    fillColor = '#69DBFF',
                    weight = 1,
                    opacity = 0.7,
                    color = "grey",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(weight = 5,
                                                 color = '#0083CF',
                                                 dashArray = "",
                                                 fillOpacity = 0.5,
                                                 bringToFront = TRUE),
                    label = ~NAME,
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                textsize = "15px",
                                                direction = "auto"),
                    layerId = ~ISO3)
    }
    return(mymap)
  })
  
  observeEvent(input$mymap1_shape_click$id, {
    ctry(input$mymap1_shape_click$id)
    output$ctry_profile <- renderUI(NULL)
    output$ctry_profile <- renderUI({
      shiny::validate(need(ctry(),'Please select country.'))
      includeHTML(paste0('www/profile_v6_',input$mymap1_shape_click$id, '.html'))
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$mymap2_shape_click$id, {
    ctry(input$mymap2_shape_click$id)
    output$ctry_profile <- renderUI(NULL)
    output$ctry_profile <- renderUI({
      shiny::validate(need(ctry(),'Please select country.'))
      includeHTML(paste0('www/profile_v6_',input$mymap2_shape_click$id, '.html'))
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$mymap3_shape_click$id, {
    ctry(input$mymap3_shape_click$id)
    output$ctry_profile <- renderUI(NULL)
    output$ctry_profile <- renderUI({
      shiny::validate(need(ctry(),'Please select country.'))
      filename <- paste0('www/profile_v6_',input$mymap3_shape_click$id, '.html')
      
      if(file.exists(filename)){
        includeHTML(filename)
      }else{
        h4('Currently not rendered.')
      }
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$mymap4_shape_click$id, {
    ctry(input$mymap4_shape_click$id)
    updateSelectInput(session,'world_country',selected = countries |> 
                        filter(iso3 == input$mymap4_shape_click$id) |> 
                        select(area) |> pull())
    output$ctry_profile <- renderUI(NULL)
    output$ctry_profile <- renderUI({
      shiny::validate(need(ctry(),'Please select country.'))
      filename <- paste0('www/profile_v6_',input$mymap4_shape_click$id, '.html')
      
      if(file.exists(filename)){
        includeHTML(filename)
      }else{
        h4('Currently not rendered.')
      }
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$mymap5_shape_click$id, {
    ctry(input$mymap5_shape_click$id)
    updateSelectInput(session,'world_country',selected = countries |> 
                        filter(iso3 == input$mymap5_shape_click$id) |> 
                        select(area) |> pull())
    output$ctry_profile <- renderUI(NULL)
    output$ctry_profile <- renderUI({
      shiny::validate(need(ctry(),'Please select country.'))
      filename <- paste0('www/profile_v6_',input$mymap5_shape_click$id, '.html')
      
      if(file.exists(filename)){
        includeHTML(filename)
      }else{
        h4('Currently not rendered.')
      }
    })
  }, ignoreInit = TRUE)

  observeEvent(input$world_country,{
    need(input$world_country, 'Please select a country!')
    iso3 <- wpp.unicef.all |> filter(area == input$world_country) |> pull(iso3)
    ctry(iso3)
    output$ctry_profile <- renderUI(NULL)
    output$ctry_profile <- renderUI({
      
      filename <- paste0('www/profile_v6_',input$world_country$id, '.html')
      
      if(file.exists(filename)){
        includeHTML(filename)
      }else{
        h4('Currently not rendered.')
      }
    })
  })
  
  observeEvent(input$type_of_countries,{
    if(input$type_of_countries == 'prospects'){
      ctry('KEN')
      output$ctry_profile <- renderUI({
        req(ctry())
        includeHTML('www/profile_v6_KEN.html')
      })
    } 
    if(input$type_of_countries == 'blueprint'){
      ctry('BGD') 
      output$ctry_profile <- renderUI({
        req(ctry())
        includeHTML('www/profile_v6_BGD.html')
      })
    } 
    if(input$type_of_countries == 'lostgen'){
      ctry('TUR') 
      output$ctry_profile <- renderUI({
        req(ctry())
        includeHTML('www/profile_v6_TUR.html')
      })
    }
    if(input$type_of_countries == 'aa'){
      ctry('COL') 
      output$ctry_profile <- renderUI({
        req(ctry())
        includeHTML('www/profile_v6_COL.html')
      })
    }
    if(input$type_of_countries == 'world'){
      ctry('AFG')
      updateSelectInput(session = session, inputId = "world_country", label = NULL,
                        choices = choices,
                        selected = choices[1])
      output$ctry_profile <- renderUI({
        req(ctry())
        includeHTML('www/profile_v6_AFG.html')
      })
    } 
  })
  
  output$country_help_text <- renderText({
    if(input$type_of_countries == 'world'){
      return(NULL)
    }
    if(input$type_of_countries == 'prospects'){
      return(HTML("<i>The </i><b>PROSPECTS</b><i> Partnership is a programme funded by the Dutch Government focusing on improving responses to forced displacement crises for the benefit of forcibly displaced populations and host communities. The programme was launched in 2019 and it brings together the International Finance Corporation (IFC), International Labour Organization (ILO), United Nations High Commissioner for Refugees (UNHCR), United Nations Children's Fund (UNICEF), and the World Bank (WB) to find innovative solutions to this growing challenge around the world.</i>"))
    }
    if(input$type_of_countries == 'blueprint'){
      return(HTML("<i>In 2020, UNHCR and UNICEF agreed on an ambitious two-year </i><b>Blueprint for Joint Action</b>. <i>The</i> Blueprint <i>represents a commitment to accelerate joint efforts in a transformational agenda, to promote and protect the rights of refugee children and the communities that host them, and to support their inclusion and access to services. In this, it will help us deliver on the pledges we made at the Global Refugee Forum in December 2019, in support of the Global Compact on Refugees.</i>"))
    }
    if(input$type_of_countries == 'lostgen'){
      return(HTML("<b>No Lost Generation</b><i> brings together key partners to achieve agreed outcomes essential for the education, protection, and adolescent and youth engagement of Syrian and Iraqi refugees. The initiative is led jointly by UNICEF and World Vision. Partners include UN agencies, international and national NGOs, institutional donors, private sector companies and the startup community; governments, and individuals.</i>"))
    }
    if(input$type_of_countries == 'aa'){
      return(HTML("<b>IDP Action Agenda</b><i> The United Nations Secretary-General’s Action Agenda on Internal Displacement aims to 1) help IDPs find a durable solution to their displacement; 2) better prevent new displacement crises from emerging; and 3) ensure those facing displacement receive effective protection and assistance.</i>"))
    }
  })
}

shinyApp(ui,server)
