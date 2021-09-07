library(shinyalert)
library(shiny)
library(leaflet)
library(reactable)
library(dplyr)

load("database.rda")

ui <-  bootstrapPage(
    useShinyalert(),
    tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
    ),
    tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }
              
        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
              '),
    leafletOutput("map"),
    tags$head(tags$script(src = "message-handler.js")),
    sliderInput("range", "Doplacilo", min(Database$doplacilo), max(Database$doplacilo),
                value = range(Database$doplacilo), step = 0.01),
    textInput("Filter_menu","Meni"),
    actionButton("do", "Click Me"),
    actionButton("locate", "Locate Me"),
    reactableOutput("table")

    
)

server <- function(input, output, session) {
    conditional <- function(condition, success) {
        if (condition) success else TRUE
    }
    load("database.rda")
    
    
    observeEvent(input$do, {
        ran<-floor(runif(1, min=1, max=length(filteredData()$Lokal)+1))
        ran2<-floor(runif(1, min=1, max=length(unlist(filteredData()$menu[[ran]]))))
        session$sendCustomMessage(type = 'testmessage',
                                  message =     shinyalert(
                                      title =   filteredData()$Lokal[ran],
                                      text =     unlist(filteredData()$menu[[ran]])[ran2],
                                      size = "xs", 
                                      closeOnEsc = TRUE,
                                      closeOnClickOutside = TRUE,
                                      html = FALSE,
                                      type = "success",
                                      showConfirmButton = TRUE,
                                      showCancelButton = FALSE,
                                      confirmButtonText = "OK",
                                      confirmButtonCol = "#AEDEF4",
                                      timer = 0,
                                      imageUrl = "",
                                      animation = TRUE
                                  )  )
    })
    # Reactive expression for the data subsetted to what the user selected
    #filteredData <- reactive({
    #    Database[Database$doplacilo >= input$range[1] & Database$doplacilo <= input$range[2],]
    #})
    zipsInBounds <- reactive({
        if (is.null(input$map_bounds))
            return(Database[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(Database,
               lat >= latRng[1] & lat <= latRng[2] &
                   lon >= lngRng[1] & lon <= lngRng[2])
    })
    
    
    
    filteredData <- reactive({
        zipsInBounds() %>%
            filter(
                conditional(input$Filter_menu != "", grepl(input$Filter_menu, menu, ignore.case = TRUE)),
                doplacilo >= input$range[1] & doplacilo <= input$range[2])
                
            
    })
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet(Database) %>% addTiles() %>%
            fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) 
    })
    
    observe({
        if(!is.null(input$lat)){
            map <- leafletProxy("map")
            dist <- 0.01
            lat <- input$lat
            lng <- input$long
            map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
        }
    })
    
    
    observe({
        
        
        if(nrow(filteredData()) == 0){
            leafletProxy("map", data = filteredData()) %>% 
                clearMarkers()
        }else{
            leafletProxy("map", data = filteredData()) %>%
                clearMarkers()  %>%
                
                clearMarkerClusters()  %>%
                addMarkers(~lon, ~lat, 
                           label = ~as.character(Lokal),
                           #labelOptions = labelOptions(noHide = F, direction = 'auto'),
                           #options = markerOptions(riseOnHover = TRUE,zIndexOffset=100),
                          clusterOptions = markerClusterOptions(),
                           popup =  paste0(
                               "<div>",
                               "<h3>",
                               filteredData()$Lokal,
                               "</h3>",
                               "Cena: ",
                               filteredData()$cena,
                               "<br>",
                               "Doplačilo: ",
                               filteredData()$doplacilo,
                               "<br>",
                               "Doplačilo: ",
                               filteredData()$naslov,
                               "<br>",
                               "Doplačilo: ",
                               filteredData()$city,
                               "<br>",
                               "Link: ",
                               paste0('<a href = "https://www.studentska-prehrana.si', filteredData()$link[2], '">Link</a>'),
                               "</div>")
                ) 
        }
        #addCircles(radius = ~10^cena/10, weight = 1, color = "#777777",
        # fillColor = ~pal(cena), fillOpacity = 0.7, popup = ~paste(cena)
        #)
    })
    
    

    
    
    
    output$table <- renderReactable({
        reactable(filteredData() %>% select(-menu, -lat, -lon, -link))
    })
    

    
    
    
    
}

shinyApp(ui, server)