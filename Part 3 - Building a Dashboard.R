library(shiny)
library(raster)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)

###############################################################################################################
# Data prep for the Map
setwd("...")

# Borough is the largest polygon shape for london and the most recognisable e.g. Highbury & Islington
Borough <- shapefile("London_Borough_Excluding_MHW.shp") 
Borough <- spTransform(Borough, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Wards are zones within boroughs
Ward <- shapefile("London_Ward_CityMerged.shp") 
Ward <- spTransform(Ward, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# MSOAs are zones within wards
MSOA <- shapefile("MSOA_2011_London_gen_MHW.shp") 
MSOA <- spTransform(MSOA, CRS("+proj=longlat +datum=WGS84 +no_defs"))


# detach becuase clashes with dplyr
detach("package:raster", unload=TRUE)


# Read in data prepped previously
data <- read.csv("data_for_map_prepped.csv")
point_data <- read.csv("point_data_prepped.csv")

# data prep for each granularity type
data_Borough <- data %>%
  na.omit() %>%
  select(-Ward_Name, -MSOA_Name, -LSOA_Name, -lat, -lon) %>%
  group_by(date, time, Borough_Name) %>%
  summarise_all(funs(mean)) %>%
  rename(NAME = Borough_Name,
         data_insight = data) %>%
  ungroup()

data_Ward <- data %>%
  na.omit() %>%
  select(-Borough_Name, -MSOA_Name, -LSOA_Name, -lat, -lon) %>%
  group_by(date, time, Ward_Name) %>%
  summarise_all(funs(mean)) %>%
  rename(NAME = Ward_Name,
         data_insight = data) %>%
  ungroup()

data_MSOA <- data %>%
  na.omit() %>%
  select(-Borough_Name, -Ward_Name, -LSOA_Name, -lat, -lon) %>%
  group_by(date, time, MSOA_Name) %>%
  summarise_all(funs(mean)) %>%
  rename(NAME = MSOA_Name,
         data_insight = data) %>%
  ungroup()


# create drop down variable from the data
dates_to_select <- unique(data$date)

# Logo for the dashboard
MainLogo <- tags$a(tags$img(src="https://image.spreadshirtmedia.com/image-server/v1/compositions/1907528/views/1,width=650,height=650,appearanceId=1,version=1547213188.jpg", height = '50', width = '50'))

###############################################################################################################
# ------- ui
ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = MainLogo,
                                    dropdownMenu(type = "notifications", badgeStatus = "warning",
                                                 notificationItem(icon = icon("cog"), status = "info",
                                                                  "Data has been updated"
                                                 ),
                                                 notificationItem(icon = icon("user", lib = "glyphicon"),
                                                                  status = "danger", "You changed your username"
                                                 )
                                    ) # Some fake notifications
                                    
                    ), # Closes the Dashboard Header section
                    
                    dashboardSidebar(
                    sidebarMenu(
                      
                      menuItem("Cool Map", tabName = "Map_tab", icon = icon("dashboard"))
                      
                    ) # Closes the Sidebar Menu (aka the contents-y bit)
                    ), # Closes the rest of the sidebar
                    
                    dashboardBody(
                      
                      tabItems(                     
                        tabItem(tabName = "Map_tab", class = "active",
                                fluidPage(
                                  titlePanel("Look at this Cool Map"),
                                  tags$style("#map {height: calc(100vh - 200px) !important;}"),
                                  column(2, 
                                         selectInput("Date",
                                                     "Date:",
                                                     choices = dates_to_select,
                                                     selected  = "09/06/2018"),
                                         sliderTextInput("TOD",
                                                         "Time of Day:",
                                                         choices = c("Early Morning", "Morning Rush", "Midday", "Afternoon Rush", "Evening"),
                                                         selected  = "Afternoon Rush",
                                                         animate = TRUE, grid = TRUE),
                                         selectInput("Granularity",
                                                     "Granularity of Map:",
                                                     choices = c("Borough", "Ward"),
                                                     selected = "Borough"),
                                         checkboxInput("Marker_data", "Marker Overlay", value = FALSE)
                                         
                                  ),
                                  column(10,
                                         leafletOutput("map")
                                         ) # closes the section of the page for the map
                                ) # closes the 'fluidpage' of the map and the sliders/inputs     
                        ) # closes the content of the menu item 'Map_tab'
                      )# closes the content info for all menu items
                  
  ) # closes the dashboard body
) # closes the UI

###############################################################################################################
# ------- server
server <- function(input, output, session) {
  
  # reactive filtering data from UI
  Borough_data_for_map <- reactive({data_Borough %>% filter(date == input$Date & time == input$TOD)})
  Ward_data_for_map <- reactive({data_Ward %>% filter(date == input$Date & time == input$TOD)})
  MSOA_data_for_map <- reactive({data_MSOA %>% filter(date == input$Date & time == input$TOD)})
  
  # Merge data to shape files
  Borough_map <- reactive({merge(Borough, Borough_data_for_map(), by='NAME')})
  Ward_map <- reactive({merge(Ward, Ward_data_for_map(), by='NAME')})
  MSOA_map <- reactive({merge(MSOA, MSOA_data_for_map(), by='NAME')}) 

  # Data for points
  point_data_for_map <- reactive({point_data %>% filter(date == input$Date & time == input$TOD) })

  # selects which is the granularity to use
  mapdata <- reactive({
    if(input$Granularity == "MSOA"){mapdata = MSOA_map()}
    if(input$Granularity == "Borough"){mapdata = Borough_map()}
    if(input$Granularity == "Ward"){mapdata = Ward_map()}
    else{mapdata = Borough_map()}
  })
  
  
  # Set the colour of the chlorpleth map
  pal <- reactive({colorNumeric(
    palette = "Reds",
    domain = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 300)
  )})
 
  
  # static backround map
output$map <- renderLeaflet({
  leaflet()  %>% 
    addTiles() %>% 
    setView(-0.1063783991, 51.494052345, zoom = 10)
    }) 


observe({
  leafletProxy("map") %>%
    clearShapes() %>%
    addPolygons(data = mapdata(), 
                weight = 0.1,
                color = 'white',
                opacity = 0.5,
                fillOpacity = 0.7,
                fillColor = ~pal()(data_insight),
                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                popup = paste(input$Granularity, " Name: ", mapdata()$NAME, "<br>",
                              "data: ", mapdata()$data_insight, "<br>"))
  
  })

observe({
  if(input$Marker_data == TRUE){
  leafletProxy("map") %>%
      clearMarkers() %>%
    addMarkers(data = point_data_for_map(),
               lng=~lon,
               lat=~lat,
               popup = paste("Time of Day: ", point_data_for_map()$time))
  }else{leafletProxy("map") %>% clearMarkers()}  
})





}

###############################################################################################################
# ------- run app
runApp(shinyApp(ui, server), launch.browser = TRUE)


