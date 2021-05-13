#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rinat)
library(lubridate)
library(sf)
library(raster)
library(leaflet)
library(leafem)
library(rgdal)
library(ggplot2)
source("LOS.R")

elevation <- raster("www/elevation.tif")
plot(elevation, col=terrain.colors(30))
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
elevation_ll <- projectRaster(elevation, crs=ll_crs)
lakes<- st_read("www/cumbria_lakes.shp")
st_crs(lakes)
lakes <- lakes %>% 
    st_set_crs(27700) %>% 
    st_transform(27700)
lakes_ll <- st_transform(lakes, 4326)

rivers <- st_read("www/cumbria_rivers.shp")
st_crs(rivers)
rivers <- rivers %>% 
    st_set_crs(27700) %>% 
    st_transform(27700)
rivers_ll <- st_transform(rivers, 4326)

roads <- st_read("www/cumbria_roads.shp")
st_crs(roads)
roads <- roads %>% 
    st_set_crs(27700) %>% 
    st_transform(27700)
roads_ll <- st_transform(roads, 4326)

settlements <- st_read("www/cumbria_settlements.shp")
st_crs(settlements)
settlements <- settlements %>% 
    st_set_crs(27700) %>% 
    st_transform(27700)
settlements_ll <- st_transform(settlements, 4326)

#Download species data
sx <- get_inat_obs(query = "European robin", place_id=30332, maxresults=2500)
nrow(sx)

sx <- sx[sx$quality_grade == "research",]
nrow(sx)

unique(sx$scientific_name)
summary(sx$datetime)

sx$datetime <- sx$datetime %>%ymd_hms()
summary(sx$datetime) 

sx <- sx %>%mutate(year = year(datetime))
ggplot(sx, aes(x=year)) +geom_histogram()
records_per_yr <- sx %>%group_by(year) %>%summarise(count_per_year = n())
ggplot(records_per_yr, aes(x = year, y=count_per_year)) +geom_line()

sx2 <- get_inat_obs(query = "Common chaffinch", place_id=30332, maxresults=2500)
nrow(sx2)
sx2 <- sx2[sx2$quality_grade == "research",]
nrow(sx2)
unique(sx2$scientific_name)
summary(sx2$datetime)
sx2$datetime <- sx2$datetime %>%ymd_hms()
summary(sx2$datetime)
sx2 <- sx2 %>%mutate(year = year(datetime))
ggplot(sx2, aes(x=year)) +geom_histogram()
records_per_yr2 <- sx2 %>%group_by(year) %>%summarise(count_per_year = n())
ggplot(records_per_yr2, aes(x = year, y=count_per_year)) +geom_line()

sx3 <- get_inat_obs(query = "Blue tit", place_id=30332, maxresults=2500)
nrow(sx3)
sx3 <- sx3[sx3$quality_grade == "research",]
nrow(sx3)
unique(sx3$scientific_name)
summary(sx3$datetime)
sx3$datetime <- sx3$datetime %>%ymd_hms()
summary(sx3$datetime)
sx3 <- sx3 %>%mutate(year = year(datetime))
ggplot(sx3, aes(x=year)) +geom_histogram()
records_per_yr3 <- sx3 %>%group_by(year) %>%summarise(count_per_year = n())
ggplot(records_per_yr3, aes(x = year, y=count_per_year)) +geom_line()

#images setting
Er <- base64enc::dataURI(file="www/European robin.jpeg", mime="image/jpeg")
Cc <- base64enc::dataURI(file="www/Common chaffinch.jpeg", mime="image/jpeg")
Bt <- base64enc::dataURI(file="www/Blue tit.jpeg", mime="image/jpeg")


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Three species of bird and the rural environment in Cumbria"),

     
    sidebarLayout(
        sidebarPanel(
            helpText("Show images of three birds."),
            
            selectInput("var", 
                        label = "Choose a Species image to display",
                        choices = c("European robin", "Common chaffinch", "Blue tit"),
                        selected = "European robin")
            
            
        ),
        
        
        
        mainPanel(p("This website will help users to learn about the three species 
                    of birds (European robin, Common chaffinch and Blue tit) 
                    that are common in the rural environment of Cumbria."),
        
            leafletOutput(outputId = "map"),
            uiOutput(outputId = "img1"))))
            
        
    
    

# Define server logic
server <- function(input, output) {
    
    output$map <- renderLeaflet({leaflet() %>% 
            addTiles(group = "OSM") %>% 
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (default)") %>% 
            addRasterImage(elevation_ll, colors=terrain.colors(25), group = "Elevation") %>%
            addCircleMarkers(sx$longitude, sx$latitude, 
                             label = sx$common_name, group = "European robin",
                             labelOptions(interactive = "TURE"),
                             radius = 2, fillOpacity = 0.5, opacity = 0.5, col="orange") %>%
            addCircleMarkers(sx2$longitude, sx2$latitude, 
                             label = sx2$common_name, group = "Common chaffinch",
                             labelOptions(interactive = "TURE"),
                             radius = 2, fillOpacity = 0.5, opacity = 0.5, col="purple") %>%
            addCircleMarkers(sx3$longitude, sx3$latitude, 
                             label = sx3$common_name, group = "Blue tit",
                             labelOptions(interactive = "TURE"),
                             radius = 2, fillOpacity = 0.5, opacity = 0.5, col="white") %>%
            addFeatures(lakes_ll, color = "red", group = "Lakes") %>%
            addFeatures(rivers_ll, color = "blue", group = "Rivers") %>%
            addFeatures(roads_ll, color = "black", group = "Roads") %>%
            addFeatures(settlements_ll, color = "green", group = "Settlements", label = settlements_ll$NAME, 
                        labelOptions = labelOptions(interactive = "TRUE"))%>%
            addLayersControl(
                baseGroups = c("Satellite (default)", "OSM"), 
                overlayGroups = c("Lakes", "Rivers", "Roads", "Settlements", "Elevation", "European robin", "Common chaffinch", "Blue tit"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        
    })
    output$img1 <- renderUI({
        if(input$var == "European robin"){            
            img(height = 240, width = 300, src = Er)
        }                                        
        else if(input$var == "Common chaffinch"){
            img(height = 240, width = 300, src = Cc)
        }
        else if(input$var == "Blue tit"){
            img(height = 300, width = 240, src = Bt)
        }
    })
    }
    

# Run the application 
shinyApp(ui = ui, server = server)
