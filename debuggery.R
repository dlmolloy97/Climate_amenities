library(leaflet)
labels <- sprintf(
  "Highest source of risk in <strong>%s is <strong>%s",
  geoframe$County, geoframe$Risktype
) %>% lapply(htmltools::HTML)
pal <- colorFactor(topo.colors(5), geoframe$Risktype)
leaflet(geoframe)  %>% addProviderTiles(providers$CartoDB) %>%
  setView(lng = -106.363590, lat=31.968483,zoom=5) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
              color = ~pal(Risktype),label = labels,highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE))


df <- read.csv("Data/index_identity.csv")
geoframe<-raster::shapefile("Data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
geoframe$GEOID<-as.integer(geoframe$GEOID)
geoframe <- geoframe[order(geoframe$GEOID),]
df <- df[order(df$GEOID),]
geoframe$Risktype<-df$Index
geoframe$County<-df$County
pal <- colorFactor(topo.colors(5), geoframe$Risktype)
leaflet(geoframe)  %>% addProviderTiles(providers$CartoDB) %>%
  setView(lng = -106.363590, lat=31.968483,zoom=5) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
              color = ~pal(Risktype),label = labels)

