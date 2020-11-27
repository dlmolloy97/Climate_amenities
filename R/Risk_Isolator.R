#' @export
Risk_Isolator<-R6::R6Class(
  "Risk Isolator",
  portable = FALSE,
  lock_objects = FALSE,
  public = list(
    df_setter=function(){
      #Downloaded from https://projects.propublica.org/climate-migration/, with FIPS codes from https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697
      self$df <- read.csv("Data/index_identity.csv")
      self$geoframe<-raster::shapefile("Data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
      self$geoframe$GEOID<-as.integer(self$geoframe$GEOID)
      self$geoframe <- self$geoframe[order(self$geoframe$GEOID),]
      self$df <- self$df[order(self$df$GEOID),]
      self$geoframe$Risktype<-self$df$Index
      self$geoframe$County<-self$df$County
    },
    leaflet_builder=function(){
      #Code for highlights sourced from https://rstudio.github.io/leaflet/choropleths.html
      self$df_setter()
      library(leaflet)
      labels <- sprintf(
        "Highest source of risk in <strong>%s is <strong>%s",
        self$geoframe$County, self$geoframe$Risktype
      ) %>% lapply(htmltools::HTML)
      pal <- colorFactor(topo.colors(5), self$geoframe$Risktype)
      leaflet(self$geoframe)%>% addProviderTiles(providers$CartoDB) %>%
        setView(lng = -106.363590, lat=31.968483,zoom=5) %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
                    color = ~pal(Risktype),label = labels)%>%
        addLegend("bottomright", pal = pal, values = ~Risktype,
                  title = "Category of risk most likely to cause damage",
                  opacity = 1)
    }))
