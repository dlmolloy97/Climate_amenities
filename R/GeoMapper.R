#' @export
GeoMapper <- R6::R6Class(classname="GeoMapper",
                                   public=list(
                                     spatial_transformer=function(){
                                       usda <- readxl::read_excel("Data/usda.xls",sheet=1,skip=104)
                                       rhodium <- read.csv("Data/rhodium.csv")
                                       usda$`FIPS Code`<-as.integer(usda$`FIPS Code`)
                                       fulldata<-dplyr::full_join(usda,rhodium,by=c("FIPS Code" = "FIPS"))
                                       newerdata <- fulldata[order("FIPS Code"),]
                                       self$geoframe<-raster::shapefile("Data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
                                       self$geoframe$GEOID<-as.integer(self$geoframe$GEOID)
                                       self$geoframe <- self$geoframe[order(self$geoframe$GEOID),]
                                       county_vector<-data.frame(self$geoframe$GEOID)
                                       colnames(county_vector)<-"GEOID"
                                       self$df<-dplyr::left_join(county_vector,fulldata,by=c("GEOID"="FIPS Code"))
                                       self$df$Liveability<-self$df$Heat+self$df$Wet.Bulb+self$df$Farm.Crop.Yields+self$df$Sea.Level.Rise+self$df$Very.Large.Fires+self$df$Economic.Damages
                                       self$df$Attractiveness.USDA<-self$df$Scale
                                       self$geoframe$Liveability<-self$df$Liveability},
                                     leaflet_builder=function(){
                                       self$spatial_transformer()
                                       library(leaflet)
                                       bins <- c(12:20)
                                       pal <- colorBin("Reds", domain = self$geoframe$Liveability, bins = bins)
                                       leaflet(self$geoframe)  %>% addProviderTiles(providers$CartoDB) %>%
                                         setView(lng = -106.363590, lat=31.968483,zoom=9) %>%
                                         addPolygons(
                                           fillColor = ~pal(Liveability),
                                           weight = 0,
                                           opacity = 1,
                                           fillOpacity = 0.7)
                                     }
                                   ),
                                   lock_objects = FALSE)
