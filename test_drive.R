Risk_Isolator<-R6::R6Class(
  "Risk Isolator",
  portable = FALSE,
  lock_objects = FALSE,
  public = list(
    df_setter=function(){
      #Downloaded from https://projects.propublica.org/climate-migration/, with FIPS codes from https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697
      self$df <- read.csv("Data/rhodium.csv")
      df_gathered<-self$df%>%
        gather('Index','Risk',-FIPS,-County)%>%
        group_by(County,FIPS)%>%
        top_n(1, Risk)

      self$geoframe<-raster::shapefile("Data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
      self$geoframe$GEOID<-as.integer(self$geoframe$GEOID)
      self$geoframe <- self$geoframe[order(self$geoframe$GEOID),]
      county_vector<-data.frame(self$geoframe$GEOID)
      colnames(county_vector)<-"GEOID"
      temp_df<-dplyr::left_join(county_vector,df_gathered,by=c("GEOID"="FIPS Code"))
      self$geoframe$Risktype<-temp_df$Index
    },
    leaflet_builder=function(){
      self$df_setter()
      library(leaflet)
      bins <- c(12:20)
      pal <- colorBin("Reds", domain = self$geoframe$Risktype, bins = bins)
      leaflet(self$geoframe)  %>% addProviderTiles(providers$CartoDB) %>%
        setView(lng = -106.363590, lat=31.968483,zoom=9) %>%
        addPolygons(
          fillColor = ~pal(Risktype),
          weight = 0,
          opacity = 1,
          fillOpacity = 0.7)
    }))
library(tidyverse)
df <- read.csv("Data/rhodium.csv")
df_gathered<-df%>%
  gather('Index','Risk',-FIPS,-County)%>%
  group_by(County,FIPS)%>%
  top_n(1, Risk)
