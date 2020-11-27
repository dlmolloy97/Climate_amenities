#' @export
Index_Selector<-function(){
  usda <- readxl::read_excel("Data/usda.xls",sheet=1,skip=104)
  rhodium <- read.csv("Data/rhodium.csv")
  usda$`FIPS Code`<-as.integer(usda$`FIPS Code`)
  fulldata<-dplyr::full_join(usda,rhodium,by=c("FIPS Code" = "FIPS"))
  newerdata <- fulldata[order("FIPS Code"),]
  geoframe<-raster::shapefile("Data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
  geoframe$GEOID<-as.integer(geoframe$GEOID)
  geoframe <- geoframe[order(geoframe$GEOID),]
  county_vector<-data.frame(geoframe$GEOID)
  colnames(county_vector)<-"GEOID"
  df<-dplyr::left_join(county_vector,fulldata,by=c("GEOID"="FIPS Code"))
  newdata <- data.frame(df$GEOID,df$County,df$Wet.Bulb,df$Farm.Crop.Yields,df$Very.Large.Fires,df$Sea.Level.Rise,df$Economic.Damages)
  holding_frame<-data.frame(GEOID=0000,Index='Wet.Bulb',Risk=1)
  colnames(newdata)<-c("GEOID", "County", "an increase in wet bulb temperatures","loss of crop yields","fire damage","rising sea levels","overall economic loss")
  for (i in 1:3233){
    df_temp<-filter(newdata,GEOID==index_vector[[i]])
    df_gathered<-df_temp%>%
      gather('Index','Risk',-GEOID,-County)%>%
      arrange(-Risk)
    extract_row<-df_gathered[1,]
    holding_frame<-dplyr::bind_rows(holding_frame,extract_row)
  }
  holding_frame<-select(holding_frame,-Risk)
  #colnames(holding_frame)<-c("Index","GEOID","County")
  holding_frame<-holding_frame[-1,]
  write.csv(holding_frame,'/Users/desmondmolloy/Desktop/Repositories/Climate_attractiveness/Data/index_identity.csv')
}
