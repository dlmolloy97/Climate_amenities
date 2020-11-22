#' @export
Linear_Inferrer<-R6::R6Class(
  "Linear Inferrer",
  portable = FALSE,
  lock_objects = FALSE,
  public = list(
    df_setter=function(){
      #Downloaded from https://www.ers.usda.gov/data-products/natural-amenities-scale.aspx
      usda <- readxl::read_excel("Data/usda.xls",sheet=1,skip=104)
      #Downloaded from https://projects.propublica.org/climate-migration/, with FIPS codes from https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697
      rhodium <- read.csv("Data/rhodium.csv")
      usda$`FIPS Code`<-as.integer(usda$`FIPS Code`)
      self$df<-dplyr::full_join(usda,rhodium,by=c("FIPS Code" = "FIPS"))
      self$df$Liveability<-self$df$Heat+self$df$Wet.Bulb+self$df$Farm.Crop.Yields+self$df$Sea.Level.Rise+self$df$Very.Large.Fires+self$df$Economic.Damages
      self$df$Attractiveness.USDA<-self$df$Scale
    },
    simple_linear=function(){
      self$df_setter()
      library(tidyverse)
      self$l_model <- lm(Liveability~Attractiveness.USDA,data=self$df)
      self$linear_fitplot<-ggplot2::ggplot(self$df, aes(x = Attractiveness.USDA, y = Liveability)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
      summary(l_model)},
    linear_multivariate=function(){
      self$df_setter()
      library(tidyverse)
      myvars <- c("Heat","Wet.Bulb", "Farm.Crop.Yields", "Very.Large.Fires","Economic.Damages")
      x<- self$df[myvars]
      x <- sapply(x, as.numeric )
      y <- self$df$Attractiveness.USDA
      cor(x, y, use = "complete.obs")
    }

  ))
