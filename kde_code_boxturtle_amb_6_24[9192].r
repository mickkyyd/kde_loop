library(sf)
library(tidyverse)
library(adehabitatHR)
library(rgdal)
library(raster)
library(dplyr)


# Read the csv file
df <- read_csv('newflbox_23.csv')

df<-data.frame(df)

# For each column check what type it is
# just a check, not needed for results

sapply(df, 
       class)

# Now lets make an sf data.frame with WGS84 projection

df <- st_as_sf(df,
               coords = c('X',
                          'Y'),
               crs = 26959,
               remove = F) 



#transform sf dataframe to a sp dataframe

df<-as_Spatial(df)
plot(df)


#loop for KDE
indv_vec<-unique(df$ID)
year_vec<-unique(df$Year)
#so for this loop, I want a loop that will separate for ID and year, so example-> IID 116 home range for year 2020,2021 etc.
#outLoopbox<-data.frame()

for(indv in indv_vec)
  for(year in year_vec)
    #filter dataset (for turtle and year)
    new<-subset(df,ID==indv & Year==year,c("X","Y"))
    
    test.kde<-kernelUD(new,h="href")
    
    ##plot it
    image(test.kde)
    
    #turn KDe into a raster file
    test.rast<-(raster(as(test.kde,"SpatialPixelsDataFrame")))
    #plotit
    plot(test.rast)
  
    new.test.kde <- getverticeshr(test.kde, percent = 95, unin = "m", unout="km2")     
    new.test.kde
    print(new.test.kde)
    plot(new.test.kde)
  
    #returns the area of each polygon
    
    print(new.test.kde)
  }
}

      
    #fill summary data frame
    #outLoopbox <- outLoopbox %>% 
      #I want to create rows with the confidence intervals of home ranges 95% for each Id and year and bind them in a csv file
      # Need to define confidence interval vector "CI" - amb
      
      # commented out for now
      
      #bind_rows(data.frame(individual = indv,year=year,
                           #low.95 = CI[1],
                           #est.95 = CI[2],
                          # high.95 = CI[3]))
      
    #lastly I want to import them all as spatial polygon files for each individual for each year.
    boxturt<-as(new.test.kde,"SpatialPolygonsDataFrame")
    
    writeOGR(obj=boxturt, dsn=paste("Fboxturt_",indv,"_",year), layer="boxturt", 
             driver="ESRI Shapefile")
  }
}
#write.csv(outLoopbox)

# write the data as a geopackage (I prefer geopackage to other formats, for more info check this: http://switchfromshapefile.org/), if you need it to be a shapefile just replace .gpkg for .shp
# write_sf(df,'BoxTurtle.shp')
# view(datetime_parsed)
