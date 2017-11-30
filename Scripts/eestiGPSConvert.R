##Required Libraries
library(sp)
library(magrittr)
library(data.table)

eestiGPSConvert <- function(data_csv, X_COORD_COLUMN_NAME, Y_COORD_COLUMN_NAME){
  
  WGS84_PROJ <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  LEST_PROJ <- CRS("+init=epsg:3301")
  
  gpsFixedData <- data.table(data_csv)
  
  X_COORD_COLUMN_NAME <- as.factor(X_COORD_COLUMN_NAME)
  Y_COORD_COLUMN_NAME <- as.factor(Y_COORD_COLUMN_NAME)
  
  X_COORD_COLUMN_NAME <- as.numeric(as.character(X_COORD_COLUMN_NAME))
  Y_COORD_COLUMN_NAME <- as.numeric(as.character(Y_COORD_COLUMN_NAME))
  
  dataFrame <-data.table(cbind(X_COORD_COLUMN_NAME,Y_COORD_COLUMN_NAME))
  
  xy_wgs84 <- SpatialPointsDataFrame(dataFrame[,.(Y_COORD_COLUMN_NAME,X_COORD_COLUMN_NAME)],gpsFixedData,proj4string = LEST_PROJ)%>% spTransform(WGS84_PROJ)
  xy_coord <- as.data.frame(xy_wgs84@coords)
  xy_data <- xy_wgs84@data
  
  names(xy_coord)[1] <- paste("Lon")
  names(xy_coord)[2] <- paste("Lat")
  
  gpsDataNewCoordinates <- cbind(xy_coord, xy_data)
  gpsDataNewCoordinates <<- gpsDataNewCoordinates
}