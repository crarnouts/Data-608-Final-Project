
library(jsonlite)

### https://datadrivendetroit.org/blog/2018/03/20/open-data-portals/ ###


detroit_rms_crime <- fromJSON("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")
detroit_rms_crime2 <- detroit_rms_crime$features$properties
detroit_rms_crime2$incident_timestamp2 <- as.POSIXct(detroit_rms_crime2$incident_timestamp)


