
##SEVERE WEATHER DATA INVENTORY 
##R Wrapper

###SWDI API: Basic datasets that can be extracted
#  - 'nx3tvs'       - (Point)   NEXRAD Level-3 Tornado Vortex Signatures
#  - 'nx3meso'      - (Point)   NEXRAD Level-3 Mesocyclone Signatures
#  - 'nx3hail'      - (Point)   NEXRAD Level-3 Hail Signatures
#  - 'nx3structure' - (Point)   NEXRAD Level-3 Storm Cell Structure Information
#  - 'plsr'         - (Point)   Preliminary Local Storm Reports
#  - 'warn'         - (Polygon) Severe Thunderstorm, Tornado, Flash Flood and Special Marine warnings

##Wrapper for SWDI API needs three parameters:
#- start date
#- end date
#- series

#SWDI API
swdi_pull <- function(start_date,end_date,series){
  
  ##translate the string into a date, and range
  start <- as.Date(start_date,"%Y-%m-%d")
  range <- as.numeric(as.Date(end_date,"%Y-%m-%d")-as.Date(start_date,"%Y-%m-%d"))
  
  ##Placeholder for the result
  raw <- data.frame()
  
  ##Loop through Day 0 through the full range of days
  for(i in seq(0,range,30)){
    
    ##Load in parameters, hit API
    print(i)
    period <- start + i
    increment0 <- paste(format(period,"%Y"),format(period,"%m"),format(period,"%d"),sep="")
    increment1 <- paste(format(period+30,"%Y"),format(period+30,"%m"),format(period+30,"%d"),sep="")
    temp <- read.csv(paste("http://www.ncdc.noaa.gov/swdiws/csv/",series,"/",increment0,":",increment1,sep=""))
    
    ##If the API kicks back a result
    if(ncol(temp)!=1 && colnames(temp)[1]!="summary"){
      raw <- rbind(raw,temp)
      raw <- raw[!is.na(raw$LAT),]
    }
  }
  
  ##Clean up time steps -- remove data outside of specified period
  raw$DATE<-as.Date(substr(raw$ZTIME,1,10),"%Y-%m-%d")
  raw<-raw[raw$DATE<=as.Date(end_date,"%Y-%m-%d"),]
  raw$HOUR<-substr(raw$ZTIME,12,13)
  raw<-raw[,c("ZTIME","DATE","HOUR","WSR_ID","CELL_ID","PROB","SEVPROB","MAXSIZE","LAT","LON")]
  
  ##kickback result
  return(raw)
}


##Example Use -- 2 year sample, set parameters
  start_date = "2014-01-01"
  end_date = "2014-03-01"
  series = "nx3hail"
  raw <- swdi_pull(start_date,end_date,series)