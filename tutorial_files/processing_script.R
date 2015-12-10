#################################
##SEVERE WEATHER DATA INVENTORY##
#################################

############
##OVERVIEW##
############
#Sections in code
# (1) Write a wrapper to pull down select data series from NOAA's SWDI database;
# (2) Pull down geographic shapefiles from the US Census Bureau;
# (3) De-duplicate hail events and process events into gridded, daily hail signature counts
# (4) Calculate hourly and monthly climatologies
# (5) Map the data using leaflet.js with a popup containing the hail risk and the nearest county 
#To get started quickly, the code for this tutorial can be found at the following Github repo (https://github.com/CommerceDataService/tutorial_noaa_hail).

  setwd("your-dir-here")
  
  library(sqldf)
  library(RColorBrewer)
  library(leaflet)
  library(googleVis)  
  library(rgdal)

####################################
##SECTION 1 - TAPPING THE SWDI API##
####################################

###SWDI API: Basic datasets that can be extracted
#- 'nx3tvs'       - (Point)   NEXRAD Level-3 Tornado Vortex Signatures
#- 'nx3meso'      - (Point)   NEXRAD Level-3 Mesocyclone Signatures
#- 'nx3hail'      - (Point)   NEXRAD Level-3 Hail Signatures
#- 'nx3structure' - (Point)   NEXRAD Level-3 Storm Cell Structure Information
#- 'plsr'         - (Point)   Preliminary Local Storm Reports
#- 'warn'         - (Polygon) Severe Thunderstorm, Tornado, Flash Flood and Special Marine warnings

##Wrapper for SWDI API needs three parameters:
#- start date
#- end date
#- series

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

##TRY OUT THE API
##Note that in the tutorial, we draw down 10 years, but it takes about 30 minutes to download 9.3 million records
##In this sample script, we'll just do two months

##Set Parameters: for 
  start_date = "2014-01-01"
  end_date = "2014-03-01"
  series = "nx3hail"  
  
##Tap API
  raw <- swdi_pull(start_date,end_date,series)
  
##Parameters for data processing
  range <- as.Date(end_date,"%Y-%m-%d")-as.Date(start_date,"%Y-%m-%d")
  fraction = 0.25

##########################################
##SECTION 2 - GEOGRAPHIC REFERENCE FILES##
##########################################

##As the SWDI data is point-level data that will be processed into equal-interval grid points, 
##we will want to add spatial context to the data by spatially joining points to county boundary files. 
##The US Census Bureau provides boundary shapefiles through their website (http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip). 
##To efficiently load it in, we'll write a simple function to download, unzip and load a shapefile.
  
  shape_direct <- function(url, shp) {
    library(rgdal)
    temp = tempfile()
    download.file(url, temp) ##download the URL taret to the temp file
    unzip(temp,exdir=getwd()) ##unzip that file
    return(readOGR(paste(shp,".shp",sep=""),shp))
  }

##To run the shape_direct function, we just need the url and the shapefile name.
  shp <- shape_direct(url="http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip", 
  shp= "cb_2014_us_county_20m")


##In addition, we're going to pull in a reference table that links Federal Information Processing System (FIPS) codes that contain numeric identifiers 
##for states. This'll be useful for clearly indicating in plain language which counties are in a given state.
  fips <- read.delim("http://www2.census.gov/geo/docs/reference/state.txt",sep="|")
  fips <- fips[,c("STATE","STUSAB")]  ##Keep the FIPS code and state abbreviation
  fips$STATE <- as.numeric(fips$STATE) ##Convert FIPS code to numeric
  fips$STUSAB <- as.character(fips$STUSAB) ##Convert state name to character


###############################
##SECTION 3: Basic Processing##
###############################

###Processing for mapping
#At this point, we have all the data (hail signatures, county shapefiles, FIPs codes).
  paste("Number of records in Hail data: ",nrow(raw),sep="")
  paste("Number of counties in shapefile: ",nrow(as.data.frame(shp)))

#Now, we can begin the process down the data. 
#The first issue is to convert hail signatures from events to regularly spaced grid points at the daily level. 
#As two or more radar stations may detect the same hail event at the same time, 
#this basic gridding process will help to reduce double counting as well as allow for calculating a climatology.

#To start, we'll set a bounding box of the continental US.
#Cut down bounding box
  raw <- raw[raw$LON<(-50) & raw$LON>(-140) & raw$LAT > 25,]

#Also, the hail event coordinates will be rounded to the nearest fraction as specified in the starting parameters. 
#In this case, the fraction is 1/4 of a degree or about 17.25 miles latitudinally. 
#Using SQL, we will group hail events by date, lat, lon, and hail size. 
#Then based on the hail size, we'll produce dummy variables are produced for each 2+ inch and 3+ inch thresholds. 
#For context, the diameter of a baseball is approximately 2.9 inches.

##Round coordinates
  raw$LON <- round(raw$LON/fraction)*fraction
  raw$LAT <- round(raw$LAT/fraction)*fraction

##De-duplicate by day, latitude and longitude
  deduped_day <- sqldf("SELECT DATE, LON, LAT, MAXSIZE
                        FROM raw
                        GROUP BY DATE, LON, LAT, MAXSIZE")

##Dummy variable (and 3+ in)
  deduped_day$lvl_3in<-0
  deduped_day$lvl_3in[deduped_day$MAXSIZE>3]<-1

#Based on the de-duplicated daily, gridded data, we'll now group by once again by lat and lon coordinates. 
#This time, we'll count the number of records per gridpoint (cnt = any hail event) as well as sum the dummy
#variables for 3+ inch (cnt_3) events. These count variables are then normalized by the number 
#of days specified in the API call (range).

##DAILY GRIDDED FREQUENCIES
  singles <- sqldf("SELECT LON, LAT,COUNT(DATE) cnt, SUM(lvl_3in) cnt_3
                    FROM deduped_day
                    GROUP BY LON, LAT")

##NORMALIZE GRIDDED FREQUENCIES BY TOTAL DAYS TO DERIVE A PROBABILITY
  for(i in 3:ncol(singles)){
    singles[,i]<-singles[,i]/as.numeric(range)
  }

##########################################
##SECTION 4: HOURLY/MONTHLY CLIMATOLOGY##
##########################################

  #Similar to gridded processing, hourly and monthly climatologies can be processed using simple group by statements. Note that climatologies are calculating based on day-grid cells 
  #(e.g. whether a hail signature was detected  in a 0.25 degree grid cell in a given day) as opposed to the raw hail signatures.

  ##Process Into Weeks
    deduped_day$DATE <- as.Date(as.character(deduped_day$DATE,"%Y-%m-%d"))
    deduped_day$MONTH <- as.numeric(format(deduped_day$DATE,"%m"))

  #Frequency: Hour
    hourly <- sqldf("SELECT HOUR, COUNT(HOUR) count_all
                      FROM deduped_day
                      GROUP BY HOUR")
    hourly$`All Hail` <- round(100*hourly$count_all/sum(hourly$count_all),2)

  #Frequency: Month
    monthly <- sqldf("SELECT MONTH, COUNT(DATE) count_all
                      FROM deduped_day
                      GROUP BY MONTH")
    monthly$`All Hail` <- round(100*monthly$count_all/sum(monthly$count_all),2)
    
  #Assign monthly string label to each month
    monthly$mon <- "Jan"
    monthly$mon[monthly$MONTH == 2] <- "Feb"
    monthly$mon[monthly$MONTH == 3] <- "Mar"
    monthly$mon[monthly$MONTH == 4] <- "Apr"
    monthly$mon[monthly$MONTH == 5] <- "May"
    monthly$mon[monthly$MONTH == 6] <- "Jun"
    monthly$mon[monthly$MONTH == 7] <- "Jul"
    monthly$mon[monthly$MONTH == 8] <- "Aug"
    monthly$mon[monthly$MONTH == 9] <- "Sep"
    monthly$mon[monthly$MONTH == 10] <- "Oct"
    monthly$mon[monthly$MONTH == 11] <- "Nov"
    monthly$mon[monthly$MONTH == 12] <- "Dec"


#################################
##SECTION 5: VISUALIZING RISKS##
#################################

#At this point, the data has been processed into a manageable form and visualizing the data is fairly straight. 
#The bulk of the work going forward is focused on formatting and adding features to the visualizations. 
#To do so, the data could be exported into JSON or CSVs to be ingested into visualization libraries such as D3.js, 
#Google Charts, Dygraphs.js, and leaflet.js. In the R statistical programming language, there have been advancements 
#in extending the functionality and interactivity such that web-based visualizations can be produced without leaving the platform. 
#In this section, we'll build climatology graphs using the googleVis library that leverages the Google Charts API as well
#as build climatology maps using leaflet.js.

##**Climatology Graphs**##
  #GoogleVis functions largely like any other plotting library and accepts drames, sends them through to the Google Charts API, and kicks back a JavaScript-based visualization. 

#Column chart
  mon_clim <- gvisColumnChart(monthly, 
                              xvar="mon", 
                              yvar="All Hail",
                              options=list(vAxis.gridlines.count=1,
                                           hAxis="{title:'Months'}", ##Title
                                           vAxis="{gridlines:{ count:0}}", #Remove gridlines
                                           series="[{color:'darkred',
                                           targetAxisIndex: 0}]")) #Series color
  hour_clim <- gvisColumnChart(hourly, xvar="HOUR", yvar=c("All Hail"),
                               options=list(vAxis.gridlines.count=1,
                                            hAxis="{title:'Hours (24h)'}", 
                                            vAxis="{gridlines:{color:'red', count:0}}",
                                            series="[{color:'darkred', targetAxisIndex: 0}, 
                                                    {color: 'red',targetAxisIndex:1}]"))
  plot(mon_clim)
  plot(hour_clim)


##**Climatology Maps**###
###Geoprocessing for the tooltip popup
  #Knowing where risks occur is not sufficient. A good risk map contains context. To do this for grid points, 
  #we'll spatially join the grid points to the closest county, that is see which county a grid point falls. 
  #It's not a perfect match, but gives some basic context for the local area.
  #To start, we need to convert the gridded data into a shapefile and define the projection as WGS84 -- 
  #one of the most common spatial projections for global data. 

  ##Set up spatial
    points<-singles
    coordinates(points)=~LON+LAT
    proj4string(points)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
    shp <- spTransform(shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") )
    
  ##Spatial join -- really simple!
     a<-over(points,shp) 
  
  ##Join the results back to singles
    singles <- cbind(singles,a[,c("STATEFP","NAME")])
    singles$NAME <- as.character(singles$NAME)
  
  #Merge state abbreviation via FIPS
    singles$STATEFP <-as.numeric(singles$STATEFP)
    singles <- merge(singles,fips,by.x="STATEFP",by.y="STATE",all.x=T, sort=F) 
    singles$loc <- paste(singles$NAME,", ",singles$STUSAB,sep="")
    singles$loc[is.na(singles$NAME)]<-""
    singles <- singles[!is.na(singles$STUSAB),]
  
  #For the popup, we'll need to create a separate subset for 3+ inch events, vectorize the fields, 
  #and combine fields for the popup message using paste0. The message needs to be written in HTML as this will be directly rendered in browser.

  #Subset data into separate frames (needed for layers in leaflet.js)
    in3 <- singles[singles$cnt_3>0,]
  
  #Vectorize data for inclusion for popup text
  #All points
    county_all <- singles$loc
    x_all <- singles$LON
    y_all <- singles$LAT
    pr <- round(100*singles$cnt,3)
  
  #Hail balls > 3in
    county_3 <- in3$loc
    x3 <- in3$LON
    y3 <- in3$LAT
    pr3 <- round(100*in3$cnt_3,3)
  
  #Popup
    content_all <- paste("<h3>",county_all,"</h3>Grid Point: ",x_all,", ",y_all,"<p>Prob of Any Hail <span style='color:red'><strong>: ",pr,"%</strong></span></p>")
    content_3 <- paste0("<h3>",county_3,"</h3>Grid Point: ",x3,", ",y3,"<p>Prob of Hail (> 3in)<span style='color:red'><strong>: ",pr,"%</strong></span></p>")
    
  ###Color palettes
  #For each series, we will need two color schemes that are sscaled to the range and variability in a data series.     
  #The color palette "Set1" used is pulled from the RColorBrewer package. The color scheme is high contrast and divergent, perfect for maps with varying levels of activity.
  
  #All hail palette
    pal <- colorNumeric(
    palette = "Set1",
    domain = pr
    )
  
  #3+ inch hail palette
    pal2 <- colorNumeric(
    palette = "Set1",
    domain = pr3
    )

##############
###THE MAPS##
##############

#We'll initialize a leaflet.js map, setting the view centered on the mean coordinates of the continental US at zoom 4 
# (higher the value, closer the zoom), attributing the layer to CartoDB. With this basic code, we can build out maps nationally of all hail events and 3+ inch events and customize the styles.

##10-year climatology map of all hail events
leaflet(width="100%") %>% 
  setView(lat = mean(y_all), lng = mean(x_all),4) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution = "NOAA NCEI SWDI, US Census Bureau TIGER, Cartodb basemap") %>% 
  addCircleMarkers(data = singles, lat = ~ LAT, lng = ~ LON,radius=(pr/5), 
                   fillOpacity = 0.8,stroke = FALSE, 
                   color = ~pal(pr), popup = content_all) %>%
  addLegend("bottomright", pal = pal, values = pr,
            title = "Prob(Hail)",labFormat = labelFormat(suffix = "%")
  )


##10-year climatology map of events with 3+ inch hail 
leaflet(width="100%") %>% 
  setView(lat = mean(y_all), lng = mean(x_all),4) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution = "NOAA NCEI SWDI, US Census Bureau TIGER, Cartodb basemap") %>% 
  addCircleMarkers(data = in3, lat = ~ LAT, lng = ~ LON, radius=2*(pr3), 
                   fillOpacity = 0.8,stroke = FALSE, 
                   color = ~pal2(pr3), popup = content_all) %>%
  addLegend("bottomright", pal = pal2, values = pr3,
            title = "Prob(Hail diameter > 3in)",labFormat = labelFormat(suffix = "%")
  )

