auto.ARTI.history_v1.0 <- function(site.name,interval=3600,local.time.zone,zoom=12,map.type="terrain"){

#Make some track histories!!
#Always load more data than you need on either side of the interval you are interested in. This function rounds the start and end times of data provided to the nearest hour.  
  ##Valid Site Names:
      ### "DFW_JRC"
      ### "SEA_Olympic"
      ### "SEA_Cascades"
  ##Interval: number of seconds - Default is 1 hour
  ##Valid Time Zones
      ### "US/Eastern", "US/Central", "US/Mountain", "US/Pacific"

#v1.0 There's just something hypnotic about maps.

#Check that the required packages are installed.
  if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
  if("bit64" %in% rownames(installed.packages()) == FALSE) {install.packages("bit64")}
  if("ggmap" %in% rownames(installed.packages()) == FALSE) {install.packages("ggmap")}
  if("geosphere" %in% rownames(installed.packages()) == FALSE) {install.packages("geosphere")}
  if("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
  
#Load necessary things
  require(data.table)
  require(bit64)
  require(ggmap)
  require(geosphere)
  require(MASS)
  
#Here is where we have radars!
  ifelse(site.name == "DFW_JRC",radar.location <- data.frame(lon = -97.047128 , lat =  32.864114),print("Thinking..."))
  ifelse(site.name == "SEA_Olympic",radar.location <- data.frame(lon = -122.31577 , lat =  47.45204),print("Thinking..."))
  ifelse(site.name == "SEA_Cascades",radar.location <- data.frame(lon = -122.30943 , lat =  47.44973),print("Thinking..."))
  assign("radar.location",radar.location,.GlobalEnv)
  
#Define some variables.
  working.directory<-file.path(path.expand("~"),"ARTI_Analysis")
  CSV.to.Process<-"CSV_to_Process"
  CSV.finished<-"CSV_Finished"
  Track.Histories<-"Track_Histories"
#Make some Directories
  setwd(working.directory)
  dir.create(file.path(working.directory,"Data",CSV.finished),showWarnings = FALSE)
  dir.create(file.path(working.directory,"Results",Track.Histories),showWarnings = FALSE)
  
#Download a map  
  map<-get_map(location = c(lon = radar.location$lon, radar.location$lat), zoom = zoom, maptype = map.type)
  #assign("map",map,.GlobalEnv)
#Get map attirbutes (corners)  
  bb<-attr(map,"bb")
  assign("bb",bb,.GlobalEnv)
#Use map attributes to create a scale bar
  sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                     lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                     lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                     lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))
  sbar$distance = distGeo(p1 = c(sbar$lon.start,sbar$lat.start),p2 = c(sbar$lon.end,sbar$lat.end)) #Distance across downloaded map in meters
  ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.
#Load some data. Accipiter Radar data (updates) in CSV format (preferrably).
  setwd(file.path(working.directory,"Data",CSV.to.Process))
  filelist<-list.files(pattern="\\.csv$")
#Check if there are files to run... if not don't do anything
  if(length(filelist) >= 1){
  datalist<-lapply(filelist,fread)
  data<-rbindlist(datalist)
#Cleanup
  rm(datalist)
  arti.header<-unlist(read.csv(filelist[1],header=F,nrows=1,stringsAsFactors = F))
  setnames(data, c(arti.header),c("Update.Time", "Track.ID", "Start.Time..UTM.", "Latitude", "Longitude", "Speed..m.s.", "Heading..deg.N.", "Height...m.", "RCS..dBsm.", "Range.from.radar...m.", "Azimuth.from.radar..deg..", "Intensity", "UTM.Zone", "UTM.Northing", "UTM.Easting"))
  
#Make the time correct
  data$Update.Time<-as.POSIXct(data$Update.Time,format="%Y/%m/%d %H:%M:%S",tz="UTM")
  data$Update.Time.Local<-format(data$Update.Time,tz=local.time.zone)
  data$Update.Time.Local<-as.POSIXct(data$Update.Time.Local,tz=local.time.zone)
#Find out how much data you have, in local time.
  start.date.time<-min(data$Update.Time.Local)
  end.date.time<-max(data$Update.Time.Local)
#Round the start and end times of the data to the nearest hour.
  round.start.date.time<- as.POSIXct(format(round(start.date.time, units="hours")),tz=local.time.zone)
  round.end.date.time<- as.POSIXct(format(round(end.date.time, units="hours")),tz=local.time.zone)
#Add a unique ID
  data$long.ID<-paste0(data$Track.ID,data$Start.Time..UTM.)
#Calculate the antenna angle, why not?
  data$antenna.angle<-round(atan2(data$Height...m.,data$Range.from.radar...m.)*180/pi,digits=0)
#Find individual tracks, for counts
  data$dup<-duplicated(data$long.ID)
#Determine interval for Track Histories
  seq<-seq(round.start.date.time,round.end.date.time,by=interval)
#Break up the data based on the interval
  data$breaks<-cut(data$Update.Time.Local,seq)
  data<-na.exclude(data)
  assign("ARTI Data",data,.GlobalEnv)
#For plotting heading information, create a dataset containin the maximum time interval for each individual track
  df.agg <- aggregate(Update.Time.Local ~ long.ID, data, max)
  track.max.time.data <- merge(df.agg, data)
#How many graphs will we be making today?
  how.many.graphs<-length(seq)-1
  
#Show some progress
  pb<- txtProgressBar(min=0,max=how.many.graphs,style=3)
  setTxtProgressBar(pb,0)
#Graph it!
for(x in 1:how.many.graphs){
  sub.data<-subset(data, as.integer(breaks)==x)
  sub.track.max.time.data<-subset(track.max.time.data, as.integer(breaks)==x)
  p<- ggmap(map, extent = "normal", maprange=FALSE) %+% sub.data + aes(x = radar.location$lon, y = radar.location$lat) #Plot the map
  p1<-p  + geom_line(data=sub.data,aes(x=Longitude,y=Latitude,group=long.ID),color="dark red",size=.15,alpha=0.7, na.rm = T) #Plot the Track Lines
  p2<-p1 + geom_point(data=sub.track.max.time.data,aes(x=Longitude,y=Latitude),color="dark red",size=.65,alpha=0.7, na.rm = T) #Plot the Track Direction Indicators
  p3<-p2 + geom_density2d(data = sub.data, aes(x = Longitude, y = Latitude),size=0.1, na.rm=T) #Plot the density contours
  p4<-p3 + stat_density2d(data = sub.data, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon',na.rm=T) #Add Rransparent fill to density contours
  p5<-p4 + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.25), guide = FALSE)
  p6<-p5 + geom_point(x=radar.location$lon,y=radar.location$lat, shape=10, colour="blue", size=1) #Add Radar Location
  p7<-p6 + ggtitle(paste0(site.name,"\n Track History (",local.time.zone,")"))+xlab("Longitude")+ylab("Latitude") #Add title and axis labels
  p8<-p7 + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end))+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm)  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
  p9<-p8 + facet_wrap(~breaks,drop=T)+theme(axis.text=element_text(size=14,face="bold"),legend.position="none") #Facet and add theme
  #Filename for the maps
  file.datetime1<-as.character(round.start.date.time+(x*interval)-interval,format="%Y.%m.%d_%H%M")
  file.datetime2<-as.character(round.start.date.time+(x*interval),format="%H%M")
  file.time.string<-paste0(file.datetime1,"-",file.datetime2)
  filename<-paste0(site.name,"_",file.time.string,"_Track.History.png")
  #Save it!
  ggsave(file=file.path(working.directory,"Results","Track_Histories",filename),plot=p9,width= 8.5,height=11,pointsize=12,units="in")
  setTxtProgressBar(pb,x) 
}
  file.rename(from=file.path(working.directory,"Data",CSV.to.Process,filelist),to=file.path(working.directory,"Data",CSV.finished,filelist))
  print("Finished!")
  }
else{print("No data to process. Please add CSV files to the CSV_to_Process directory.")}
}