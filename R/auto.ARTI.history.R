auto.ARTI.history <- function(site.lat, site.long,site.name,start.date.time,end.date.time,interval,how.many.hours,local.time.zone){
#please enter start and and times in the following format: "%Y-%m-%d %H:%M:%S"
#If you didn't enter the time properly, I really don't care. It just won't work and I'll laugh when you ask me why.
#This function requires the haversine function. Please edit the path in line 11 to reflect where you have it saved.   
#Interval: number of seconds
  
#Load necessary things  
  require(ggmap)
  require(ggplot2)
  require(MASS)
  source("/home/mayer/Documents/SLC/R/haversine.R")
#Download a map  
  map<-get_map(location=c(lon=site.long,lat=site.lat),color="color",zoom=12)
  #assign("map",map,.GlobalEnv)
#Get map attirbutes (corners)  
  bb<-attr(map,"bb")
  assign("bb",bb,.GlobalEnv)
#Use map attributes to create a scale bar
  sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                     lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                     lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                     lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))
  sbar$distance = distHaversine(long = c(sbar$lon.start,sbar$lon.end),
                                lat = c(sbar$lat.start,sbar$lat.end))
  ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.
#Load some data. Accipiter Radar data (updates) in CSV format preferrably.
  filelist<-list.files(pattern="*.csv")
  datalist<-lapply(filelist,read.csv,header=T,row.names=NULL,sep=",")
  data<-do.call(rbind.data.frame,datalist)
#Make the time correct
  data$proper.time<-as.POSIXct(data$Update.Time,format="%Y/%m/%d %H:%M:%S",tz="UTM")
  data$proper.time.local<-format(data$proper.time,tz=local.time.zone)
  data$proper.time.local<-as.POSIXct(data$proper.time.local,tz=local.time.zone)
#Add a unique ID
  data$long.ID<-paste0(data$Track.ID,data$Start.Time..UTM.)
#Calculate the antenna angle, why not?
  data$antenna.angle<-round(atan2(data$Height...m.,data$Range.from.radar...m.)*180/pi,digits=0)
#Find individual tracks, for counts
  data$dup<-duplicated(data$long.ID)
#Determine interval for Track Histories
  seq<-seq(as.POSIXct(start.date.time,format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),as.POSIXct(end.date.time,format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),by=interval)
#Break up the data based on the interval
  data$breaks<-cut(data$proper.time.local,seq)
  data<-na.exclude(data)
  assign("data",data,.GlobalEnv)
#For plotting heading information, create a dataset containin the maximum time interval for each individual track
  df.agg <- aggregate(proper.time.local ~ long.ID, data, max)
  track.max.time.data <- merge(df.agg, data)
#How many graphs will we be making today?
  how.many.graphs<-(how.many.hours*3600)/interval
#Graph it!
for(x in 1:how.many.graphs){
  sub.data<-subset(data, as.integer(breaks)==x)
  sub.track.max.time.data<-subset(track.max.time.data, as.integer(breaks)==x)
  p<-ggmap(map)+geom_line(data=na.exclude(sub.data),aes(x=Longitude,y=Latitude,group=long.ID),color="dark red",size=.05,alpha=0.7)+geom_point(data=sub.track.max.time.data,aes(x=Longitude,y=Latitude),color="dark red",size=.35,alpha=0.7)+ geom_density2d(data = na.exclude(sub.data), aes(x = Longitude, y = Latitude),size=0.1) + stat_density2d(data = na.exclude(sub.data), aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.25), guide = FALSE)
  final.graph<-p+facet_wrap(~breaks,drop=T)+ggtitle(paste0(site.name,"\n Track History (",local.time.zone,")"))+xlab("Longitude")+ylab("Latitude")+theme(axis.text=element_text(size=14,face="bold"))+geom_point(x=site.long,y=site.lat,color="black",size=1)+geom_text(x=site.long+0.01,y=site.lat,label="Radar",size=2)+geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end))+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm)  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))
  file.datetime1<-as.character(as.POSIXct(start.date.time, format="%Y-%m-%d %H:%M:%S",tz=local.time.zone)+(x*interval)-interval,format="%Y.%m.%d_%H%M")
  file.datetime2<-as.character(as.POSIXct(start.date.time, format="%Y-%m-%d %H:%M:%S",tz=local.time.zone)+(x*interval),format="%H%M")
  file.time.string<-paste0(file.datetime1,"-",file.datetime2)
  filename<-paste0(file.time.string,"_Track.History.png")
  ggsave(file=filename,plot=final.graph,width= 8.5,height=11,pointsize=12,units="in")
  }
}