ARTI.rcs.per.hour<-function(start.date.time,end.date.time,interval=3600,local.time.zone,title){
#please enter start and and times in the following format: "%Y-%m-%d %H:%M:%S"
  require(ggplot2)
  require(scales)
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
#Find individual tracks, for counts
  data$dup<-duplicated(data$long.ID)
#Determine interval for Track Histories
  seq<-seq(as.POSIXct(start.date.time,format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),as.POSIXct(end.date.time,format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),by=interval)
#Break up the data based on the interval
  data$breaks<-cut(data$proper.time.local,seq)
  assign("data",data,.GlobalEnv)
#Subset the data, only Unique TrackID's
  sub.data<-subset(data,dup==FALSE)
#Convert speed to MPH
  data$Speed..mph<-data$Speed..m.s.*2.23694
#Plot it!
  p<-ggplot(data=na.exclude(data),aes(x=RCS..dBsm.))+geom_histogram(binwidth=1)
  final.graph<-p+xlab("Track Update RCS (dBsm)")+ggtitle(paste0(title,"\nTrack Update RCS (dBsm)"))+facet_wrap(~ breaks)+theme(plot.title = element_text(lineheight=.8, face="bold"))
  file.date<-as.character(as.POSIXct(start.date.time, format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),format="%Y.%m.%d")
  filename<-paste0(file.date,"_Track.rcs.per.hour.png")
  ggsave(file=filename,plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
}