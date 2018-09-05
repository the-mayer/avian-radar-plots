ARTI.heading.per.hour<-function(start.date.time,end.date.time,interval=3600,local.time.zone,title){
#please enter start and and times in the following format: "%Y-%m-%d %H:%M:%S"
  require(ggplot2)
  require(circular)
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
#Circular things
  data$Heading..deg.N.CIRC<-circular(data$Heading..deg.N.,units="degrees",template="geographics",modulo="2pi",rotation="clock")
#Subset the data, only Unique TrackID's
  sub.data<-subset(data,dup==FALSE)
#Plot it!
  p<-ggplot(data=na.exclude(data),aes(x=Heading..deg.N.CIRC))
  final.graph<-p+geom_bar(binwidth=360/24,colour="black")+coord_polar()+scale_x_continuous(breaks=seq(0,360,by=360/12),lim=c(0,360))+xlab("Track Update Heading (degrees)")+facet_wrap(~ breaks)+labs(title=paste0(title,"\nTrack Update Heading"))+theme(plot.title = element_text(lineheight=.8, face="bold"))
  file.date<-as.character(as.POSIXct(start.date.time, format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),format="%Y.%m.%d")
  filename<-paste0(file.date,"_Track.heading.per.hour.png")
  ggsave(file=filename,plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
}