ARTI.tracks.per.hour<-function(start.date.time,end.date.time,interval=3600,local.time.zone,title){
#please enter start and and times in the following format: "%Y-%m-%d %H:%M:%S"
  
#Check that the required packages are installed.
  if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
  if("bit64" %in% rownames(installed.packages()) == FALSE) {install.packages("bit64")}
  if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
  if("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}
#Load the required packages.  
  require(data.table)
  require(bit64)
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
  sub.data$breaks<-as.POSIXct(sub.data$breaks)
#Prep the data
  freqs <- aggregate(sub.data$breaks, by=list(sub.data$breaks), FUN=length)
  freqs$names <- as.POSIXct(freqs$Group.1, format="%H:%M")
#Break up data by day for big datasets
  freqs$breaks2<-cut(freqs$Group.1,breaks="day")
#Plot it!
#p<-ggplot(data=na.exclude(sub.data),aes(x=breaks)) + geom_histogram(aes(fill=..count..),binwidth=24) + scale_fill_gradient("Count",low="green",high="red")+facet_wrap(~breaks2,scales="free_x",nrow=4) 
p<-ggplot(freqs, aes(x=names, y=x,fill=x)) + geom_bar(stat="identity") + scale_x_datetime(breaks="hour", labels=date_format("%H:%M"))+ scale_fill_gradient2("Track Count",low="dark green",mid="yellow",high="red",limits=c(0,15000),midpoint=5000) + facet_wrap(~breaks2,scales="free_x",nrow=5) 
final.graph<-p+theme(axis.text.x=element_text(angle=45,hjust=1))+xlab(paste0("Local Time (",local.time.zone,")"))+ylab("Frequency")+ggtitle(paste0(title,"\nTracks Per Hour"))+theme(plot.title = element_text(lineheight=.8, face="bold"))
filename<-"Tracks.per.hour.png"
ggsave(file=filename,plot=final.graph,width= 8.5,height=11,pointsize=12,units="in")
}