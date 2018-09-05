ARTI.tracks.per.interval_v1.0<-function(title="", interval=3600, local.time.zone){
#Create a bar chart depicting track counts per user defined time interval
  ##Title: "What you want your graph to be called"
  ##Interval: number of seconds - Default is 1 hour
  ##Valid Time Zones
    ### "US/Eastern", "US/Central", "US/Mountain", "US/Pacific"
  
#v1.0 Time is what we want most, but what we use worst.
  
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

#Define some variables.
  working.directory<-file.path(path.expand("~"),"ARTI_Analysis")
  CSV.to.Process<-"CSV_to_Process"
  CSV.finished<-"CSV_Finished"
  Track.Count<-"Track_Count"
  
#Make some Directories
  setwd(working.directory)
  dir.create(file.path(working.directory,"Data",CSV.finished),showWarnings = FALSE)
  dir.create(file.path(working.directory,"Results",Track.Count),showWarnings = FALSE)
  
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
      p<-ggplot(freqs, aes(x=names, y=x,fill=x)) + geom_bar(stat="identity") + scale_x_datetime(breaks="hour", labels=date_format("%H:%M",tz=local.time.zone))+ scale_fill_gradient2("Track Count",low="dark green",mid="yellow",high="red",limits=c(0,15000),midpoint=5000,space="Lab") + facet_wrap(~breaks2,scales="free_x",nrow=5) 
      final.graph<-p+theme(axis.text.x=element_text(angle=45,hjust=1))+xlab(paste0("Local Time (",local.time.zone,")"))+ylab("Frequency")+ggtitle(paste0(title,"\nTracks Per ",interval," seconds"))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme_bw()
    #Save it
      filename<-paste0(title,"_Tracks.per.",interval,"s.png")
      ggsave(file=file.path(working.directory,"Results",Track.Count,filename),plot=final.graph,width= 8.5,height=11,pointsize=12,units="in")
    #Finish up
      file.rename(from=file.path(working.directory,"Data",CSV.to.Process,filelist),to=file.path(working.directory,"Data",CSV.finished,filelist))
      print("Finished!")
  }
else{print("No data to process. Please add CSV files to the CSV_to_Process directory.")}
}