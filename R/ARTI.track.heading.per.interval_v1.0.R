ARTI.track.heading.per.interval_v1.0<-function(title="", interval=3600, local.time.zone){
#Create graph depicting frequency of average track headings at defined intervals.
  ##Title: "What you want your graph to be called"
  ##Interval: number of seconds - Default is 1 hour
  ##Valid Time Zones
    ### "US/Eastern", "US/Central", "US/Mountain", "US/Pacific"
  
#v1.0 If you do not change direction, you may end up where you are heading.
  
#Check that the required packages are installed.
  if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
  if("bit64" %in% rownames(installed.packages()) == FALSE) {install.packages("bit64")}
  if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
  if("circular" %in% rownames(installed.packages()) == FALSE) {install.packages("circular")}
  if("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}
  
#Load the required packages.
  require(data.table)
  require(bit64)
  require(ggplot2)
  require(circular)
  require(scales)
  
#Define some variables.
  working.directory<-file.path(path.expand("~"),"ARTI_Analysis")
  CSV.to.Process<-"CSV_to_Process"
  CSV.finished<-"CSV_Finished"
  Track.Heading<-"Track_Heading"
  
#Make some Directories
  setwd(working.directory)
  dir.create(file.path(working.directory,"Data",CSV.finished),showWarnings = FALSE)
  dir.create(file.path(working.directory,"Results",Track.Heading),showWarnings = FALSE)
  
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
      data$breaks<-cut(data$Update.Time.Local,seq,include.lowest=T)
      data<-na.exclude(data)
      assign("ARTI Data",data,.GlobalEnv)
    #Circular things
      data$Heading..deg.N.CIRC<-circular(data$Heading..deg.N.,type="angles",units="degrees",template="none",modulo="2pi",rotation="clock")
    #Find average Circular Track Headings
      avg.circular.heading<-setNames(aggregate(data$Heading..deg.N.CIRC ~ data$long.ID, FUN=mean.circular),c("long.ID","AverageCircularHeading.degrees"))
    #Subset the data, only Unique TrackID's
      sub.data<-subset(data,dup==FALSE)  
    #Add average circular heading to dataframe
      sub.data<-merge(sub.data, avg.circular.heading,by="long.ID")
      
    #Plot it!
      p<-ggplot(data=sub.data,aes(x=AverageCircularHeading.degrees))
      final.graph<-p+geom_bar(binwidth=360/24,colour="black")+coord_polar()+scale_x_continuous(breaks=seq(0,360,by=360/12),lim=c(0,360))+xlab("Average Track Heading (degrees)")+facet_wrap(~ breaks)+labs(title=paste0(title,"\nAverage Track Heading"))+theme(plot.title = element_text(lineheight=.8, face="bold")) + theme_bw()
      file.date<-as.character(round.start.date.time,format="%Y.%m.%d")
      filename<-paste0(file.date,"_Avg.Track.heading.per.",interval,".s.png")
      ggsave(file=file.path(working.directory,"Results",Track.Heading,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
    #Finish up
      file.rename(from=file.path(working.directory,"Data",CSV.to.Process,filelist),to=file.path(working.directory,"Data",CSV.finished,filelist))
      print("Finished!")
  }
else{print("No data to process. Please add CSV files to the CSV_to_Process directory.")}
}