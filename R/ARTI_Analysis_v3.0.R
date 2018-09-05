ARTI_Analysis_v3.0 <- function(track.history="Y",track.count="Y",track.heading="Y",track.speed="Y",track.height="Y",track.RCS="Y",track.range="Y",track.intensity="Y",interval=3600,zoom=12,map.type="terrain"){

  #example
  # ARTI_Analysis_v3.0 (track.history="Y",track.count="Y",track.heading="Y",track.speed="Y",track.height="Y",track.RCS="Y",track.range="Y",track.intensity="Y",interval=3600,zoom=12,map.type="terrain")
  
  #Analyze all the things
  ##Interval: number of seconds - Default is 1 hour
  
    if("ggmap" %in% rownames(installed.packages()) == FALSE) {install.packages("ggmap")}
    if("geosphere" %in% rownames(installed.packages()) == FALSE) {install.packages("geosphere")}
  #v3.0 want to add more statistical analysis
  
  #Check that the required packages are installed.
    if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
    if("bit64" %in% rownames(installed.packages()) == FALSE) {install.packages("bit64")}
    if("circular" %in% rownames(installed.packages()) == FALSE) {install.packages("circular")}
    if("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}
    if("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
  
  #Load necessary things
    require(data.table)
    require(bit64)
    require(ggmap)
    require(geosphere)
    require(circular)
    require(scales)
    require(MASS)
  
  #Define some variables.
    working.directory<-file.path(path.expand("~"),"ARTI_Analysis")
    CSV.to.Process<-"CSV_to_Process"
    CSV.Finished<-"CSV_Finished"
    Track.Histories<-"Track_Histories"
    Track.Count<-"Track_Count"
    Track.Heading<-"Track_Heading"
    Track.Speed<-"Track_Speed"
    Track.Height<-"Track_Height"
    Track.RCS<-"Track_RCS"
    Track.Range<-"Track_Range"
    Track.Intensity<-"Track_Intensity"
    
  #Make some Directories
    setwd(working.directory)
    dir.create(file.path(working.directory,"Data",CSV.Finished),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Histories),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Count),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Heading),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Speed),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Height),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.RCS),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Range),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Intensity),showWarnings = FALSE)
    
  #What files are we working with? Accipiter Radar data (updates) in CSV format (preferrably).
    setwd(file.path(working.directory,"Data",CSV.to.Process))
    filelist<-list.files(pattern="\\.csv$")
  #Check if there are files to run... if not don't do anything
    if(length(filelist) >= 1){
      #Find the radar location from the data.
        temp<-read.csv(filelist[1],header=T,nrow=1,stringsAsFactors = F)
        temp$Heading..deg.N.<-circular(temp$Heading..deg.N.,type="angles",units="degrees",template="none",modulo="2pi",rotation="clock")
        
        #More Variables
          lat<- temp$Latitude[1]
          lon<-temp$Longitude[1]
          bearing<- temp$Azimuth.from.radar..deg.. +180
          bearing<-ifelse(bearing >= 360, bearing-360, bearing)
          distance<- temp$Range.from.radar...m.[1]/1000 #Distance from radar in km
          rad<-pi/180
        #Do some Math  
          a1<-lat*rad
          a2<-lon*rad
          tc<-bearing*rad
          d<- distance/6378.145
          nlat <- asin(sin(a1) * cos(d) + cos(a1) * sin(d) * cos(tc))
          dlon <- atan2(sin(tc) * sin(d) * cos(a1), cos(d) - sin(a1) * sin(nlat))
          nlon <- ((a2 + dlon + pi)%%(2 * pi)) - pi
          radar.location <- data.frame(lon = nlon/rad, lat = nlat/rad)
          assign("radar.location",radar.location,.GlobalEnv)
      #Here is where we have radars!
        ifelse(round(radar.location$lon,digits=2) == round(-97.047128,digits=2) & round(radar.location$lat,digits = 2) == round(32.864114,digits=2),site.name<-"DFW_JRC",print("Thinking"))  
        ifelse(round(radar.location$lon,digits=2) == round(-122.31577,digits=2) & round(radar.location$lat,digits = 2) == round(47.45204,digits=2),site.name<-"SEA_Olympic",print("Thinking"))  
        ifelse(round(radar.location$lon,digits=2) == round(-122.30943,digits=2) & round(radar.location$lat,digits = 2) == round(47.44973,digits=2),site.name<-"SEA_Cascades",print("Thinking")) 
        ifelse(round(radar.location$lon,digits=2) == round(-122.300511,digits=2) & round(radar.location$lat,digits = 2) == round(47.441712,digits=2),site.name<-"SEA_Roof",print("Thinking"))
        ifelse(round(radar.location$lon,digits=2) == round(-122.66681,digits=2) & round(radar.location$lat,digits = 2) == round(48.35510,digits=2),site.name<-"Whidbey",print("Thinking"))
        if(exists("site.name")== FALSE){site.name<-"Unknown Location"}
        
        #ifelse(site.name == "DFW_JRC",radar.location <- data.frame(lon = -97.047128 , lat =  32.864114),print("Thinking..."))
        #ifelse(site.name == "SEA_Olympic",radar.location <- data.frame(lon = -122.31577 , lat =  47.45204),print("Thinking..."))
        #ifelse(site.name == "SEA_Cascades",radar.location <- data.frame(lon = -122.30943 , lat =  47.44973),print("Thinking..."))
        #ifelse(site.name == "SEA_Roof",radar.location <- data.frame(lon = -122.300511 , lat =  47.441712),print("Thinking..."))
      
      #Here's something new... look up the local timezone!
        url<-paste("http://ws.geonames.org/timezone?lat=",radar.location$lat,"&lng=",radar.location$lon,"&style=full","&username=herricks.group",sep="")
        u<-url(url,open="r")
        text<-readLines(u)
        local.time.zone <- text[grep("timezoneId",text)]
        local.time.zone <- sub("<timezoneId>","",local.time.zone)
        local.time.zone <- sub("</timezoneId>","",local.time.zone)
        close(u)
        

    #Load up all the Accipiter Data  
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
    #Circular things
      data$Heading..deg.N.CIRC<-circular(data$Heading..deg.N.,type="angles",units="degrees",template="none",modulo="2pi",rotation="clock")
    #Calculate the antenna angle, why not?
      data$antenna.angle<-round(atan2(data$Height...m.,data$Range.from.radar...m.)*180/pi,digits=0)
    #Find individual tracks, for counts
      data$dup<-duplicated(data$long.ID)
    #Determine sequence to cut the data by.
      seq<-seq(round.start.date.time,round.end.date.time,by=interval)
    #Make Sure the time sequence has more than 1 break point.
      if(length(seq) > 1){
      #Break up the data based on the interval
        data$breaks<-cut(data$Update.Time.Local,seq)
        data<-na.exclude(data)
        assign("ARTI Data",data,.GlobalEnv)
      
        if(track.history=="Y"){  
          #Track Histories
          print("Creating Track Histories")
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
                p7<-p6 + ggtitle(paste0(site.name," Track History \nLocal Timezone: ",local.time.zone))+xlab("Longitude")+ylab("Latitude") #Add title and axis labels
                p8<-p7 + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end))+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm)  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
                p9<-p8 + facet_wrap(~breaks,drop=T)+theme(axis.text=element_text(size=14,face="bold"),legend.position="none") #Facet and add theme
              #Filename for the maps
                file.datetime1<-as.character(round.start.date.time+(x*interval)-interval,format="%Y.%m.%d_%H%M")
                file.datetime2<-as.character(round.start.date.time+(x*interval),format="%H%M")
                file.time.string<-paste0(file.datetime1,"-",file.datetime2)
                filename<-paste0(site.name,"_",file.time.string,"_",map.type,zoom,"_Track.History.png")
              #Save it!
                ggsave(file=file.path(working.directory,"Results","Track_Histories",filename),plot=p9,width= 8.5,height=11,pointsize=12,units="in")
                setTxtProgressBar(pb,x)
              } }
        if(track.count=="Y"){
          print("Generating track counts...")
          #Subset the data, only Unique TrackID's
            sub.data<-subset(data,dup==FALSE)
            sub.data$breaks<-as.POSIXct(sub.data$breaks,tz=local.time.zone)
          #Prep the data
            freqs <- aggregate(sub.data$breaks, by=list(sub.data$breaks), FUN=length)
            freqs$names <- as.POSIXct(freqs$Group.1, format="%H:%M")
          #Break up data by day for big datasets
            freqs$breaks2<-cut(freqs$Group.1,breaks="day")
          #Plot it!
            #p<-ggplot(data=na.exclude(sub.data),aes(x=breaks)) + geom_histogram(aes(fill=..count..),binwidth=24) + scale_fill_gradient("Count",low="green",high="red")+facet_wrap(~breaks2,scales="free_x",nrow=4) 
            #names(freqs)
            p<-ggplot(freqs, aes(x=names, y=x,fill=x)) + geom_bar(stat="identity") + scale_x_datetime(date_breaks="1 hour", labels=date_format("%H:%M",tz=local.time.zone))+ scale_fill_gradient2("Track Count",low="dark green",mid="yellow",high="red",limits=c(0,15000),midpoint=5000,space="Lab") + facet_wrap(~breaks2,scales="free_x",nrow=5)
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2011/09/19 06:51","2011/09/19 19:13"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2011/09/28 07:03","2011/09/28 18:55"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2011/10/13 07:24","2011/10/13 18:25"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2011/10/13 18:25"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2011/10/23 07:39","2011/10/23 18:06"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2011/11/05 07:58","2011/11/05 17:46"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2011/11/19 07:19","2011/11/19 16:28"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2009/09/27 07:03","2009/09/27 18:59"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2009/10/02 07:10","2009/10/02 18:45"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2009/10/27 07:45","2009/10/27 17:59"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2009/11/03 06:56","2009/11/03 16:48"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            #p <- p + geom_vline(xintercept= as.numeric(as.POSIXct(c("2009/11/12 07:10","2009/11/12 16:36"), format="%Y/%m/%d %H:%M", tz=local.time.zone)), colour="red")
            final.graph<-p+ xlab(paste0("Local Time (",local.time.zone,")"))+ylab("Frequency")+ggtitle(paste0(site.name,"\nTracks Per ",interval," seconds"))+theme(plot.title = element_text(lineheight=.8, face="bold")) + theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1))+ theme(strip.text.x = element_text(size = 8))
            #Save it
            file.date<-as.character(round.start.date.time,format="%Y.%m.%d")
            filename<-paste0(file.date,"_",site.name,"_Tracks.per.",interval,"s.png")
            ggsave(file=file.path(working.directory,"Results",Track.Count,filename),plot=final.graph,width= 8.5,height=11,pointsize=12,units="in")
            write.table(freqs,file=file.path(working.directory,"Results",Track.Count,"freqs.txt"))
        }
        if(track.heading=="Y"){
          print("Calculating average track heading...")
          #Find average Circular Track Headings
            avg.circular.heading<-setNames(aggregate(data$Heading..deg.N.CIRC ~ data$long.ID, FUN=mean.circular),c("long.ID","AverageCircularHeading.degrees"))
          #Subset the data, only Unique TrackID's
            sub.data<-subset(data,dup==FALSE)  
          #Add average circular heading to dataframe
            sub.data<-merge(sub.data, avg.circular.heading,by="long.ID")
          
          #Plot it!
            p<-ggplot(data=sub.data,aes(x=AverageCircularHeading.degrees))
            final.graph<-p+geom_bar(binwidth=360/24,colour="black")+coord_polar()+scale_x_continuous(breaks=seq(0,360,by=360/12),lim=c(0,360))+xlab("Average Track Heading (degrees)") + ylab("Frequency") + facet_wrap(~ breaks)+ggtitle(paste0(site.name,": Average Track Heading","\nLocal Timezone: ",local.time.zone))+ theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8)) + theme(axis.text.x = element_text(size=5))
          #Save it!  
            file.date<-as.character(round.start.date.time,format="%Y.%m.%d")
            filename<-paste0(file.date,"_",site.name,"_Avg.Track.heading.per.",interval,".s.png")
            ggsave(file=file.path(working.directory,"Results",Track.Heading,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
        }
        if(track.speed=="Y"){
          print("Calculating average track speed...")
          #Find average track speed
            avg.speed<-setNames(aggregate(data$Speed..m.s. ~ data$long.ID, FUN=mean),c("long.ID","AverageSpeed.m.s"))
          #Subset the data, only Unique TrackID's
            sub.data<-subset(data,dup==FALSE)
          #Add average speed to dataframe
            sub.data<-merge(sub.data, avg.speed,by="long.ID")
          #Convert speed to MPH
            sub.data$Speed..mph<-sub.data$AverageSpeed.m.s*2.23694
          #Plot it!
            #p<-ggplot(data=sub.data,aes(x=Speed..mph))+geom_histogram(binwidth=1)
            #p<-ggplot(data=sub.data,aes(x=Speed..mph))+geom_histogram(binwidth=1)+xlim(c(0,100))
            #p<-ggplot(data=sub.data,aes(x=Speed..mph))+geom_histogram(binwidth=1)+xlim(c(20,40))
            p<-ggplot(data=sub.data,aes(x=Speed..mph))+geom_histogram(binwidth=1)+xlim(c(0,100))
            final.graph<-p+xlab("Average Track Speed (mph)") + ylab("Frequency") + ggtitle(paste0(site.name,": Average Track Speed (mph)","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks) + theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8))
            file.date<-as.character(as.POSIXct(start.date.time, format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),format="%Y.%m.%d")
            filename<-paste0(file.date,"_",site.name,"_Avg.Track.speed.per.",interval,"s.png")
            ggsave(file=file.path(working.directory,"Results",Track.Speed,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
            #write.table(sub.data,file=file.path(working.directory,"Results",Track.Speed,"speed.txt"))
        }
        
        if(track.height=="Y"){
          print("Calculating average track height...")
          #Find average track height
          avg.height<-setNames(aggregate(data$Height...m. ~ data$long.ID, FUN=mean),c("long.ID","AverageHeight.m"))
          #Subset the data, only Unique TrackID's
          sub.data<-subset(data,dup==FALSE)
          #Add average height to dataframe
          sub.data<-merge(sub.data, avg.height,by="long.ID")
          #Plot it!
          #p<-ggplot(data=sub.data,aes(x=AverageHeight.m))+geom_histogram(binwidth=10)+xlim(c(0,1000))
          p<-ggplot(data=sub.data,aes(x=AverageHeight.m))+geom_histogram(binwidth=10)+xlim(c(0,800))
          #p<-ggplot(data=sub.data,aes(x=AverageHeight.m))+geom_histogram(binwidth=10)+xlim(c(0,500))
          #p<-ggplot(data=sub.data,aes(x=AverageHeight.m))+geom_histogram(binwidth=10)+xlim(c(0,100))
          final.graph<-p+xlab("Average Track Height (m)") + ylab("Frequency") + ggtitle(paste0(site.name,": Average Track Height (m)","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks) + theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8))
          file.date<-as.character(as.POSIXct(start.date.time, format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),format="%Y.%m.%d")
          filename<-paste0(file.date,"_",site.name,"_Avg.Track.height.per.",interval,"s.png")
          ggsave(file=file.path(working.directory,"Results",Track.Height,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
          #write.table(sub.data,file=file.path(working.directory,"Results",Track.Height,"height.txt"))
        }
        
        if(track.RCS=="Y"){
          print("Calculating average track RCS...")
          #Find average track RCS
          avg.RCS<-setNames(aggregate(data$RCS..dBsm. ~ data$long.ID, FUN=mean),c("long.ID","AverageRCS.dBsm"))
          #Subset the data, only Unique TrackID's
          sub.data<-subset(data,dup==FALSE)
          #Add average RCS to dataframe
          sub.data<-merge(sub.data, avg.RCS,by="long.ID")
          #Plot it!
          p<-ggplot(data=sub.data,aes(x=AverageRCS.dBsm))+geom_histogram(binwidth=1)
          final.graph<-p+xlab("Average Track RCS (dBsm)") + ylab("Frequency") + ggtitle(paste0(site.name,": Average Track RCS (dBsm)","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks) + theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8))
          file.date<-as.character(as.POSIXct(start.date.time, format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),format="%Y.%m.%d")
          filename<-paste0(file.date,"_",site.name,"_Avg.Track.RCS.per.",interval,"s.png")
          ggsave(file=file.path(working.directory,"Results",Track.RCS,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
          #write.table(sub.data,file=file.path(working.directory,"Results",Track.RCS,"RCS.txt"))
        }
        
        if(track.range=="Y"){
          print("Calculating average track range from radar...")
          #Find average track Range
          avg.range<-setNames(aggregate(data$Range.from.radar...m. ~ data$long.ID, FUN=mean),c("long.ID","AverageRange.m"))
          #Subset the data, only Unique TrackID's
          sub.data<-subset(data,dup==FALSE)
          #Add average RCS to dataframe
          sub.data<-merge(sub.data, avg.range,by="long.ID")
          #Plot it!
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(breaks=seq(1000,10000),by=100,col="red",fill="green",alpha=.2)
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(breaks=seq(0,5000),by=10,col="red",fill="green",alpha=.2)
          p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(0,5000))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(0,1000))
          #DifferentRangeBins
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(0,1000))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(500,2000))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(1500,2500))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(2000,3500))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(3000,4500))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(3500,5000))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(4500,6000))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(5500,6500))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(6000,7500))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(7000,8500))
          #p<-ggplot(data=sub.data,aes(x=AverageRange.m))+geom_histogram(binwidth=100)+xlim(c(3000,8500))
          final.graph<-p+xlab("Average Track Range from Radar (m)") + ylab("Frequency") + ggtitle(paste0(site.name,": Average Track Range (m)","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks) + theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8))
          file.date<-as.character(as.POSIXct(start.date.time, format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),format="%Y.%m.%d")
          filename<-paste0(file.date,"_",site.name,"_Avg.Track.Range.per.",interval,"s.png")
          ggsave(file=file.path(working.directory,"Results",Track.Range,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
          #write.table(sub.data,file=file.path(working.directory,"Results",Track.Range,"range.txt"))
        }
        if(track.intensity=="Y"){
          print("Calculating average track intensity...")
          #Find average track Intensity
          avg.intensity<-setNames(aggregate(data$Intensity ~ data$long.ID, FUN=mean),c("long.ID","AverageIntensity"))
          #Subset the data, only Unique TrackID's
          sub.data<-subset(data,dup==FALSE)
          #Add average RCS to dataframe
          sub.data<-merge(sub.data, avg.intensity,by="long.ID")
          #Plot it!
          p<-ggplot(data=sub.data,aes(x=AverageIntensity))+geom_histogram(binwidth=10)
          final.graph<-p+xlab("Average Track Intensity") + ylab("Frequency") + ggtitle(paste0(site.name,": Average Track Intensity","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks) + theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8))
          file.date<-as.character(as.POSIXct(start.date.time, format="%Y-%m-%d %H:%M:%S",tz=local.time.zone),format="%Y.%m.%d")
          filename<-paste0(file.date,"_",site.name,"_Avg.Track.Intensity.per.",interval,"s.png")
          ggsave(file=file.path(working.directory,"Results",Track.Intensity,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
        }
file.rename(from=file.path(working.directory,"Data",CSV.to.Process,filelist),to=file.path(working.directory,"Data",CSV.Finished,filelist))
    print("Finished!")
      }
      else{print("Not enough data for specified interval. Shorten interval or add more data.")}
  }
  else{print("No data to process. Please add CSV files to the CSV_to_Process directory.")}
}