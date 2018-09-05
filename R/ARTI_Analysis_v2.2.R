ARTI_Analysis_v2.2 <- function(radar.designation,track.history="Y",track.count="Y",track.heading="Y",track.speed="Y",track.altitude="Y",antenna.angle="Y",track.RCS="Y",track.range="Y",interval,zoom=12,map.type="terrain"){

  # ARTI_Analysis_v2.2("SeaTac_Olympic","N","Y","N","N","N","N","N","N",interval=3600,zoom=12,map.type="terrain")
  #Analyze all the things
  ##Interval: number of seconds - Default is 1 hour, 3600, 86400
  #v2.1 Changes to heading graphs, and reduced file size for track histories.
  #v2.0 What could go wrong? -Dave Mayer
  #Consists of:
    #v1.0 There's just something hypnotic about maps. (auto.ARTI.history_v1.0.R)
    #v1.0 If you do not change direction, you may end up where you are heading. (ARTI.track.heading.per.interval_v1.0.R)
    #v1.0 Time is what we want most, but what we use worst. (ARTI.tracks.per.interval_v1.0.R)
  
  #Check that the required packages are installed.
    if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
    if("bit64" %in% rownames(installed.packages()) == FALSE) {install.packages("bit64")}
    if("ggmap" %in% rownames(installed.packages()) == FALSE) {install.packages("ggmap")}
    if("geosphere" %in% rownames(installed.packages()) == FALSE) {install.packages("geosphere")}
    if("circular" %in% rownames(installed.packages()) == FALSE) {install.packages("circular")}
    if("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}
    if("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
    if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
    if("ggthemes" %in% rownames(installed.packages()) == FALSE) {install.packages("ggthemes")}
  
  #Load necessary things
    require(data.table)
    require(bit64)
    require(ggmap)
    require(geosphere)
    require(circular)
    require(scales)
    require(MASS)
    require(grid)
    require(ggthemes)
  
  #Define some variables.
    working.directory<-file.path(path.expand("~"),"ARTI_Analysis")
    CSV.to.Process<-"CSV_to_Process"
    CSV.finished<-"CSV_Finished"
    Track.Histories<-"Track_Histories"
    Track.Count<-"Track_Count"
    Track.Heading<-"Track_Heading"
    Track.Speed<-"Track_Speed"
    Track.Altitude<-"Track_Altitude"
    Antenna.Angle<-"Antenna_Angle"
    Track.RCS<-"Track_RCS"
    Track.Range<-"Track_Range"
    
  #Make some Directories
    setwd(working.directory)
    dir.create(file.path(working.directory,"Data",CSV.finished),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Histories),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Count),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Heading),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Speed),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Altitude),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Antenna.Angle),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.RCS),showWarnings = FALSE)
    dir.create(file.path(working.directory,"Results",Track.Range),showWarnings = FALSE)
    
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
      #Where are you?
          site.name<-revgeocode(c(radar.location$lon,radar.location$lat))
      
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
      setnames(data, c(arti.header),c("Update.Time", "Track.ID", "Start.Time..UTC.", "Latitude", "Longitude", "Speed..m.s.", "Heading..deg.N.", "Height...m.", "RCS..dBsm.", "Range.from.radar...m.", "Azimuth.from.radar..deg..", "Intensity", "UTM.Zone", "UTM.Northing", "UTM.Easting"))
    
    #Make the time correct
      data$Update.Time<-as.POSIXct(data$Update.Time,format="%Y/%m/%d %H:%M:%S",tz="UTC")
      data$Update.Time.Local<-format(data$Update.Time,tz=local.time.zone)
      data$Update.Time.Local<-as.POSIXct(data$Update.Time.Local,tz=local.time.zone)
    #Find out how much data you have, in local time.
      start.date.time<-min(data$Update.Time.Local)
      end.date.time<-max(data$Update.Time.Local)
    #Round the start and end times of the data to the nearest hour.
      round.start.date.time<- as.POSIXct(format(round(start.date.time, units="hours")),tz=local.time.zone)
      round.end.date.time<- as.POSIXct(format(round(end.date.time, units="hours")),tz=local.time.zone)
    #Add a unique ID
      data$long.ID<-paste0(data$Track.ID,data$Start.Time..UTC.)
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
      #Break up data by day for big datasets
        data$breaks2<-cut(data$Update.Time.Local,breaks="DSTday")
        data$breaks.day<- as.POSIXct(data$breaks2)
      #Break up data by week for really big datasets
        data$breaks3<-cut(data$breaks.day,breaks="week",start.on.monday=FALSE)
        data<-na.exclude(data)
        data<-data[order(Update.Time.Local),]
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
                sub.data<- sub.data[order(Update.Time.Local),]
                sub.track.max.time.data<-subset(track.max.time.data, as.integer(breaks)==x)
                #Make a title for your graphic
                  graph.title<-paste0("CEAT Track History\n","Location: ",site.name,"\nRadar Designation: ",radar.designation,"\nLocal Timezone: ",local.time.zone)
                  #grob.title <- grobTree(textGrob(graph.title, x=bb$ll.lon,  y=bb$ur.lat, hjust=0,vjust=1, gp=gpar(col="black", fontsize=6, fontface="bold")))
                p<- ggmap(map, extent = "normal", maprange=FALSE) %+% sub.data + aes(x = radar.location$lon, y = radar.location$lat) #Plot the map
                
                
                # Marked by Jia for changing track colors
                # Link to define colors in R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
                p1<-p  + geom_path(data=sub.data,aes(x=Longitude,y=Latitude,group=long.ID),color="chartreuse1",size=.15,alpha=0.7, na.rm = T) #Plot the Track Lines
                p2<-p1 + geom_point(data=sub.track.max.time.data,aes(x=Longitude,y=Latitude),color="chartreuse1",size=.65,alpha=0.7, na.rm = T) #Plot the Track Direction Indicators
                
                
                p3<-p2 + geom_density2d(data = sub.data, aes(x = Longitude, y = Latitude),size=0.1, na.rm=T) #Plot the density contours
                p4<-p3 + stat_density2d(data = sub.data, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon',na.rm=T) #Add Rransparent fill to density contours
                ifelse(nrow(sub.data) >0, p5<-p4 + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.25), guide = FALSE),p5<-p4)
                p6<-p5 + geom_point(x=radar.location$lon,y=radar.location$lat, shape=10, colour="blue", size=1) #Add Radar Location
                p7<-p6 + annotate("text",x=bb$ll.lon,y=bb$ur.lat,hjust=0,vjust=1,size=2,fontface="bold",label=graph.title) #Add title
                p8<-p7 + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end))+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm)  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
                #ifelse(nrow(sub.data)>0,p9<-p8 + facet_wrap(~breaks,drop=T)+theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none",panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),legend.position="none"),p9<-p8+theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none",panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),legend.position="none")+annotate("text",x=bb$ll.lon,y=bb$ur.lat,hjust=0,vjust=1,size=2,fontface="bold",label=paste0(graph.title,"\nNo Data Recorded"))) #Facet and add theme
                ifelse(nrow(sub.data)>0,p9<-p8 + facet_wrap(~breaks,drop=T)+theme_map(),p9<-p8+theme_map() + annotate("text",x=bb$ll.lon,y=bb$ur.lat,hjust=0,vjust=1,size=2,fontface="bold",label=paste0(graph.title,"\nNo Data Recorded"))) #Facet and add theme 
                #Filename for the maps
                file.datetime1<-as.character(round.start.date.time+(x*interval)-interval,format="%Y.%m.%d_%H%M")
                file.datetime2<-as.character(round.start.date.time+(x*interval),format="%H%M")
                file.time.string<-paste0(file.datetime1,"-",file.datetime2)
                filename<-paste0(file.time.string,"_",radar.designation,"_",map.type,zoom,"_Track.History.jpeg")
              #Save it!
                ggsave(file=file.path(working.directory,"Results","Track_Histories",filename),plot=p9,width= 5,height=5,pointsize=12,units="in")
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
            freqs$breaks2<- cut.POSIXt(freqs$Group.1,breaks="DSTday")
            freqs$breaks.day<- as.POSIXct(freqs$breaks2)
            freqs$breaks3<-cut(freqs$breaks.day,breaks="week",start.on.monday=FALSE)
          #Plot it!
          for(y in 1:length(unique(freqs$breaks3))){
            sub.freqs<-subset(freqs,as.integer(breaks3) == y)
          #p<-ggplot(data=na.exclude(sub.data),aes(x=breaks)) + geom_histogram(aes(fill=..count..),binwidth=24) + scale_fill_gradient("Count",low="green",high="red")+facet_wrap(~breaks2,scales="free_x",nrow=4) 
            p<-ggplot(sub.freqs, aes(x=names, y=x,fill=x)) + geom_bar(stat="identity") + scale_x_datetime(date_breaks="1 hour", labels=date_format("%H:%M",tz=local.time.zone))+ scale_fill_gradient2("Track Count",low="dark green",mid="yellow",high="chartreuse1",limits=c(0,15000),midpoint=5000,space="Lab") + facet_wrap(~breaks2,scales="free_x",ncol=2) 
            intermediate.graph<-p+ xlab(paste0("Local Time (",local.time.zone,")"))+ylab("Frequency")+ggtitle(paste0(radar.designation,"\nTracks Per ",interval," seconds"))+ theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(axis.text.x=element_text(angle=45,hjust=1,size=6)) + theme(axis.text.y = element_text(size=6)) + theme(strip.text.x = element_text(size = 8))
            final.graph<-intermediate.graph + geom_text(data=sub.freqs,aes(x=names,y=x,label=x),vjust=0,size=1.5) + element_blank() + theme(panel.grid.major = element_line(size = .5, color = "grey"),axis.line = element_line(size=.7, color = "black"))
          #Save it
            file.date<-as.character(as.POSIXct(sub.freqs$breaks3[1]),format="%Y.%m.%d")
            filename<-paste0(file.date,"_",radar.designation,"_Tracks.per.",interval,"s.png")
            ggsave(file=as.character(file.path(working.directory,"Results",Track.Count,filename)),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
            write.table(sub.freqs,file=as.character(file.path(working.directory,"Results","Track_Count","freqs.txt")))
            }
        }
        if(track.heading=="Y"){
          print("Calculating average track heading...")
          
          #Show some progress
            pb<- txtProgressBar(min=0,max=length(unique(data$breaks2)),style=3)
            setTxtProgressBar(pb,0)
          for(x in 1:length(unique(data$breaks2))){  
            sub.data<-subset(data, as.integer(breaks2)==x)
            if(nrow(sub.data)>0){
            #Find average Circular Track Headings
              avg.circular.heading<-setNames(aggregate(sub.data$Heading..deg.N.CIRC ~ sub.data$long.ID, FUN=mean.circular),c("long.ID","AverageCircularHeading.degrees"))
            #Subset the data, only Unique TrackID's
              sub.data<-subset(sub.data,dup==FALSE)  
            #Add average circular heading to dataframe
              sub.data<-merge(sub.data, avg.circular.heading,by="long.ID")
              sub.data<-sub.data[order(Update.Time.Local),]
            #Massage the data a bit
              sub.data$AverageCircularHeading.degrees.2<-ifelse(sub.data$AverageCircularHeading.degrees >= 352.5, -1*(360-sub.data$AverageCircularHeading.degrees), sub.data$AverageCircularHeading.degrees)
            #Cut the heading data into 15 degree increments
              heading.seq<-seq(-7.5,352.5,by=15)
              heading.labels<-seq(0,345,by=15)
              sub.data$heading.break<-cut(sub.data$AverageCircularHeading.degrees.2, breaks = heading.seq,labels = heading.labels)
            #Plot it!
              p<-ggplot(data=na.omit(sub.data),aes(x=heading.break))
              final.graph<-p+geom_bar(color="black")+coord_polar(start = -7.5*pi/180)+xlab("Average Track Heading (degrees)") + ylab("Frequency") + facet_wrap(~ breaks,ncol=6)+ggtitle(paste0(radar.designation,": Average Track Heading","\nLocal Timezone: ",local.time.zone))+ theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8)) + theme(axis.text.x = element_text(size=5)) + theme(axis.text.y = element_text(size=6))+ theme(panel.grid.major = element_line(size = .5, color = "grey"),axis.line = element_line(size=.7, color = "black"))
            #Save it!  
              file.date<-as.character(unique(as.POSIXct(sub.data$breaks2,local.time.zone)),format="%Y.%m.%d")
              filename<-paste0(file.date,"_",radar.designation,"_Avg.Track.heading.per.",interval,".s.png")
              ggsave(file=file.path(working.directory,"Results",Track.Heading,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
            }
            else{ 
              file.date<-as.character(unique(as.POSIXct(data$breaks2[x],local.time.zone)),format="%Y.%m.%d")
              no.data.filename<-paste0(file.date,"_NO.DATA_",radar.designation,"_Avg.Track.heading.per.",interval,".s.csv")
              write.csv(file = file.path(working.directory,"Results",Track.Heading,no.data.filename),sub.data,row.names = F)
            }
              setTxtProgressBar(pb,x)
            }
          }
        if(track.speed=="Y"){
          print("Calculating average track speed...")
          #Show some progress
            pb<- txtProgressBar(min=0,max=length(unique(data$breaks2)),style=3)
            setTxtProgressBar(pb,0)
          for(x in 1:length(unique(data$breaks2))){
            sub.data<-subset(data, as.integer(breaks2)==x)
            if(nrow(sub.data)>0){
            #Find average track speed
              avg.speed<-setNames(aggregate(sub.data$Speed..m.s. ~ sub.data$long.ID, FUN=mean),c("long.ID","AverageSpeed.m.s"))
            #Subset the data, only Unique TrackID's
              sub.data<-subset(sub.data,dup==FALSE)
            #Add average speed to dataframe
              sub.data<-merge(sub.data, avg.speed,by="long.ID")
              sub.data<-sub.data[order(Update.Time.Local),]
            #Convert speed to MPH
              sub.data$Speed..mph<-sub.data$AverageSpeed.m.s*2.23694
            #Plot it!
              p<-ggplot(data=sub.data,aes(x=Speed..mph))+geom_histogram(binwidth=1)
              final.graph<-p+xlab("Average Track Speed (mph)") +scale_x_continuous(breaks=seq(0,100,by=10),lim=c(0,100))+ ylab("Frequency") + ggtitle(paste0(radar.designation,": Average Track Speed (mph)","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks,ncol=6) + theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1,size=6)) + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8)) + theme(axis.text.y = element_text(size=6))+ theme(panel.grid.major = element_line(size = .5, color = "grey"),axis.line = element_line(size=.7, color = "black"))
            #Save it!
              file.date<-as.character(unique(as.POSIXct(sub.data$breaks2,local.time.zone)),format="%Y.%m.%d")
              filename<-paste0(file.date,"_",radar.designation,"_Avg.Track.speed.per.",interval,"s.png")
              ggsave(file=file.path(working.directory,"Results",Track.Speed,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
            }
            else{
              file.date<-as.character(unique(as.POSIXct(data$breaks2[x],local.time.zone)),format="%Y.%m.%d")
              no.data.filename<-paste0(file.date,"_NO.DATA_",radar.designation,"_Avg.Track.speed.per.",interval,"s.csv")
              write.csv(file = file.path(working.directory,"Results",Track.Speed,no.data.filename),sub.data,row.names = F)
            }
              setTxtProgressBar(pb,x)
              }
        }
        if(track.altitude=="Y"){
          print("Calculating average track altitude...")
          
          #Show some progress
          pb<- txtProgressBar(min=0,max=length(unique(data$breaks2)),style=3)
          setTxtProgressBar(pb,0)
          for(x in 1:length(unique(data$breaks2))){
            sub.data<-subset(data, as.integer(breaks2)==x)
            if(nrow(sub.data)>0){
            #Find average track speed
              avg.alt<-setNames(aggregate(sub.data$Height...m. ~ sub.data$long.ID, FUN=mean),c("long.ID","AverageHeight.m"))
            #Subset the data, only Unique TrackID's
              sub.data<-subset(sub.data,dup==FALSE)
            #Add average speed to dataframe
              sub.data<-merge(sub.data, avg.alt,by="long.ID")
              sub.data<-sub.data[order(Update.Time.Local),]
            #Convert speed to MPH
              sub.data$Height..ft<-sub.data$AverageHeight.m*3.28084
            #Plot it!
              p<-ggplot(data=sub.data,aes(x=Height..ft))+geom_histogram(binwidth=100)
              final.graph<-p+xlab("Average Track Altitude (ft)") + ylab("Frequency") + coord_flip()+ ggtitle(paste0(radar.designation,": Average Track Altitude (ft)","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks,ncol=6) + theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1,size=6)) + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8)) + theme(axis.text.y = element_text(size=6))+ theme(panel.grid.major = element_line(size = .5, color = "grey"),axis.line = element_line(size=.7, color = "black"))
            #Save it!
              file.date<-as.character(unique(as.POSIXct(sub.data$breaks2,local.time.zone)),format="%Y.%m.%d")
              filename<-paste0(file.date,"_",radar.designation,"_Avg.Track.alt.per.",interval,"s.png")
              ggsave(file=file.path(working.directory,"Results",Track.Altitude,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
            }
            else{
              file.date<-as.character(unique(as.POSIXct(data$breaks2[x],local.time.zone)),format="%Y.%m.%d")
              no.data.filename<-paste0(file.date,"_NO.DATA_",radar.designation,"_Avg.Track.alt.per.",interval,"s.csv")
              write.csv(file=file.path(working.directory,"Results",Track.Altitude,filename),sub.data,row.names = F)
            }
            
            setTxtProgressBar(pb,x)
          }
        }
        if(antenna.angle=="Y"){
          print("Determining antenna angle...")
          for(z in 1:length(unique(data$breaks3))){
            sub.data<-subset(data,as.integer(breaks3)==z)
          #Plot it!
            p<-ggplot(data=sub.data,aes(x=Update.Time.Local, y=antenna.angle)) 
            final.graph<-p + geom_line(size=.25)+facet_wrap(~ breaks2,ncol=2,scales="free_x") + theme_bw() + xlab(paste0("Local Time (",local.time.zone,")")) + ylab ("Antenna Angle (degrees)") +ggtitle(paste0(radar.designation,"\nAntenna Angle (degrees)"))+ scale_x_datetime(date_breaks="1 hour", labels=date_format("%H:%M",tz=local.time.zone)) +scale_y_continuous(breaks=seq(0,100,by=5))+ theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(axis.text.x=element_text(angle=45,hjust=1,size=6)) + theme(axis.text.y = element_text(size=6))+ theme(panel.grid.major = element_line(size = .5, color = "grey"),axis.line = element_line(size=.7, color = "black"))
            #Save it!
          #Save it
            file.date<-as.character(as.POSIXct(sub.data$breaks3[1]),format="%Y.%m.%d")
            filename<-paste0(file.date,"_",radar.designation,"_Antenna_Angle.png")
            ggsave(file=file.path(working.directory,"Results",Antenna.Angle,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
            }
        }
        if(track.RCS=="Y"){
          print("Calculating average track RCS...")
          #Show some progress
          pb<- txtProgressBar(min=0,max=length(unique(data$breaks2)),style=3)
          setTxtProgressBar(pb,0)
          for(x in 1:length(unique(data$breaks2))){
            sub.data<-subset(data, as.integer(breaks2)==x)
            if(nrow(sub.data)>0){
              #Find average track RCS
              avg.rcs<-setNames(aggregate(sub.data$RCS..dBsm. ~ sub.data$long.ID, FUN=mean),c("long.ID","AverageRCS.dBsm"))
              #Subset the data, only Unique TrackID's
                sub.data<-subset(sub.data,dup==FALSE)
              #Add average RCS to dataframe
                sub.data<-merge(sub.data, avg.rcs,by="long.ID")
                #sub.data<-subset(sub.data, sub.data$RCS..dBsm.<=0)
                sub.data<-sub.data[order(Update.Time.Local),]
                #Plot it!
                p<-ggplot(data=sub.data,aes(x=AverageRCS.dBsm))+geom_histogram(binwidth=1)
                final.graph<-p+xlab("Average Track RCS (dBsm)") + ylab("Frequency") + ggtitle(paste0(radar.designation,": Average Track RCS (dBsm)","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks,ncol=6) + theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1,size=6)) + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8)) + theme(axis.text.y = element_text(size=6))+ theme(panel.grid.major = element_line(size = .5, color = "grey"),axis.line = element_line(size=.7, color = "black"))
                #final.graph<-p+xlab("Average Track RCS (dBsm)") + scale_x_continuous(breaks=seq(-40,0,by=1),lim=c(-40,0))+ylab("Frequency") + ggtitle(paste0(radar.designation,": Average Track RCS (dBsm)","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks,ncol=6) + theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1,size=6)) + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8)) + theme(axis.text.y = element_text(size=6))+ theme(panel.grid.major = element_line(size = .5, color = "grey"),axis.line = element_line(size=.7, color = "black"))
              #Save it!
                file.date<-as.character(unique(as.POSIXct(sub.data$breaks2,local.time.zone)),format="%Y.%m.%d")
                filename<-paste0(file.date,"_",radar.designation,"_Avg.Track.RCS.per.",interval,"s.png")
                ggsave(file=file.path(working.directory,"Results",Track.RCS,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
            }
            else{
                file.date<-as.character(unique(as.POSIXct(data$breaks2[x],local.time.zone)),format="%Y.%m.%d")
                no.data.filename<-paste0(file.date,"_NO.DATA_",radar.designation,"_Avg.Track.RCS.per.",interval,"s.csv")
                write.csv(file = file.path(working.directory,"Results",Track.RCS,no.data.filename),sub.data,row.names = F)
            }
            setTxtProgressBar(pb,x)
          }
        }
        if(track.range=="Y"){
          print("Calculating average track range from radar...")
          #Show some progress
          pb<- txtProgressBar(min=0,max=length(unique(data$breaks2)),style=3)
          setTxtProgressBar(pb,0)
          for(x in 1:length(unique(data$breaks2))){
            sub.data<-subset(data, as.integer(breaks2)==x)
            if(nrow(sub.data)>0){
              #Find average track range from radar
                avg.range<-setNames(aggregate(sub.data$Range.from.radar...m. ~ sub.data$long.ID, FUN=mean),c("long.ID","AverageRange.m"))
              #Subset the data, only Unique TrackID's
                sub.data<-subset(sub.data,dup==FALSE)
              #Convert from meters to miles
                avg.range$avg.range.mi<-avg.range$AverageRange.m * 0.000621371
              #Add average range to dataframe
                sub.data<-merge(sub.data, avg.range,by="long.ID")
                sub.data<-sub.data[order(Update.Time.Local),]
              #Plot it!
                p<-ggplot(data=sub.data,aes(x=avg.range.mi))+geom_histogram(binwidth=.25)
                final.graph<-p+xlab("Average Track Range from Radar (mi)") + scale_x_continuous(breaks=seq(0,5,by=0.5),lim=c(0,5))+ ylab("Frequency") + ggtitle(paste0(radar.designation,": Average Track Range from Radar (mi)","\nLocal Timezone: ", local.time.zone))+facet_wrap(~ breaks,ncol=6) + theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1,size=6)) + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(strip.text.x = element_text(size = 8)) + theme(axis.text.y = element_text(size=6))+ theme(panel.grid.major = element_line(size = .5, color = "grey"),axis.line = element_line(size=.7, color = "black"))
              #Save it!
              file.date<-as.character(unique(as.POSIXct(sub.data$breaks2,local.time.zone)),format="%Y.%m.%d")
              filename<-paste0(file.date,"_",radar.designation,"_Avg.Track.range.per.",interval,"s.png")
              ggsave(file=file.path(working.directory,"Results",Track.Range,filename),plot=final.graph,width= 11,height=8.5,pointsize=12,units="in")
            }
            else{
              file.date<-as.character(unique(as.POSIXct(data$breaks2[x],local.time.zone)),format="%Y.%m.%d")
              no.data.filename<-paste0(file.date,"_NO.DATA_",radar.designation,"_Avg.Track.range.per.",interval,"s.csv")
              write.csv(file = file.path(working.directory,"Results",Track.Range,no.data.filename),sub.data,row.names = F)
            }
            setTxtProgressBar(pb,x)
          }
        }
    
    file.rename(from=file.path(working.directory,"Data",CSV.to.Process,filelist),to=file.path(working.directory,"Data",CSV.finished,filelist))
    print("Finished!")
      }
      else{print("Not enough data for specified interval. Shorten interval or add more data.")}
  }
  else{print("No data to process. Please add CSV files to the CSV_to_Process directory.")}
}