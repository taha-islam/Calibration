Calibration <- function(Path, year=2011, month=10, week=1, No_of_nodes, No_of_links, No_of_links_per_line=10
                        , remove_zeros=FALSE, obs.readings=TRUE, sim.readings=TRUE, start=0, end=60
                        , sim.start=6) {
  #start & end in min; sim.start in hours
  setwd(Path)
  con <- ".//Detectors.csv"
  Detectors = read.csv(con, header = TRUE)
  #on.exit(close(con))
  Detectors <- Detectors[!is.na(Detectors[,1]),,drop=FALSE]
  Detectors <- Detectors[!is.na(Detectors[,2]),,drop=FALSE]
  
  #Extracting simualtion results
  if (sim.readings) {
    Link_ID <- as.numeric(Detectors[,2])
    Link_Avg_Speed <- vector("numeric", length = length(Link_ID))
    Link_Vol <- vector("numeric", length = length(Link_ID))
    #####Reading files from DynusT
    skipped = No_of_nodes + 1
    con <- paste("..//network.dat", sep = "")
    network <- read.table(con, skip=skipped, nrows=max(Link_ID), header=FALSE, fill=TRUE)
    NoOfLanes<- vector("numeric", length = length(Link_ID))
    
    #Calculating Avg speed from DynusT
    Section_size = ceiling(No_of_links/No_of_links_per_line)
    skipped = start*(Section_size + 3) + 2
    speed1min = matrix(0,nrow=Section_size, ncol=10)
    for (i in 1:(end-start)) {
      speed1min = speed1min + read.table(paste("..//fort.900", sep = "")
                                         , skip=skipped, nrows=Section_size, header=FALSE, fill=TRUE)
      skipped = skipped + (Section_size + 3)
    }
    speed = speed1min/(end-start)
    rm(speed1min)
    
    #Calculating vol from DynusT
    Vol_at_start = matrix(0,nrow=Section_size, ncol=10)
    Vol_at_end = matrix(0,nrow=Section_size, ncol=10)
    
    skipped = start*(Section_size + 3) + 7
    Vol_at_start = read.table(paste("..//OutAccuVol.dat", sep = "")
                          , skip=skipped, nrows=Section_size, header=FALSE, fill=TRUE)
    skipped = end*(Section_size + 3) + 7
    Vol_at_end = read.table(paste("..//OutAccuVol.dat", sep = "")
                          , skip=skipped, nrows=Section_size, header=FALSE, fill=TRUE)
    
    vol = Vol_at_end - Vol_at_start
    rm(Vol_at_start, Vol_at_end)
    #####End of (Reading files from DynusT)
    for (i in 1:length(Link_ID)) {
      index1 = ceiling(Link_ID[i]/10)
      index2 = Link_ID[i] - (index1-1)*10
      Link_Avg_Speed[i] <- speed[index1,index2]*1.60934 #mph to kph
      Link_Vol[i] <- vol[index1,index2]
      
      NoOfLanes[i] <- network[Link_ID[i],6]
    }
  }#End of extracting simualtion results
  
  if (obs.readings) {
    Detector_ID <- as.character(Detectors[,1])
    Det_Avg_Speed <- vector("numeric", length = length(Detector_ID))
    Det_Speed_Tue <- vector("numeric", length = length(Detector_ID))
    Det_Speed_Wed <- vector("numeric", length = length(Detector_ID))
    Det_Speed_Thu <- vector("numeric", length = length(Detector_ID))
    Det_Vol <- vector("numeric", length = length(Detector_ID))
    Det_Vol_Tue <- vector("numeric", length = length(Detector_ID))
    Det_Vol_Wed <- vector("numeric", length = length(Detector_ID))
    Det_Vol_Thu <- vector("numeric", length = length(Detector_ID))
  }
  
  if (sim.readings & obs.readings) {
    date_of_obs <- vector("character", length = length(Link_ID))
    GEH <- matrix(1000000, nrow = length(Link_ID))
    
    Output <- data.frame(Detector_ID, Det_Speed_Tue, Det_Speed_Wed, Det_Speed_Thu, Det_Avg_Speed
                         , Det_Vol_Tue, Det_Vol_Wed, Det_Vol_Thu, Det_Vol, Link_ID, Link_Avg_Speed
                         , Link_Vol, NoOfLanes, GEH, date_of_obs)
  } else if (sim.readings) {
    Output <- data.frame(Link_ID, Link_Avg_Speed, Link_Vol, NoOfLanes)
  } else if (obs.readings) {
    Output <- data.frame(Detector_ID, Det_Speed_Tue, Det_Speed_Wed, Det_Speed_Thu, Det_Avg_Speed
                         , Det_Vol_Tue, Det_Vol_Wed, Det_Vol_Thu, Det_Vol)
  }  
  
  if (obs.readings) {
    #Find the exact dates of core days in <month> & <year>
    #week: starting form first, second, third, or fourth Tuesday
    if (week>4) print("Out of range date")  #What about February
    
    if (!file.exists("Downloads")) {
      dir.create("Downloads")
    }
    
    skipped.speed = (start + sim.start*60)*3 + 5  #3: each row represents 20sec
    size.speed = (end - start)*3
    skipped.vol = (start/60 + sim.start)*2 + 5  #2: each row represents 30min
    size.vol = (end - start)*2/60
    for (j in 1:length(year)) {
      for (k in 1:length(month)) {
        for (i in (week*7-6):(week*7)) {
          x <- as.Date(paste(year[j],month[k],i,sep="-"))
          if (weekdays(x)=="Tuesday") {
            day <- i
            break
          }
        }
        for (i in 1:length(Detector_ID)) {
          flag <- tryCatch(
            {
              if (file.exists(paste(".//Downloads//speed_",Detector_ID[i],"_",year[j],"_",month[k]
                                    ,".csv", sep = ""))) {
                con <- paste(".//Downloads//speed_",Detector_ID[i],"_",year[j],"_",month[k]
                             ,".csv", sep = "")
                speed20sec = read.csv(con, skip=skipped.speed, nrows=size.speed, header=FALSE
                                      , na.strings = c(" X", "0", " "))
              } else {
                next
              }
              return <- 1
            },
            error=function(cond) {
              message(paste("Here's the original error message [speed, detector id = ",Detector_ID[i]
                            ,", month = ", month[k],", year = ", year[j], "]:",sep = ""))
              message(cond)
              return(0)
            }
          )
          if (flag==0) {
            next
          }
          
          Tuesday = as.numeric(speed20sec[,day+1])
          Wednesday = as.numeric(speed20sec[,day+2])
          Thursday = as.numeric(speed20sec[,day+3])
          
          bad <- as.numeric(sum(is.na(Tuesday)))
          if(bad>10)
            next
          Tuesday <- Tuesday[!is.na(Tuesday),drop=FALSE]
          Det_Speed_Tue[i] <- mean(Tuesday)
          
          bad <- as.numeric(sum(is.na(Wednesday)))
          if(bad>10)
            next
          Wednesday<-Wednesday[!is.na(Wednesday),drop=FALSE]
          Det_Speed_Wed[i] <- mean(Wednesday)
          
          bad <- as.numeric(sum(is.na(Thursday)))
          if(bad>10)
            next
          Thursday<-Thursday[!is.na(Thursday),drop=FALSE]
          Det_Speed_Thu[i] <- mean(Thursday)
          
          Det_Avg_Speed[i] <- mean(c(Tuesday,Wednesday,Thursday))
          
          rm(speed20sec, Tuesday, Wednesday, Thursday)
          ###########################################################################
          flag <- tryCatch(
            {
              if (file.exists(paste(".//Downloads//volume_",Detector_ID[i],"_",year[j],"_",month[k]
                                    ,".csv", sep = ""))) {
                con <- paste(".//Downloads//volume_",Detector_ID[i],"_",year[j],"_",month[k]
                             ,".csv", sep = "")
                vol30min = read.csv(con, skip=skipped.vol, nrows=size.vol
                                    , header=FALSE, na.strings = c(" X", "0", " "))
              } else {
                next
              }
              return <- 1
            },
            error=function(cond) {
              message(paste("Here's the original error message [volume, detector id = ",Detector_ID[i]
                            ,", month = ", month[k],", year = ", year[j], "]:",sep = ""))
              message(cond)
              return(0)
            }
          )
          
          if (flag==0) {
            next
          }
          
          
          Tuesday = as.numeric(vol30min[,day+1])
          Wednesday = as.numeric(vol30min[,day+2])
          Thursday = as.numeric(vol30min[,day+3])
          
          bad <- as.numeric(sum(is.na(Tuesday)))
          if(bad>0)
            next
          Det_Vol_Tue[i] <- sum(Tuesday)
          
          bad <- as.numeric(sum(is.na(Wednesday)))
          if(bad>0)
            next
          Det_Vol_Wed[i] <- sum(Wednesday)
          
          bad <- as.numeric(sum(is.na(Thursday)))
          if(bad>0)
            next
          Det_Vol_Thu[i] <- sum(Thursday)
          
          Det_Vol[i] <- (Det_Vol_Tue[i]+Det_Vol_Wed[i]+Det_Vol_Thu[i])/3
          rm(vol30min, Tuesday, Wednesday, Thursday)
        }
        if (sim.readings) {
          GEH_new <- sqrt(2*(Det_Vol - Link_Vol)^2/(Det_Vol + Link_Vol))
          
          GEH_new[is.na(GEH_new)] <- 1000000
          
          Output[GEH_new < GEH,"Det_Avg_Speed"] <- Det_Avg_Speed[GEH_new < GEH]
          Output[GEH_new < GEH,"Det_Speed_Tue"] <- Det_Speed_Tue[GEH_new < GEH]
          Output[GEH_new < GEH,"Det_Speed_Wed"] <- Det_Speed_Wed[GEH_new < GEH]
          Output[GEH_new < GEH,"Det_Speed_Thu"] <- Det_Speed_Thu[GEH_new < GEH]
          Output[GEH_new < GEH,"Det_Vol"] <- Det_Vol[GEH_new < GEH]
          Output[GEH_new < GEH,"Det_Vol_Tue"] <- Det_Vol_Tue[GEH_new < GEH]
          Output[GEH_new < GEH,"Det_Vol_Wed"] <- Det_Vol_Wed[GEH_new < GEH]
          Output[GEH_new < GEH,"Det_Vol_Thu"] <- Det_Vol_Thu[GEH_new < GEH]
          
          date_of_obs[GEH_new < GEH] <- paste(year[j],"-",month[k],"-",day, sep="")
          GEH[GEH_new < GEH] <- GEH_new[GEH_new < GEH]
        }
      }
    }
    
    
    if (sim.readings) {
      Output[,"GEH"] <- GEH
      Output[,"date_of_obs"] <- date_of_obs
    } else {
      Output <- data.frame(Detector_ID, Det_Avg_Speed, Det_Vol)
    }
    
    if (remove_zeros)
      Output <- Output[Det_Avg_Speed_7to8!=0,,drop=FALSE]
  }
  
  write.csv(Output, paste("Detectors_Readings_",(start/60+sim.start),"to",(end/60+sim.start),".csv", sep="")
            , row.names = FALSE)
}


#         fileurl <- paste("http://128.100.217.245/oneits_client/DownloadTrafficReport?year=",year[j],"&month="
#                          ,month[k],"&reportType=speed&sensorName=",Detector_ID[i],sep="")
#         tryCatch(
#           {download.file(fileurl, paste(".//Downloads//speed_",Detector_ID[i],"_",year[j],"_",month[k]
#                                         ,".csv", sep = ""))},
#           error=function(cond) {
#             
#           },
#           warning=function(cond) {
#             print(cond)
#           },
#           finally={}
#         )
#         fileurl <- paste("http://128.100.217.245/oneits_client/DownloadTrafficReport?year=",year[j],"&month="
#                          ,month[k],"&reportType=min_30&sensorName=",Detector_ID[i],sep="")
#         #Vol20Sec
#         #   fileurl <- paste("http://128.100.217.245/oneits_client/DownloadTrafficReport?year=",year[j],"&month="
#         #                  ,month[k],"&reportType=Full_Details&sensorName=",Detector_ID[i],sep="")
#       
#         tryCatch(
#           {download.file(fileurl, paste(".//Downloads//volume_",Detector_ID[i],"_",year[j],"_",month[k]
#                                         ,".csv", sep = ""))},
#           error=function(cond) {
#              
#           },
#           warning=function(cond) {
#             print(cond)
#           },
#           finally={}
#         )