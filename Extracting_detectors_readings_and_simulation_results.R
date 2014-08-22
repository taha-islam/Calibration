sim_results=TRUE
remove_zeros <- FALSE
No_of_nodes = 14225
No_of_links = 26444
No_of_links_per_line = 10
year <- c(2011,2012,2013)
month <- c(9,10,11)
week <- 1
Path <- "C://Users//Islam//Desktop//GTA_9_7_2014//Calibration"
Calibration(Path, c(2011,2012,2013), c(9,10,11), 1, 14225, 26444)

Calibration <- function(Path, year, month, week = 1, No_of_nodes, No_of_links, No_of_links_per_line = 10
                        , remove_zeros=FALSE, sim_results=TRUE) {

  setwd(Path)
  
  #con <- file(".//Detectors.csv", open="rt")
  con <- ".//Detectors.csv"
  Detectors = read.csv(con, header = TRUE)
  #on.exit(close(con))
  Detectors <- Detectors[!is.na(Detectors[,1]),,drop=FALSE]
  Detectors <- Detectors[!is.na(Detectors[,2]),,drop=FALSE]
  Detector_ID <- as.character(Detectors[251:264,1])
#   Detector_ID <- c("401DW0110DEC","401DE0010DEC","401DE0080DEC","401DE0200DEC","401DE0320DEC","401DE0350DEC"
#                     ,"401DE0380DEC","401DE0410DEC","401DE0430DEC","401DW0070DES","401DW0060DES"
#                     ,"401DW0040DES","401DW0030DES","401DW0020DES","401DW0110DEE","401DE0080DEE"
#                     ,"401DE0120DEE","401DE0470DEE","401DW0090DWC","401DW0080DWC","401DE0010DWC"
#                     ,"401DE0040DWC","401DE0130DWC","401DE0150DWC","401DE0190DWC","401DE0210DWC"
#                     ,"401DE0280DWC","401DE0320DWC","401DE0340DWC","401DE0490DWC","401DW0220DWE"
#                     ,"401DW0070DWS","401DW0060DWS","401DW0040DWS","401DW0010DWS","401DW0080DWE"
#                     ,"401DE0040DWE","401DE0060DWE","401DE0110DWE","401DE0150DWE","401DE0210DWE"
#                     ,"401DE0280DWE","401DE0320DWE","401DE0340DWE","401DE0390DWE","401DE0410DWE"
#                     ,"401DE0430DWE","401DE0510DWE","401DE0570DWE","400DN0030DNE","400DN0020DNE"
#                     ,"400DN0110DNS","400DN0090DNS","400DN0080DNS","400DN0070DNS","400DN0060DNS"
#                     ,"400DN0040DNS","400DN0030DSE","400DN0020DSE","400DN0120DSS","400DN0100DSS"
#                     ,"400DN0090DSS","400DN0070DSS","400DN0040DSS","400DN0005DSS","404DN0011DSH"
#                     ,"404DN0010DSE","QEWDE0550DES","QEWDE0530DES","QEWDE0490DES","QEWDE0470DES"
#                     ,"QEWDE0460DES","QEWDE0430DES","QEWDE0380DES","QEWDE0280DES","QEWDE0270DES"
#                     ,"QEWDE0230DES","QEWDE0190DES","QEWDE0130DES","QEWDE0110DES","QEWDE0090DES"
#                     ,"QEWDE0070DES","QEWDE0040DES","QEWDE0550DWS","QEWDE0540DWS","QEWDE0530DWS"
#                     ,"QEWDE0490DWS","QEWDE0470DWS","QEWDE0460DWS","QEWDE0430DWS","QEWDE0380DWS"
#                     ,"QEWDE0330DWS","QEWDE0280DWS","QEWDE0270DWS","QEWDE0190DWS","QEWDE0130DWS"
#                     ,"QEWDE0110DWS","QEWDE0090DWS","QEWDE0070DWS")
  Det_Avg_Speed_7to8 <- vector("numeric", length = length(Detector_ID))
  Det_Avg_Speed_8to9 <- vector("numeric", length = length(Detector_ID))
  Det_Vol_7to8 <- vector("numeric", length = length(Detector_ID))
  Det_Vol_8to9 <- vector("numeric", length = length(Detector_ID))
  
  #Extracting simualtion results
  if (sim_results) {
    Link_ID <- as.numeric(Detectors[251:264,2])
    Link_Avg_Speed_7to8 <- vector("numeric", length = length(Link_ID))
    Link_Avg_Speed_8to9 <- vector("numeric", length = length(Link_ID))
    Link_Vol_7to8 <- vector("numeric", length = length(Link_ID))
    Link_Vol_8to9 <- vector("numeric", length = length(Link_ID))
    
    date_of_obs_7to8 <- vector("character", length = length(Link_ID))
    date_of_obs_8to9 <- vector("character", length = length(Link_ID))
    GEH_7to8 <- matrix(1000000, nrow = length(Link_ID))
    GEH_8to9 <- matrix(1000000, nrow = length(Link_ID))
    #####Reading files from DynusT
    skipped = No_of_nodes + 1
    #con <- file(paste("..//DynusTNetwork.v10//network.dat", sep = ""), open="rt")
    con <- paste("..//DynusTNetwork.v10//network.dat", sep = "")
    network <- read.table(con, skip=skipped, nrows=max(Link_ID), header=FALSE, fill=TRUE)
    #on.exit(close(con))
    NoOfLanes<- vector("numeric", length = length(Link_ID))
    
    Section_size = ceiling(No_of_links/No_of_links_per_line)
    #Calculating Avg speed from DynusT
    skipped = 60*(Section_size + 3) + 2
    speed1min = matrix(0,nrow=Section_size, ncol=10)
    
    for (i in 1:60) {
      speed1min = speed1min + read.table(paste("..//DynusTNetwork.v10//fort.900", sep = "")
                                         , skip=skipped, nrows=Section_size, header=FALSE, fill=TRUE)
      skipped = skipped + (Section_size + 3)
    }
    
    speed_7to8 = speed1min/60
    speed1min = matrix(0,nrow=Section_size, ncol=10)
    
    for (i in 1:60) {
      speed1min = speed1min + read.table(paste("..//DynusTNetwork.v10//fort.900", sep = "")
                                         , skip=skipped, nrows=Section_size, header=FALSE, fill=TRUE)
      skipped = skipped + (Section_size + 3)
    }
    
    speed_8to9 = speed1min/60
    rm(speed1min)
    #Calculating vol from DynusT
    skipped = 60*(Section_size + 3) + 7
    Vol_at_7 = matrix(0,nrow=Section_size, ncol=10)
    Vol_at_8 = matrix(0,nrow=Section_size, ncol=10)
    Vol_at_9 = matrix(0,nrow=Section_size, ncol=10)
    
    Vol_at_7 = read.table(paste("..//DynusTNetwork.v10//OutAccuVol.dat", sep = "")
                          , skip=skipped, nrows=Section_size, header=FALSE, fill=TRUE)
    skipped = skipped + 60*(Section_size + 3)
    Vol_at_8 = read.table(paste("..//DynusTNetwork.v10//OutAccuVol.dat", sep = "")
                          , skip=skipped, nrows=Section_size, header=FALSE, fill=TRUE)
    skipped = skipped + 60*(Section_size + 3)
    Vol_at_9 = read.table(paste("..//DynusTNetwork.v10//OutAccuVol.dat", sep = "")
                          , skip=skipped, nrows=Section_size, header=FALSE, fill=TRUE)
    
    vol_7to8 = Vol_at_8 - Vol_at_7
    vol_8to9 = Vol_at_9 - Vol_at_8
    rm(Vol_at_7, Vol_at_8, Vol_at_9)
    #####End of (Reading files from DynusT)
    for (i in 1:length(Link_ID)) {
      index1 = ceiling(Link_ID[i]/10)
      index2 = Link_ID[i] - (index1-1)*10
      Link_Avg_Speed_7to8[i] <- speed_7to8[index1,index2]*1.60934 #mph to kph
      Link_Avg_Speed_8to9[i] <- speed_8to9[index1,index2]*1.60934 #mph to kph
      Link_Vol_7to8[i] <- vol_7to8[index1,index2]
      Link_Vol_8to9[i] <- vol_8to9[index1,index2]
      
      NoOfLanes[i] <- network[Link_ID[i],6]
    }
  }#End of extracting simualtion results

  if (sim_results) {
    Output <- data.frame(Detector_ID, Det_Avg_Speed_7to8, Det_Avg_Speed_8to9, Det_Vol_7to8, Det_Vol_8to9
                         , Link_ID, Link_Avg_Speed_7to8, Link_Avg_Speed_8to9, Link_Vol_7to8, Link_Vol_8to9
                         , NoOfLanes, GEH_7to8, GEH_8to9, date_of_obs_7to8, date_of_obs_8to9)
  } else {
    Output <- data.frame(Detector_ID, Det_Avg_Speed_7to8, Det_Avg_Speed_8to9, Det_Vol_7to8, Det_Vol_8to9)
  }  


  #Find the exact dates of core days in <month> & <year>
  #week: starting form first, second, third, or fourth Tuesday
  if (week>4) print("Out of range date")  #What about February
  
  
  if (!file.exists("Downloads")) {
    dir.create("Downloads")
  }
  
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
        
        fileurl <- paste("http://128.100.217.245/oneits_client/DownloadTrafficReport?year=",year[j],"&month="
                         ,month[k],"&reportType=speed&sensorName=",Detector_ID[i],sep="")
        tryCatch(
          {download.file(fileurl, paste(".//Downloads//speed_",Detector_ID[i],"_",year[j],"_",month[k]
                                        ,".csv", sep = ""))},
          error=function(cond) {
            
          },
          warning=function(cond) {
            print(cond)
          },
          finally={}
        )
        fileurl <- paste("http://128.100.217.245/oneits_client/DownloadTrafficReport?year=",year[j],"&month="
                         ,month[k],"&reportType=min_30&sensorName=",Detector_ID[i],sep="")
        #Vol20Sec
        #   fileurl <- paste("http://128.100.217.245/oneits_client/DownloadTrafficReport?year=",year[j],"&month="
        #                  ,month[k],"&reportType=Full_Details&sensorName=",Detector_ID[i],sep="")
      
        tryCatch(
          {download.file(fileurl, paste(".//Downloads//volume_",Detector_ID[i],"_",year[j],"_",month[k]
                                        ,".csv", sep = ""))},
          error=function(cond) {
             
          },
          warning=function(cond) {
            print(cond)
          },
          finally={}
        )
      
        flag <- tryCatch(
          {
            if (file.exists(paste(".//Downloads//speed_",Detector_ID[i],"_",year[j],"_",month[k]
                                  ,".csv", sep = ""))) {
              #con <- file(paste(".//Downloads//speed_",Detector_ID[i],"_",year[j],"_",month[k]
              #                ,".csv", sep = ""), open="rt")
              con <- paste(".//Downloads//speed_",Detector_ID[i],"_",year[j],"_",month[k]
                                ,".csv", sep = "")
              speed20sec = read.csv(con, skip=1265, nrows=360, header=FALSE
                                    , na.strings = c(" X", "0", " "))
              #on.exit(close(con))
            } else {
              next
            }
            return <- 1
          },
          error=function(cond) {
            message(paste("Here's the original error message [speed, detector id = ",Detector_ID[i],"]:",sep = ""))
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
        
        bad1 <- as.numeric(sum(is.na(Tuesday[1:180])))
        if(bad1>45)
          next
        bad2 <- as.numeric(sum(is.na(Tuesday[181:360])))
        if(bad2>45)
          next
        Tuesday<-Tuesday[!is.na(Tuesday),drop=FALSE]
        first_hour <- Tuesday[1:(180-bad1)]
        second_hour <- Tuesday[(181-bad1):(360-bad1-bad2)]
        
        bad1 <- as.numeric(sum(is.na(Wednesday[1:180])))
        if(bad1>45)
          next
        bad2 <- as.numeric(sum(is.na(Wednesday[181:360])))
        if(bad2>45)
          next
        Wednesday<-Wednesday[!is.na(Wednesday),drop=FALSE]
        first_hour <- c(first_hour, Wednesday[1:(180-bad1)])
        second_hour <- c(second_hour, Wednesday[(181-bad1):(360-bad1-bad2)])
        
        bad1 <- as.numeric(sum(is.na(Thursday[1:180])))
        if(bad1>45)
          next
        bad2 <- as.numeric(sum(is.na(Thursday[181:360])))
        if(bad2>45)
          next
        Thursday<-Thursday[!is.na(Thursday),drop=FALSE]
        first_hour <- c(first_hour, Thursday[1:(180-bad1)])
        second_hour <- c(second_hour, Thursday[(181-bad1):(360-bad1-bad2)])
        
        Det_Avg_Speed_7to8[i] <- mean(first_hour)
        Det_Avg_Speed_8to9[i] <- mean(second_hour)
        
        rm(speed20sec, first_hour, second_hour, Tuesday, Wednesday, Thursday)
      ###########################################################################
        flag <- tryCatch(
          {
            if (file.exists(paste(".//Downloads//volume_",Detector_ID[i],"_",year[j],"_",month[k]
                                  ,".csv", sep = ""))) {
              #con <- file(paste(".//Downloads//volume_",Detector_ID[i],"_",year[j],"_",month[k]
              #                ,".csv", sep = ""), open = "rt")
              con <- paste(".//Downloads//volume_",Detector_ID[i],"_",year[j],"_",month[k]
                                ,".csv", sep = "")
              vol30min = read.csv(con, skip=19, nrows=4
                                , header=FALSE, na.strings = c(" X", "0", " "))
              #on.exit(close(con))
            } else {
              next
            }
            return <- 1
          },
          error=function(cond) {
            message(paste("Here's the original error message [volume, detector id = ",Detector_ID[i],"]:",sep = ""))
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
        
        bad1 <- as.numeric(sum(is.na(Tuesday[1:2])))
        if(bad1>0)
          next
        bad2 <- as.numeric(sum(is.na(Tuesday[3:4])))
        if(bad2>0)
          next
        first_hour <- Tuesday[1] + Tuesday[2]
        second_hour <- Tuesday[3] + Tuesday[4]
        
        bad1 <- as.numeric(sum(is.na(Wednesday[1:2])))
        if(bad1>0)
          next
        bad2 <- as.numeric(sum(is.na(Wednesday[3:4])))
        if(bad2>0)
          next
        first_hour <- c(first_hour, Wednesday[1] + Wednesday[2])
        second_hour <- c(second_hour, Wednesday[3] + Wednesday[4])
        
        bad1 <- as.numeric(sum(is.na(Thursday[1:2])))
        if(bad1>0)
          next
        bad2 <- as.numeric(sum(is.na(Thursday[3:4])))
        if(bad2>0)
          next
        first_hour <- c(first_hour, Thursday[1] + Thursday[2])
        second_hour <- c(second_hour, Thursday[3] + Thursday[4])
        
        Det_Vol_7to8[i] <- mean(first_hour)
        Det_Vol_8to9[i] <- mean(second_hour)
        rm(vol30min, first_hour, second_hour, Tuesday, Wednesday, Thursday)
        
        #Close all open connections
        #allCon <- as.numeric(row.names(showConnections()))
        #for(a in 1:length(allCon)) {
        #  con <- getConnection(allCon[a])
        #  close.connection(con)
        #}
      }
      if (sim_results) {
        GEH_7to8_new <- sqrt(2*(Det_Vol_7to8 - Link_Vol_7to8)^2/(Det_Vol_7to8 + Link_Vol_7to8))
        GEH_8to9_new <- sqrt(2*(Det_Vol_8to9 - Link_Vol_8to9)^2/(Det_Vol_8to9 + Link_Vol_8to9))
        
        GEH_7to8_new[is.na(GEH_7to8_new)] <- 1000000
        GEH_8to9_new[is.na(GEH_8to9_new)] <- 1000000
        #GEH_7to8_new[is.nan(GEH_7to8_new)] <- 1000000
        #GEH_8to9_new[is.nan(GEH_8to9_new)] <- 1000000
        
        Output[GEH_7to8_new < GEH_7to8,"Det_Avg_Speed_7to8"] <- Det_Avg_Speed_7to8
        Output[GEH_8to9_new < GEH_8to9,"Det_Avg_Speed_8to9"] <- Det_Avg_Speed_8to9
        Output[GEH_7to8_new < GEH_7to8,"Det_Vol_7to8"] <- Det_Vol_7to8
        Output[GEH_8to9_new < GEH_8to9,"Det_Vol_8to9"] <- Det_Vol_8to9
        date_of_obs_7to8[GEH_7to8_new < GEH_7to8] <- paste(year[j],"-",month[k],"-",day, sep="")
        date_of_obs_8to9[GEH_8to9_new < GEH_8to9] <- paste(year[j],"-",month[k],"-",day, sep="")
        GEH_7to8[GEH_7to8_new < GEH_7to8] <- GEH_7to8_new[GEH_7to8_new < GEH_7to8]
        GEH_8to9[GEH_8to9_new < GEH_8to9] <- GEH_8to9_new[GEH_8to9_new < GEH_8to9]
      }
    }
  }
  
  
  if (sim_results) {
    Output[,"GEH_7to8"] <- GEH_7to8
    Output[,"GEH_8to9"] <- GEH_8to9
  } else {
    Output <- data.frame(Detector_ID, Det_Avg_Speed_7to8, Det_Avg_Speed_8to9, Det_Vol_7to8, Det_Vol_8to9)
  }
  if (remove_zeros)
    Output <- Output[Det_Avg_Speed_7to8!=0,,drop=FALSE]
  
  write.csv(Output, "Detectors_Readings.csv", row.names = FALSE)
}
