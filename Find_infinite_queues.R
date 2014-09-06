
No_of_nodes = 14225
No_of_links = 26443
No_of_links_per_line = 10
Path <- "F://PhD//GTA_9_7_2014//DynusTNetwork.v14//Calibration/"
setwd(Path)

Section_size = ceiling(No_of_links/No_of_links_per_line)
skipped = 299*(Section_size + 3) + 2

queue = read.table(paste("..//fort.600", sep = "")
                                     , skip=skipped, nrows=Section_size, header=FALSE, fill=TRUE)

#str(queue)
queue_1col = vector("numeric", length = nrow(queue)*ncol(queue))
k = 1
for (i in 1:nrow(queue)) {
  for (j in 1:ncol(queue)) {
    queue_1col[k] = queue[i,j]
    k = k+1
  }
}

#queue_1col = c(queue$V1,queue$V2,queue$V3,queue$V4,queue$V5,queue$V6,queue$V7,queue$V8,queue$V9,queue$V10)
queue_1col = queue_1col[!is.na(queue_1col)]
#str(queue_1col)

sum(queue_1col[queue_1col==1])
Link_IDs_equal1 <- which(queue_1col==1)
Link_IDs_largerthan0 <- which(queue_1col>0)
length(Link_IDs_equal1)
length(Link_IDs_largerthan0)

head(Link_IDs_equal1)
head(Link_IDs_largerthan0)

Links <- Link_IDs_largerthan0
########################################################################################
skipped = No_of_nodes + 1
con <- paste("..//network.dat", sep = "")
network <- read.table(con, skip=skipped, nrows=max(Links), header=FALSE, fill=TRUE)
links_by_nodes <- matrix(nrow=length(Links) , ncol=3)
for (i in seq_along(Links)) {
  links_by_nodes[i,1] <- Links[i]
  links_by_nodes[i,2] <- network[Links[i],1]
  links_by_nodes[i,3] <- network[Links[i],2]
}
rm(network)

con <- paste("..//movement.dat", sep = "")
movement <- read.table(con, header=FALSE, fill=TRUE)
links_with_movements <- matrix(nrow=length(Links) , ncol=9)
for (i in seq_along(Links)) {
  links_with_movements[i,1] <- Links[i]
  links_with_movements[i,2:9] <- data.matrix(movement[Links[i],])
  if (movement[Links[i],8] == 1) {
    movement[Links[i],8] <- 0
  }
}


con <- paste("..//control.dat", sep = "")
control <- read.table(con, skip=(No_of_nodes+2), header=FALSE, fill=TRUE)
links_with_allowable_movements <- matrix(nrow=length(Links) , ncol=6)
for (i in seq_along(Links)) {
  links_with_allowable_movements[i,1] <- Links[i]
  index1 <- which(control[,1]==links_with_movements[i,2])
  index2 <- which(control[,2]==links_with_movements[i,3])
  index1 <- index1[index1 %in% index2, drop=FALSE]
  if (length(index1)>2) {
    print(paste("Error in link ",Links[i]))
  } else if (length(index1)==2) {
    if (sum(is.na(control[index1[1],])) < sum(is.na(control[index1[2],]))) {
      links_with_allowable_movements[i,2:6] <- data.matrix(control[index1[1],c(1,2,5,6,7)])
    } else {
      links_with_allowable_movements[i,2:6] <- data.matrix(control[index1[2],c(1,2,5,6,7)])
    }
    index <- which(!(links_with_movements[i,4:8] %in% links_with_allowable_movements[i,4:6]) & (links_with_movements[i,4:8]!=0))
    movement[Links[i],index+2] <- 0
  } else if (length(index1)==1) {
    links_with_allowable_movements[i,2:6] <- data.matrix(control[index1[1],c(1,2,5,6,7)])
    index <- which(!(links_with_movements[i,4:8] %in% links_with_allowable_movements[i,4:6]) & (links_with_movements[i,4:8]!=0))
    movement[Links[i],index+2] <- 0
  } else {
    print(paste("Error in link ",Links[i]))
  }
}
rm(control)

# for (i in seq_along(Links)) {
#   index <- which(!(links_with_movements[i,4:8] %in% links_with_allowable_movements[i,4:6]) & (links_with_movements[i,4:8]!=0))
#   movement[Links[i],index+2] <- 0
# }

con <- paste("..//movement-modified.dat", sep = "")
#write.table(movement, con, row.names = FALSE, col.names = FALSE)
#write.table(movement, con, row.names = FALSE, col.names = FALSE, sep="\t")
write.fwf(movement, con, rownames = FALSE, colnames = FALSE
          , width = c(6,6,6,6,6,6,6,6))