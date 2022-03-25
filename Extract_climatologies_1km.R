nearestG <- function(Plonlat,Glonlat){
  
  library(sp)
  
  Plonlat <- as.matrix(Plonlat)
  Glonlat <- as.matrix(Glonlat)
  
  # i in 1:n(points), for each point: calculate the distance between the point and each CRU grid 
  # res: a list with n columns, each column contains the calculated distances (to each CRU grid) for that point
  res <- lapply(1:nrow(Plonlat), function(i) spDistsN1(Glonlat,Plonlat[i,]))     
  
  # figure out the shortest distance for each point, and return the CRU grid ID
  return(sapply(res, function(x) which.min(x)))
  
}

sites <- read.table("Data/Sites_China traits_SHP.csv",header=T,sep=",")

temp <- read.fwf("Data/Climatology_1km/China1km_temp.txt",widths=rep(7,14))
prec <- read.fwf("Data/Climatology_1km/China1km_prec.txt",widths=rep(7,14))
clou <- read.fwf("Data/Climatology_1km/China1km_clou.txt",widths=rep(7,14))
bioclim <- read.delim("Data/Climatology_1km/Bioclim_tab.txt")

Gid <- nearestG(Plonlat <- sites[,3:2],
                Glonlat <- temp[,1:2])

sites.temp <- temp[Gid,3:14]
sites.prec <- prec[Gid,3:14]
sites.clou <- clou[Gid,3:14]
sites.bioclim <- bioclim[Gid,3:14]