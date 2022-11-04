# Define the function nearestG to obtain the cell id from the gridded climate map

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

# Define or input the longtidude and latitude in order of the sample sites  
sites <- read.table("Data/CPTDv2/Sites information.csv",header=T,sep=",") 

# Read in the climate data with the first two columns represents longtitude and latitude respectively, the rows represent different grids
climate <- read.table("define your file path here")

# apply the nearestG function to get the cell id for the sites fell into the climate grids
Gid <- nearestG(Plonlat <- sites[,3:2], # Plonlat is the longitude and latitude of the sampled sites
                Glonlat <- climate[,1:2]) # Glonlat is the longitude and latitude of the climate grids

# output the extracted climate information
sites.clim <- clim[Gid,]
