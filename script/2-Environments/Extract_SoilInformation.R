# this code was used to extract soil information from HWSD dataset
library(dplyr)
library(raster)
library(readxl)
library(expss)
# read the location information of each site
site <- read_excel('your file path and name here') 
# read the HWSD soil ID raster
soil_ID <- raster('hwsd_raster.tif')  # download the soil raster file from HWSD website
# ectract soil ID form HWSD raster file
coordinates(site) <- ~ Longitude + Latitude
cc <- extract(x=soil_ID,y=site,cellnumbers=T,factors=T)
fin <- cbind(site@coords,cc)
fin_data <- cbind(dplyr::select(site@data,1),fin)
soil_ID_site <- dplyr::select(fin_data, c(1,2,4,5))
names(soil_ID_site) <- c('lat','lon','soil_ID','site_ID')
# download the soil properties database from HWSD website and load the file
HWSD <- read.csv('HWSD_DATA.csv')
# extract soil properites from databased according to soil_ID
soil_info <- merge(x=soil_ID_site, y=HWSD, by.x='Soil_ID',by.y='ID',all.x=T)
soil <- soil_info %>% select(c(4,1:3,5:60)) # select the soil variables you are interested in by identify their column ID in the "select" function
# save the extracted results
write.csv(soil,file='Define your output file path and name here')