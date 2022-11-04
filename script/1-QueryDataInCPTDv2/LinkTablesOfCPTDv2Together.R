# the CPTDv2 dataset can be download at 
# https://figshare.com/articles/dataset/The_China_Plant_Trait_Database_Version_2_0/19448219

##-------------------------------------------------------------------------
# Link tables of CPTDv2 together by 'Site ID', 'SPECIES ID' and 'SAMPLE ID'
##-------------------------------------------------------------------------
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)

# load 'csv' files included in CPTDv2
CPTDv2 <- list()
mypath <- '../../Data/CPTDv2/' # please replace your path in which CPTDv2 is saved 
file.name <- list.files(path = mypath,pattern = '*.csv')
for (i in 1:length(file.name)) {
  CPTDv2[[i]] <- read.csv(paste0('../../Data/CPTDv2/',file=file.name[i]))
  names(CPTDv2)[[i]] <- strsplit(file.name[i],split = '[.]')[[1]][1]
}

  # tables connected by 'Site ID'
  con.site.id1 <- merge(x=CPTDv2$`Sites information`,y=CPTDv2$`Vegetation types`,by='Site.ID',all=T)
  con.site.id2 <- merge(x=CPTDv2$`Soil properties`,y=CPTDv2$`High-resolution climate`,by='Site.ID',all=T)
  con.site.id3 <- merge(x=CPTDv2$`Soil properties`,y=CPTDv2$`Gongga local climate`,by='Site.ID',all.y=T)
  con.site.id_Hi <- merge(x=con.site.id1,y=con.site.id2,by='Site.ID',all = T)
  con.site.id_Go <- merge(x=con.site.id1,y=con.site.id3,by='Site.ID',all.y =  T)
  # tables connected by 'SPECIES ID'
  con.species.id1 <- merge(x=CPTDv2$`Species translations`,y=CPTDv2$`Taxonomic standardisation`,
                           by=c('SPECIES.ID','ACCEPTED.GENUS','ACCEPTED.SPECIES','Field.identified.Genus','Field.identified.Species'),
                           all.x =  T)
  con.species.id2 <- merge(x=CPTDv2$`Photo Pathway`,y=CPTDv2$`Species Chinese Names`,
                           by='SPECIES.ID',all = T)
  con.species.id <- merge(x=con.species.id1,y=con.species.id2,
                          by=c('SPECIES.ID','ACCEPTED.GENUS','ACCEPTED.SPECIES'),
                          all.x = T)
  # tables connected by 'SAMPLE ID'
  con.sample.id1 <- merge(x=CPTDv2$`Chemical traits`,y=CPTDv2$`Photosynthetic traits`,
                         by='SAMPLE.ID',all = T)
  con.sample.id2 <- merge(x=merge(x=CPTDv2$`Plant functional types`,y=CPTDv2$`Morphometric traits`,
                          by='SAMPLE.ID',all = T),y=CPTDv2$`Hydraulic Trait`,
                          by='SAMPLE.ID',all = T)
  con.sample.id <- merge(x=con.sample.id1,y=con.sample.id2,by='SAMPLE.ID',all = T)
  
  # connect  all tables together
  con.data1 <- merge(x=con.species.id,y=con.sample.id,by='SAMPLE.ID',all = T)
  con.data_Hi <- merge(x=con.site.id_Hi,y=con.data1,by='Site.ID',all.x=T)
  con.data_Hi1 <- select(con.data_Hi,Lat_grid,Lon_grid)
  con.data_Hi2 <- select(con.data_Hi,-Lat_grid,-Lon_grid)
  # link with high-resolution climate
  con.data_Hi <- cbind(con.data_Hi1,con.data_Hi2)
  # link with Gongga local climate
  con.data_Go <- merge(x=con.site.id_Go,y=con.data1,by='Site.ID',all.x=T)
  
  # romove temporary data
  rm(con.data_Hi1,con.data_Hi2,con.data1,
     con.sample.id,con.sample.id1,con.sample.id2,
     con.site.id1,con.site.id2,con.site.id3,con.site.id_Go,con.site.id_Hi,
     con.species.id,con.species.id1,con.species.id2,
     file.name,i)

##-------------------------------------------------------------------------
# define the function for data query and extract
##-------------------------------------------------------------------------
    
  extract.data <- function(cli.type='Hi',
                           id.site='all',
                           id.species='all',
                           id.variable='all'){
    
    if (id.site[1]=='all') {
      id.site <- paste0('si',1:140)
    }
    if (id.species[1]=='all') {
      id.species <- paste0('sp',1:2949)
    }
    if (id.variable[1]=='all') {
      id.variable <- paste0('va',1:168)
    }
    
    if (cli.type=='Hi') {
      data <- con.data_Hi
      site.id <- info.site$Site.ID[which(info.site$search.site.id %in% id.site)]
      species.id <- info.species$SPECIES.ID[which(info.species$search.species.id %in% id.species)]
      variables <- info.variables$variables[which(info.variables$search.variables.id %in% id.variable)]
        
      fin_data <- data[which(data$Site.ID %in% site.id),]
      fin_data <- fin_data[which(fin_data$SPECIES.ID %in% species.id),]
      fin_data <- select(fin_data,variables)
    }  
    
    if (cli.type=='Go'){
      id.variable <- setdiff(id.variable,c('va1','va2'))
      data <- con.data_Go
      site.id <- info.site$Site.ID[which(info.site$search.site.id %in% id.site)]
      species.id <- info.species$SPECIES.ID[which(info.species$search.species.id %in% id.species)]
      variables <- info.variables$variables[which(info.variables$search.variables.id %in% id.variable)]
      
      
      fin_data <- data[which(data$Site.ID %in% site.id),]
      fin_data <- fin_data[which(fin_data$SPECIES.ID %in% species.id),]
      fin_data <- select(fin_data,variables)
    }
    return(fin_data)
  }
  

##-------------------------------------------------------------------------
# define tables of identification key for sites/species/variables
##-------------------------------------------------------------------------
  # table of identification key for sites
  info.site <- CPTDv2$`Sites information`
  info.site$search.site.id <- paste0('si',1:length(info.site$Site.ID))
  info.site <- select(info.site,c(10,1:9))
  # table of identification key for species
  info.species <- CPTDv2$`Species translations`
  info.species$search.species.id <- paste0('sp',1:length(info.species$SPECIES.ID))
  info.species <- select(info.species,c(8,6,2:5))
  # table of identification key for variables
  info.variables <- data.frame(variables=colnames(con.data_Hi))
  info.variables$search.variables.id <- paste0('va',1:length(info.variables$variables))
  info.variables <- select(info.variables,2,1)















