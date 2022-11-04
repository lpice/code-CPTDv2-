# this script can be used to query and extract data in CPTDv2 by sites/species/variables
# the CPTDv2 dataset can be download at https://figshare.com/articles/dataset/The_China_Plant_Trait_Database_Version_2_0/19448219
# please complete the following steps
# step 1: load CPTDv2 and function used for extracting data
# step 2: view the identification key then determine which sites/species/variables you want
# step 3: declare which data you want then extract them from CPTDv2
# step 4: save the data


# the fellowing is case about how to query and extract data from CPTDv2
##----------------------------------------------------------------------
# step1: load CPTDv2 dataset and functions used for for extracting data
##----------------------------------------------------------------------
source('LinkTablesOfCPTDv2Together.R')

##----------------------------------------------------------------------
# step2: view the identification key which provides the information of sites/species/variables
#        then determine which sites/species/variables you want
##----------------------------------------------------------------------
View(info.site) # view sites included in CPTDv2
View(info.species) # view species included in CPTDv2
View(info.variables) # view variables included in CPTDv2

##----------------------------------------------------------------------
# step3: declare which data you want by 'search.id' 
#        then extract them from CPTDv2
##----------------------------------------------------------------------
# you can replace the number in 'c()' with numbers corresponding to sites/species/variables you are interested
search.site <- paste0('si',c(1:20)) # declare which sites you want by 'search.site.id'
search.species <- paste0('sp',c(1:12)) # declare which species you want by 'search.species.id'
search.variable <- paste0('va',c(1:10,33:45,50:70)) # declare which variables you want by 'search.variables.id'

# extract data you want
# 'Hi' High-resolution climate, this is available for all sites included in CPTDv2.
# 'Go' Gongga local climate, this is only available for sites in Gongga mountain.
dd <- extract.data(cli.type = 'Hi', # the climate data used,
                   id.site = search.site, # sites you want to extract
                   id.species = search.species, # species you want to extract
                   id.variable = search.variable) # variables you want to extract

##----------------------------------------------------------------------
# step 4: save the data
##----------------------------------------------------------------------
# you can define your file name and path
write.csv(x=dd, file = '../../results/QueryAndExtractDataFromCPTDv2.csv')


