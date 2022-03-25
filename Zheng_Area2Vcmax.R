# the published paper recorded the traits data and some useful info to figure out Vcmax
# data was collected at the growing season in 2005

year <- 2005
Tleaf <- 25 # degreeC
ca <- 390 # ppm

# read in the published trait data for each measured species
ZhengData <- read.table("Data/1_Original Data Sets/Photosynthesis/Literatures/Zhengdata.csv",header=T,sep=",")

lat <- ZhengData$Latitude# degree
elv <-  ZhengData$Elevation*0.001 # km
d13C <- ZhengData$d13C12C
Asat <- ZhengData$Asat_Photo..umol.m2.s. # umol m-2 s-1 (though recorded as mmol/m2/s in the paper, it must be wrong)

# convert leaf d13C to leaf ∆13C

#-1 calculate d13C air at 1992 according to site latitude
a <- 0.0819
b <- 0.0983
c <- 7.7521
d13C.air.1992 <- a*(sin(lat*pi/180))^2 + b*sin(lat*pi/180) - c

#-2 calculate d13C air at 2005
g <- -0.0227
d13C.air.2005 <- d13C.air.1992 + g*(year-1992)

#-3 calculate ∆13C for the leaf
D13C <- (d13C.air.2005-d13C)/(1+d13C/1000)

# estimate ci/ca from ∆13C
a <- 4.4
b <- 27
x <- (D13C - a)/(b - a) # Bauhinia yunnanensis without d13C recorded in the published paper

# estimate Vcmax and Jmax with two-point method

#-1 estimate ca in the unit of Pa
pa <- ca*(101.3*exp(-0.114*elv)*0.001) # Pa

#-2 estimate KK coefficient in the unit of Pa
Kc25 <- 41.03 # Kc25 at sea level in Pa
Ko25 <- 28210 # Ko25 at sea level in Pa
Kc <- Kc25*exp((79430/8.3145)*(1/298-1/(Tleaf+273)))
Ko <- Ko25*exp((36380/8.3145)*(1/298-1/(Tleaf+273)))
Po <- 21000*exp(-0.114*elv) # Pa
K <- Kc*(1+Po/Ko) # Pa
k <- K/pa # deminsionless

gs25 <- 4.220    # Pa, assuming 25 deg C & 98.716 kPa)
dha  <- 37830    # J/mol
kR   <- 8.3145   # J/mol/K  
Y <- gs25 * exp( dha * ( Tleaf - 25.0 ) / ( 298.15 * kR * ( Tleaf + 273.15 ) ) ) # Pa
y <- Y/pa # demensionless

#-3 estimate Vcmax and Jmax
Vcmax <- Asat/((x-y)/(x+k)-0.01) # mmol m-2 s-1

write.table(data.frame(ZhengData,Vcmax),file="Data/5_Processed Data/Photosynthesis/Literatures/Zheng_Vcmax.csv",sep=",",row.names=F)




