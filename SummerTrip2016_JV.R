# read in photosynthesis measurements in Yunnan campaign measured by Zhou and Henrique
# ambient CO2 for Jmax was set as 2000 ppm by Henrique

PhotoData <- read.table("Data/1_Original Data Sets/Photosynthesis/Yangling/our 2016 sites.csv",
                        header=T,sep=",",
                        stringsAsFactors = F)

elv <-  PhotoData$Elevation*0.001 # km

Asat <- PhotoData$Asat_Photo..umol.m2.s. # umol m-2 s-1 (though recorded as mmol/m2/s in the paper, it must be wrong)
Amax <- as.numeric(PhotoData$Amax_Photo..umol.m2.s.)
Tleaf.asat <- PhotoData$Asat_Tleaf...C. # degreeC
Tleaf.amax <- PhotoData$Amax_Tleaf..C. # degreeC
x.asat <- PhotoData$Asat_CiCa
x.amax <- PhotoData$Amax_CiCa

# estimate Vcmax and Jmax with two-point method

#-1 estimate ca in the unit of Pa
ca.asat <- PhotoData$Asat_CO2..ppm.
ca.amax <- PhotoData$Amax_CO2..ppm.
pa.asat <- ca.asat*(101.3*exp(-0.114*elv)*0.001) # Pa
pa.amax <- ca.amax*(101.3*exp(-0.114*elv)*0.001) # Pa

#-2 estimate KK coefficient in the unit of Pa
Kc25 <- 41.03 # Kc25 at sea level in Pa
Ko25 <- 28210 # Ko25 at sea level in Pa
Kc <- Kc25*exp((79430/8.3145)*(1/298-1/(Tleaf.asat+273)))
Ko <- Ko25*exp((36380/8.3145)*(1/298-1/(Tleaf.asat+273)))
Po <- 21000*exp(-0.114*elv) # Pa
K <- Kc*(1+Po/Ko) # Pa
k <- K/pa.asat # deminsionless

#-3 Gammastar in the unit of Pa
gs25 <- 4.220    # Pa, assuming 25 deg C & 98.716 kPa)
dha  <- 37830    # J/mol
kR   <- 8.3145   # J/mol/K  
Y.asat <- gs25 * exp( dha * ( Tleaf.asat - 25.0 ) / ( 298.15 * kR * ( Tleaf.asat + 273.15 ) ) ) # Pa
Y.amax <- gs25 * exp( dha * ( Tleaf.amax - 25.0 ) / ( 298.15 * kR * ( Tleaf.amax + 273.15 ) ) ) # Pa
y.asat <- Y.asat/pa.asat # demensionless
y.amax <- Y.amax/pa.amax # demensionless

#-4 estimate Vcmax and Jmax
Vcmax <- Asat/((x.asat-y.asat)/(x.asat+k)-0.01) # mmol m-2 s-1

Jmax <- 4*(Amax + 0.01*Vcmax)*(x.amax+2*y.amax)/(x.amax-y.amax) # mmol m-2 s-1

plot(Jmax,Vcmax)

write.table(data.frame(PhotoData,Vcmax,Jmax),file="Data/5_Processed Data/Photosynthesis/Yangling/Fieldwork2016_JV.csv",sep=",",row.names=F)

