# V is a matrix variable of precipitation: N rows for different sites, 12 columns for the total precipitation of each month
# readin the V variable from your data
TS <- function(V)
{
	day_start <- c(0,31,59,90,120,151,181,212,243,273,304,334)
	
	midday <- c()
	midday[1:11] <- (day_start[1:11] + day_start[2:12]-1)/2
	midday[12] <- (day_start[12]+365)/2

	angle_mid <- 2*pi*midday/365

	mVx <- t(t(V)*cos(angle_mid))
	mVy <- t(t(V)*sin(angle_mid))

	Vx <- apply(mVx,1,sum)
	Vy <- apply(mVy,1,sum)
	totalV <- apply(V,1,sum)

	timing <- atan2(Vy,Vx)
	timing[which((Vx!=0)&(timing<0))] <- 2*pi+timing[which((Vx!=0)&(timing<0))]   # convert from -pi to pi => 0 to 2pi
	timing <- timing/pi*6  # constrain timing at the range of 0 to 12

	seasonality <- sqrt(Vx^2+Vy^2)/totalV

	return(list(timing,seasonality))
}
