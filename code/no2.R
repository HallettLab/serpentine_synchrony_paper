dat_loc <- "../data/"
NO2dat_loc <- paste0(dat_loc, "NO2_epa/")

no2raw <- lapply(paste0(NO2dat_loc,list.files(NO2dat_loc)), read.csv)

dim(no2raw[[1]])
head(no2raw[[1]])

date <- unlist(lapply(no2raw, function(x){x$Date}))
no2con <- unlist(lapply(no2raw, function(x){x$Daily.Max.1.hour.NO2.Concentration}))

no2 <- data.frame(date=date, conc=no2con)
years <- format(as.Date(no2$date, format="%m/%d/%Y"), "%Y")
months <- format(as.Date(no2$date, format="%m/%d/%Y"), "%m")
days <- as.numeric(format(as.Date(no2$date, format="%m/%d/%Y"), "%d"))

time <- data.frame(yr=as.numeric(years), mnth=as.numeric(months))
str(time)
no2 <- cbind(no2, time, days)
str(no2)
head(no2)
time <- unique(time)
time <- time[order(time$yr,time$mnth),]
no2 <- no2[order(no2$yr, no2$mnth, no2$days),]

par(mfrow=c(5,5), mar=c(1,1,1,0))
for (y in unique(time$yr)){
	dat <- no2[no2$yr==y,]
	plot(dat$mnth, dat$conc, main=y)
}

plot(no2con)

i <- 1
for (y in unique(time$yr)){
	for (m in unique(time$mnth)){
		no2$monthMean[i] <- mean(no2$conc[no2$yr==y & no2$mnth==m])
		i <- i+1
	}
}


monthMean <- NA
for (t in 1:nrow(time)){
	yr <- time$yr[t]; mnth <- time$mnth[t]
	dat <- no2$conc[no2$yr==yr & no2$mnth==mnth]
	print(paste(yr, mnth)); print(dat)
	monthMean[t] <- mean(dat)	
}
str(monthMean); 
time$monthMean <- monthMean
head(no2)

par(mfrow=c(1,1))
plot(time$monthMean, type='l')








