dat_loc <- "../data/"
NO2dat_loc <- paste0(dat_loc, "NO2_epa/")

no2raw <- lapply(paste0(NO2dat_loc,list.files(NO2dat_loc)), read.csv)

dim(no2raw[[1]])
head(no2raw[[1]])

date <- unlist(lapply(no2raw, function(x){x$Date}))
no2con <- unlist(lapply(no2raw, function(x){x$Daily.Max.1.hour.NO2.Concentration}))

no2 <- data.frame(date=date, conc=no2con)
no2$year <- format(as.Date(no2$date, format="%m/%d/%Y"), "%Y")
no2$month <- format(as.Date(no2$date, format="%m/%d/%Y"), "%m")
head(no2)

plot(no2con)



