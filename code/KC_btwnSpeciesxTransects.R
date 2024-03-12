library(wsyn)

dat_loc <- "../data/"
#NO2dat_loc <- paste0(dat_loc, "NO2_epa/")
KCfiles <- list.files(dat_loc, pattern="KC")
filename <- paste0(dat_loc,KCfiles[[1]])

KC <- read.csv(filename, skip=1)
head(KC)[1:10]

str(KC)
dim(KC)
sum(KC$Transect1==KC$Transect2) #same

KC[is.na(KC)] <- 0

transects <- unique(KC$Transect2)
KC[KC$Transect2=="",]
transects <- transects[transects!=""]

years <- unique(KC$Year)
years <- years[!is.na(years)]

#what species? 
names(KC)

sp <- names(KC)[c(32, 94, 113, 140, 156)]
species <- as.list(sp); names(species) <- sp

sum(KC$Calycadenia.multiglandulosa) #always zero??

transects_species <- as.list(transects)
names(transects_species) <- transects 


for (t in transects){
	for (s in sp){
		spallyears <- KC[KC$Transect2==t,c("Year",s)]
		spyearmean <- data.frame(year=years, cover=-1)
		for (y in years){
			yearmean <- mean(spallyears[,s][spallyears$Year==y])
			spyearmean$cover[spyearmean$year==y] <- yearmean
		}
		species[[s]] <- spyearmean
	}
	transects_species[[t]] <- species
}

###########synchrony stuff########
spPairs <- t(combn(sp,2))
#spPairs <- spPairs[5:6,]

fig_loc <- "../figures/SpTransectPairs_wpmf.pdf"
pdf(fig_loc)
par(mfrow=c(4,3), mar=c(2,2,2,0), xpd=F, oma=c(0,1,0,1))
#layout(matrix(c(	1,1,1,1,2,2,2,2,3,3,3,3,
#				4,4,4,4,5,5,5,5,6,6,6,6,
#				7,7,7,8,8,8,9,9,9,10,10,10), nrow=3, byrow=T))
#layout.show(10)
for (t in transects){
	plot.new(); title(main=t, cex.main=2, line=-4)
	transect <- transects_species[[t]]
	for (s in 1:nrow(spPairs)){
		s1 <- spPairs[s,1]; s2 <- spPairs[s,2]
		sp2 <- transect[[s2]]$cover
		sp1 <- transect[[s1]]$cover
		
		times <- 1:length(years)
		ts <- rbind(sp1,sp2)
		dat<-cleandat(ts, times=times, clev=1)$cdat
		res<-wpmf(dat, times, sigmethod="quick")
				
		transectSp <- paste(s1,s2, sep=" - ")
		#fig_loc <- paste0("../figures/",transectSp,"_wpmf.pdf")
				
		if(any(rowSums(ts)==0)){
			plot(years, ts[1,], ylim=c(0,max(ts)), type='l', ylab="cover")
			lines(years, ts[2,], lty=2)
		} else{
			#pdf(fig_loc)
			plotmag(res, colorbar=(s==2)); 
			#dev.off()
		}
		title(main=transectSp, font.main=1, cex.main=0.8, line=0.5)
	}
	plot.new()
}
dev.off()









