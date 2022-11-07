#retrieval, imputation, and summary of instantaneous USGS velocity data
#USGS 11455350 CACHE SLOUGH A RYER ISLAND (inactive)
#USGS 11455385 CACHE SLOUGH AB RYER ISLAND FERRY NR RIO VISTA CA (active)

library(dataRetrieval)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(imputeTS)
library(glue)
library(zoo)

#data retrieval pull through NWIS web services

siteNumbers <- c("11455350")  #(2007-10-01 - 2019-04-01)

startDate <- ""

endDate <- ""

parameterCd <- "72255"

uvRYI <- readNWISuv(siteNumbers,parameterCd,startDate,endDate)

#convert to PST

hrs <- 8 * 60 * 60

uvRYI$dateTime_PST <- as.POSIXct(uvRYI$dateTime, format = "%Y-%m-%d %H:%M:%S") - hrs

uvRYI <- subset(uvRYI, select = c(4, 7))

uvRYI <- uvRYI %>%
  rename(velocity_ft_s = "X_72255_00000")

# write out to .rds
write_rds(uvRYI, glue("data-raw/Hydrology/uvRYI.rds"))

#Godin filter
Tgodinfn<-function(tint,xint) {

  # apply Godin filter
  # it includes three passes in time domain, with windows 24, 24, and 25 hrs long
  # will assume 15-min data here, so
  # for first pass, 48 points have to be skipped and last 44
  # window is 12-1-11 hrs in length, i.e. use previous 12 hours and
  # subsequent 11 hrs to define result at 13th hour.

  xfilt<-rep_len(NA,length.out=length(xint))

  # do first pass, with window of 12,1,11 hrs
  for (i in seq(from=49,to=(length(xint)-44))) {
    xfilt[i]=sum(xint[(i-48):(i+44)])/93
  }

  xfilt1<-xfilt
  # now 2nd pass, same approach but window is 11,1,12 hrs
  for (i in seq(from=45, to=(length(xint)-48))) {
    xfilt1[i]=sum(xfilt[(i-44):(i+48)])/93;
  }
  xfilt=xfilt1;
  # now 3rd pass with 12-1-12 window
  for (i in seq(from=49,to=(length(xint)-48))) {
    xfilt[i]=sum(xfilt1[(i-48):(i+48)])/97;
  }
  # Now throw away 36 hrs at each end
  xfilt[1:(36*4)]<-NA
  isize=length(xfilt)
  xfilt[(isize-36*4):isize]<-NA

  return(xfilt)

}

#apply Godin filter------------------------
uvRYI$vel_tf <- Tgodinfn(uvRYI$dateTime_PST, uvRYI$velocity_ft_s)

#tidal flow = velocity - net velocity(tidally-filtered)

uvRYI$tidal_vel <- uvRYI$velocity_ft_s - uvRYI$vel_tf

#plots - take a minute to load
#plot(uvRYI$dateTime_PST, uvRYI$velocity_ft_s)
#lines(uvRYI$dateTime_PST, uvRYI$tidal_vel, col = 3)
#lines(uvRYI$dateTime_PST, uvRYI$vel_tf, col = 2)
#legend("topright", legend = c("velocity", "tidal_vel", "net_vel"), col = c(1:3), lwd = 2)

#calculate amplitude- first break df in two
uvRYI_a <- subset(uvRYI, select = c(1, 2, 3))
uvRYI_b <- subset(uvRYI, select = c(2, 4))

#calcualte 30 hr moving window
uvRYI_b$roll_max <- rollapply(uvRYI_b$tidal_vel, 120, max, by = 1, partial = TRUE)
uvRYI_b$roll_min <- rollapply(uvRYI_b$tidal_vel, 120, min, by = 1, partial = TRUE)

#add high and low amplitude and divide by two
uvRYI_b$amp <- (uvRYI_b$roll_max + abs(uvRYI_b$roll_min))/2

#plots - take a minute to load
#plot(uvRYI_b$dateTime_PST, uvRYI_b$tidal_vel, col = 1, ylim = c(-5, 5), xlim = c(as.POSIXct('2020-10-01 17:30:00', format="%Y-%m-%d %H:%M:%S"), as.POSIXct('2020-10-15 17:30:00', format="%Y-%m-%d %H:%M:%S")))
#lines(uvRYI_b$dateTime_PST, uvRYI_b$roll_max, col = 2)
#lines(uvRYI_b$dateTime_PST, uvRYI_b$roll_min, col = 3)
#lines(uvRYI_b$dateTime_PST, uvRYI_b$amp, col = 5)
#legend("top", inset = c(-0.45, 0), legend = c("tidal_vel", "env_max", "env_min", "amp"), col = c(1:3, 5), lwd = 2)

#merge two dfs back together

uvRYI_c <- merge(uvRYI_a, uvRYI_b, by = 'dateTime_PST')

#subset

uvRYI_c <- subset(uvRYI_c, select = c(1:4, 7))

#calculate ratio of mean amplitude: mean tidal velocity

uvRYI_c$ratio <- uvRYI_c$amp/uvRYI_c$vel_tf

#>1 tidal influence, <1 river influence

#plot(uvRYI_c$dateTime_PST, uvRYI_c$ratio, ylim = c(-1000, 1000))

#write_rds(uvRYI_c, glue("data-raw/Hydrology/RYI_vel.rds"))

#downstep to daily - add n column

dvRYI <- uvRYI_c %>%
  mutate(date = as.Date(dateTime_PST)) %>%
  group_by(date) %>%
  summarise(mean_ratio = mean(ratio), mean_tidal_vel = mean(tidal_vel), mean_net_vel = mean(vel_tf), max_abs_velocity = max(abs(velocity_ft_s), na.rm = TRUE), min_vel_ft_s = min(velocity_ft_s, na.rm = TRUE), max_vel_ft_s = max(velocity_ft_s, na.rm = TRUE), mean_vel_ft_s = mean(velocity_ft_s, na.rm = TRUE), mean_amp = mean(amp), n_vel = n())

continous.dates <- data.frame (x = 1:4201, date = seq(as.Date('2007-10-01'),as.Date('2019-04-01'), by='day'))

RYI_vel_daily <- merge(continous.dates, dvRYI, by = "date", all = TRUE)

plot(RYI_vel_daily$date, RYI_vel_daily$n_vel)

plot(RYI_vel_daily$date, RYI_vel_daily$mean_vel_ft_s)

#if n_flow is NA replace with 0

RYI_vel_daily$n_vel[is.na(RYI_vel_daily$n_vel)] <- 0

sum(RYI_vel_daily$n_vel<=91)#78 days with <95% of flow measurements

#add column to identify if flow data is measured or will be imputed

RYI_vel_daily$group <- ifelse(RYI_vel_daily$n_vel>= 91, "measure", "impute")

#if n_value is <91, change mean, max, and min velocity to NA

RYI_vel_daily$mean_final_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$mean_vel_ft_s, NA)

RYI_vel_daily$max_final_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$max_vel_ft_s, NA)

RYI_vel_daily$min_final_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$min_vel_ft_s, NA)

RYI_vel_daily$max_abs_final_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$max_abs_velocity, NA)

RYI_vel_daily$final_tidal_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$mean_tidal_vel, NA)

RYI_vel_daily$final_net_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$mean_net_vel, NA)

RYI_vel_daily$final_ratio <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$mean_ratio, NA)

RYI_vel_daily$final_amp <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$mean_amp, NA)

#impute missing values

RYI_vel_daily$final_mean_vel_final <- na_ma(RYI_vel_daily$mean_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_max_vel_final <- na_ma(RYI_vel_daily$max_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_min_vel_final <- na_ma(RYI_vel_daily$min_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_max_abs_final <- na_ma(RYI_vel_daily$max_abs_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_ratio_final <- na_ma(RYI_vel_daily$final_ratio, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_tidal_vel_final <- na_ma(RYI_vel_daily$final_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_net_vel_final <- na_ma(RYI_vel_daily$final_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_amp_final <- na_ma(RYI_vel_daily$final_amp, k = 7, weighting = "exponential", maxgap = Inf)

summary(RYI_vel_daily)

plot(RYI_vel_daily$date, RYI_vel_daily$final_mean_vel_final, ylim = c(-4, 6))
lines(RYI_vel_daily$date, RYI_vel_daily$final_max_vel_final, col = 2)
lines(RYI_vel_daily$date, RYI_vel_daily$final_min_vel_final, col = 3)
lines(RYI_vel_daily$date, RYI_vel_daily$final_max_abs_final, col = 4)

RYI_vel_daily <- RYI_vel_daily %>%
  rename(mean_vel = final_mean_vel_final,
         max_vel = final_max_vel_final,
         min_vel = final_min_vel_final,
         max_abs_vel = final_max_abs_final,
         mean_tide_vel = final_tidal_vel_final,
         ratio_mean = final_ratio_final,
         net_vel_mean = final_net_vel_final,
         amp = final_amp_final)

RYI_vel_daily <- subset(RYI_vel_daily, select = c(1, 21:28))

#write out to .rds

#write_rds(RYI_vel_daily, glue("data-raw/Hydrology/RYI_vel_daily.rds"))

#repeat for RYF----------------------

siteNumbers <- c("11455385")  # 2018-07-03 	2022-07-17

startDate <- ""

endDate <- ""

parameterCd <- "72255"

uvRYF <- readNWISuv(siteNumbers,parameterCd,startDate,endDate)

#convert to PST

hrs <- 8 * 60 * 60

uvRYF$dateTime_PST <- as.POSIXct(uvRYF$dateTime, format = "%Y-%m-%d %H:%M:%S") - hrs

uvRYF <- subset(uvRYF, select = c(4, 7))

uvRYF <- uvRYF %>%
  rename(velocity_ft_s = "X_72255_00000")

#write out to .rds

write_rds(uvRYF, glue("data-raw/Hydrology/uvRYF.rds"))

#apply Godin filter

Tgodinfn<-function(tint,xint) {

  # apply Godin filter
  # it includes three passes in time domain, with windows 24, 24, and 25 hrs long
  # will assume 15-min data here, so
  # for first pass, 48 points have to be skipped and last 44
  # window is 12-1-11 hrs in length, i.e. use previous 12 hours and
  # subsequent 11 hrs to define result at 13th hour.

  xfilt<-rep_len(NA,length.out=length(xint))

  # do first pass, with window of 12,1,11 hrs
  for (i in seq(from=49,to=(length(xint)-44))) {
    xfilt[i]=sum(xint[(i-48):(i+44)])/93
  }

  xfilt1<-xfilt
  # now 2nd pass, same approach but window is 11,1,12 hrs
  for (i in seq(from=45, to=(length(xint)-48))) {
    xfilt1[i]=sum(xfilt[(i-44):(i+48)])/93;
  }
  xfilt=xfilt1;
  # now 3rd pass with 12-1-12 window
  for (i in seq(from=49,to=(length(xint)-48))) {
    xfilt[i]=sum(xfilt1[(i-48):(i+48)])/97;
  }
  # Now throw away 36 hrs at each end
  xfilt[1:(36*4)]<-NA
  isize=length(xfilt)
  xfilt[(isize-36*4):isize]<-NA

  return(xfilt)

}

#apply godin filter------------------------
uvRYF$vel_tf <- Tgodinfn(uvRYF$dateTime_PST, uvRYF$velocity_ft_s)

summary(uvRYF$vel_tf)

#tidal flow = velocity - net velocity(tidally-filtered)

uvRYF$tidal_vel <- uvRYF$velocity_ft_s - uvRYF$vel_tf

#plots - take a minute to load
#plot(uvRYF$dateTime_PST, uvRYF$velocity_ft_s)
#lines(uvRYF$dateTime_PST, uvRYF$tidal_vel, col = 3)
#lines(uvRYF$dateTime_PST, uvRYF$vel_tf, col = 2)
#legend("topright", legend = c("velocity", "tidal_vel", "net_vel"), col = c(1:3), lwd = 2)

#calculate amplitude- first break df in two
uvRYF_a <- subset(uvRYF, select = c(1, 2, 3))
uvRYF_b <- subset(uvRYF, select = c(2, 4))

#calcualte 30 hr moving window
uvRYF_b$roll_max <- rollapply(uvRYF_b$tidal_vel, 120, max, by = 1, partial = TRUE)
uvRYF_b$roll_min <- rollapply(uvRYF_b$tidal_vel, 120, min, by = 1, partial = TRUE)

#add high and low amplitude and divide by two
uvRYF_b$amp <- (uvRYF_b$roll_max + abs(uvRYF_b$roll_min))/2

#plots - take a minute to load
#plot(uvRYF_b$dateTime_PST, uvRYF_b$tidal_vel, col = 1, ylim = c(-5, 5), xlim = c(as.POSIXct('2020-10-01 17:30:00', format="%Y-%m-%d %H:%M:%S"), as.POSIXct('2020-10-15 17:30:00', format="%Y-%m-%d %H:%M:%S")))
#lines(uvRYF_b$dateTime_PST, uvRYF_b$roll_max, col = 2)
#lines(uvRYF_b$dateTime_PST, uvRYF_b$roll_min, col = 3)
#lines(uvRYF_b$dateTime_PST, uvRYF_b$amp, col = 5)
#legend("top", inset = c(-0.45, 0), legend = c("tidal_vel", "env_max", "env_min", "amp"), col = c(1:3, 5), lwd = 2)

#merge two dfs back together

uvRYF_c <- merge(uvRYF_a, uvRYF_b, by = 'dateTime_PST')

#subset

uvRYF_c <- subset(uvRYF_c, select = c(1:4, 7))

#calculate ratio of mean amplitude: mean tidal velocity

uvRYF_c$ratio <- uvRYF_c$amp/uvRYF_c$vel_tf

#>1 tidal influence, <1 river influence

#plot(uvRYF_c$dateTime_PST, uvRYF_c$ratio, ylim = c(-1000, 1000))

#write_rds(uvRYF_c, glue("data-raw/Hydrology/RYF_vel.rds"))

#downstep to daily - add n column

dvRYF <- uvRYF_c %>%
  mutate(date = as.Date(dateTime_PST)) %>%
  group_by(date) %>%
  summarise(mean_ratio = mean(ratio), mean_tidal_vel = mean(tidal_vel), mean_net_vel = mean(vel_tf), max_abs_velocity = max(abs(velocity_ft_s), na.rm = TRUE), min_vel_ft_s = min(velocity_ft_s, na.rm = TRUE), max_vel_ft_s = max(velocity_ft_s, na.rm = TRUE), mean_vel_ft_s = mean(velocity_ft_s, na.rm = TRUE), mean_amp = mean(amp), n_vel = n())

continous.dates <- data.frame (x = 1:1476, date = seq(as.Date('2018-07-03'),as.Date('2022-07-17'), by='day'))

RYF_vel_daily <- merge(continous.dates, dvRYF, by = "date", all = TRUE)

plot(RYF_vel_daily$date, RYF_vel_daily$n_vel)

plot(RYF_vel_daily$date, RYF_vel_daily$mean_vel_ft_s)

#if n_flow is NA replace with 0

RYF_vel_daily$n_vel[is.na(RYF_vel_daily$n_vel)] <- 0

sum(RYF_vel_daily$n_vel<=91)#24 days with <95% of flow measurements

#add column to identify if flow data is measured or will be imputed

RYF_vel_daily$group <- ifelse(RYF_vel_daily$n_vel>= 91, "measure", "impute")

#if n_value is <91, change mean, max, and min velocity to NA

RYF_vel_daily$mean_final_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$mean_vel_ft_s, NA)

RYF_vel_daily$max_final_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$max_vel_ft_s, NA)

RYF_vel_daily$min_final_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$min_vel_ft_s, NA)

RYF_vel_daily$max_abs_final_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$max_abs_velocity, NA)

RYF_vel_daily$final_tidal_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$mean_tidal_vel, NA)

RYF_vel_daily$final_net_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$mean_net_vel, NA)

RYF_vel_daily$final_ratio <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$mean_ratio, NA)

RYF_vel_daily$final_amp <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$mean_amp, NA)

#impute missing values

RYF_vel_daily$final_mean_vel_final <- na_ma(RYF_vel_daily$mean_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_max_vel_final <- na_ma(RYF_vel_daily$max_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_min_vel_final <- na_ma(RYF_vel_daily$min_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_max_abs_final <- na_ma(RYF_vel_daily$max_abs_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_ratio_final <- na_ma(RYF_vel_daily$final_ratio, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_tidal_vel_final <- na_ma(RYF_vel_daily$final_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_net_vel_final <- na_ma(RYF_vel_daily$final_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_amp_final <- na_ma(RYF_vel_daily$final_amp, k = 7, weighting = "exponential", maxgap = Inf)

summary(RYF_vel_daily)

plot(RYF_vel_daily$date, RYF_vel_daily$final_mean_vel_final, ylim = c(-4, 6))
lines(RYF_vel_daily$date, RYF_vel_daily$final_max_vel_final, col = 2)
lines(RYF_vel_daily$date, RYF_vel_daily$final_min_vel_final, col = 3)
lines(RYF_vel_daily$date, RYF_vel_daily$final_max_abs_final, col = 4)

RYF_vel_daily <- RYF_vel_daily %>%
  rename(mean_vel = final_mean_vel_final,
         max_vel = final_max_vel_final,
         min_vel = final_min_vel_final,
         max_abs_vel = final_max_abs_final,
         mean_tide_vel = final_tidal_vel_final,
         ratio_mean = final_ratio_final,
         net_vel_mean = final_net_vel_final,
         amp = final_amp_final)

RYF_vel_daily <- subset(RYF_vel_daily, select = c(1, 21:28))

#write out to .rds

#write_rds(RYF_vel_daily, glue("data-raw/Hydrology/RYF_vel_daily.rds"))

#merge RYI and RYF datasets---------------------
#first rename columns

#RYF_vel_daily <- read_rds("data-raw/Hydrology/RYF_vel_daily.rds")

#RYI_vel_daily <- read_rds("data-raw/Hydrology/RYI_vel_daily.rds")

RYI_vel_daily <- RYI_vel_daily %>%
  mutate(RYI_mean_vel = mean_vel,
         RYI_max_vel = max_vel,
         RYI_min_vel = min_vel,
         RYI_max_abs_vel = max_abs_vel,
         RYI_mean_tide_vel = mean_tide_vel,
         RYI_ratio_mean = ratio_mean,
         RYI_net_vel_mean = net_vel_mean,
         RYI_amp = amp
  )

RYF_vel_daily <- RYF_vel_daily %>%
  mutate(RYF_mean_vel = mean_vel,
         RYF_max_vel = max_vel,
         RYF_min_vel = min_vel,
         RYF_max_abs_vel = max_abs_vel,
         RYF_mean_tide_vel = mean_tide_vel,
         RYF_ratio_mean = ratio_mean,
         RYF_net_vel_mean = net_vel_mean,
         RYF_amp = amp
  )

RYI_RYF <- full_join(RYI_vel_daily, RYF_vel_daily, na.rm = TRUE, by = "date")

#run when using read_rds function
#RYI_RYF <- RYI_RYF[-c(2:9, 18:25)]

#create final vel vector with if else statement: if RYF is na then fill in with RYI

RYI_RYF$mean_vel <- with(RYI_RYF, ifelse(is.na(RYF_mean_vel), RYI_mean_vel, RYF_mean_vel))

RYI_RYF$min_vel <- with(RYI_RYF, ifelse(is.na(RYF_min_vel), RYI_min_vel, RYF_min_vel))

RYI_RYF$max_vel <- with(RYI_RYF, ifelse(is.na(RYF_max_vel), RYI_max_vel, RYF_max_vel))

RYI_RYF$max_abs_vel <- with(RYI_RYF, ifelse(is.na(RYF_max_abs_vel), RYI_max_abs_vel, RYF_max_abs_vel))

RYI_RYF$mean_tide_vel <- with(RYI_RYF, ifelse(is.na(RYF_mean_tide_vel), RYI_mean_tide_vel, RYF_mean_tide_vel))

RYI_RYF$ratio_mean <- with(RYI_RYF, ifelse(is.na(RYF_ratio_mean), RYI_ratio_mean, RYF_ratio_mean))

RYI_RYF$net_vel_mean <- with(RYI_RYF, ifelse(is.na(RYF_net_vel_mean), RYI_net_vel_mean, RYF_net_vel_mean))

RYI_RYF$amp <- with(RYI_RYF, ifelse(is.na(RYF_amp), RYI_amp, RYF_amp))

RYI_RYF <- rename(RYI_RYF, Date = date)

RYI_RYF <- subset(RYI_RYF, select = c(1, 18:25))

#create sign column

RYI_RYF <- RYI_RYF %>%
  mutate(sign=case_when(abs(min_vel) > max_vel ~ "-", abs(min_vel) < max_vel ~ "+"))

RYI_RYF$station <- "Cache"

#write to .rds

write_rds(RYI_RYF, "data-raw/Hydrology/dv_cache.rds")



