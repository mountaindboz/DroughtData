#retrieval, imputation, and summary of instantaneous USGS velocity data
#USGS 11312676 MIDDLE R AT MIDDLE RIVER CA

library(dataRetrieval)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(imputeTS)
library(glue)
library(zoo)

#data retrieval pull through NWIS web services

siteNumbers <- c("11312676")  #(2007-10-01 - 2019-04-01)

startDate <- ""

endDate <- ""

parameterCd <- "72255"

uvMDM <- readNWISuv(siteNumbers,parameterCd,startDate,endDate)

#convert to PST

hrs <- 8 * 60 * 60

uvMDM$dateTime_PST <- as.POSIXct(uvMDM$dateTime, format = "%d/%m/%Y %H:%M:%S") - hrs

uvMDM <- subset(uvMDM, select = c(4, 7))

uvMDM <- uvMDM %>%
  rename(velocity_ft_s = "X_72255_00000")

write_rds(uvMDM, glue("data-raw/Hydrology/uvMDM.rds"))

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

#apply godin filter------------------------
uvMDM$vel_tf <- Tgodinfn(uvMDM$dateTime_PST, uvMDM$velocity_ft_s)

#tidal flow = velocity - net velocity(tidally-filtered)

uvMDM$tidal_vel <- uvMDM$velocity_ft_s - uvMDM$vel_tf

#plots - take a minute to load
#plot(uvMDM$dateTime_PST, uvMDM$velocity_ft_s)
#lines(uvMDM$dateTime_PST, uvMDM$tidal_vel, col = 3)
#lines(uvMDM$dateTime_PST, uvMDM$vel_tf, col = 2)

#calculate amplitude - first break df in two
uvMDM_a <- subset(uvMDM, select = c(1, 2, 3))
uvMDM_b <- subset(uvMDM, select = c(2, 4))

#calcualte 30 hr moving window
uvMDM_b$roll_max <- rollapply(uvMDM_b$tidal_vel, 120, max, by = 1, partial = TRUE)
uvMDM_b$roll_min <- rollapply(uvMDM_b$tidal_vel, 120, min, by = 1, partial = TRUE)

#add high and low amplitude and divide by two
uvMDM_b$amp <- (uvMDM_b$roll_max + abs(uvMDM_b$roll_min))/2

#plots - take a minute to load
#plot(MDM_vel_b$dateTime_PST, MDM_vel_b$tidal_vel, col = 1, ylim = c(-5, 5)) #xlim = c(as.POSIXct('2020-10-01 17:30:00', format="%Y-%m-%d %H:%M:%S"), as.POSIXct('2020-10-15 17:30:00', format="%Y-%m-%d %H:%M:%S")))
#lines(MDM_vel_b$dateTime_PST, MDM_vel_b$roll_max, col = 2)
#lines(MDM_vel_b$dateTime_PST, MDM_vel_b$roll_min, col = 3)
#lines(MDM_vel_b$dateTime_PST, MDM_vel_b$amp, col = 5)

#merge two dfs back together

uvMDM_c <- merge(uvMDM_a, uvMDM_b, by = 'dateTime_PST')

#subset

uvMDM_c <- subset(uvMDM_c, select = c(1:4, 7))

#calculate ratio of mean amplitude: mean tidal velocity

uvMDM_c$ratio <- uvMDM_c$amp/uvMDM_c$vel_tf

#>1 tidal influence, <1 river influence

#plot(MDM_vel_c$dateTime_PST, MDM_vel_c$ratio, ylim = c(-1000, 1000))

#wrote out .rds

#write_rds(uvMDM_c, glue("data-raw/Hydrology/MDM_vel.rds"))

#downstep to daily - add n column
dvMDM <- uvMDM_c %>%
  mutate(date = as.Date(dateTime_PST)) %>%
  group_by(date) %>%
  summarise(mean_ratio = mean(ratio), mean_tidal_vel = mean(tidal_vel), mean_net_vel = mean(vel_tf), max_abs_vel_ft_s = max(abs(velocity_ft_s), na.rm = TRUE), min_vel_ft_s = min(velocity_ft_s, na.rm = TRUE), max_vel_ft_s = max(velocity_ft_s, na.rm = TRUE), mean_vel_ft_s = mean(velocity_ft_s, na.rm = TRUE), mean_amp = mean(amp), n_vel = n())

continous.dates <- data.frame (x = 1:5418, date = seq(as.Date('2007-10-01'),as.Date('2022-07-31'), by='day'))

MDM_vel_daily <- merge(continous.dates, dvMDM, by = "date", all = TRUE)

plot(MDM_vel_daily$date, MDM_vel_daily$n_vel)

plot(MDM_vel_daily$date, MDM_vel_daily$mean_vel_ft_s)

#if n_flow is NA replace with 0

MDM_vel_daily$n_vel[is.na(MDM_vel_daily$n_vel)] <- 0

sum(MDM_vel_daily$n_vel<=91)#201 days with <95% of flow measurements

#add column to identify if flow data is measured or will be imputed

MDM_vel_daily$group <- ifelse(MDM_vel_daily$n_vel>= 91, "measure", "impute")

#if n_value is <91, change mean, max, and min velocity to NA

MDM_vel_daily$mean_final_vel <- ifelse(MDM_vel_daily$n_vel>= 91, MDM_vel_daily$mean_vel_ft_s, NA)

MDM_vel_daily$max_final_vel <- ifelse(MDM_vel_daily$n_vel>= 91, MDM_vel_daily$max_vel_ft_s, NA)

MDM_vel_daily$min_final_vel <- ifelse(MDM_vel_daily$n_vel>= 91, MDM_vel_daily$min_vel_ft_s, NA)

MDM_vel_daily$max_abs_final_vel <- ifelse(MDM_vel_daily$n_vel>= 91, MDM_vel_daily$max_abs_vel_ft_s, NA)

MDM_vel_daily$final_tidal_vel <- ifelse(MDM_vel_daily$n_vel>= 91, MDM_vel_daily$mean_tidal_vel, NA)

MDM_vel_daily$final_net_vel <- ifelse(MDM_vel_daily$n_vel>= 91, MDM_vel_daily$mean_net_vel, NA)

MDM_vel_daily$final_ratio <- ifelse(MDM_vel_daily$n_vel>= 91, MDM_vel_daily$mean_ratio, NA)

MDM_vel_daily$final_amp <- ifelse(MDM_vel_daily$n_vel>= 91, MDM_vel_daily$mean_amp, NA)

#impute missing values

MDM_vel_daily$final_mean_vel_final <- na_ma(MDM_vel_daily$mean_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

MDM_vel_daily$final_max_vel_final <- na_ma(MDM_vel_daily$max_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

MDM_vel_daily$final_min_vel_final <- na_ma(MDM_vel_daily$min_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

MDM_vel_daily$final_max_abs_final <- na_ma(MDM_vel_daily$max_abs_final_vel, k = 7, weighting = "exponential", maxgap = Inf)

MDM_vel_daily$final_ratio_final <- na_ma(MDM_vel_daily$final_ratio, k = 7, weighting = "exponential", maxgap = Inf)

MDM_vel_daily$final_tidal_vel_final <- na_ma(MDM_vel_daily$final_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

MDM_vel_daily$final_net_vel_final <- na_ma(MDM_vel_daily$final_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

MDM_vel_daily$final_amp_final <- na_ma(MDM_vel_daily$final_amp, k = 7, weighting = "exponential", maxgap = Inf)

#summary(MDM_vel_daily)

#plot(MDM_vel_daily$date, MDM_vel_daily$final_max_abs_final, ylim = c(-0.6, 2))
#lines(MDM_vel_daily$date, MDM_vel_daily$final_tidal_vel_final, col = 2)
#lines(MDM_vel_daily$date, MDM_vel_daily$final_net_vel_final, col = 3)

MDM_vel_daily <- MDM_vel_daily %>%
  rename(mean_vel = final_mean_vel_final,
         max_vel = final_max_vel_final,
         min_vel = final_min_vel_final,
         max_abs_vel = final_max_abs_final,
         mean_tide_vel = final_tidal_vel_final,
         ratio_mean = final_ratio_final,
         net_vel_mean = final_net_vel_final,
         amp = final_amp_final)

MDM_vel_daily <- subset(MDM_vel_daily, select = c(1, 21:28))

MDM_vel_daily <- rename(MDM_vel_daily, Date = date)

MDM_vel_daily$station <- "Middle"

#create column to assign sign for max abs velocity column

MDM_vel_daily <- MDM_vel_daily %>%
  mutate(sign=case_when(abs(min_vel) > max_vel ~ "-", abs(min_vel) < max_vel ~ "+"))

write_rds(MDM_vel_daily, glue("data-raw/Hydrology/dv_middle.rds"))
