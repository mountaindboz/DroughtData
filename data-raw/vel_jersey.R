#retrieval, imputation, and summary of instantaneous USGS velocity data
#USGS 11337190 SAN JOAQUIN R A JERSEY POINT CA

library(dataRetrieval)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(imputeTS)
library(zoo)
library(glue)

siteNumbers <- c("11337190")

startDate <- ""

endDate <- ""

parameterCd <- "72255"

uvSJJ <- readNWISuv(siteNumbers,parameterCd,startDate,endDate)

#convert to PST

hrs <- 8 * 60 * 60

uvSJJ$dateTime_PST <- as.POSIXct(uvSJJ$dateTime, format = "%Y-%m-%d %H:%M:%S") - hrs

uvSJJ <- subset(uvSJJ, select = c(4, 7))

uvSJJ <- uvSJJ %>%
  rename(velocity_ft_s = "X_72255_00000")

uvSJJ <- subset(uvSJJ, select = c(2:3))

#write out to .rds

write_rds(uvSJJ, glue("data-raw/Hydrology/uvSJJ.rds"))

uvSJJ <- read_rds("data-raw/Hydrology/uvSJJ.rds")

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
uvSJJ$vel_tf <- Tgodinfn(uvSJJ$dateTime_PST, uvSJJ$velocity_ft_s)

#tidal flow = velocity - net velocity(tidally-filtered)

uvSJJ$tidal_vel <- uvSJJ$velocity_ft_s - uvSJJ$vel_tf

#plots - take a minute to load
#plot(uvSJJ$dateTime_PST, uvSJJ$velocity_ft_s)
#lines(uvSJJ$dateTime_PST, uvSJJ$tidal_vel, col = 3)
#lines(uvSJJ$dateTime_PST, uvSJJ$vel_tf, col = 2)
#legend("topright", legend = c("velocity", "tidal_vel", "net_vel"), col = c(1:3), lwd = 2)

#calculate amplitude- first break df in two
uvSJJ_a <- subset(uvSJJ, select = c(1, 2, 3))
uvSJJ_b <- subset(uvSJJ, select = c(2, 4))

#calcualte 30 hr moving window
uvSJJ_b$roll_max <- rollapply(uvSJJ_b$tidal_vel, 120, max, by = 1, partial = TRUE)
uvSJJ_b$roll_min <- rollapply(uvSJJ_b$tidal_vel, 120, min, by = 1, partial = TRUE)

#add high and low amplitude and divide by two
uvSJJ_b$amp <- (uvSJJ_b$roll_max + abs(uvSJJ_b$roll_min))/2

#plots - take a minute to load
#plot(uvSJJ_b$dateTime_PST, uvSJJ_b$tidal_vel, col = 1, ylim = c(-5, 5), xlim = c(as.POSIXct('2020-10-01 17:30:00', format="%Y-%m-%d %H:%M:%S"), as.POSIXct('2020-10-15 17:30:00', format="%Y-%m-%d %H:%M:%S")))
#lines(uvSJJ_b$dateTime_PST, uvSJJ_b$roll_max, col = 2)
#lines(uvSJJ_b$dateTime_PST, uvSJJ_b$roll_min, col = 3)
#lines(uvSJJ_b$dateTime_PST, uvSJJ_b$amp, col = 5)
#legend("top", inset = c(-0.45, 0), legend = c("tidal_vel", "env_max", "env_min", "amp"), col = c(1:3, 5), lwd = 2)

#merge two dfs back together

uvSJJ_c <- merge(uvSJJ_a, uvSJJ_b, by = 'dateTime_PST')

#subset

uvSJJ_c <- subset(uvSJJ_c, select = c(1:4, 7))

#calculate ratio of mean amplitude: mean tidal velocity

uvSJJ_c$ratio <- uvSJJ_c$amp/uvSJJ_c$vel_tf

#>1 tidal influence, <1 river influence

#plot(uvSJJ_c$dateTime_PST, uvSJJ_c$ratio, ylim = c(-1000, 1000))

#write out to .rds

#write_rds(uvSJJ_c, glue("data-raw/Hydrology/SJJ_vel.rds"))

#downstep to daily - add n column
dvSJJ <- uvSJJ_c %>%
  mutate(Date = as.Date(dateTime_PST, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
  group_by(Date) %>%
  summarise(max_tidal_vel = max(tidal_vel, na.rm = TRUE),
            min_tidal_vel = min(tidal_vel, na.rm = TRUE),
            max_net_vel = max(vel_tf, na.rm = TRUE),
            min_net_vel = min(vel_tf, na.rm = TRUE),
            mean_net_vel = mean(vel_tf, na.rm = TRUE),
            max_abs_tidal = max(abs(tidal_vel), na.rm = TRUE),
            n_vel = n())

continous.dates <- data.frame (x = 1:5540, Date = seq(as.Date('2007-10-01'),as.Date('2022-11-30'), by='day'))

SJJ_vel_daily <- merge(continous.dates, dvSJJ, by = "Date", all = TRUE)

#if n_flow is NA replace with 0

#SJJ_vel_daily[is.nan(SJJ_vel_daily)]<-NA

SJJ_vel_daily$n_vel[is.na(SJJ_vel_daily$n_vel)] <- 0

sum(SJJ_vel_daily$n_vel<=91)#132 days with <95% of flow measurements

#add column to identify if flow data is measured or will be imputed

SJJ_vel_daily$group <- ifelse(SJJ_vel_daily$n_vel>= 91, "measure", "impute")

#if n_value is <91, change mean, max, and min velocity to NA

SJJ_vel_daily$max_tidal_vel <- ifelse(SJJ_vel_daily$n_vel>= 91, SJJ_vel_daily$max_tidal_vel, NA)

SJJ_vel_daily$min_tidal_vel <- ifelse(SJJ_vel_daily$n_vel>= 91, SJJ_vel_daily$min_tidal_vel, NA)

SJJ_vel_daily$max_net_vel <- ifelse(SJJ_vel_daily$n_vel>= 91, SJJ_vel_daily$max_net_vel, NA)

SJJ_vel_daily$min_net_vel <- ifelse(SJJ_vel_daily$n_vel>= 91, SJJ_vel_daily$min_net_vel, NA)

SJJ_vel_daily$max_abs_tidal <- ifelse(SJJ_vel_daily$n_vel>= 91, SJJ_vel_daily$max_abs_tidal, NA)

SJJ_vel_daily$mean_net_vel <- ifelse(SJJ_vel_daily$n_vel>= 91, SJJ_vel_daily$mean_net_vel, NA)
#impute missing values

SJJ_vel_daily$final_max_tidal <- na_ma(SJJ_vel_daily$max_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

SJJ_vel_daily$final_min_tidal <- na_ma(SJJ_vel_daily$min_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

SJJ_vel_daily$final_max_net <- na_ma(SJJ_vel_daily$max_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

SJJ_vel_daily$final_min_net <- na_ma(SJJ_vel_daily$min_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

SJJ_vel_daily$final_max_abs_tidal <- na_ma(SJJ_vel_daily$max_abs_tidal, k = 7, weighting = "exponential", maxgap = Inf)

SJJ_vel_daily$final_mean_net <- na_ma(SJJ_vel_daily$mean_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

summary(SJJ_vel_daily)

SJJ_vel_daily <- SJJ_vel_daily %>%
  rename(max_tidal = final_max_tidal,
         min_tidal = final_min_tidal,
         max_net = final_max_net,
         min_net = final_min_net,
         maxabs_tidal = final_max_abs_tidal,
         mean_net = final_mean_net)

SJJ_vel_daily <- subset(SJJ_vel_daily, select = c(1, 11:16))

SJJ_vel_daily$station <- "SJJ"

#create column to assign sign for max abs velocity column

SJJ_vel_daily <- SJJ_vel_daily %>%
  mutate(net_sign=case_when(abs(min_net) > max_net ~ "-", abs(min_net) < max_net ~ "+"),
         tide_sign=case_when(abs(min_tidal) > max_tidal ~ "-", abs(min_tidal) < max_tidal ~ "+"))

write_rds(SJJ_vel_daily, glue("data-raw/Hydrology/dv_jersey.rds"))



