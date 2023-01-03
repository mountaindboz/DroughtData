#retrieval, imputation, and summary of instantaneous USGS velocity data
#USGS 11313405 OLD R A BACON ISLAND CA

library(dataRetrieval)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(imputeTS)
library(glue)
library(zoo)

#data retrieval pull through NWIS web services

siteNumbers <- c("11313405")  #(2007-10-01 - present)

startDate <- ""

endDate <- ""

parameterCd <- "72255"

uvOLD <- readNWISuv(siteNumbers,parameterCd,startDate,endDate)

hrs <- 8 * 60 * 60

uvOLD$dateTime_PST <- as.POSIXct(uvOLD$dateTime, format = "%Y-%m-%d %H:%M:%S") - hrs

uvOLD <- subset(uvOLD, select = c(4, 9))

uvOLD <- uvOLD %>%
  rename(velocity_ft_s = "X_72255_00000")

write_rds(uvOLD,glue("data-raw/Hydrology/uvOLD.rds"))

uvOLD <- read_rds("uvOLD.rds")

#plot(uvOLD$dateTime_PST, uvOLD$velocity_ft_s)

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
uvOLD$vel_tf <- Tgodinfn(uvOLD$dateTime_PST, uvOLD$velocity_ft_s)

#tidal flow = velocity - net velocity(tidally-filtered)

uvOLD$tidal_vel <- uvOLD$velocity_ft_s - uvOLD$vel_tf

#plots - take a minute to load
#plot(uvOLD$dateTime_PST, uvOLD$velocity_ft_s)
#lines(uvOLD$dateTime_PST, uvOLD$tidal_vel, col = 3)
#lines(uvOLD$dateTime_PST, uvOLD$vel_tf, col = 2)
#legend("topright", legend = c("velocity", "tidal_vel", "net_vel"), col = c(1:3), lwd = 2)

#calculate amplitude- first break df in two
uvOLD_a <- subset(uvOLD, select = c(1, 2, 3))
uvOLD_b <- subset(uvOLD, select = c(2, 4))

#calcualte 30 hr moving window
uvOLD_b$roll_max <- rollapply(uvOLD_b$tidal_vel, 120, max, by = 1, partial = TRUE)
uvOLD_b$roll_min <- rollapply(uvOLD_b$tidal_vel, 120, min, by = 1, partial = TRUE)

#add high and low amplitude and divide by two
uvOLD_b$amp <- (uvOLD_b$roll_max + abs(uvOLD_b$roll_min))/2

#plots - take a minute to load
#plot(uvOLD_b$dateTime_PST, uvOLD_b$tidal_vel, col = 1, ylim = c(-5, 5), xlim = c(as.POSIXct('2020-10-01 17:30:00', format="%Y-%m-%d %H:%M:%S"), as.POSIXct('2020-10-15 17:30:00', format="%Y-%m-%d %H:%M:%S")))
#lines(uvOLD_b$dateTime_PST, uvOLD_b$roll_max, col = 2)
#lines(uvOLD_b$dateTime_PST, uvOLD_b$roll_min, col = 3)
#lines(uvOLD_b$dateTime_PST, uvOLD_b$amp, col = 5)
#legend("top", inset = c(-0.45, 0), legend = c("tidal_vel", "env_max", "env_min", "amp"), col = c(1:3, 5), lwd = 2)

#merge two dfs back together

uvOLD_c <- merge(uvOLD_a, uvOLD_b, by = 'dateTime_PST')

#subset

uvOLD_c <- subset(uvOLD_c, select = c(1:4, 7))

#calculate ratio of mean amplitude: mean tidal velocity

uvOLD_c$ratio <- uvOLD_c$amp/uvOLD_c$vel_tf

#>1 tidal influence, <1 river influence

#plot(uvOLD_c$dateTime_PST, uvOLD_c$ratio, ylim = c(-1000, 1000))

#write out to .rds

#write_rds(uvOLD_c, glue("data-raw/Hydrology/OLD_vel.rds"))

#downstep to daily - add n column
dvOLD <- uvOLD_c %>%
  mutate(Date = as.Date(dateTime_PST, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
  group_by(Date) %>%
  summarise(max_tidal_vel = max(tidal_vel, na.rm = TRUE),
            min_tidal_vel = min(tidal_vel, na.rm = TRUE),
            max_net_vel = max(vel_tf, na.rm = TRUE),
            min_net_vel = min(vel_tf, na.rm = TRUE),
            mean_net_vel = mean(vel_tf, na.rm = TRUE),
            max_abs_tidal = max(abs(tidal_vel), na.rm = TRUE),
            n_vel = n())

continous.dates <- data.frame (x = 1:5175, Date = seq(as.Date('2007-10-01'),as.Date('2021-11-30'), by='day'))

OLD_vel_daily <- merge(continous.dates, dvOLD, by = "Date", all = TRUE)

#if n_flow is NA replace with 0

#OLD_vel_daily[is.nan(OLD_vel_daily)]<-NA

OLD_vel_daily$n_vel[is.na(OLD_vel_daily$n_vel)] <- 0

sum(OLD_vel_daily$n_vel<=91)#132 days with <95% of flow measurements

#add column to identify if flow data is measured or will be imputed

OLD_vel_daily$group <- ifelse(OLD_vel_daily$n_vel>= 91, "measure", "impute")

#if n_value is <91, change mean, max, and min velocity to NA

OLD_vel_daily$max_tidal_vel <- ifelse(OLD_vel_daily$n_vel>= 91, OLD_vel_daily$max_tidal_vel, NA)

OLD_vel_daily$min_tidal_vel <- ifelse(OLD_vel_daily$n_vel>= 91, OLD_vel_daily$min_tidal_vel, NA)

OLD_vel_daily$max_net_vel <- ifelse(OLD_vel_daily$n_vel>= 91, OLD_vel_daily$max_net_vel, NA)

OLD_vel_daily$min_net_vel <- ifelse(OLD_vel_daily$n_vel>= 91, OLD_vel_daily$min_net_vel, NA)

OLD_vel_daily$max_abs_tidal <- ifelse(OLD_vel_daily$n_vel>= 91, OLD_vel_daily$max_abs_tidal, NA)

OLD_vel_daily$mean_net_vel <- ifelse(OLD_vel_daily$n_vel>= 91, OLD_vel_daily$mean_net_vel, NA)
#impute missing values

OLD_vel_daily$final_max_tidal <- na_ma(OLD_vel_daily$max_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

OLD_vel_daily$final_min_tidal <- na_ma(OLD_vel_daily$min_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

OLD_vel_daily$final_max_net <- na_ma(OLD_vel_daily$max_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

OLD_vel_daily$final_min_net <- na_ma(OLD_vel_daily$min_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

OLD_vel_daily$final_max_abs_tidal <- na_ma(OLD_vel_daily$max_abs_tidal, k = 7, weighting = "exponential", maxgap = Inf)

OLD_vel_daily$final_mean_net <- na_ma(OLD_vel_daily$mean_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

summary(OLD_vel_daily)

OLD_vel_daily <- OLD_vel_daily %>%
  rename(max_tidal = final_max_tidal,
         min_tidal = final_min_tidal,
         max_net = final_max_net,
         min_net = final_min_net,
         maxabs_tidal = final_max_abs_tidal,
         mean_net = final_mean_net)

OLD_vel_daily <- subset(OLD_vel_daily, select = c(1, 11:16))

OLD_vel_daily$station <- "OLD"

#create column to assign sign for max abs velocity column

OLD_vel_daily <- OLD_vel_daily %>%
  mutate(net_sign=case_when(abs(min_net) > max_net ~ "-", abs(min_net) < max_net ~ "+"),
         tide_sign=case_when(abs(min_tidal) > max_tidal ~ "-", abs(min_tidal) < max_tidal ~ "+"))

write_rds(OLD_vel_daily, glue("dv_old.rds"))



