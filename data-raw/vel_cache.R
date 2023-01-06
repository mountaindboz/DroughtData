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

#read .rds

uvRYI <- read_rds("data-raw/Hydrology/uvRYI.rds")

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

#write out to .rds

#write_rds(uvRYI_c, glue("data-raw/Hydrology/RYI_vel.rds"))

#downstep to daily - add n column
dvRYI <- uvRYI_c %>%
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

RYI_vel_daily <- merge(continous.dates, dvRYI, by = "Date", all = TRUE)

#if n_flow is NA replace with 0

#RYI_vel_daily[is.nan(RYI_vel_daily)]<-NA

RYI_vel_daily$n_vel[is.na(RYI_vel_daily$n_vel)] <- 0

sum(RYI_vel_daily$n_vel<=91)#132 days with <95% of flow measurements

#add column to identify if flow data is measured or will be imputed

RYI_vel_daily$group <- ifelse(RYI_vel_daily$n_vel>= 91, "measure", "impute")

#if n_value is <91, change mean, max, and min velocity to NA

RYI_vel_daily$max_tidal_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$max_tidal_vel, NA)

RYI_vel_daily$min_tidal_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$min_tidal_vel, NA)

RYI_vel_daily$max_net_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$max_net_vel, NA)

RYI_vel_daily$min_net_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$min_net_vel, NA)

RYI_vel_daily$max_abs_tidal <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$max_abs_tidal, NA)

RYI_vel_daily$mean_net_vel <- ifelse(RYI_vel_daily$n_vel>= 91, RYI_vel_daily$mean_net_vel, NA)
#impute missing values

RYI_vel_daily$final_max_tidal <- na_ma(RYI_vel_daily$max_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_min_tidal <- na_ma(RYI_vel_daily$min_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_max_net <- na_ma(RYI_vel_daily$max_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_min_net <- na_ma(RYI_vel_daily$min_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_max_abs_tidal <- na_ma(RYI_vel_daily$max_abs_tidal, k = 7, weighting = "exponential", maxgap = Inf)

RYI_vel_daily$final_mean_net <- na_ma(RYI_vel_daily$mean_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

summary(RYI_vel_daily)

RYI_vel_daily <- RYI_vel_daily %>%
  rename(max_tidal = final_max_tidal,
         min_tidal = final_min_tidal,
         max_net = final_max_net,
         min_net = final_min_net,
         maxabs_tidal = final_max_abs_tidal,
         mean_net = final_mean_net)

RYI_vel_daily <- subset(RYI_vel_daily, select = c(1, 11:16))

RYI_vel_daily$station <- "RYI"

#create column to assign sign for max abs velocity column

RYI_vel_daily <- RYI_vel_daily %>%
  mutate(net_sign=case_when(abs(min_net) > max_net ~ "-", abs(min_net) < max_net ~ "+"),
         tide_sign=case_when(abs(min_tidal) > max_tidal ~ "-", abs(min_tidal) < max_tidal ~ "+"))

write_rds(RYI_vel_daily, glue("data-raw/Hydrology/dv_RYI.rds"))

dvRYI <- read_rds("data-raw/Hydrology/dv_RYI.rds")

#repeat for RYF----------------------

siteNumbers <- c("11455385")  # 2018-07-03 	- present

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

uvRYF <- read_rds("data-raw/Hydrology/uvRYF.rds")

#apply godin filter------------------------
uvRYF$vel_tf <- Tgodinfn(uvRYF$dateTime_PST, uvRYF$velocity_ft_s)

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

#write out to .rds

#write_rds(uvRYF_c, glue("data-raw/Hydrology/RYF_vel.rds"))

#downstep to daily - add n column
dvRYF <- uvRYF_c %>%
  mutate(Date = as.Date(dateTime_PST, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
  group_by(Date) %>%
  summarise(max_tidal_vel = max(tidal_vel, na.rm = TRUE),
            min_tidal_vel = min(tidal_vel, na.rm = TRUE),
            max_net_vel = max(vel_tf, na.rm = TRUE),
            min_net_vel = min(vel_tf, na.rm = TRUE),
            mean_net_vel = mean(vel_tf, na.rm = TRUE),
            max_abs_tidal = max(abs(tidal_vel), na.rm = TRUE),
            n_vel = n())

continous.dates <- data.frame (x = 1:1611, Date = seq(as.Date('2018-07-04'),as.Date('2022-11-30'), by='day'))

RYF_vel_daily <- merge(continous.dates, dvRYF, by = "Date", all = TRUE)

#if n_flow is NA replace with 0

#RYF_vel_daily[is.nan(RYF_vel_daily)]<-NA

RYF_vel_daily$n_vel[is.na(RYF_vel_daily$n_vel)] <- 0

sum(RYF_vel_daily$n_vel<=91)#26 days with <95% of flow measurements

#add column to identify if flow data is measured or will be imputed

RYF_vel_daily$group <- ifelse(RYF_vel_daily$n_vel>= 91, "measure", "impute")

#if n_value is <91, change mean, max, and min velocity to NA

RYF_vel_daily$max_tidal_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$max_tidal_vel, NA)

RYF_vel_daily$min_tidal_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$min_tidal_vel, NA)

RYF_vel_daily$max_net_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$max_net_vel, NA)

RYF_vel_daily$min_net_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$min_net_vel, NA)

RYF_vel_daily$max_abs_tidal <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$max_abs_tidal, NA)

RYF_vel_daily$mean_net_vel <- ifelse(RYF_vel_daily$n_vel>= 91, RYF_vel_daily$mean_net_vel, NA)
#impute missing values

RYF_vel_daily$final_max_tidal <- na_ma(RYF_vel_daily$max_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_min_tidal <- na_ma(RYF_vel_daily$min_tidal_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_max_net <- na_ma(RYF_vel_daily$max_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_min_net <- na_ma(RYF_vel_daily$min_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_max_abs_tidal <- na_ma(RYF_vel_daily$max_abs_tidal, k = 7, weighting = "exponential", maxgap = Inf)

RYF_vel_daily$final_mean_net <- na_ma(RYF_vel_daily$mean_net_vel, k = 7, weighting = "exponential", maxgap = Inf)

summary(RYF_vel_daily)

RYF_vel_daily <- RYF_vel_daily %>%
  rename(max_tidal = final_max_tidal,
         min_tidal = final_min_tidal,
         max_net = final_max_net,
         min_net = final_min_net,
         maxabs_tidal = final_max_abs_tidal,
         mean_net = final_mean_net)



RYF_vel_daily <- subset(RYF_vel_daily, select = c(1, 11:16))

RYF_vel_daily$station <- "RYF"

#create column to assign sign for max abs velocity column

RYF_vel_daily <- RYF_vel_daily %>%
  mutate(net_sign=case_when(abs(min_net) > max_net ~ "-", abs(min_net) < max_net ~ "+"),
         tide_sign=case_when(abs(min_tidal) > max_tidal ~ "-", abs(min_tidal) < max_tidal ~ "+"))

write_rds(RYF_vel_daily, glue("data-raw/Hydrology/dv_RYF.rds"))

dvRYF <- read_rds("data-raw/Hydrology/dv_RYF.rds")

#merge RYI and RYF datasets---------------------
#first rename columns

#RYF_vel_daily <- read_rds("data-raw/Hydrology/RYF_vel_daily.rds")

#RYI_vel_daily <- read_rds("data-raw/Hydrology/RYI_vel_daily.rds")

RYI_vel_daily <- dvRYI %>%
  rename(RYI_max_tidal_vel = max_tidal,
         RYI_min_tidal_vel = min_tidal,
         RYI_max_net_vel = max_net,
         RYI_min_net_vel = min_net,
         RYI_max_abs_tidal= maxabs_tidal,
         RYI_mean_net = mean_net
  )

RYF_vel_daily <- dvRYF %>%
  rename(RYF_max_tidal_vel = max_tidal,
         RYF_min_tidal_vel = min_tidal,
         RYF_max_net_vel = max_net,
         RYF_min_net_vel = min_net,
         RYF_max_abs_tidal= maxabs_tidal,
         RYF_mean_net = mean_net
  )

RYI_RYF <- full_join(RYI_vel_daily, RYF_vel_daily, na.rm = TRUE, by = "Date")

#subset
RYI_RYF <- RYI_RYF[-c(8:10, 17:19)]

#create final vel vector with if else statement: if RYF is na then fill in with RYI

RYI_RYF$max_tidal <- with(RYI_RYF, ifelse(is.na(RYF_max_tidal_vel), RYI_max_tidal_vel, RYF_max_tidal_vel))

RYI_RYF$min_tidal <- with(RYI_RYF, ifelse(is.na(RYF_min_tidal_vel), RYI_min_tidal_vel, RYF_min_tidal_vel))

RYI_RYF$max_net <- with(RYI_RYF, ifelse(is.na(RYF_max_net_vel), RYI_max_net_vel, RYF_max_net_vel))

RYI_RYF$min_net <- with(RYI_RYF, ifelse(is.na(RYF_min_net_vel), RYI_min_net_vel, RYF_min_net_vel))

RYI_RYF$max_abs_tidal <- with(RYI_RYF, ifelse(is.na(RYF_max_abs_tidal), RYI_max_abs_tidal, RYF_max_abs_tidal))

RYI_RYF$mean_net <- with(RYI_RYF, ifelse(is.na(RYF_mean_net), RYI_mean_net, RYF_mean_net))

#edit once impute occurs
RYI_RYF <- subset(RYI_RYF, select = c(1, 14:19))

#create sign column

RYI_RYF <- RYI_RYF %>%
  mutate(net_sign=case_when(abs(min_net) > max_net ~ "-", abs(min_net) < max_net ~ "+"),
         tide_sign=case_when(abs(min_tidal) > max_tidal ~ "-", abs(min_tidal) < max_tidal ~ "+"))


RYI_RYF$station <- "Cache"

#write to .rds

write_rds(RYI_RYF, "data-raw/Hydrology/dv_cache.rds")



