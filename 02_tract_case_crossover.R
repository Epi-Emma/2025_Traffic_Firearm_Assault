##################################
## Portland Case-Only Data Prep ##
##################################

# Written By: Emma Gause
# Date: 08/14/24
# Last Updated: 05/29/25

#Load in Libraries
library("tidyverse")
library("dplyr")
library("sf")
library("mapview")
library(lubridate)
library(survival)


options(scipen=999)

#Create path to directory
datadir <- "[INSERT DIRECTORY PATH]"

#read in tract shapefile
trctshp <- st_read(paste0(datadir, "1479373_OD_Tracts_2020xx_origin.shp"))

#read in tract prepped car traffic
monthly <- readRDS(paste0(datadir, "Monthly_Car_Traffic_Tracts.rds"))
monthdat <- readRDS(paste0(datadir, "Precise_Join_Traffic_Shootings.rds"))

#read in all shootings data from PD (spatial)
shoots <- read_sf(paste0(datadir, "Portland_PD_analysis_incs.shp"))

##----------------------------------------------------------------------------##

#join tract info to shooting data
str(trctshp)
trct <- trctshp %>% select(zone_name = "name")
shoot_trct <- st_join(shoots, trct)

rm(shoots, trctshp, trct)

##----------------------------------------------------------------------------##

#convert traffic counts to standard deviations 
monthdatx <- monthdat %>% group_by(zone_name, weekend, day_part) %>% 
  mutate(Tsd = sd(avg_daily_traffic),
         Tmean = mean(avg_daily_traffic),
         traffic_sds = (avg_daily_traffic-Tmean)/Tsd) %>%
  ungroup()

hist(monthdatx$traffic_sds)

str(monthdatx) #THIS IS THE MONTHS DATA ALREADY MERGED WITH SHOOTINGS DATA

#####
## We want to retain every shooting month and create a dataset with pre-period control, 
  ## and post-period control to perform the case-crossover conditional regression analysis.

#create a binary for injury count
monthdatx <- monthdatx %>% mutate(inj01 = if_else(inj_cnt>=1, 1, 0))

#create month ts indicator (running month number) to ensure lag/lead rows cross annual boundaries
monthdatx <- monthdatx %>% mutate(month_ts = case_when(
  year==2019 ~ month_num, 
  year==2020 ~ month_num + 12, 
  year==2021 ~ month_num + 24),
  month_lag = month_ts - 1,
  month_lead = month_ts + 1)

#now assign control data, start with keeping cases
cases <- monthdatx %>% filter(inj01==1)

#create control datasets (pre and post)
str(monthdatx)
pre_exp <- monthdatx %>% select(zone_name, weekend, day_part, month_lag, 
                                "lag_traffic"=avg_daily_traffic, 
                                "lag_sds"=traffic_sds,
                                "lag_inj01"=inj01)
post_exp <- monthdatx %>% select(zone_name, weekend, day_part, month_lead, 
                                 "lead_traffic"=avg_daily_traffic, 
                                 "lead_sds"=traffic_sds,
                                 "lead_inj01"=inj01)

#merge together
casesx <- left_join(cases, pre_exp, by = c("zone_name", "weekend", "day_part", 
                                           "month_ts"="month_lag"))
casesxx <- left_join(casesx, post_exp, by = c("zone_name", "weekend", "day_part", 
                                           "month_ts"="month_lead"))  %>% 
  select(-month_lead, -month_lag)

#assign a unique ID to the cases for identification in the conditional log reg
str(casesxx)
casesxx <- casesxx %>% mutate(uid = row_number())

#look at average differences
casesxx <- casesxx %>% mutate(delta_lag = avg_daily_traffic-lag_traffic,
                              delta_lag_sd = traffic_sds-lag_sds,
                              delta_lead = avg_daily_traffic-lead_traffic,
                              delta_lead_sd = traffic_sds-lead_sds)
summary(casesxx$delta_lag) #median = 108.5, mean = 125.5
summary(casesxx$delta_lead) #median = 57.0, mean = 124.2
summary(casesxx$delta_lag_sd) #median = 0.07739 SDs, mean = 0.08796 SDs
summary(casesxx$delta_lead_sd) #median = 0.04034 SDs, mean = 0.08460 SDs

#convert to long for regression
conddat <- casesxx %>% select(uid, zone_name, weekend, day_part, month, year, 
                          "traffic_case"=avg_daily_traffic, 
                          "sds_case"=traffic_sds, 
                          "inj01_case"=inj01, 
                          "traffic_lag"=lag_traffic, 
                          "sds_lag"=lag_sds, 
                          "inj01_lag"=lag_inj01, 
                          "traffic_lead"=lead_traffic, 
                          "sds_lead"=lead_sds, 
                          "inj01_lead"=lead_inj01) %>% 
  pivot_longer(cols = c(-uid, -zone_name, -weekend, -day_part, -month, -year),
               names_to = c(".value", "period"),
               names_sep = "_")

##----------------------------------------------------------------------------##

str(conddat)

pre <- conddat %>% filter(period=="case"|period=="lag")
post <- conddat %>% filter(period=="case"|period=="lead")

str(pre)
str(post)

#we want to adjust for the case month - need to edit the control months to match case month
  #*control month always the same for each case month so just include one for the seasonality control
pre <- pre %>% mutate(monthx = case_when(
  period=="lag" & month=="Jan" ~ "Feb",
  period=="lag" & month=="Feb" ~ "Mar",
  period=="lag" & month=="Mar" ~ "Apr",
  period=="lag" & month=="Apr" ~ "May",
  period=="lag" & month=="May" ~ "Jun",
  period=="lag" & month=="Jun" ~ "Jul",
  period=="lag" & month=="Jul" ~ "Aug",
  period=="lag" & month=="Aug" ~ "Sep",
  period=="lag" & month=="Sep" ~ "Oct",
  period=="lag" & month=="Oct" ~ "Nov",
  period=="lag" & month=="Nov" ~ "Dec",
  period=="lag" & month=="Dec" ~ "Jan",
  period=="case" ~ month))
table(pre$month, pre$monthx, deparse.level = 2)

#fit the conditional logistic regression for the pre analysis
cond_pre <- clogit(inj01 ~ sds + monthx + strata(uid), 
                   data=pre)
summary(cond_pre)


#we want to adjust for the case month - need to edit the control months to match case month 
  #*control month always the same for each case month so just include one for the seasonality control
post <- post %>% mutate(monthx = case_when(
  period=="lead" & month=="Feb" ~ "Jan",
  period=="lead" & month=="Mar" ~ "Feb",
  period=="lead" & month=="Apr" ~ "Mar",
  period=="lead" & month=="May" ~ "Apr",
  period=="lead" & month=="Jun" ~ "May",
  period=="lead" & month=="Jul" ~ "Jun",
  period=="lead" & month=="Aug" ~ "Jul",
  period=="lead" & month=="Sep" ~ "Aug",
  period=="lead" & month=="Oct" ~ "Sep",
  period=="lead" & month=="Nov" ~ "Oct",
  period=="lead" & month=="Dec" ~ "Nov",
  period=="lead" & month=="Jan" ~ "Dec",
  period=="case" ~ month))
table(post$month, post$monthx, deparse.level = 2)

#fit the conditional logistic regression for the post analysis
cond_post <- clogit(inj01 ~ sds + monthx + strata(uid), 
                    data=post)
summary(cond_post)

