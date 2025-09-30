######################################
## Hazelwood Corridor Data Analysis ##
######################################

# Written By: Emma Gause
# Date: 11/18/24
# Last Updated: 11/19/24

#Load in Libraries
library("tidyverse")
library("dplyr")
library("sf")
library("mapview")
library(lubridate)
library(lmtest)
library(sandwich)
library(survival)

options(scipen=999)

#Create path to directory
datadir <- "[INSERT DIRECTORY PATH]"

#read in only Hazelwood incident data - injuries and shootings 
shoot <- read_sf(paste0(datadir, "Hazelwood_incs.shp"))

#read in shapefile of annual streets data 
lines <- read_sf(paste0(datadir, "Annual_Hstreets_traffic.shp"))

#read in monthly Hazelwood lines traffic
cars <- readRDS(paste0(datadir, "Monthly_Hazelwood_Traffic.rds"))

table(cars$day_part)

##----------------------------------------------------------------------------##

#look at map of roadway traffic and shootings
mapview(lines, zcol = "trffc_3") + mapview(shoot, zcol = "inc_typ")

#keep only necessary info to join in line ID
colnames(lines)
lines_id <- lines %>% select(id, name, rod_typ, trffc_3, kph_3yr)

#spatial join segments to shooting points to get segment ID
dat <- st_join(shoot, lines_id, join = st_nearest_feature)
str(dat)
dat <- dat %>% rename(occur_time = "occr_tm",month_num = "occr_mn", 
                      year = "occr_yr", occur_date_chr = "occr_d_")
  
#now create year, month, day type and time of day variables to join into monthly traffic data
str(dat)
str(cars)

#what are the day type and day part entries?
table(cars$day_type)
# 0: All Days (M-Su)      1: Weekday (M-Th) 2: Weekend Day (Sa-Su)

table(cars$day_part)
# 0: All Day (12am-12am) 1: Early AM (12am-6am)  2: Peak AM (6am-10am)  3: Mid-Day (10am-3pm)   
# 4: Peak PM (3pm-7pm)  5: Late PM (7pm-12am)

table(dat$occur_time) # times too sparse for regression (positivity violations), condense

dat <- dat %>% mutate(month = month(date, label = TRUE),
                      dow = wday(date, label = TRUE),
                      weekend = if_else(dow=="Sun"|dow=="Sat", 1, 0),
                      day_type = case_when(
                        weekend==1 ~ "1: Weekday (M-Th)",
                        weekend==0 ~ "2: Weekend Day (Sa-Su)"),
                      day_part = case_when(
                        occur_time=="0000 - 0259"|occur_time=="0300 - 0559" ~ "1: 0000-0559 (12am-6am)",
                        occur_time=="0600 - 0859"|occur_time=="0900 - 1159" ~ "3: 0600-1159 (6am-12noon)",
                        occur_time=="1200 - 1459"|occur_time=="1500 - 1759" ~ "5: 1200-1759 (12noon-6pm)",
                        occur_time=="1800 - 2059"|occur_time=="2100 - 2359" ~ "7: 1800-2359 (6pm-12am)"
                      ))

#drop the geometry 
datx <- dat %>% st_drop_geometry()

#summarize the incidents by strata vars (i.e. count if there are multiple)
datsum <- datx %>% group_by(id, year, month_num, day_type, day_part, inc_typ) %>% 
  summarise(count = n()) %>% ungroup() %>%
  pivot_wider(id_cols = c("id", "year", "month_num", "day_type", "day_part"), 
              names_from = "inc_typ", values_from = "count")

#set missing to zero (i.e. no incidents)
datsum$Injury[is.na(datsum$Injury)] <- 0
datsum$Shooting[is.na(datsum$Shooting)] <- 0

#create total inc var
datsum <- datsum %>% mutate(total_incs = Injury+Shooting)
summary(datsum)

#combine injurious and non-injurious data to get total incidents 
carsx <- cars %>% filter(day_type!="0: All Days (M-Su)" & day_part!="0: All Day (12am-12am)") %>% 
  mutate(year = as.double(year),
         id = as.character(id))

table(carsx$day_type)
table(datsum$day_type)

table(carsx$day_part)
table(datsum$day_part)

#need to adjust day_type due to positivity violations
str(carsx)
carsx <- carsx %>% mutate(day_partx = day_part,
  day_part = case_when(
    day_partx=="1: 0000-0259 (12am-3am)"|day_partx=="2: 0300-0559 (3am-6am)" ~ "1: 0000-0559 (12am-6am)",
    day_partx=="3: 0600-0859 (6am-9am)"|day_partx=="4: 0900-1159 (9am-12noon)" ~ "3: 0600-1159 (6am-12noon)",
    day_partx=="5: 1200-1459 (12noon-3pm)"|day_partx=="6: 1500-1759 (3pm-6pm)" ~ "5: 1200-1759 (12noon-6pm)",
    day_partx=="7: 1800-2059 (6pm-9pm)"|day_partx=="8: 2100-2359 (9pm-12am)" ~ "7: 1800-2359 (6pm-12am)"
))

#need to summarize for regression to sum traffic over combined times
carsxx <- carsx %>% group_by(id, year, month_num, day_type, day_part) %>% 
  summarise(avg_daily_traffic = sum(avg_daily_traffic)) %>% ungroup()

#merge together!
summary(carsxx)
summary(datsum)
data <- left_join(carsxx, datsum, by = c("id", "year", "month_num",
                                        "day_type", "day_part"))
data[is.na(data)] <- 0 #merged tract/month combos that are missing are zero incident occurrences

##----------------------------------------------------------------------------##

#start exploring regression analyses
str(data)

#how do traffic counts vary?
summary(data) 
hist(data$avg_daily_traffic)
hist(data$avg_daily_traffic[data$day_type=="1: Weekday (M-Th)"])
hist(data$avg_daily_traffic[data$day_type=="2: Weekend Day (Sa-Su)"])


#create a traffic volume exposure dataset that is a difference of 100 cars
data <- data %>% mutate(traffic1h = avg_daily_traffic/100)

#create binary indicator for shooting or no
data <- data %>% mutate(inj01 = if_else(Injury>0, 1, 0),
                        shoot01 = if_else(total_incs>0, 1, 0))

#relevel to specify reference group for day part
table(data$day_part)
data <- within(data, day_part <- relevel(as.factor(day_part), ref = "3: 0600-1159 (6am-12noon)"))

#fit model - logistic [max of 3 incs within these strata -- make it binary for better 
# distributional assumptions]. BUT use log link to interpret as %
mod1 <- glm(inj01 ~ traffic1h + as.factor(year) + as.factor(month_num) + day_type + day_part, 
            family = binomial(link = "log"), data = data)
#Now estimate with zone-clustered standard errors
clust_se1 <- coeftest(mod1, vcov = vcovCL(mod1, cluster = ~ id))
cbind(exp(coef(clust_se1)), exp(confint(clust_se1)))

#fit model - all shootings
mod2 <- glm(shoot01 ~ traffic1h + as.factor(year) + as.factor(month_num) + day_type + day_part, 
            family = poisson(link = "log"), data = data)
#Now estimate with zone-clustered standard errors
clust_se2 <- coeftest(mod2, vcov = vcovCL(mod2, cluster = ~ id))
cbind(exp(coef(clust_se2)), exp(confint(clust_se2)))



#create quantiles of traffic for categorical analysis 
str(data)
data <- data %>% mutate(traffic_cat = ntile(avg_daily_traffic, 4)) 
table(data$traffic_cat, useNA = "ifany")

#now test for categorical assoc.  
mod3 <- glm(inj01 ~ as.factor(traffic_cat) + as.factor(year) + as.factor(month_num) + day_type + day_part, 
            family = binomial(link = "log"), data = data)
#Now estimate with zone-clustered standard errors
clust_se3 <- coeftest(mod3, vcov = vcovCL(mod3, cluster = ~ id))
cbind(exp(coef(clust_se3)), exp(confint(clust_se3)))



##----------------------------------------------------------------------------##

#convert traffic volumes into units of SDs
data <- data %>% group_by(id, day_type, day_part) %>% 
  mutate(Tsd = sd(avg_daily_traffic),
         Tmean = mean(avg_daily_traffic),
         traffic_sds = (avg_daily_traffic-Tmean)/Tsd) %>% 
  ungroup()

hist(data$traffic_sds)


#####
## We want to retain every shooting month and create a dataset with pre-period control, 
## and post-period control to perform the conditional regression analysis.

#create month ts indicator (running month number) to ensure lag/lead rows cross annual boundaries
data <- data %>% mutate(month_ts = case_when(
  year==2019 ~ month_num, 
  year==2020 ~ month_num + 12, 
  year==2021 ~ month_num + 24),
  month_lag = month_ts - 1,
  month_lead = month_ts + 1)

#now assign control data, start with keeping cases
cases <- data %>% filter(inj01==1)

#create control datasets (pre and post)
str(data)
pre_exp <- data %>% select(id, day_type, day_part, month_lag, 
                                "traffic_lag"=avg_daily_traffic, 
                                "sds_lag"=traffic_sds,
                                "inj01_lag"=inj01)
post_exp <- data %>% select(id, day_type, day_part, month_lead, 
                                 "traffic_lead"=avg_daily_traffic, 
                                 "sds_lead"=traffic_sds,
                                 "inj01_lead"=inj01)

#merge together
casesx <- left_join(cases, pre_exp, by = c("id", "day_type", "day_part", 
                                           "month_ts"="month_lag"))
casesxx <- left_join(casesx, post_exp, by = c("id", "day_type", "day_part", 
                                              "month_ts"="month_lead"))  %>% 
  select(-month_lead, -month_lag)

#assign a unique ID to the cases for identification in the conditional log reg
str(casesxx)
casesxx <- casesxx %>% mutate(uid = row_number())

#look at differences
casesxx <- casesxx %>% mutate(delta_lag = avg_daily_traffic-traffic_lag,
                              delta_lag_sd = traffic_sds-sds_lag,
                              delta_lead = avg_daily_traffic-traffic_lead,
                              delta_lead_sd = traffic_sds-sds_lead)
summary(casesxx$delta_lag) #median = 24.00, mean = 88.84
summary(casesxx$delta_lead) #median = -13.0, mean = -25.4
summary(casesxx$delta_lag_sd) #median = 0.1118 SDs, mean = 0.1577 SDs
summary(casesxx$delta_lead_sd) #median = -0.19287 SDs, mean = 0.00719 SDs


#convert to long for regression
conddat <- casesxx %>% select(uid, id, day_type, day_part, month_num, year, 
                              "traffic_case"=avg_daily_traffic, 
                              "sds_case"=traffic_sds, 
                              "inj01_case"=inj01, 
                              traffic_lag, sds_lag, inj01_lag, 
                              traffic_lead, sds_lead, inj01_lead) %>% 
  pivot_longer(cols = c(-uid, -id, -day_type, -day_part, -month_num, -year),
               names_to = c(".value", "period"),
               names_sep = "_")

##----------------------------------------------------------------------------##

str(conddat)

#fit the conditional logistic regression for the pre analysis
pre <- conddat %>% filter(period=="case"|period=="lag")
pre <- pre %>% mutate(monthx = case_when(
  period=="lag" & month_num==1 ~ 2,
  period=="lag" & month_num==2 ~ 3,
  period=="lag" & month_num==3 ~ 4,
  period=="lag" & month_num==4 ~ 5,
  period=="lag" & month_num==5 ~ 6,
  period=="lag" & month_num==6 ~ 7,
  period=="lag" & month_num==7 ~ 8,
  period=="lag" & month_num==8 ~ 9,
  period=="lag" & month_num==9 ~ 10,
  period=="lag" & month_num==10 ~ 11,
  period=="lag" & month_num==11 ~ 12,
  period=="lag" & month_num==12 ~ 1,
  period=="case" ~ month_num))
table(pre$month_num, pre$monthx, deparse.level = 2)

cond_pre <- clogit(inj01 ~ sds + strata(uid), 
                   data=pre)
summary(cond_pre)

#fit the conditional logistic regression for the post analysis
post <- conddat %>% filter(period=="case"|period=="lead")
post <- post %>% mutate(monthx = case_when(
  period=="lead" & month_num==2 ~ 1,
  period=="lead" & month_num==3 ~ 2,
  period=="lead" & month_num==4 ~ 3,
  period=="lead" & month_num==5 ~ 4,
  period=="lead" & month_num==6 ~ 5,
  period=="lead" & month_num==7 ~ 6,
  period=="lead" & month_num==8 ~ 7,
  period=="lead" & month_num==9 ~ 8,
  period=="lead" & month_num==10 ~ 9,
  period=="lead" & month_num==11 ~ 10,
  period=="lead" & month_num==12 ~ 11,
  period=="lead" & month_num==1 ~ 12,
  period=="case" ~ month_num))
table(post$month_num, post$monthx, deparse.level = 2)

cond_post <- clogit(inj01 ~ sds + strata(uid), 
                    data=post)
summary(cond_post)








