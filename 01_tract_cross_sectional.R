###########################################################
## Begin analyzing tract-level traffic and shooting data ##
###########################################################

# Written By: Emma Gause
# Date: 07/19/24
# Last Updated: 11/19/24

#Load in Libraries
library("tidyverse")
library("dplyr")
library(lmtest)
library(sandwich)
library(survival)
library(ggthemes)

options(scipen=999)

#Create path to directory
datadir <- "[INSERT DIRECTORY PATH]"

#read in data
months <- readRDS(paste0(datadir, "Precise_Join_Traffic_Shootings.rds"))

pop <- read_csv(paste0(datadir, "nhgis0029_ds254_20215_tract.csv"))

##----------------------------------------------------------------------------##

#start exploring regression analyses
str(months)

#keep only tract population for Portland tracts
pop_pdx <- pop %>% filter(TL_GEO_ID %in% months$zone_name) %>% select(TL_GEO_ID, "tract_pop"=AON4E001) 
#utrct <- months %>% select(zone_name) %>% unique() #yup!

#join them
monthsx <- left_join(months, pop_pdx, by = c("zone_name"="TL_GEO_ID"),
                     relationship = "many-to-one")
#summary(monthsx$tract_pop)

#how do traffic counts vary?
summary(monthsx) #ranged from 186 to 87689
hist(monthsx$avg_daily_traffic)
hist(monthsx$avg_daily_traffic[monthsx$weekend==1])
hist(monthsx$avg_daily_traffic[monthsx$weekend==0])

sd(monthsx$avg_daily_traffic) #15307.34
median(monthsx$avg_daily_traffic) #10410

#create a traffic volume exposure dataset that is a difference of 10,000 cars - roughly the median
monthsx <- monthsx %>% mutate(traffic10k = avg_daily_traffic/10000)

#create binary indicator for injury or no
monthsx <- monthsx %>% mutate(inj01 = if_else(inj_cnt>0, 1, 0))

#relevel to specify reference group for day part
monthsx <- within(monthsx, day_part <- relevel(as.factor(day_part), ref = "3: Mid-Day (10am-3pm)"))
monthsx <- within(monthsx, month <- relevel(as.factor(month), ref = "Jan"))


#fit model - logistic [max of 3 incs within these strata -- make it binary for better 
  # distributional assumptions]. BUT use log link to interpret as %
mod1 <- glm(inj01 ~ traffic10k + as.factor(year) + month + weekend + day_part, 
            family = binomial(link = "log"), data = monthsx)
#Now estimate with zone-clustered standard errors
clust_se1 <- coeftest(mod1, vcov = vcovCL(mod1, cluster = ~ zone_name))
cbind(exp(coef(clust_se1)), exp(confint(clust_se1)))


#now adjust for tract population 
mod2 <- glm(inj01 ~ traffic10k + tract_pop + as.factor(year) + month + weekend + day_part, 
            family = binomial(link = "log"), data = monthsx)
#Now estimate with zone-clustered standard errors
clust_se2 <- coeftest(mod2, vcov = vcovCL(mod2, cluster = ~ zone_name))
cbind(exp(coef(clust_se2)), exp(confint(clust_se2)))


#sensitivity analysis using traffic deciles
str(monthsx)
hist(monthsx$avg_daily_traffic)

#create quantiles of traffic for analysis 
monthsx <- monthsx %>% mutate(traffic_cat = ntile(avg_daily_traffic, 4),
                              traffic_cat_f = factor(traffic_cat, levels = c(1, 2, 3, 4),
                                                     labels = c("lowest", "below average", "above average", "highest"),
                                                     ordered = TRUE)) 
table(monthsx$traffic_cat, monthsx$traffic_cat_f, useNA = "ifany")

#now test for categorical assoc.  
mod3 <- glm(inj01 ~ as.factor(traffic_cat) + tract_pop + as.factor(year) + month + weekend + day_part, 
            family = binomial(link = "log"), data = monthsx)
#Now estimate with zone-clustered standard errors
clust_se3 <- coeftest(mod3, vcov = vcovCL(mod3, cluster = ~ zone_name))
cbind(exp(coef(clust_se3)), exp(confint(clust_se3)))


#what about pre and during COVID?
preCov <- monthsx %>% filter(year==2019|(year==2020&month_num<3))
covid <- monthsx %>% filter(year==2021|(year==2020&month_num>=3))


modpre <- glm(inj01 ~ traffic10k + tract_pop + as.factor(year) + month + weekend + day_part, 
            family = binomial(link = "log"), data = preCov)
#Now estimate with zone-clustered standard errors
clust_sepre <- coeftest(modpre, vcov = vcovCL(modpre, cluster = ~ zone_name))
cbind(exp(coef(clust_sepre)), exp(confint(clust_sepre)))

modcov <- glm(inj01 ~ traffic10k + tract_pop + as.factor(year) + month + weekend + day_part, 
            family = binomial(link = "log"), data = covid)
#Now estimate with zone-clustered standard errors
clust_secov <- coeftest(modcov, vcov = vcovCL(modcov, cluster = ~ zone_name))
cbind(exp(coef(clust_secov)), exp(confint(clust_secov)))

##----------------------------------------------------------------------------##

#create some interesting visualizations

str(months)

#relevel day part so prder is pretty
table(months$day_part)
table(months$day_type)
months <- months %>% mutate(day_part_pretty = factor(day_part, 
                                                     levels = c("1: Early AM (12am-6am)",
                                                                "2: Peak AM (6am-10am)",
                                                                "3: Mid-Day (10am-3pm)",
                                                                "4: Peak PM (3pm-7pm)",
                                                                "5: Late PM (7pm-12am)"),
                                                     labels = c("Early AM (12am-6am)",
                                                                "Peak AM (6am-10am)",
                                                                "Mid-Day (10am-3pm)",
                                                                "Peak PM (3pm-7pm)",
                                                                "Late PM (7pm-12am)")),
                            day_type_pretty = factor(day_type,
                                                     levels = c("1: Weekday (M-Th)",
                                                                "2: Weekend Day (Sa-Su)"),
                                                     labels = c("Weekday (M-Th)",
                                                                "Weekend Day (Sa-Su)")))



ybrks = seq(0, 90000, 10000)
bptime <- months %>% 
  ggplot(aes(x = day_type_pretty, y = avg_daily_traffic, fill = day_part_pretty)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue3", "goldenrod1", "goldenrod3", 
                               "royalblue3", "midnightblue")) + 
  scale_y_continuous(breaks = ybrks) + 
  labs(fill = "Time of Day", 
       y = "Average Traffic Volume",
       x = "") + 
  theme_hc() + theme(legend.position = "right", 
                     legend.box.margin = margin())

bptime

#ggsave(plot = bptime,
#       filename = paste0(datadir, "Figures/Average_Traffic_Over_Times.jpeg"),
#       height = 4, width = 7, units = "in")


#now injuries 
stats <- months %>% group_by(day_part_pretty, day_type_pretty) %>% 
  summarise(injs = sum(inj_cnt))

ybrks2 = seq(0, 175, 25)
bars <- stats %>% ggplot(aes(x = day_type_pretty, y = injs, fill = day_part_pretty)) + 
  geom_col(position = "dodge") + 
  labs(fill = "Time of Day", 
       y = "Count of Firearm Injuries \n(by type and time of day)",
       x = "") + 
  scale_fill_manual(values = c("lightblue3", "goldenrod1", "goldenrod3", 
                               "royalblue3", "midnightblue")) + 
  scale_y_continuous(breaks = ybrks2) + 
  theme_hc() + theme(legend.position = "right", 
                     legend.box.margin = margin())

bars

#ggsave(plot = bars,
#       filename = paste0(datadir, "Figures/Injury_Cnt_by_Time.jpeg"),
#       height = 4, width = 7, units = "in")
