library(dplyr)
##emp_wq <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=98b400de8472d2a3d2e403141533a2cc')
##emp_stations <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af')
integratedwq <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.1&entityid=6c5f35b1d316e39c8de0bfadfb3c9692')
ybp_chloro <- read.csv('https://github.com/jessicaguo/swg-21-connectivity/blob/dc9c19018e43f3e2dccc39afba5e0a2628e15bd0/data/YBFMP_Nutrients_Chlorophyll_2009_2019.csv')
library(lubridate)
devtools::install_github("ryanpeek/wateRshedTools")
library(wateRshedTools)

integrated_wq_filtered_WET <-
  integratedwq %>%
  mutate(Date=ymd(Date)) %>%
  mutate(Month=month(Date)) %>%
  filter(Month==c(10, 11, 12, 1, 2, 3, 4, 5)) %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(Date>ymd("1998-01-01")) 


wet_lm <- lm(integrated_wq_filtered_WET$Chlorophyll~integrated_wq_filtered_WET$Date) 
plot(integrated_wq_filtered_WET$Chlorophyll~integrated_wq_filtered_WET$Date)
summary(wet_lm)


  integrated_wq_filtered_ALL <-
  integratedwq %>%
  mutate(Date=ymd(Date)) %>%
  mutate(Month=month(Date)) %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(Date>ymd("1998-01-01")) 

##lm(formula = integrated_wq_filtered_ALL$Chlorophyll ~ integrated_wq_filtered_ALL$Date)

##Residuals:
##  Min      1Q  Median      3Q     Max 
##-7.067  -3.373  -1.867   0.344 255.067 

##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)                      1.189e+01  4.354e-01   27.31   <2e-16 ***
##  integrated_wq_filtered_ALL$Date -4.419e-04  2.956e-05  -14.95   <2e-16 ***
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##Residual standard error: 9.035 on 17652 degrees of freedom
##Multiple R-squared:  0.01251,	Adjusted R-squared:  0.01245 
##F-statistic: 223.5 on 1 and 17652 DF,  p-value: < 2.2e-16

  integrated_wq_filtered_ALL <-
    integratedwq %>%
    mutate(Date=ymd(Date)) %>%
    mutate(Month=month(Date)) %>%
    filter(!is.na(Chlorophyll))

  
ALL_lm <- lm(integrated_wq_filtered_ALL$Chlorophyll~integrated_wq_filtered_ALL$Date) 
plot(integrated_wq_filtered_ALL$Chlorophyll~integrated_wq_filtered_ALL$Date)
summary(ALL_lm)
  


##Residuals:
##  Min      1Q  Median      3Q     Max 
##-7.240  -3.504  -2.052   0.085 148.210 

##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)                     13.1231811  1.5475808   8.480  < 2e-16 ***
##  integrated_wq_filtered_WET$Date -0.0005163  0.0001052  -4.909 1.02e-06 ***
---
  ##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  ##Residual standard error: 9.319 on 1480 degrees of freedom
  ##Multiple R-squared:  0.01602,	Adjusted R-squared:  0.01536 
  ##F-statistic:  24.1 on 1 and 1480 DF,  p-value: 1.017e-06

##Adding Water Year Info
integrated_wq_filtered_ALL <- add_WYD(integrated_wq_filtered_ALL, "Date") 




plot(integrated_wq_filtered_ALL$Chlorophyll~integrated_wq_filtered_ALL$Date)

integrated_wq_filtered_1998 <-
  integrated_wq_filtered_ALL %>%
  mutate(Date=ymd(Date)) %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(Date>ymd("1998-01-01")) 

plot(integrated_wq_filtered_1998$Chlorophyll~integrated_wq_filtered_1998$Date)
plot(integrated_wq_filtered_1998$Chlorophyll~integrated_wq_filtered_1998$Temperature)
plot(integrated_wq_filtered_1998$Chlorophyll~integrated_wq_filtered_1998$Salinity)
boxplot(integrated_wq_filtered_1998$Chlorophyll~integrated_wq_filtered_1998$Source, outline=FALSE)
boxplot(integrated_wq_filtered_1998$Date~integrated_wq_filtered_1998$Source)
chloro_time <- lm(integrated_wq_filtered_1998$Chlorophyll~integrated_wq_filtered_1998$Date)
summary(chloro_time)

unique(integrated_wq_filtered_1998[c("Source")])

##October 1st-May 30--Filter by month

usbr_wq_filtered <-
  integrated_wq_filtered_1998 %>%
    filter(Station==c('USBR 16', 'USBR 34', 'USBR 44'))
##16, 34 and 44 are downstream
##2012-2019 
##Only yields 5 stations

usgs_wq <-
  integrated_wq_filtered_1998 %>%
  filter(Source=="USGS")

usgs_wq_657 <-
  integrated_wq_filtered_1998 <-
  integratedwq %>%
  mutate(Date=ymd(Date)) %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(Date>ymd("1998-01-01")) %>%
  filter(Source=="USGS")%>%
  filter(Station=="USGS 657")

usgs_wq_649 <-
  integrated_wq_filtered_1998 <-
  integratedwq %>%
  mutate(Date=ymd(Date)) %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(Date>ymd("1998-01-01")) %>%
  filter(Source=="USGS")%>%
  filter(Station=="USGS 649")

emp_wq <-
  integrated_wq_filtered_1998 %>%
  filter(Source=="EMP")


plot(usgs_wq_657$Chlorophyll~usgs_wq_657$Date, ylim=c(0,10))

plot(usgs_wq_649$Chlorophyll~usgs_wq_649$Date, ylim=c(0,10))


usgs_lm <- lm(usgs_wq_649$Chlorophyll~usgs_wq_649$Date)
summary(usgs_lm)
  unique(usgs_wq[c("Station")])
  
emp_wq <-
  integrated_wq_filtered_1998 <-
  integratedwq %>%
  mutate(Date=ymd(Date)) %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(Date>ymd("1998-01-01")) %>%
  filter(Source=="EMP")

boxplot()

usbr_wq <-
  integrated_wq_filtered <-
  integratedwq %>%
  mutate(Date=ymd(Date)) %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(Date>ymd("2013-10-01")) %>%
  filter(Source=="USBR")



###MAP CODE
library(sf)
library(mapview)
mapviewOptions(fgb=TRUE)


##USBR all sites map
df_cl_sf <- usbr_wq %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords=c("Longitude", "Latitude"),
           crs=4326, remove=FALSE) %>%
distinct(Station, .keep_all = T) 
mapview(df_cl_sf, zcol="Station")

##EMP
df_cl_sf <- emp_wq %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords=c("Longitude", "Latitude"),
           crs=4326, remove=FALSE) %>%
  distinct(Station, .keep_all = T) 
mapview(df_cl_sf, zcol="Station")


##********
  ##************
  ##**************SPATIAL AUTOCORRELATION

library(ape)



