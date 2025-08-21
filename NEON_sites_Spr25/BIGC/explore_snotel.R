# snotel phenology 
library(snotelr)
library(tidyverse)
library(imputeTS)
library(dataRetrieval)

# download from the California website 

# then use the snow_phenology() function 

#################################################################
# testing snotelr::snow_phenology() with snotel site

# check with snotel dataframe 
snotelr::snotel_info() %>% filter(state=="CA")

alpine <- snotelr::snotel_download(site_id=1258, internal=TRUE)

# plot 
ggplot_na_distribution(alpine$snow_water_equivalent, x_axis_labels=as.Date(alpine$date))
# change NA to 0 
alpine$snow_water_equivalent[is.na(alpine$snow_water_equivalent)] <- 0

# test phenology function
pheno = snotel_phenology(alpine)
# outputs doy for: first snow acc, cont snow acc, first snow melt, last snow melt, max swe doy, and max swe amount 

#################################################################

# CA Water Resources Query: https://cdec.water.ca.gov/dynamicapp/wsSensorData
# Station: Tamarack Summit 
# Daily snow water quality 
# dates 1/1/2015 to 4/9/2025 

url = "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=TMR&SensorNums=3&dur_code=D&Start=2015-01-01&End=2025-04-09"

snow <- read.csv(url)

write_csv(snow, "BIGC/TMR_snow_raw_data.csv")

head(snow)

# need to fix date format, get year and yd 

# convert units to mm 

date_time = str_split(snow$DATE.TIME, pattern = " ")

# check the time in date time column 
tmp = data.frame(do.call(rbind, date_time))
colnames(tmp) <- c("date","time")
unique(tmp$time) # only 0000

# use date to format for snow df 
tmp$date_formatted = as.Date(tmp$date, format="%Y%m%d")
# create new columns for dates 
snow$date <- tmp$date_formatted
snow$year <- year(snow$date)
snow$yday <- yday(snow$date)
snow$wy = calcWaterYear(snow$date)

# unit conversions to mm 
snow$obs_mm <- as.double(snow$VALUE)*25.44

# check distribution
ggplot(snow) + geom_density(aes(x=obs_mm))

# check timeseries for NAs 
imputeTS::ggplot_na_distribution(snow$obs_mm, x_axis_labels=snow$date)

imputeTS::ggplot_na_distribution2(snow$obs_mm)

# what is happening in 2020 
snow20 = snow %>% filter(wy==2020)  
ggplot_na_distribution(snow20$obs_mm, x_axis_labels=snow20$date)

# also checked data from map: https://wcc.sc.egov.usda.gov/reportGenerator/view/customChartReport/daily/start_of_period/TMR:CA:MSNT%257Cid=%2522%2522%257Cname/2019-10-01,2021-03-31/WTEQ::value,WTEQ::median_1991,PREC::value,PREC::median_1991?fitToScreen=false&useLogScale=false

# let's not use that water year :( 

# fill timeseries NAs 
snow$obs_mm[snow$obs_mm<0] <- 0

obs_mm_fill <- snow %>% 
  mutate(obs_fill = na_interpolation(obs_mm))

# check NAs again 
ggplot_na_distribution(obs_mm_fill$obs_fill)

# match with phenology data 
obs_mm_fill = obs_mm_fill %>% 
  mutate(snow_water_equivalent=obs_fill,
         temperature_min = 0) # phenology function requires columns called 'snow_water_equivalent', 'date', and 'temperature_min' (which it only uses for a check)

# run phenology function
tmr_pheno = snotelr::snotel_phenology(obs_mm_fill)

# plot, check max swe 
ggplot(obs_mm_fill, aes(x=yday, y=snow_water_equivalent, group=as.factor(year),col=as.factor(year))) + geom_line() + 
  geom_point(data=tmr_pheno,
             aes(x=max_swe_doy, 
                 y=max_swe,
                 group=as.factor(year),
                 col=as.factor(year)), size=2)

ggplot(obs_mm_fill, aes(x=yday, y=snow_water_equivalent, group=as.factor(year),col=as.factor(year))) + geom_line() + 
  geom_point(data=tmr_pheno,
             aes(x=max_swe_doy, 
                 y=max_swe,
                 group=as.factor(year),
                 col=as.factor(year)), size=2) + facet_grid('year')

# plot continuous snow accumulation 
ggplot(obs_mm_fill, aes(x=date, 
                        y=snow_water_equivalent)) + 
  geom_line() + 
  geom_vline(data=tmr_pheno,
             aes(xintercept=cont_snow_acc, 
                 #y=max_swe,
                 group=as.factor(year),
                 col=as.factor(year)))


# remove water year 2020 because it was messed up 
tmr_towrite = tmr_pheno %>% filter(year!=2020) %>% 
  mutate(station_ID = snow$STATION_ID[1])

ggplot(obs_mm_fill, aes(x=date, 
                        y=snow_water_equivalent)) + 
  geom_line() + 
  geom_point(data=tmr_towrite,
             aes(x=max_swe_date, 
                 y=max_swe,
                 group=as.factor(year),
                 col=as.factor(year)), size=3)

write_csv(tmr_towrite, "../Documents/GitHub/lsamp_pheno/data/TMR_snotel_pheno.csv")

# plot annual values 

ggplot(tmr_towrite, aes(x=year, y=max_swe)) +
  geom_point() + geom_smooth(method='lm')

ggplot(tmr_towrite, aes(x=year, y=max_swe_doy)) +
  geom_point() + geom_smooth(method='lm')


my_lm = lm(data=tmr_towrite, max_swe_doy~year)
summary(my_lm)



## repeat with huntington 
url = "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=TMR&SensorNums=3&dur_code=D&Start=2015-01-01&End=2025-04-09"

snow <- read.csv(url)

ggplot_na_distribution(as.numeric(snow$VALUE))
