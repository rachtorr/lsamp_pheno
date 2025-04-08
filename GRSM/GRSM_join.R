## Combining gcc transition date data from phenocamr and phenoR packages with 
### environmental driver data from NeonUtilities packages 

# load packages 
library(tidyverse)
library(neonUtilities)
library(phenor)
library(phenocamr)
library(tsibble)
library(fable)
library(fabletools)
library(distributional)
library(dataRetrieval)

# set your site - name is slightly different in phenocamr package 
my_site = "NEON.D07.GRSM.DP1.00033"

# download phenocam - this should create 2 csv files 
phenocamr::download_phenocam(
  veg_type = "DB", #DB for decid broadleaf, EN for evergreen, default is ALL
  roi_id = 1000,
  site = my_site,
  phenophase = TRUE,
  out_dir = "." # saves in current working dir
)

# read in the phenocamr transition dates 
pheno_td = read_phenocam(paste(my_site,"DB_1000_3day_transition_dates.csv", sep="_"))$data

# download neon clim data (or load in if you already have it saved somewhere)
neon_temps <- loadByProduct(dpID=c("DP1.00003.001"),
                      site=c("GRSM"),
                      tabl=c("TAAT_1min")) # for temperature 
neon_pre <- loadByProduct(dpID=c("DP1.00006.001"),
                            site=c("GRSM"),
                            tabl=c("THRPRE_30min"))

# read in RDS file that was previously saved - soil water content and salinity 
neon_sws <- readRDS("data/GRSM_SWS_30_minute_CP1.00094.001.rds")

#-------------------------------------------------------------
# first going to get annual values for both temp and soil water 
#-------------------------------------------------------------

# precip in winter 
winter_pre <- neon_pre$THRPRE_30min %>% 
  mutate(precip_bulk = na_interpolation(TFPrecipBulk),
         date = date(endDateTime)) %>% 
  group_by(date) %>% 
  summarize(precip = sum(precip_bulk)) %>% 
  mutate(year = year(date),
         wy = calcWaterYear(date),
         month=month(date)) %>% 
  filter(month==12 | month <=2) %>% 
  group_by(wy) %>% 
  summarize(precip_sum = sum(precip),
            precip_mean = mean(precip),
            precip_var = variance(precip)) %>% 
  rename(year=wy)


# temperature variation in march

# check temp data frame 
names(neon_temps$TAAT_1min)

daily_temps <- neon_temps$TAAT_1min %>% 
  mutate(date = date(startDateTime)) %>% 
  fill(tempTripleMean, tempTripleMinimum, tempTripleMaximum) %>% 
  group_by(date) %>% 
  summarize(tmin = min(tempTripleMean),
               tmax = max(tempTripleMean))

# all data points that are in the month of march 
march_df_temp <- neon_temps$TAAT_1min %>% 
  mutate(date = date(startDateTime),
         month=month(date),
         year=year(date)) %>% 
 # mutate(temp_mean = na.approx(tempTripleMean)) %>% 
  filter(month==3) 

# check the NAs 
ggplot_na_distribution(march_df_temp$tempTripleMean)

# group by year and apply function 
ann_df_temp = march_df_temp %>% 
  group_by(year) %>% 
  summarize(var_temp = variance(temp_mean),
            mean_temp = mean(temp_mean),
            max_temp = max(temp_mean),
            min_temp = min(temp_mean))


# soil moisture data 
soil_df <- neon_sws$SWS_30_minute %>% 
  mutate(year = year(startDateTime),
         month = month(startDateTime)) %>% 
  fill(VSWCMean) %>% 
  filter(month==3) %>% 
  group_by(year) %>% 
  summarize(var_sw = variance(VSWCMean),
            mean_sw = mean(VSWCMean))

# join neon data 
join_df = full_join(ann_df_temp, soil_df, by="year")

# ----------
# reformat transition dates
# ----------

spring_td <- pheno_td %>% 
  filter(direction=="rising",
         gcc_value =="gcc_90") %>% 
  select(transition_50, threshold_50) %>% 
  mutate(year = year(transition_50),
         yday_t50 = yday(transition_50))

# --------------------
# join data frames
# --------------------

# join transition dates and neon data 
join_neon_td <- full_join(spring_td, join_df, by='year')

# join with winter precip 
join_neon_wp <- full_join(join_neon_td, winter_pre, by='year')
  

# save your data frame once you have all the x and y variables 
write_csv(join_neon_wp, "grsm_clean_df.csv")

join_neon_td <- read.csv("grsm_clean_df.csv")

# ----------------------
# explore plots 
# ----------------------

ggplot(join_neon_td, aes(x=var_temp, y=yday_t50)) +
  geom_point() + 
  geom_smooth(method='lm')

ggplot(join_neon_td, aes(x=var_sw, y=yday_t50)) +
  geom_point() + 
  geom_smooth(method='lm')

join_neon_wp %>% 
  pivot_longer(cols=c('var_temp', 'mean_temp', 'max_temp', 'min_temp', 'var_sw', 'mean_sw', 'precip_sum', 'precip_mean', 'precip_var')) %>% 
  ggplot(aes(x=value, y=yday_t50)) + 
  geom_point() +
  geom_smooth(method='lm') + 
  facet_wrap('name', scales='free')
  
# ------------
# lin models 
# ------------





