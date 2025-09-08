# comparing all sites 
library(tidyverse)
library(phenor)
library(phenocamr)

# WREF, SOAP, GRSM, SERC, SJER  

wref <- read_phenocam("NEON_sites_Spr25/WREF/NEON.D16.WREF.DP1.00033_EN_1000_3day.csv")$data %>% 
  mutate(site="WREF",
         coast ="West")

soap <- read_phenocam("NEON_sites_Spr25/BIGC/NEON.D17.SOAP.DP1.00033_EN_1000_3day.csv")$data %>% 
  mutate(site="SOAP",
         coast="West")

grsm <- read_phenocam("NEON_sites_Spr25/GRSM/NEON.D07.GRSM.DP1.00033_DB_1000_3day.csv")$data %>% 
  mutate(site="GRSM",
         coast="East")

serc <- read_phenocam("NEON_sites_Spr25/MLBS-SERC/NEON.D02.SERC.DP1.00033_DB_1000_3day.csv")$data %>% 
  mutate(site="SERC",
         coast="East")

sjer <- read_phenocam("data/NEON.D17.SJER.DP1.00033_EN_1000_3day.csv")$data %>% 
  mutate(site = "SJER", 
         coast="West")

# combine all together 
sites <- rbind(sjer, serc, wref, grsm, soap)

# check when years start 
sites %>% group_by(site) %>% summarise(max(year))

# check when sjer ends 4/23/2022
tail(sjer$date)

# plot daily vals 
sites %>% 
  ggplot(aes(x=as.Date(date), y=smooth_gcc_90, col=site)) +
  geom_point() +
#  facet_grid('site') + 
  labs(x="Date", y="GCC 90 smoothed")

sites %>% 
  ggplot(aes(x=as.Date(date), y=smooth_gcc_90, col=site)) +
  geom_point() +
  facet_grid('site') + 
  labs(x="Date", y="GCC 90 smoothed")

# plot year day 
sites %>% 
  filter(site!="SJER" & year==2024) %>% 
  ggplot(aes(x=doy, y=smooth_gcc_90, col=site)) +
  geom_line(size=2)

sites %>% 
  filter(site!="SJER" & year==2024) %>% 
  ggplot(aes(x=doy, y=smooth_gcc_90, col=site, linetype=coast)) +
  geom_line(size=2)

# transition dates from phenocam 

wref_td <- read_phenocam("NEON_sites_Spr25/WREF/NEON.D16.WREF.DP1.00033_EN_1000_3day_transition_dates.csv")$data %>% 
  mutate(site="WREF",
         coast ="West")

soap_td <- read_phenocam("NEON_sites_Spr25/BIGC/NEON.D17.SOAP.DP1.00033_EN_1000_3day_transition_dates.csv")$data %>% 
  mutate(site="SOAP",
         coast="West")

grsm_td <- read_phenocam("NEON_sites_Spr25/GRSM/NEON.D07.GRSM.DP1.00033_DB_1000_3day_transition_dates.csv")$data %>% 
  mutate(site="GRSM",
         coast="East")

serc_td <- read_phenocam("NEON_sites_Spr25/MLBS-SERC/NEON.D02.SERC.DP1.00033_DB_1000_3day_transition_dates.csv")$data %>% 
  mutate(site="SERC",
         coast="East")

sjer_td <- read_phenocam("data/NEON.D17.SJER.DP1.00033_EN_1000_3day_transition_dates.csv")$data %>% 
  mutate(site = "SJER", 
         coast="West")

all_td <- rbind(wref_td, soap_td, grsm_td, serc_td, sjer_td) %>% 
  mutate(year = year(transition_10),
         t10_doy = yday(transition_10),
         t50_doy = yday(transition_50))

lm_all_dt = all_td %>% 
  filter(direction=="rising" &
           gcc_value=="gcc_90") %>% 
  group_by(site) %>% 
  mutate(slope = round(lm(year~t10_doy)$coefficients[2], 2),
         significance = summary(lm(year~t10_doy))$coefficients[2, 4],
         y=mean(t10_doy))

lm_all_dt %>% 
  ggplot(aes(x=year, y=t10_doy, col=site)) + 
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  geom_text(aes(x=2016, y=y, label=paste(slope, round(significance,3), sep=", ")))

# 50 transition day 
lm_all_dt = all_td %>% 
  filter(direction=="rising" &
           gcc_value=="gcc_90") %>% 
  group_by(site) %>% 
  mutate(slope = round(lm(year~t50_doy)$coefficients[2], 2),
         significance = summary(lm(year~t50_doy))$coefficients[2, 4],
         y=mean(t50_doy))

lm_all_dt %>% 
  ggplot(aes(x=year, y=t50_doy, col=site)) + 
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  geom_text(aes(x=2016, y=y, label=paste(slope, round(significance,3), sep=", ")))

# get environmental neon data 
my_sites = unique(all_td$site)

library(neonUtilities)

# set data ID - summary weather 
data_id = "DP4.00001.001"

# download the summary weather statistics files - for site SJER - check your console, this may take a minute
summary_weather <- loadByProduct(dpID=c(data_id),
                                 site=c(my_sites),
                                 check.size=F)

temps = summary_weather$wss_daily_temp %>% mutate(site=siteID)
unique(temps$siteID)

sites$date = as.Date(sites$date)

temp_join = inner_join(temps, sites, by=c('date', 'site'))
unique(temp_join$site)
# only site that had summary weather downloaded was WREF 

ggplot(temp_join, aes(x=wssTempTripleMean, y = smooth_gcc_90)) +
  geom_point()
