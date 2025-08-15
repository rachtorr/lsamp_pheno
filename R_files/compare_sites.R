# comparing all sites 
library(tidyverse)
library(phenor)
library(phenocamr)

# WREF, SOAP, GRSM, SERC, SJER  

wref <- read_phenocam("WREF/NEON.D16.WREF.DP1.00033_EN_1000_3day.csv")$data %>% 
  mutate(site="WREF",
         coast ="West")

soap <- read_phenocam("BIGC/NEON.D17.SOAP.DP1.00033_EN_1000_3day.csv")$data %>% 
  mutate(site="SOAP",
         coast="West")

grsm <- read_phenocam("GRSM/NEON.D07.GRSM.DP1.00033_DB_1000_3day.csv")$data %>% 
  mutate(site="GRSM",
         coast="East")

serc <- read_phenocam("MLBS-SERC/NEON.D02.SERC.DP1.00033_DB_1000_3day.csv")$data %>% 
  mutate(site="SERC",
         coast="East")

sjer <- read_phenocam("example_data/NEON.D17.SJER.DP1.00033_EN_1000_3day.csv")$data %>% 
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




