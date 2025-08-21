
# load packages 
library(tidyverse)
library(neonUtilities)
library(phenor)
library(phenocamr)
library(patchwork)


#### code to create a plot of daily atm CO2 on x-axis and daily gcc_90 on y-axis 

##### gcc 90 data - load by url, same as NEON workflow 

url <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/phenology-targets.csv.gz"

# get data frame from website
phenology_targets <- read_csv(url, show_col_types = FALSE)

# filter by site id
grsm_site <- phenology_targets %>% filter(site_id=="WREF")

# get gcc 90 only 
gcc_daily <- grsm_site %>% 
  filter(variable=="gcc_90") %>% 
  mutate(date=date(datetime)) %>% 
  select(gcc_90=observation, date)

###### CO2 data 

# download neon flux data 
neon_co2 <- loadByProduct(dpID=c("DP4.00200.001"),
                          site=c("WREF"))

# error from loadByProduct: Error in loadByProduct(dpID = c("DP4.00200.001"), site = c("GRSM")) : The bundled eddy covariance data product cannot be stacked and loaded directly from download. To use these data, download with zipsByProduct() and then stack with stackEddy().

neon_co2 <- zipsByProduct(dpID=c("DP4.00200.001"),
                          site=c("WREF"))


# stack the files
filelist = list.files("filesToStack00200/")
# get list 
filelist23 = filelist[str_detect(filelist, "2023")]

# unzip files 
for(file in filelist){
  
  unzip(paste("filesToStack00200/",file, sep=""))
}

getVarsEddy("NEON.D16.WREF.DP4.00200.001.nsae.2023-12.basic.20250123T171500Z.h5")

# get variable names 
vars = read.csv("NEON.D16.WREF.DP4.00200.001.variables.20250123T180945Z.csv")

# stack files for fluxCO2 data 
fluxco2 <- stackEddy(filepath = ".",
                     var = "fluxCo2") 

# save original 
saveRDS(fluxco2, "WREF_eddy_covariance_fluxco2.rds")

# summarize by daily values 
fluxco2_daily <- fluxco2$WREF %>% 
  select(timeBgn, data.fluxCo2.nsae.flux,
         data.fluxCo2.stor.flux,
         data.fluxCo2.turb.flux) %>% 
  mutate(date = date(timeBgn)) %>% 
  group_by(date) %>% 
  summarize(fluxco2.nsae.flux = sum(data.fluxCo2.nsae.flux),
            fluxco2.stor.flux = sum(data.fluxCo2.stor.flux),
            fluxco2.turb.flux = sum(data.fluxCo2.turb.flux)) %>% 
  mutate(year = year(date),
         month = month(date),
         doy = yday(date))


# join with gcc_daily 
gcc_co2 = full_join(fluxco2_daily, gcc_daily, by='date')

# save file 
write.csv(gcc_co2, "WREF_daily_GCC90_fluxCO2.csv")

### if you need to read it in again
gcc_co2 <- read.csv("WREF_daily_GCC90_fluxCO2.csv")

# plot 
ggplot(gcc_co2, aes(x=fluxco2.nsae.flux, y=gcc_90)) + geom_point()

ggplot(gcc_co2, aes(x=date, y=gcc_90)) + geom_line() 

ggplot(gcc_co2, aes(x=date, y=fluxco2.nsae.flux)) + geom_point() 

# if you want to only look at 2023 
gcc_co2_23 <- gcc_co2 %>% filter(year==2023) 

# plot daily values in 2023 
gcc23_plot <- ggplot(gcc_co2, aes(x=date, y=gcc_90)) + geom_line() 

co2_flux_plot <- ggplot(gcc_co2, aes(x=date, y=fluxco2.nsae.flux)) + geom_point() 

gcc23_plot / co2_flux_plot

