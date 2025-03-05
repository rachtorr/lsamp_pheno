##############################################################################################
#' @title Workflow for downloading dp04 data from unpublished file list in GCS

#' @author
#' David Durden \email{eddy4R.info@gmail.com}

#' @description
#' Workflow. Downloading unpublished SAE data from GCS.

#' @param Currently none

#' @return Currently none

#' @references

#' @keywords eddy-covariance, NEON

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   David (2024-09-23)
#     original creation
##############################################################################################

#Site for analysis
Site <- "WREF"

#Date begin
dateBgn <- as.Date("2022-09-01")
#Date end
dateEnd <- as.Date("2022-09-10")

#Download directory
DirDnld <- tempdir()

#Unpublished SAE file list
listFile <- read.csv("https://storage.googleapis.com/neon-sae-files/ods/sae_files_unpublished/sae_file_url_unpublished.csv")

#Date interval
setDate <- lubridate::interval(start = dateBgn, end = dateEnd)

#Subset file list by dates and site
listFileSub <- listFile[as.Date(listFile$date) %within% setDate & listFile$site == Site,]

#Download filename (full path)
fileDnld <-  paste0(DirDnld, "\\", str_extract(string = listFileSub$url,pattern = "NEON.*.h5$"))

#Download data
lapply(seq_along(listFileSub$url), function(x){
  download.file(url = listFileSub$url[x], destfile = fileDnld[x])
})

#Read in data
dp04 <- neonUtilities::stackEddy(DirDnld, level = "dp04")      

# rhdf5 is a Bioconductor package. To install, use:
#  install.packages('BiocManager')
#  BiocManager::install('rhdf5')

# manually download here: https://data.neonscience.org/data-products/DP4.00200.001

# We suggest the following data use workflow:
#   Download the desired site-months from the data portal with documentation included.
#   Review the Eddy-Covariance Data Product Bundle ATBD (NEON.DOC.004571). This document contains
# definitions of terms used in the HDF5 file, as well as specific HDF5 file paths for each data product.
# To interact with the HDF5 files, we recommend downloading the free HDFView software to become
# familiar with file contents. For further analysis, we suggest using the neonUtilities::stackEddy() function
# or the rhdf5 package in R, and the h5py package in Python.
# Search the Data Portal News and data product issue log for information regarding known data quality
# concerns.

library(stringr)
library(tidyverse)

# downloaded zip file - extracted and saved to data folder 
# there is a folder for each month (downloaded full year 2019)
month = seq(1:12)

# create list of folder names for future lapply 
dirs_single = paste("data/NEON_eddy-flux/NEON.D16.WREF.DP4.00200.001.2019-0", month,".basic.20250129T000730Z.RELEASE-2025/", sep="")[1:9]
dirs_double =  paste("data/NEON_eddy-flux/NEON.D16.WREF.DP4.00200.001.2019-", month,".basic.20250129T000730Z.RELEASE-2025/", sep="")[10:12]

# combine for all folders 
dirs = c(dirs_single, dirs_double)

#Read in data - this creates a large list of 6 
dp04 <- neonUtilities::stackEddy(dirs[1], level = "dp04")   

# preview in console 
str(dp04)
# note the object type is listed after the name of each item 
# lets preview the first data frame 
str(dp04$WREF)
head(dp04$WREF)
# note the name of each column starts with either 'time', 'data', or 'qfqm', telling us the what that column represents
## data = represents data fields
## qfqm = Quality flag and quality metrics, represents quality flags and quality metrics that accompany the provided data
## timeBgn = The beginning time of the aggregation period
## timeEnd = The end time of the aggregation period

# see a full list of data categories and labels 
View(dp04$variables)
View(dp04$objDesc)

# different variables for the system include 
## nsae = Net surface atmosphere exchange (turbulent + storage fluxes)
## stor = storage
## turb = turbulent 

# lets return to data frame with observations 
# which columns do we want to explore
names(dp04$WREF)

# column names where there is Co2 in the name 
co2_names = names(dp04$WREF)[str_detect(names(dp04$WREF), "Co2")]

# create new data frame, select only co2 cols and time 
df_co2 = dp04$WREF %>% 
  select(all_of(co2_names), timeBgn, timeEnd)

summary(df_co2) 

plot(df_co2$timeBgn, df_co2$data.fluxCo2.nsae.flux)

# create wrapper function to do this for all months 
get_data <- function(dir, var="Co2"){
  
  dp04 <- neonUtilities::stackEddy(dir, level = "dp04")   
  
  get_names = names(dp04$WREF)[str_detect(names(dp04$WREF), var)]
  
  df_co2 = dp04$WREF %>% 
    select(all_of(get_names), timeBgn, timeEnd) %>% # select columns
    mutate(date = date(timeBgn)) %>% # create date col
    group_by(date) %>% # aggregate to date
    summarize_at(all_of(get_names), list(mean=mean, sum=sum), na.rm=T)
  
  return(df_co2)
}

test = lapply(dirs, get_data)
# bind all in the list 
allmonths_daily = do.call(test, rbind)

# preview new dataframe
str(allmonths_daily)

# note about aggregation of quality flags 
# if mean is closer to 1, that is good
# if sum is closer to 48 that is good 


ggplot(allmonths_daily) + 
  geom_line(aes(x=date, y=data.fluxCo2.nsae.flux_sum)) +
  geom_point(aes(x=date, y=data.fluxCo2.nsae.flux_sum, col=qfqm.fluxCo2.nsae.qfFinl_mean)) + scale_color_viridis_c()
