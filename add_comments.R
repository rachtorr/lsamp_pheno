
library(neonUtilities)

start = "2016-01-01"
end = "2022-12-30"

neon_df <- loadByProduct(dpID=c("DP1.00095.001"),
                          site=c("WREF"),
                          startdate=start,
                          enddate=end)

str(neon_df)

names(neon_df)

dwsd2 = neon_df$twoDWSD_2min

head(dwsd2)
dim(dwsd2)
names(dwsd2)

daily_avg_wind = dwsd2 %>% 
  mutate(date = date(startDateTime)) %>% 
  group_by(date) %>% 
  summarize(mean=mean(windSpeedMean),
            max =max(windSpeedMean),
            min =min(windSpeedMean))

ggplot(daily_avg_wind, aes(x=date, y=mean)) + geom_point()  
