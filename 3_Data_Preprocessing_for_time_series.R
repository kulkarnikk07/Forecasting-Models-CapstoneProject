ls()
rm(list=ls())

#install.packages("padr")
#install.packages("forecast")

require(padr)
require(forecast)
#require(zoo)

mdat <- read.csv("air_combined.csv", header = TRUE, na.strings = '', stringsAsFactors = FALSE)
res <- unique(mdat$air_store_id)
#mdat$weekday <- weekdays(as.Date(mdat$visit_date))

final_df <- mdat[0,]

for (i in 1:length(res)){
  temp_dat <- mdat[grep(res[i], mdat$air_store_id),]
  temp_dat$visit_date <- as.Date(temp_dat$visit_date, "%m/%d/%Y")
  #genre <- temp_dat$air_genre_name[1]
  temp_dat <- pad(temp_dat)
  #temp_dat$air_store_id <- na.locf(temp_dat$air_store_id)
  temp_dat[,1:5] <- na.locf(temp_dat[,1:5])
  temp_dat$weekday <- weekdays(temp_dat$visit_date)
  final_df <- rbind(final_df,temp_dat)
}

## converting all character columns to factors expcept for lat and long
#final_df$air_store_id <-as.factor(final_df$air_store_id)
#final_df$air_genre_name <- as.factor(final_df$air_genre_name)
final_df[,c(1:3,9)] <- lapply(final_df[,c(1:3,9)], factor)

#creating and filling values for closed_days in te df
final_df$closed_days <- ifelse(is.na(final_df$visitors_visited),1,0)

#replacing missing values (NA) with zeros:
final_df[is.na(final_df)] <- 0

final_df$log_visitors <- log(final_df$visitors_visited + 1)
final_df$log_reserves <- log(final_df$visitors_reserved + 1)

#re-ordering the last three columns
final_df <- final_df[,c(1:9,11,12,10)]


#inserting dummy for weekly (sunday is reference)
mon_thurs <- c("Monday", "Tuesday", "Wednesday", "Thursday")
final_df$mon_thurs <- ifelse(final_df$weekday %in% mon_thurs, 1, 0)
final_df$friday <- ifelse(final_df$weekday == "Friday", 1, 0)
final_df$saturday <- ifelse(final_df$weekday == "Saturday", 1, 0)

#writing the resulting final dataframe to a separate file and saving it
write.csv(final_df, file = "final_dataset.csv", row.names = FALSE)

