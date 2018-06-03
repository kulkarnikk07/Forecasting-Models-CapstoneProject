
library('ggplot2')
library('RColorBrewer') 
library('dplyr') 
library('readr') 
library('data.table')
library('tibble') 
library('ggrepel') 
library('ggridges') 
library('ggExtra') 
library('ggforce') 
library('viridis') 
library('magrittr')
library('lubridate') 
library('timeDate') 
library('tseries') 
library('forecast')

# tibble is used to read the .csv file into the database
air_visits <-as.tibble(read.csv(file="air_visit_data.csv",head=TRUE,sep=","))
air_reserve <- as.tibble(read.csv(file="air_reserve.csv",head=TRUE,sep=","))
hpg_reserve <-as.tibble(read.csv(file="hpg_reserve.csv",head=TRUE,sep=","))
air_store <-as.tibble( read.csv(file="air_store_info.csv",head=TRUE,sep=","))
hpg_store <- as.tibble(read.csv(file="hpg_store_info.csv",head=TRUE,sep=","))
holidays <- as.tibble(read.csv(file="date_info.csv",head=TRUE,sep=","))
store_ids <-as.tibble( read.csv(file="store_id_relation.csv",head=TRUE,sep=","))
test <-as.tibble( read.csv(file="sample_submission.csv",head=TRUE,sep=","))

#using mutate to append the date in year month and day format
air_visits <- mutate(air_visits,visit_date = ymd(visit_date))

air_reserve <- dplyr::mutate(air_reserve,visit_datetime = ymd_hms(visit_datetime),
                             reserve_datetime = ymd_hms(reserve_datetime))

hpg_reserve <-dplyr::mutate(hpg_reserve,visit_datetime = ymd_hms(visit_datetime),
                reserve_datetime = ymd_hms(reserve_datetime))

air_store <-dplyr::mutate(air_store , air_genre_name = as.factor(air_genre_name),
                air_area_name = as.factor(air_area_name))

hpg_store <-dplyr::mutate( hpg_store, hpg_genre_name = as.factor(hpg_genre_name),
                hpg_area_name = as.factor(hpg_area_name))

holidays <-dplyr::mutate(holidays, holiday_flg = as.logical(holiday_flg),
                date = ymd(calendar_date))

#AirVisits
summary(air_visits)
glimpse(air_visits)
nrow( distinct(air_store_id), air_visits)

#AirReserve
summary(air_reserve)
glimpse(air_reserve)
nrow(distinct(air_store_id),air_reserve)

# HPG Reserve
summary(hpg_reserve)
glimpse(hpg_reserve)
nrow(distinct(hpg_store_id),hpg_reserve )


#Analysis_AirVisits
p1 <- air_visits %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line(col = "Orange") +
  labs(y = "Total Number of Visitors", x = "Date: Jan 2016 - April 2017")

plot (p1)


p2 <- air_visits %>%
  ggplot(aes(visitors)) +
  geom_vline(xintercept = 20, color = "black") +
  geom_histogram(fill = "blue", bins = 30) +
  scale_x_log10()

plot (p2)

p3 <- air_visits %>%
  mutate(wday = lubridate::wday(visit_date, label = TRUE,abbr = FALSE)) %>%
  group_by(wday) %>%
  summarise(visits = mean(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Mean visitors")

plot (p3)

p4 <- air_visits %>%
  mutate(month = lubridate::month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = mean(visitors)) %>%
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Average visitors")

plot (p4)


#Analysis_AirReserve
res <- air_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
  )

p5 <- res %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color="orange") +
  labs(x = "'air' visit date", y= "number of reservation")

plot(p5)

p6 <- res %>%
  group_by(visit_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = "red")+
  labs(x = "Visit hours",y = "number of reservation")
plot(p6)

p7 <- res %>%
  filter(diff_hour < 24*5) %>%
  group_by(diff_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = "red") +
  labs(x = "Time from reservation to visit [hours]",y = "number of reservation")
plot(p7)


res %>%
  arrange(desc(diff_day)) %>%
  select(reserve_datetime, visit_datetime, diff_day, air_store_id) %>%
  head(5)

#Analysis_HPGReserve

Hres<- hpg_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
  )

p8 <- Hres %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color="orange") +
  labs(x = "'hpg' visit date")+
  labs(x = "'HPG' visit date", y= "number of reservation")

plot(p8)

p9 <- Hres %>%
  group_by(visit_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = "red")+
  labs(x = "Visit hours",y = "number of reservation")

plot(p9)

p10 <- Hres %>%
  filter(diff_hour < 24*5) %>%
  group_by(diff_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = "red") +
  labs(x = "Time from reservation to visit" ,y = "number of reservation")
plot(p10)


## Reservations vs Visits

 all_reserve %>%
  filter(reserve_visitors < 120) %>%
  ggplot(aes(reserve_visitors, visitors)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_smooth(method = "lm", color = "blue")
ggMarginal(p11, type="histogram", fill = "#FF9999", bins=50)

