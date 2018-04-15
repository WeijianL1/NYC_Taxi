library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('alluvial') # visualisation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps
library('xgboost') # modelling
library('caret') # modelling
library('forcats')

data = fread('/Users/Chen/Desktop/train.csv')
# see if there are missing data in the file 
sum(is.na(data))
# turn date and time from characters to date objects; recode vendor_id as a factor 
data <- data %>% mutate(pickup_datetime = ymd_hms(pickup_datetime),
                        dropoff_datetime = ymd_hms(dropoff_datetime),
                        vendor_id = factor(vendor_id),
                        passenger_count = numeric(passenger_count))

# find the distance between each pair of pickup and dropoff 
pick_coord <- data %>% select(pickup_longitude, pickup_latitude)
drop_coord <- data %>% select(dropoff_longitude, dropoff_latitude)
data$dist <- distCosine(pick_coord, drop_coord)
data$bearing = bearing(pick_coord, drop_coord)

# add external location into the data 
jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)
data$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
data$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
data$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
data$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)

# add new variables to the data by manipulating the original variables ###
data <- data %>% mutate(speed = (dist/data$trip_duration)*3.6,
       date = date(pickup_datetime),
       month = month(pickup_datetime, label = TRUE),
       wday = wday(pickup_datetime, label = TRUE),
       wday = fct_relevel(wday, c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")),
       hour = hour(pickup_datetime),
       work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
       jfk_trip = (data$jfk_dist_pick < 2e3) | (data$jfk_dist_drop < 2e3),
       lg_trip = (data$lg_dist_pick < 2e3) | (data$lg_dist_drop < 2e3),
       blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
       )

# trips longer than a day --> 
day_plus_trips <- data %>% filter(trip_duration > 24*3600)
day_plus_trips %>% select(pickup_datetime, dropoff_datetime)
# these points will be removed from the data 

ny_map <- as.tibble(map_data("state", region = "new york:manhattan"))
tpick <- day_plus_trips %>% select(lon = pickup_longitude, lat = pickup_latitude)
tdrop <- day_plus_trips %>% select(lon = dropoff_longitude, lat = dropoff_latitude)
# plotting out the outliers on the Mnahattan map 
p1 <- ggplot() +
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1) +
  geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1)

for (i in seq(1,nrow(tpick))){
  inter <- as.tibble(gcIntermediate(tpick[i,],  tdrop[i,], n=30, addStartEnd=TRUE))
  p1 <- p1 +  geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.75)
}

p1 + ggtitle("Longer than a day trips in relation to Manhattan")

# trips close to 24 hours-->outlier: remove trips longer than 22 hours 
day_trips <- data %>% filter(trip_duration < 24*3600 & trip_duration > 22*3600)
day_trips %>% arrange(desc(dist)) %>% select(dist, pickup_datetime, dropoff_datetime, speed) %>%
head(5)

ny_map <- as.tibble(map_data("state", region = "new york:manhattan"))

set.seed(2017)
day_trips <- day_trips %>% sample_n(200) # choose 200 sample out of the 1800 population 
tpick <- day_trips %>% select(lon = pickup_longitude, lat = pickup_latitude)
tdrop <- day_trips %>% select(lon = dropoff_longitude, lat = dropoff_latitude)

p1 <- ggplot() +
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1) +
  geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1)

for (i in seq(1,nrow(tpick))){
  inter <- as.tibble(gcIntermediate(tpick[i,],  tdrop[i,], n=30, addStartEnd=TRUE))
  p1 <- p1 +  geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.25)
}

p1 + ggtitle("Day-long trips in relation to Manhattan")
# red points are pickup and blue points are dropoff 

# trips shorter than a few minutes (check duration and speed)
min_trips <- data %>% filter(trip_duration < 5*60)
min_trips %>% arrange(dist) %>% select(dist, pickup_datetime, dropoff_datetime, speed) %>%
head(5)

# zero-distance trips --> find number of them: remove zero-distance that took more than a minute
zero_dist <- data %>% filter(near(dist,0))
nrow(zero_dist)

zero_dist %>% arrange(desc(trip_duration)) %>% select(trip_duration, pickup_datetime, dropoff_datetime, vendor_id) %>%
head(5)

# impossible speed trips
min_trips <- data %>% filter(trip_duration < 5*60 & dist > 0)
min_trips %>% arrange(desc(speed)) %>% select(trip_duration, dist, pickup_datetime, speed) %>%
head(10)

ny_map <- as.tibble(map_data("state", region = "new york:manhattan"))
set.seed(1234)
foo <- min_trips %>% sample_n(600)

tpick <- foo %>% select(lon = pickup_longitude, lat = pickup_latitude)
tdrop <- foo %>% select(lon = dropoff_longitude, lat = dropoff_latitude)

p1 <- ggplot() +
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1) +
  geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1)

for (i in seq(1,nrow(tpick))){
  inter <- as.tibble(gcIntermediate(tpick[i,],  tdrop[i,], n=30, addStartEnd=TRUE))
  p1 <- p1 +  geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.25)
}

p1 + ggtitle("Minute-long trips in relation to Manhattan")
p1 <- 1
# impose a lower trip_duration limit of 10 seconds and a (very conservative) speed limit of 100 km/h (62 mph

# pickup or dropoff more than 300km away from NYC 
long_dist <- data %>% filter( (jfk_dist_pick > 3e5) | (jfk_dist_drop > 3e5) )
long_dist_coord <- long_dist %>% select(lon = pickup_longitude, lat = pickup_latitude)

long_dist %>% select(id, jfk_dist_pick, jfk_dist_drop, dist, trip_duration, speed) %>% arrange(desc(jfk_dist_pick))

leaflet(long_dist_coord) %>% addTiles() %>%
  setView(-92.00, 41.0, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(popup = ~as.character(long_dist$dist), 
             label = ~as.character(long_dist$id))

# final cleaning: include only trips that are shorter than 22 hrs, non-zero distance, 
#near zero distance but with only a minute of trip duration, the pickup and dropoff distance 
#to JFK is smaller than 300km away, and the trip duration larger than 10 and speed smaller than 100 
data <- data %>% filter(trip_duration < 22*3600,
         dist > 0 | (near(dist, 0) & trip_duration < 60),
         jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5,
         trip_duration > 10,
         speed < 100)

# external fastest route data 
data1 = fread('/Users/Chen/Desktop/fastest_routes_train_part_1.csv') #foo
data2 = fread('/Users/Chen/Desktop/fastest_routes_train_part_2.csv') #bar
fastest_route <- bind_rows(data1, data2)
data1 <- fastest_route %>% select(id, total_distance, total_travel_time, number_of_steps,
         step_direction, step_maneuvers) %>%
  mutate(fastest_speed = total_distance/total_travel_time*3.6,
         left_turns = str_count(step_direction, "left"),
         right_turns = str_count(step_direction, "right"),
         turns = str_count(step_maneuvers, "turn")
  ) %>% select(-step_direction, -step_maneuvers)

# join external data with original data 
data_full <- left_join(data, data1, by = "id") %>% mutate(fast_speed_trip = total_distance/trip_duration*3.6)
head(5)

# see if there are missing values and exclude them 
sum(is.na(data_full))
taxi = na.omit(data_full)

# This is the final data before we remove the variables 
taxi<- data_full %>%
  filter(passenger_count>0,vendor_id==1,total_distance>0, total_distance<30000, trip_duration < 3*3600, trip_duration > 0)

summary(taxi)

# Removing the variables that we do not need 
taxi <- taxi %>% select(-id)
taxi <- taxi %>% select(-vendor_id)
taxi <- taxi %>% select(-store_and_fwd_flag)
taxi <- taxi %>% select(-pickup_datetime)
taxi <- taxi %>% select(-dropoff_datetime)
taxi <- taxi %>% select(-blizzard)
taxi <- taxi %>% select(-bearing)
taxi <- taxi %>% select(-dist)
taxi <- taxi %>% select(-jfk_dist_pick)
taxi <- taxi %>% select(-jfk_dist_drop)
taxi <- taxi %>% select(-jfk_trip)
taxi <- taxi %>% select(-lg_dist_pick)
taxi <- taxi %>% select(-lg_dist_drop)
taxi <- taxi %>% select(-lg_trip)
taxi <- taxi %>% select(-fastest_speed)
taxi <- taxi %>% select(-speed)
taxi <- taxi %>% select(-date)
taxi <- taxi %>% select(-total_travel_time)
taxi <- taxi %>% select(-fast_speed_trip)
taxi <- taxi %>% select(-work)

summary(taxi)

# add dummy variables for month, wday and hour 
isJan <- rep(NA, nrow(taxi))
isJan <- ifelse(taxi$month == "Jan", 1, 0)
isFeb <- rep(NA, nrow(taxi))
isFeb <- ifelse(taxi$month == "Feb", 1, 0)
isMar <- rep(NA, nrow(taxi))
isMar <- ifelse(taxi$month == "Mar", 1, 0)
isApr <- rep(NA, nrow(taxi))
isApr <- ifelse(taxi$month == "Apr", 1, 0)
isMay <- rep(NA, nrow(taxi))
isMay <- ifelse(taxi$month == "May", 1, 0)
isJun <- rep(NA, nrow(taxi))
isJun <- ifelse(taxi$month == "Jun", 1, 0)

isMon <- rep(NA, nrow(taxi))
isMon <- ifelse(taxi$wday == "Mon", 1, 0)
isTue <- rep(NA, nrow(taxi))
isTue <- ifelse(taxi$wday == "Tue", 1, 0)
isWed <- rep(NA, nrow(taxi))
isWed <- ifelse(taxi$wday == "Wed", 1, 0)
isThu <- rep(NA, nrow(taxi))
isThu <- ifelse(taxi$wday == "Thu", 1, 0)
isFri <- rep(NA, nrow(taxi))
isFri <- ifelse(taxi$wday == "Fri", 1, 0)
isSat <- rep(NA, nrow(taxi))
isSat <- ifelse(taxi$wday == "Sat", 1, 0)
isSun <- rep(NA, nrow(taxi))
isSun <- ifelse(taxi$wday == "Sun", 1, 0)

isRush <- rep(NA, nrow(taxi))
isRush <- ifelse(taxi$hour>=6 & taxi$hour<=9, 1, 0)
isRush <- ifelse(taxi$hour>=15 & taxi$hour<=19, 1, 0)


taxi <- cbind(taxi, isJan, isFeb, isMar, isApr, isMay, isJun, isMon, isTue, isWed, isThu, isFri, isSat, isSun, isRush)
summary(taxi)

taxi <- taxi %>% select(-month)
taxi <- taxi %>% select(-wday)
taxi <- taxi %>% select(-hour)

# split data into training and test 
set.seed(4)
test=sample(1:NROW(taxi),size = NROW(taxi)/2)
taxi.test=taxi[test,]
taxi.train=taxi[-test,]


# PCA 
pr.out=prcomp(taxi.train[,-6], scale=TRUE)

pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type="b")

cumsum(pve) # we can see that we will use 16 PCs (where the PC reach 90%)

# get the new PCA reduced dimension data frame 
taxi_pca = prcomp(taxi.train[,-6])$x[,1:16]
taxi_data = data.frame(cbind(taxi_pca,taxi.train$trip_duration))
# change the name of V17 
names(taxi_data)[names(taxi_data) == "V17"] = "trip_duration"
summary(taxi_data)

# run linear regression on the PCA data 
lm.fit = lm(trip_duration~.,data = taxi_data)
summary(lm.fit)

taxi_data.test = taxi.test * (((taxi.svd$u) %*% diag(taxi.svd$d))[,1:16])
lm.predict = predict(lm.fit, data = taxi_data.test)

test.lm_error = mean((taxi.test$trip_duration - lm.predict)^2)
test.lm_error