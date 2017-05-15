
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
theme_set(theme_bw())
options(scipen = 999)

#read in listings data sets
listings_full <- read_csv("listings.csv")

#select certain variables for initial investigation on distribution of multi-listing hosts 
host_df <- listings_full %>% select(host_id, 
                                    host_name, 
                                    host_since, 
                                    host_location, 
                                    host_is_superhost, 
                                    host_neighbourhood, 
                                    host_listings_count,
                                    id)

##Concentration of multi-listers
#counting by group_by(host_id)
host_count <- host_df %>% group_by(host_id) %>% summarise(num_list=n())

h1 <- host_count %>% 
  group_by(num_list) %>% 
  summarise(count_list=n()) %>% 
  ungroup() %>%  
  mutate(percent = count_list/sum(count_list))

#most hosts only have one listing (88%)
ggplot(h1, aes(x=num_list, y=count_list)) + 
  geom_line() + 
  scale_x_log10() +
  scale_y_continuous(labels = comma) +
  labs(x="Number of Listings", y="Frequency") +
  ggtitle("Distribution of Number of Listings by Host ID \n (all room types)") +
  ggsave('dist-of-multihosts.png', width = 12, height =6) 

#filter out hosts with only 1 listing
h1_filter <- h1 %>% filter(num_list>1)

#also create a dataframe that has all host details, but only for multi-listers! 
host_plus <- host_count %>% filter(num_list>1)
host_join_full <- left_join(host_plus, host_df) 
host_join <- host_join_full[!duplicated(host_join_full$host_id), ]

#11.4% of hosts have 2 or more listings
summarise(h1_filter, sum(percent))

##filter only to entire home
list_room <- listings_full %>% select(id, 
                                      room_type,
                                      property_type)

host_room_join <- left_join(host_join_full, list_room, by="id")

host_df_home <- left_join(host_df, list_room, by="id")
host_df_home <- host_df_home %>% filter(room_type=="Entire home/apt")

host_home_count <- host_df_home %>% group_by(host_id) %>% summarise(num_list=n())

host_home_count_total <- host_home_count %>% 
  group_by(num_list) %>% 
  summarise(count_list=n()) %>% 
  ungroup() %>%  
  mutate(percent = count_list/sum(count_list))

#3.6% of users who list entire home/apartment have multiple entire home/apartment listings
host_home_count_total %>% filter(num_list>1) %>% summarise(sum(percent))

ggplot(host_home_count_total, aes(x=num_list, y=count_list)) + 
  geom_line() + 
  scale_x_log10() +
  scale_y_continuous(labels = comma) +
  labs(x="Number of Entire Home/Apartment Listings", y="Frequency") +
  ggtitle("Distribution of Number of Listings by Host ID \n (Entire Home/Apartment Only)") +
  ggsave('dist-apt-only-multihosts.png', width = 12, height =6) 

#comparing the variable "host_listings_count" with actually counting the group_by of host_ids 
host_count2 <- host_df[!duplicated(host_df$host_id), ] 
host_count2 <-host_count2 %>% select(host_id, host_listings_count)
host_count_join <- left_join(host_count, host_count2)

#6.7% of counts are wrong, and the range is way off
host_count_join <- host_count_join %>% 
  mutate(same=ifelse(host_listings_count==num_list, "yes", "no")) %>% 
  group_by(same) %>% summarise(count=n())
wrong_count <- host_count_join %>% filter(same!="yes")
sum(wrong_count$count)/sum(host_count_join$count)

range(host_count2$host_listings_count, na.rm=TRUE)
range(host_count$num_list, na.rm=TRUE)

##Who are multi-listers?
#Of all rentals listed by a multi-host, what is the breakdown by property type?
host_property <- host_room_join %>% group_by(property_type) %>% summarise(count=n())
ggplot(host_property, aes(x=reorder(property_type, count), y=count)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(x="Property Type", y="Count") +
  ggtitle("Property Type Breakdown of Rentals by a Multi-Lister")+
  ggsave('multihosts-proptype.png', width = 12, height =6) 

#Of all rentals listed by a multi-host, what is the breakdown by room type?
host_room <- host_room_join %>% group_by(room_type) %>% summarise(count=n())
ggplot(host_room, aes(x=reorder(room_type, count), y=count)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(x="Room Type", y="Count") +
  ggtitle("Room Type Breakdown of Rentals by a Multi-Lister")+
  ggsave('multihosts-roomtype.png', width = 12, height =6) 

#What year did these multi-listing hosts join?
host_year_month <- host_join %>% mutate(year = format(host_since,'%Y'),
                                        month = format(host_since,'%m'))

host_year_summ <- host_year_month %>% group_by(year) %>% summarise(count=n())
host_year_summ <- host_year_summ %>% filter(year!="NA")

ggplot(host_year_summ, aes(x=year, y=count)) + 
  geom_col()+
  labs(x="Year", y="Count") +
  ggtitle("What year did multi-listers join?")+
  ggsave('multihosts-year.png', width = 12, height =6) 

#13% of these multiple renters are superhosts
super <- host_join %>% 
  group_by(host_is_superhost) %>% 
  summarise(num_super=n()) %>% 
  mutate(percent = num_super/sum(num_super))

##Location of multi-lister homes
#join with listing location details
list_loc <- listings_full %>% select(id, 
                                     neighbourhood_cleansed, 
                                     neighbourhood_group_cleansed, 
                                     city,
                                     zipcode,
                                     latitude, 
                                     longitude,
                                     room_type)

host_loc_join <- left_join(host_join, list_loc, by="id") %>% mutate(val = 1)

list_df <- listings_full %>% select(id, neighbourhood_group_cleansed, neighbourhood_cleansed, latitude, longitude)

#by neighbourhood
host_neighb <- host_loc_join %>% group_by(neighbourhood_cleansed) %>% summarise(count=n()) 

names(host_neighb)[names(host_neighb) == 'neighbourhood_cleansed'] <- 'neighbourhood'
names(host_neighb)[names(host_neighb) == 'neighbourhood_group_cleansed'] <- 'Boroughs'
neigh <- list_df %>% select(neighbourhood_group_cleansed, neighbourhood_cleansed) 
neigh <- unique(neigh)
names(neigh)[names(neigh) == 'neighbourhood_cleansed'] <- 'neighbourhood'
names(neigh)[names(neigh) == 'neighbourhood_group_cleansed'] <- 'Boroughs'
host_neighb_bor <- left_join(host_neighb,neigh)

#looking at only neighborhoods with more than 50 listings by multi-listers
host_neighb_bor_50 <- host_neighb_bor %>% filter(count>=50)

ggplot(host_neighb_bor_50, aes(x=reorder(neighbourhood, count), y=count, fill=Boroughs)) + 
  geom_col() + 
  coord_flip() +
  labs(y="Count", x="") +
  ggtitle("Neighborhoods with more than 50 listings by multi-listers") +
  ggsave('multi-location-all.png', width = 12, height =6) 

#only one of these 20 listings is outside Brooklyn or Manhattan

#distribution of entire home
list_loc_apt <- host_loc_join %>% filter(room_type=="Entire home/apt")
host_neighb_apt <- list_loc_apt %>% group_by(neighbourhood_cleansed) %>% summarise(count=n()) 
names(host_neighb_apt)[names(host_neighb_apt) == 'neighbourhood_cleansed'] <- 'neighbourhood'
names(host_neighb_apt)[names(host_neighb_apt) == 'neighbourhood_group_cleansed'] <- 'Boroughs'

host_neighb_bor_apt <- left_join(host_neighb_apt,neigh)

#filter to areas with 15 or more entire homes listed by multi-listers
host_neighb_bor_apt_15 <- host_neighb_bor_apt %>% filter(count>=15)
ggplot(host_neighb_bor_apt_15, aes(x=reorder(neighbourhood, count), y=count, fill=Boroughs)) + 
  geom_col() + 
  coord_flip()+
  labs(y="Count", x="") +
  ggtitle("Neighborhoods with more than 15 entire home/apartment listings \n by multi-listers") +
  ggsave('multi-location-apartment.png', width = 12, height =6) 


# Mapping this data by zipcode. To do this, need to install choroplethrZip
# Install devtools, then install the choroplethrZip library from github. Restart your R after you install from github.
install.packages("devtools")
library(devtools)
install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethrZip)

#first ALL room types
host_loc_map <- host_loc_join %>% select(zipcode) %>% group_by(zipcode) %>% summarise(value=n())
host_loc_map$region <- as.character(host_loc_map$zipcode)
#initially got an error indicating these zip codes were not mappable
not_mappable <- c(7302, 10270, 11249, NA)
host_loc_map_edit <- host_loc_map %>% filter(!region %in% not_mappable)
zip_choropleth(host_loc_map_edit, zip_zoom = host_loc_map_edit$region, 
               title = "New York City Airbnb Listings by Multi-Listers",
               legend = "Listings by Multi-Listers")


#now only entire homes
host_loc_map_apt <- list_loc_apt %>% select(zipcode) %>% group_by(zipcode) %>% summarise(value=n())
host_loc_map_apt$region <- as.character(host_loc_map_apt$zipcode)
not_mappable <- c(7302, 10270, 11249, NA)
#set these values to 0 after filtering to only entire apartment 
not_included <- c(11229, 10461, 10304, 11421, 11414, 10473, 11374, 10467, 10312, 10280, 11356, 11423, 10471, 11419, 11219, 10306, 10468, 11413, 11428, 11432, 10455, 11415, 11364, 10069, 10453, 11204, 11429, 10472, 10475, 11362, 10452, 11354, 10474, 11420, 11360, 10470, 10462, 10044, 11109, 11411)
zer <- rep(0)
test_zer <- merge(zer, not_included)
names(test_zer) <- c("value", "region")
host_loc_map_apt_edit <- host_loc_map_apt %>% filter(!region %in% not_mappable) %>% select(-zipcode)
host_loc_map_apt_edit_final <- rbind(host_loc_map_apt_edit, test_zer)
zip_choropleth(host_loc_map_apt_edit_final, zip_zoom = host_loc_map_edit$region, 
               title = "New York City Airbnb Entire Home/Apartment Listings by Multi-Listers",
               legend = "Entire Apartment Listings \n by Multi-Listers")

##Are these home/apt in the same building??????
#filter data set to only multi-listers who rent entire homes
ent_apt <- host_room_join %>% filter(room_type=="Entire home/apt")
latlong <- listings_full %>% select(id, latitude, longitude, is_location_exact)

ent_apt_join <- left_join(ent_apt, latlong, by="id")

ent_apt_join_edit <- ent_apt_join %>% group_by(host_id) %>% mutate(avg_lat = mean(latitude),
                                                                        avg_long = mean(longitude),
                                                                        diff_lat = abs(latitude-avg_lat),
                                                                        diff_long = abs(longitude-avg_long)) %>% 
  ungroup()

#here, only interested in multi-listers who rent out more than 1 entire home
ent_apt_join_edit_count <- ent_apt_join_edit %>% group_by(host_id) %>% summarise(count=n()) %>% filter(count>1)
dupes_ent_apt_list <- ent_apt_join_edit_count$host_id
same_building_list <- ent_apt_join_edit %>% filter(host_id %in% dupes_ent_apt_list)

#these apartments have bascially same lat and long by host_id
ent_apt_same_building <- same_building_list %>% filter(diff_lat<0.001 & diff_long<0.001)

#of the entire homes listed by a multi-lister, 34% of them are listed by the same host in the same building
nrow(ent_apt_same_building) / nrow(ent_apt)

#of all of the entire homes listed on Airbnb in NYC, 5% are by a multi-listers and in the same building
ent_apt_total <- listings_full %>% filter(room_type=="Entire home/apt")
nrow(ent_apt_same_building) / nrow(ent_apt_total)

#there are 463 hosts to target immediatley 
length(unique(ent_apt_same_building$host_id))

