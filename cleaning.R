library(tidyverse)
library(scales)
library(tm)
library(Matrix)
library(glmnet)
library(ROCR)

library(randomForest)

listings <- read_csv('listings.csv')

listings <- filter(listings,host_is_superhost!="NA")
listings <- filter(listings,listings$host_is_superhost!="NA")


finaldata <- (filter(listings,listings$host_response_rate!="N/A" & listings$host_is_superhost != "N/A"))
nrow(finaldata)


keeps= c("host_response_time", "host_response_rate", "host_listings_count", "city", "property_type", "room_type", "accommodates", "bathrooms", "bedrooms", "beds","price","guests_included","availability_30", "availability_60", "number_of_reviews", "review_scores_rating", "review_scores_cleanliness", "review_scores_communication", "review_scores_location", "review_scores_value", "instant_bookable")

checkdata <- finaldata[keeps]

ishost <- finaldata$host_is_superhost
ishost <- ishost=="t"

abdcsub<-checkdata[!checkdata$host_response_time=='N/A',]
abdcsub<-abdcsub[!abdcsub$host_response_rate=='N/A',]

temp<-as.character(abdcsub$host_response_rate)
temp <- gsub("%","",temp)#as.numeric went wrong because it contains % in the character
abdcsub$host_response_rate<-as.numeric(temp)

#temp<-as.character(abdcsub$price)
#temp <- gsub("$","",temp)#as.numeric went wrong because it contains % in the character
#abdcsub$price<-as.numeric(temp)
as2 <- abdcsub
#as2$reviews_per_month[!is.na(as2$reviews_per_month)]<-as.numeric(as2$reviews_per_month[!is.na(as2$reviews_per_month)])

write_csv(as2,"mycsv.csv")
write_csv(data.frame(ishost),"ylab.csv")
