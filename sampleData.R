# load data
load("imported_data//business.RData")
#load("imported_data//checkin.RData")
load("imported_data//review.RData")
#load("imported_data//tip.RData")
#load("imported_data//user.RData")

#Filter businesses to target only Restaurants
restaurants<-business[grep("Restaurants",business$categories),]

#Filter reviews for restaurants
restaurantreview <- review[review$business_id %in% restaurants$business_id,]

#Obtain low star reviews
lowstarreview<-restaurantreview[restaurantreview$stars < 3, ]

#Number of reviews to perform text mining on
length(lowstarreview$stars)

#Save the filtered data
save(lowstarreview, file="work_data//lowstarreview.RData")
