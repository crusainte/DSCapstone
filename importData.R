library(jsonlite)

# setting variables for individual JSON files
json_business <- "raw_data//yelp_academic_dataset_business.json"
json_checkin <- "raw_data//yelp_academic_dataset_checkin.json"
json_review <- "raw_data//yelp_academic_dataset_review.json"
json_tip <- "raw_data//yelp_academic_dataset_tip.json"
json_user <- "raw_data//yelp_academic_dataset_user.json"


# reading in each json file and store as RData object for ease of loading
business <- stream_in(file(json_business))
save(business, file="imported_data//business.RData")
#To save RAM
rm(business)

checkin <- stream_in(file(json_checkin))
save(checkin, file="imported_data//checkin.RData")
#To save RAM
rm(checkin)

review <- stream_in(file(json_review))
save(review, file="imported_data//review.RData")
#To save RAM
rm(review)

tip <- stream_in(file(json_tip))
save(tip, file="imported_data//tip.RData")
#To save RAM
rm(tip)

user <- stream_in(file(json_user))
save(user, file="imported_data//user.RData")
#To save RAM
rm(user)