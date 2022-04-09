library(readr)
library(dplyr)
library(caTools)
library(caret)
library(tidyr)
library(lubridate)
library(stringr)
'''
 pre-processing
 '''
#Read data
data = read.csv('/Users/zeyingliu/Documents/5200/Kaggle/analysisData copy.csv',header = T, na.strings = c("","NA","N/A",NA))
scoringData = read.csv('/Users/zeyingliu/Documents/5200/Kaggle/scoringData edited.csv', na.strings = c("","NA","N/A",NA))

# Columns overview
# i = 1
# while (i < 93) {
#   print(length(unique(data[,i])))
#   print(colnames(data)[i])
#   print(i)
#   i= i+1
# }

# data = data[, -c(15,31,35,36,46, 48,49,63,79,80,81,83)] 
# data = data[, -(1:12)]
#the number of the column might change which is difficult to add or delete columns

#checking correlation between alike variables
#1. stay nights
round(cor(data[,c(55:62)]),2)
#keep 'minimum_nights_avg_ntm','maximum_nights_avg_ntm' which is ajested, no multicollinearity, and cover other variables

#2. availability in days
round(cor(data[,c(65:68)]),2)
#keep availability_60

#3. listing counts
round(cor(data[,c(88:91)]),2)
#keep 89:91



# Select variables
data = data[,c('price'
               ,'host_since'
               ,'neighbourhood_group_cleansed','neighbourhood_cleansed'
               ,'zipcode'
               ,'room_type','property_type','accommodates','guests_included','extra_people'
               ,'bathrooms','bedrooms','beds','bed_type'
               , 'host_response_rate','host_is_superhost','host_has_profile_pic'
               ,'host_identity_verified'
               ,'security_deposit','cleaning_fee'
               ,'number_of_reviews_ltm','instant_bookable','cancellation_policy'
               ,'review_scores_rating','review_scores_accuracy','review_scores_cleanliness'
               ,'review_scores_checkin','review_scores_location'
               ,'review_scores_communication'
                ,'review_scores_value'
               ,'minimum_nights_avg_ntm','maximum_nights_avg_ntm'
               ,'availability_60'
               ,'calculated_host_listings_count_entire_homes','calculated_host_listings_count_private_rooms'
               ,'calculated_host_listings_count_shared_rooms'
                ,'last_review','first_review'
               ,'is_location_exact'
               ,'require_guest_profile_picture','require_guest_phone_verification'
               ,'host_listings_count'
               )]

scoringData=scoringData[,c('id'
                           ,'host_since'
                           ,'neighbourhood_group_cleansed','neighbourhood_cleansed'
                           ,'zipcode'
                           ,'room_type','property_type','accommodates','guests_included','extra_people'
                           ,'bathrooms','bedrooms','beds','bed_type'
                           , 'host_response_rate','host_is_superhost','host_has_profile_pic'
                           ,'host_identity_verified'
                           ,'security_deposit','cleaning_fee'
                           ,'number_of_reviews_ltm','instant_bookable','cancellation_policy'
                           ,'review_scores_rating','review_scores_accuracy','review_scores_cleanliness'
                           ,'review_scores_checkin','review_scores_location'
                           ,'review_scores_communication'
                           ,'review_scores_value'
                           ,'minimum_nights_avg_ntm','maximum_nights_avg_ntm'
                           ,'availability_60'
                           ,'calculated_host_listings_count_entire_homes','calculated_host_listings_count_private_rooms'
                           ,'calculated_host_listings_count_shared_rooms'
                           ,'last_review','first_review'
                           ,'is_location_exact'
                           ,'require_guest_profile_picture','require_guest_phone_verification'
                           ,'host_listings_count'
                           )]

# data$zipcode = as.character(data$zipcode)
# scoringData$zipcode = as.character(scoringData$zipcode)

#calculate days
#1. host days
data = data %>%
  mutate(host_since = mdy(host_since)) %>%
  mutate(host_long = today()-host_since)
data = data[,-2]
data$host_long[is.na(data$host_long)] = 0
data$host_long = as.numeric(data$host_long)
# table(data$host_long)

scoringData = scoringData %>%
  mutate(host_since = mdy(host_since)) %>%
  mutate(host_long = today()-host_since)
scoringData = scoringData[,-2]
scoringData$host_long[is.na(scoringData$host_long)] = 0
scoringData$host_long = as.numeric(scoringData$host_long)

#2. first review
data = data %>%
  mutate(first_review = mdy(first_review)) %>%
  mutate(first_review_long = today()-first_review)
data = data[,-37]
data$first_review_long[is.na(data$first_review_long)] = 0
data$first_review_long = as.numeric(data$first_review_long)

scoringData = scoringData %>%
  mutate(first_review = mdy(first_review)) %>%
  mutate(first_review_long = today()-first_review)
scoringData = scoringData[,-37]
scoringData$first_review_long[is.na(scoringData$first_review_long)] = 0
scoringData$first_review_long = as.numeric(scoringData$first_review_long)

#3. last review
data = data %>%
  mutate(last_review = mdy(last_review)) %>%
  mutate(last_review_long = today()-last_review)
data = data[,-36]
data$last_review_long[is.na(data$last_review_long)] = 0
data$last_review_long = as.numeric(data$last_review_long)

scoringData = scoringData %>%
  mutate(last_review = mdy(last_review)) %>%
  mutate(last_review_long = today()-last_review)
scoringData = scoringData[,-36]
scoringData$last_review_long[is.na(scoringData$last_review_long)] = 0
scoringData$last_review_long = as.numeric(scoringData$last_review_long)

round(cor(data[,c(40:42)]),2)
#keep36,38
data = data[,-41]
scoringData = scoringData[,-41]

# table(scoringData$last_review_long)

# Solve the problem that zipcode appears only once

data = data %>%
  group_by(zipcode) %>%
  mutate(zipcode =  replace(zipcode, n()<=2, 'other')) %>%
  ungroup()
# table(data$zipcode)

scoringData =scoringData %>%
  group_by(zipcode) %>%
  mutate(zipcode =  replace(zipcode, n()<=2, 'other')) %>%
  ungroup()
# table(scoringData$zipcode)

# Solve the problem that neighborhood appears only once
# table(data$neighbourhood_cleansed)
data = data %>% 
  group_by(neighbourhood_cleansed) %>% 
  mutate(neighbourhood_cleansed =  replace(neighbourhood_cleansed, n()<=2, 'other')) %>% 
  ungroup()
# table(data$neighbourhood_cleansed)

scoringData = scoringData %>% 
  group_by(neighbourhood_cleansed) %>% 
  mutate(neighbourhood_cleansed =  replace(neighbourhood_cleansed, n()<=2, 'other')) %>% 
  ungroup()

#Solve the problem that property appears only once
# table(data$property_type)
data = data %>% 
  group_by(property_type) %>% 
  mutate(property_type =  replace(property_type, n()<=2, 'other')) %>% 
  ungroup()
# table(data$property_type)

scoringData = scoringData %>% 
  group_by(property_type) %>% 
  mutate(property_type =  replace(property_type, n()<=2, 'other')) %>% 
  ungroup()

# get number of response rate
data$host_response_rate[is.na(data$host_response_rate)] = 0
scoringData$host_response_rate[is.na(scoringData$host_response_rate)] = 0

data = data %>% 
  mutate(host_response_rate = parse_number(host_response_rate))

scoringData = scoringData %>% 
  mutate(host_response_rate = parse_number(host_response_rate)) 


# give values to NA & change to dummy
#1. super host?
# table(data$host_is_superhost)# f is majority
table(data$host_is_superhost)
data$host_is_superhost[is.na(data$host_is_superhost)] = 'f'
# data = data %>% 
#   mutate(host_is_superhost = ifelse(host_is_superhost == 'f',0,1))

scoringData$host_is_superhost[is.na(scoringData$host_is_superhost)] = 'f'
# scoringData = scoringData %>% 
#   mutate(host_is_superhost = ifelse(host_is_superhost == 'f',0,1))
table(data$host_is_superhost)

#2.profile?
# table(data$host_has_profile_pic)# t is majority
data$host_has_profile_pic[is.na(data$host_has_profile_pic)] = 't'
# data = data %>% 
#   mutate(host_has_profile_pic = ifelse(host_has_profile_pic == 'f',0,1))

scoringData$host_has_profile_pic[is.na(scoringData$host_has_profile_pic)] = 't'
# scoringData = scoringData %>% 
#   mutate(host_has_profile_pic = ifelse(host_has_profile_pic == 'f',0,1))
table(data$host_has_profile_pic)

#3. identity
# table(data$host_identity_verified)# f is majority
table(data$host_identity_verified)
data$host_identity_verified[is.na(data$host_identity_verified)] = 'f'
# data = data %>% 
#   mutate(host_identity_verified = ifelse(host_identity_verified == 'f',0,1))

scoringData$host_identity_verified[is.na(scoringData$host_identity_verified)] = 'f'
# scoringData = scoringData %>% 
#   mutate(host_identity_verified = ifelse(host_identity_verified == 'f',0,1))


#4. beds
data$beds[is.na(data$beds)] = 1
scoringData$beds[is.na(scoringData$beds)] = 1

#5. listing counts
data$host_listings_count[is.na(data$host_listings_count)] = 0
scoringData$host_listings_count[is.na(scoringData$host_listings_count)] = 0

# #6. reviews per month
# data$reviews_per_month[is.na(data$reviews_per_month)] = 0
# scoringData$reviews_per_month[is.na(scoringData$reviews_per_month)] = 0

# write.csv(data, '/Users/zeyingliu/Documents/5200/Kaggle/dataOutput.csv',row.names = F)
# write.csv(scoringData, '/Users/zeyingliu/Documents/5200/Kaggle/scoringDataOutput.csv',row.names = F)

sum(which(is.na(data)))

# Data overview
library(skimr)
skim(data)
skim(scoringData)

#delete(temperately) words
# data = data[, -(1:12)]
# data = data[, -15]

# #check correlations
library(tidyr)
library(ggcorrplot)
library(broom)
data_cor = data[,c(1, 7:12,14, 18:20,23:35, 39:41)]
ggcorrplot(cor(data_cor),
           method = 'square',
           type = 'lower',
           show.diag = F,
           colors = c('#e9a3c9', '#f7f7f7', '#a1d76a'))

res = cor(data_cor)
a = round(res, 3)#check the correlations
#remove: review_scores_value(-0.004)
which( colnames(data)=="review_scores_value" )
data = data[,-29]
scoringData = scoringData[,-29]
#remove: host_long, last_review_long
data = data[,-c(39:40)]
scoringData = scoringData[,-c(39:40)]
#remove:review_scores_checkin, minimum_nights_avg_ntm,review_scores_communication
which( colnames(data)=="review_scores_checkin" )
data = data[,-26]
scoringData = scoringData[,-26]
which( colnames(data)=="minimum_nights_avg_ntm" )
data = data[,-28]
scoringData = scoringData[,-28]
which( colnames(data)=="review_scores_communication" )
data = data[,-27]
scoringData = scoringData[,-27]


#select features
library(leaps)
start_mod = lm(price~1,data=data_cor)
empty_mod = lm(price~1,data=data_cor)
full_mod = lm(price~.,data=data_cor)
backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='both')
# data_trt = data[c('price',
#   'extra_people','host_response_rate','review_scores_accuracy','security_deposit',
#   'guests_included','beds','maximum_nights_avg_ntm','review_scores_cleanliness',
#   'review_scores_checkin','availability_60','calculated_host_listings_count_shared_rooms',
#   'review_scores_rating','minimum_nights_avg_ntm','calculated_host_listings_count_entire_homes',
#   'calculated_host_listings_count_private_rooms','number_of_reviews_ltm','last_review_long',
#   'bedrooms','review_scores_value','bathrooms','review_scores_location','accommodates',
#   'cleaning_fee'
#                   )]

#split
library(caret)
set.seed(1031)
split = createDataPartition(y=data$price,p = 0.8,list = F,groups = 100)
train = data[split,]
test = data[-split,]

model1 = lm(price~., train)

pred_test = predict(model1, newdata = test)
rmse_test = sqrt(mean((pred_test - test$price)^2));rmse_test

#xgboost
library(vtreat)
#with zipcode
trt_data = designTreatmentsZ(dframe = data,
                             varlist = names(data)[c(2:35)])
# data2 = data[c(4, 7:12,14:21,23:38)]
# #with out zipcode
# trt_data = designTreatmentsZ(dframe = data,
#                              varlist = names(data)[c(6:11,13:20,22:37)])

# a = data[c(2,5,8:13,15,19:21,24:33)]
newvars.data = trt_data$scoreFrame[trt_data$scoreFrame$code%in% c('clean','lev'),'data.varName']

data_input = prepare(treatmentplan = trt_data,
                     dframe = data,
                     varRestriction = newvars.data)

train_input = prepare(treatmentplan = trt_data,
                     dframe = train,
                     varRestriction = newvars.data)

test_input =  prepare(treatmentplan = trt_data,
                      dframe = test,
                      varRestriction = newvars.data)

scoringData$price = as.integer(0)
scoring_input = prepare(treatmentplan = trt_data,
                        dframe = scoringData,
                        varRestriction = newvars.data)

# # pre operation
# library(Matrix)
# data_input = Matrix(data.matrix(data[,c(2:35)]), sparse = T)
# scoring_input = Matrix(data.matrix(scoringData[,c(2:35)]), sparse = T)

# head(data_input)

library(xgboost)
library(caret)
set.seed(1234)
xgb_nrounds = xgb.cv(data=as.matrix(train_input), 
                     label = train$price,
                     nrounds = 100,
                     nfold = 5,
                     eta = 0.22,
                     gamma = 0,
                     verbose = 0)

which.min(xgb_nrounds$evaluation_log$test_rmse_mean)

xgboost2 = xgboost(data=as.matrix(train_input), 
                  label = train$price,
                  nrounds= 99,
                  eta = 0.22,
                  gamma = 0,
                  verbose = 0)

pred_data = predict(xgboost2, as.matrix(test_input))
rmse_data = sqrt(mean((pred_data - test$price)^2)); rmse_data

xgb_nrounds_final = xgb.cv(data=as.matrix(data_input), 
                     label = data$price,
                     nrounds = 250,
                     nfold = 5,
                     eta = 0.22,
                     gamma = 0,
                     min_child_weight = 4,
                     verbose = 0)

which.min(xgb_nrounds_final$evaluation_log$test_rmse_mean)

xgboost3 = xgboost(data=as.matrix(data_input), 
                   label = data$price,
                   nrounds= 132,
                   eta = 0.22,
                   gamma = 0,
                   min_child_weight = 4,
                   verbose = 0)
pred_data2 = predict(xgboost3, as.matrix(data_input))
rmse_data2 = sqrt(mean((pred_data2 - data$price)^2)); rmse_data2

#submit
pred_score = predict(xgboost3, as.matrix(scoring_input))
submissionFile = data.frame(id = scoringData$id, price = pred_score)
write.csv(submissionFile, '/Users/zeyingliu/Documents/5200/Kaggle/sample_submission.csv',row.names = F)

