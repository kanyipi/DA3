################################
# Predicting apartment prices  #
#     with random forest       #
#     and other models         #
################################


rm(list=ls())

library(tidyverse)
library(caret)
library(stringr)
library(ranger)
library(modelsummary)
library(pdp)
library(gbm)
library(rattle)


data_cleaning <- function() {
  
data <- read_csv('https://raw.githubusercontent.com/kanyipi/DA3/main/ex2/listings.csv')



data <- data %>%
  filter(property_type %in% c("Entire condominium (condo)", "Entire rental unit", "Entire residential home",
                              "Private room in residential home"))
data <- data %>% mutate( property_type = 
                           ifelse(property_type == "Entire condominium (condo)", "Condo",
                                  ifelse(property_type == "Entire rental unit", "Rental Unit",
                                         ifelse(property_type == "Entire residential home", "Residental Unit",
                                                ifelse(property_type == "Private room in residential home", "Room","Error")))),f_property_type = factor(property_type))
datasummary(property_type ~ N + Percent(), data = data )

data <- data %>%
  mutate(
    f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

data <- data %>%
  mutate(f_room_type = factor(room_type))

data$f_room_type2 <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt", 
                                   ifelse(data$f_room_type== "Private room", "Private", "Error")))

data <- data %>% mutate(bathrooms = as.numeric(gsub("([0-9]+).*$", "\\1", bathrooms_text)))

data <- data %>%
  mutate(price = as.numeric(price)) %>% 
  mutate(
    usd_price_day = price,
    p_host_response_rate = as.numeric(str_replace(host_response_rate,"%","")))

numericals <- c("accommodates","bathrooms","review_scores_rating",
                "number_of_reviews","reviews_per_month",
                "minimum_nights","beds")
data <- data %>%
  mutate_at(vars(all_of(numericals)), lst("n"=as.numeric))

nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))
amenlist <- list()

for (i in 1:nrow(data)) {
  newlist <- data$amenities[i] %>% str_replace_all(",","xxxxxxxxxx") %>% 
    {gsub("[^[:alnum:][:space:]]","",.)} %>%
    str_replace_all("xxxxxxxxxx",",") %>%
    tolower() %>% 
    str_split(",") %>% unlist()
  amenlist <- unique(append(amenlist,newlist))
}

for (i in amenlist) {
  data[[i]]<-0
}

for (i in 89:753) {
  amenity <- colnames(data)[i]
  print(amenity)
  for (j in 1:nrow(data)) {
    #print(j)
    textstring  <- data$amenities[j] %>% str_replace_all(",","xxxxxxxxxx") %>% 
      {gsub("[^[:alnum:][:space:]]","",.)} %>% 
      str_replace_all("xxxxxxxxxx",",") %>%
      tolower()
    if (str_detect(textstring,amenity)) {
      data[j,i] <- 1
    }
  }
}
datasafe <- data
data <- datasafe
write.csv(data,"amenityclean")

delete_ind = list()
for (i in 89:753) {
  if (sum(data[,i])<300) {
    delete_ind <- append(delete_ind, i)
  }
}

data <- data[-unlist(delete_ind)]

data <- data[-157]

write.csv(data,"amenitycleanfilt")

dummies <- names(data)[seq(89,168)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

nums <- unlist(lapply(data, is.numeric))
notnumsid <- !nums
notnumsid["id"]<- TRUE
datanum<-data[ , nums]
datanotnum <- data[ , notnumsid]
datanum<-as.data.frame(do.call(cbind,by(t(datanum),INDICES=names(datanum),FUN=colSums)))
data <- merge(datanum,datanotnum, by = "id")

data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed,room_type,property_type)


#####################
### look at price ###
#####################
datasummary( price ~ Mean + Median + Min + Max + P25 + P75 , data = data )
data <- data %>%
  mutate(ln_price = log(price))
data <- data %>%
  filter(price <1000)


# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, 
         ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2,
         ln_beds = log(n_beds),
         ln_number_of_reviews = log(n_number_of_reviews+1)
  )

# Pool accomodations with 0,1,2,10 bathrooms
data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) )

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))


# Pool and categorize the number of minimum nights: 1,2,3, 3+
data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)
for (j in 1:56 ) data.table::set(data, which((data[[j]])==2), j, 1)


#------------------------------------------------------------------------------------------------


# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values? 
# 1. drop if no target
data <- data %>%
  drop_na(price)

# 2. imput when few, not that important
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
  ) 

to_drop <- c("p_host_response_rate")
data <- data %>%
  select(-one_of(to_drop))

# 3. drop columns when many missing not important

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    flag_n_number_of_reviews=ifelse(n_number_of_reviews==0,1, 0)
  )
table(data$flag_days_since)

# redo features
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
  )

# Look at data
datasummary( id ~ N , data = data )
datasummary_skim( data , 'categorical' )


# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


write_csv(data, "airbnb_manch.csv")
}

#########################################################################################
#
# PART I
# Loading and preparing data ----------------------------------------------
#
#########################################################################################



# Used area
data <- read_csv('airbnb_manch.csv') %>%
  mutate_if(is.character, factor) %>%
  filter(!is.na(price))

# Sample definition and preparation ---------------------------------------

# We focus on normal apartments, n<8
data <- data %>% filter(n_accommodates < 6)


# copy a variable - purpose later, see at variable importance
data <- data %>% mutate(n_accommodates_copy = n_accommodates)

# basic descr stat -------------------------------------------
skimr::skim(data)
datasummary(price~Mean+Median+P25+P75+N,data=data)
datasummary( f_room_type + f_property_type ~ N + Percent() , data = data )

# create train and holdout samples -------------------------------------------
# train is where we do it all, incl CV

set.seed(2801)
train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

# Check the number of observations
dim(data_train)
dim(data_holdout)

# Define models: simpler -> extended

# Basic Variables inc neighnourhood
basic_vars <- c(
  "n_accommodates", "n_beds", "n_days_since",
  "f_property_type","f_room_type", "n_bathrooms",
  "f_neighbourhood_cleansed")

# reviews
reviews <- c("n_number_of_reviews", "flag_n_number_of_reviews" ,
             "n_review_scores_rating", "flag_review_scores_rating")

# Dummy variables
amenities <-  grep("^d_.*", names(data), value = TRUE)

#interactions for the LASSO
# from ch14
X1  <- c("n_accommodates*f_property_type",  "f_room_type*f_property_type",
         "f_room_type*d_hostgreetsyou", "d_backyard*f_property_type",
          "d_wifi*f_property_type", "d_tv*f_property_type")
# with boroughs
X2  <- c("f_property_type*f_neighbourhood_cleansed", "f_room_type*f_neighbourhood_cleansed",
         "n_accommodates*f_neighbourhood_cleansed" )


predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews, amenities)
predictors_E <- c(basic_vars, reviews, amenities, X1,X2)


#########################################################################################
#
# PART II
# RANDOM FORESTS -------------------------------------------------------
#
# We are going to make some simplification for faster running time
#   see the original codes on the book's github page!
#

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# set tuning
tune_grid <- expand.grid(
  .mtry = c(8),
  .splitrule = "variance",
  .min.node.size = c(50)
)


# simpler model for model - using random forest
set.seed(1234)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model_1
# save( rf_model_1 , file = 'rf_model_1.RData' )
# load(url('https://github.com/regulyagoston/BA21_Coding/blob/main/Class_3/data/rf_model_1.RData?raw=true'))

# more complicated model - using random forest
set.seed(1234)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model_2
# save( rf_model_2 , file = 'rf_model_2.RData' )
# load(url('https://github.com/regulyagoston/BA21_Coding/blob/main/Class_3/data/rf_model_2.RData?raw=true'))


# auto tuning first - just takes too much time...
#set.seed(1234)
#system.time({
#   rf_model_2auto <- train(
#     formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
#     data = data_train,
#     method = "ranger",
#     trControl = train_control,
#     importance = "impurity"
#   )
#})
#rf_model_2auto 


# evaluate random forests -------------------------------------------------

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
  )
)
summary(results)


#########################################################################################
#
# PART III
# MODEL DIAGNOSTICS -------------------------------------------------------
#


#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################

# variable importance plot
# 1) full varimp plot, full
# 2) varimp plot grouped
# 3) varimp plot , top 10
# 4) varimp plot  w copy, top 10


rf_model_2_var_imp <- ranger::importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


##############################
# 1) full varimp plot, above a cutoff
##############################

# to have a quick look
plot(varImp(rf_model_2))
cutoff = 50
ggplot(rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp>cutoff,],
       aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1.5) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))

##############################
# 2) full varimp plot, top 10 only
##############################


# have a version with top 10 vars only
ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


##############################
# 2) varimp plot grouped
##############################
# grouped variable importance - keep binaries created off factors together

varnames <- rf_model_2$finalModel$xNames
f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)

groups <- list(f_neighbourhood_cleansed=f_neighbourhood_cleansed_varnames,
               f_property_type = f_property_type_varnames,
               f_room_type = f_room_type_varnames,
               f_bathroom = "f_bathroom",
               n_days_since = "n_days_since",
               n_accommodates = "n_accommodates",
               n_beds = "n_beds")

# Need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(ranger::importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}

rf_model_2_var_imp_grouped <- group.importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_2_var_imp_grouped),
                                            imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

ggplot(rf_model_2_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


#########################################################################################
# Partial Dependence Plots -------------------------------------------------------
#########################################################################################

# 1) Number of accommodates
pdp_n_acc <- pdp::partial(rf_model_2, pred.var = "n_accommodates", 
                          pred.grid = distinct_(data_holdout, "n_accommodates"), 
                          train = data_train)

pdp_n_acc %>%
  autoplot( ) +
  geom_point(color='red', size=2) +
  geom_line(color='red', size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
  theme_bw()


# 2) Room type
pdp_n_roomtype <- pdp::partial(rf_model_2, pred.var = "f_room_type", 
                               pred.grid = distinct_(data_holdout, "f_room_type"), 
                               train = data_train)
pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color='red', size=4) +
  ylab("Predicted price") +
  xlab("Room type") +
  scale_y_continuous(limits=c(60,120), breaks=seq(60,120, by=10)) +
  theme_bw()

####
# Subsample performance: RMSE / mean(y) ---------------------------------------
# NOTE  we do this on the holdout set.

# ---- cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))



######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


b <- data_holdout_w_prediction %>%
  filter(f_neighbourhood_cleansed %in% c("Westminster", "Camden",
                                         "Kensington and Chelsea", "Tower Hamlets",
                                         "Hackney", "Newham")) %>%
  group_by(f_neighbourhood_cleansed) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("Apartment", "House")) %>%
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("Neighbourhood", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

result_3


#########################################################################################
#
# PART IV
# HORSERACE: compare with other models -----------------------------------------------
#
#########################################################################################

# OLS with dummies for area
# using model B

set.seed(1234)
system.time({
  ols_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))

# * LASSO
# using extended model w interactions

set.seed(1234)
system.time({
  lasso_model <- train(
    formula(paste0("price ~", paste0(predictors_E, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.01)),
    trControl = train_control
  )
})

lasso_coeffs <- coef(
  lasso_model$finalModel,
  lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `s1`)  # the column has a name "1", to be renamed

lasso_coeffs_non_null <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]

regression_coeffs <- merge(ols_model_coeffs_df, lasso_coeffs_non_null, by = "variable", all=TRUE)

# CART with built-in pruning
set.seed(1234)
system.time({
  cart_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control
  )
})
cart_model
# Showing an alternative for plotting a tree
fancyRpartPlot(cart_model$finalModel, sub = "")

# GBM  -------------------------------------------------------
# See more e.g.:
#   http://uc-r.github.io/gbm_regression
gbm_grid <-  expand.grid(interaction.depth = 5, # complexity of the tree
                         n.trees = 250, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(1234)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model
gbm_model$finalModel
# save( gbm_model , file = 'gbm_model.RData' )
# load(url('https://github.com/regulyagoston/BA21_Coding/blob/main/Class_3/data/gbm_model.RData?raw=true'))


###
# and get prediction rmse and add to next summary table
# ---- compare these models

final_models <-
  list("OLS" = ols_model,
       "LASSO (model w/ interactions)" = lasso_model,
       "CART" = cart_model,
       "Random forest 1: smaller model" = rf_model_1,
       "Random forest 2: extended model" = rf_model_2,
       "GBM"  = gbm_model)

results <- resamples(final_models) %>% summary()
results

# Model selection is carried out on this CV RMSE
result_4 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

result_4



# evaluate preferred model on the holdout set -----------------------------

result_5 <- map(final_models, ~{
  RMSE(predict(.x, newdata = data_holdout), data_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

result_5
