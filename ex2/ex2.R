library(tidyverse)
library(modelsummary)
library(stringr)



  
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


#####################################################

