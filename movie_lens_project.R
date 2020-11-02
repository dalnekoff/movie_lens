#HarvardX: PH125.9x Data Science Capstone
#MovieLens R Code
#ddalnekoff 
#movieLens


# Create edx set, validation set
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#download and load data into R in a tidy format as a dataframe including title, genres, rating
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))), col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),title = as.character(title),genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")



# Validation set will be 10% of MovieLens data
# if using R 3.5 or earlier, use `set.seed(1)` instead

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

#create the respective training and validation datasets
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%  semi_join(edx, by = "movieId") %>% semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#doing some cleanup to remove objects which were just used to prepare and stage the data
rm(dl, ratings, movies, test_index, temp, movielens, removed)



#let's do some quick discovery on our data
#check the first few rows of training set
head(edx)

#how many distinct movies? users? genres (as the list)?
edx_count <- count(edx)
movie_count <- n_distinct(edx$movieId)
user_count <- n_distinct(edx$userId)
mean(edx$rating)
sd(edx$rating)

hist(edx$rating)

genres <- edx %>% group_by(genres) %>% summarise(n())
head(genres)


#confirm code runs without any of this commented section here as it was related to the exploratory questions
#library(stringr)
#genre_list <- edx$genres
#length(genre_list)
#sum(str_detect(genre_list, "Drama"))
#sum(str_detect(genre_list, "Comedy"))
#sum(str_detect(genre_list, "Thriller"))
#sum(str_detect(genre_list, "Romance"))
#head(edx)

#ratings_by_movie <- edx %>% group_by(title) %>% summarise(n())
#head(ratings_by_movie)
#colnames(ratings_by_movie) <- c("title","ratings")
#ratings_by_movie [order(ratings_by_movie$ratings, decreasing = TRUE),]

#ratings <- edx %>% group_by(rating) %>% summarise(count = n()) %>% arrange(desc(count))

#WE WILL DEVELOP THE MODEL USING THE EDX SET GENERATED TO THIS POINT
#WE WILL THEN GO BACK AND TEST USING THE VALIDATION SET
#THE GOAL is to minimize RMSE

#Let's have a function ready to to support testing our model: 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



#pre-submission, confirm if we need this:!!!
library(lubridate)
#install.packages(anytime)

#let's look at the first few rows to start to explore and think about this data
head(edx)

#dates can be interesting to work with because there are different features within a given datetime (month, day of the week, hour), so we will generate a set of features with the date components to help facilitate the use.


#Dates: Step 1 - Use lubridate and convert "timestamp" to an easy to work with datetime:
edx <- edx %>% transform(review_datetime = lubridate::as_datetime(timestamp))

#Dates: We also need to add this in our validation set
validation <- validation %>% transform(review_datetime = lubridate::as_datetime(timestamp))

#let's do a quick check to confirm our new feature "review_datetime"
head(edx)
day(edx$review_datetime[1])
class(edx$review_datetime)

#Dates: Step 2 - generate additional feature columns for year, month, day, hour of review
edx <- edx %>% 
  mutate(
   review_year = year(review_datetime),
   review_month = month(review_datetime),
   review_day = wday(review_datetime),
   review_mday = day(review_datetime),
   revied_mday = day(review_datetime),
   review_hour = hour(review_datetime)
         )


#Dates: We also need to add these to our validation set
validation <- validation %>% 
  mutate(
    review_year = year(review_datetime),
    review_month = month(review_datetime),
    review_day = wday(review_datetime),
    review_mday = day(review_datetime),
    review_hour = hour(review_datetime)
  )


#Dates: Step 3 - From a review of the data, we see that the year the movie was released is part of the feature Title, for example : "Boomerang (1992)" was released in 1992.  This could be another important consideration - does the age of the movie at the time of review have an effect on the rating?  For example, would some types of movies that the reviewer perceived as 'old' get a lower rating?  


#define a regex pattern to help match/extract the "(YYYY)" section of the title.
#this didn't work
#note: remove this section before submission to avoid confusion
#re <- "\\(([^()]+)\\)"
#edx <- edx %>% mutate( movie_year = as.numeric(gsub(re, "\\1", str_extract_all(title, re))))
#edx %>% filter(is.na(movie_year))



#We can use a function to pull the right 'x' characters here from the Title string

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#get the YYYY from the title and convert it to a number which would represent the Year the movie was released
edx <- edx %>% 
     mutate( year_extract = 
               as.numeric( 
                 str_sub( 
                   substrRight(title,5)
                   , end = -2)
                 )  
             )


#to support validation at the end of the algorithm, we will also add this to the validation set
validation <- validation %>% 
  mutate( year_extract = 
            as.numeric( 
              str_sub( 
                substrRight(title,5)
                , end = -2)
            )  
  )

#Now that we have both the year of review and the year of release we can calculate the age of the movie when it was reviewed
#Note: This isn't 100% perfect, as we don't have the actual release date, only a year.  For the purposes of simplicity and to manage the scope of the exercise we will do a simple comparison of the two years.

edx <- edx %>% 
  mutate( age_at_review = edx$review_year - edx$year_extract)

#We can do a quick review to confirm this looks like what we expect
edx[1:10,] %>% 
  select(title, year_extract, review_year, age_at_review)

#For the purpose of curiosity and data discovery and analysis, what was the oldest film at the time of review?
edx[which.max(edx$age_at_review),] %>%
  select(title, rating, year_extract, review_year, age_at_review)

#let's also add age at review to the validation set
validation <- validation %>% 
  mutate( age_at_review = validation$review_year - validation$year_extract)


#At this point we have a working dataset which has been cleaned to facilitate solving our data science challenge.

#Begin by looking at just the average review (3.51)
mu <- mean(edx$rating)
mu


#Let's look at our baseline loss using just the straight average across the population (1.06)
naive_rmse <- RMSE(validation$rating, mu)
naive_rmse

#We can improve the accuracy of our prediction by incorporating the effect, or bias of a number of features present in our data.  As a first pass, we can do some quick summaries to look at how the mean score can vary by these features.

#movie factor
#do we see some variation across ratings by movie?
#b_i = movie factor relative to all movies average ratings of 3.5
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu), mean_rating = mean(rating))

plot(movie_avgs$movieId, movie_avgs$mean_rating)
smoothScatter(movie_avgs$movieId, movie_avgs$mean_rating)

#let's calculate the accuracy improvement by just adjusting for the movie effect
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  pull(b_i)

#movie effect adjusted rating 
RMSE(predicted_ratings, validation$rating)
#.9439


#user effect
#do we see some variation across ratings by user?
#b_u = user effect relative to all users
user_avgs <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu), mean_rating = mean(rating))

#now let's see how our accuracy improves by adjusting for only the user effect
predicted_ratings <- mu + validation %>% 
  left_join(user_avgs, by = 'userId') %>% 
  pull(b_u)

#user effect adjusted rating
RMSE(predicted_ratings, validation$rating)
#0.9783


#now let's combine both of these to see how much of an improvement we can observe
#first, re-do user effect to already account for the movie factor
user_avgs <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i), mean_rating = mean(rating))

plot(user_avgs$userId, user_avgs$mean_rating)
mean(user_avgs$mean_rating)

#then, re-run our prediction
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  mutate(pred = mu + b_i + b_u) %>% 
  pull(pred)

#accuracy when adjusted for user and movie effects
RMSE(predicted_ratings, validation$rating)
#0.8653



#now add in effects for the other features in a similar pattern (rating year, month, day, age at rating, movie year)


#do we see some variation across ratings by movie year?
ratings_by_year <- edx %>% 
  group_by( movie_year = edx$year_extract) %>% 
  summarise( mean_rating = mean(rating), count = n())

#this plot indicates yes, we see a general decline in ratings from ~ the mid 1960s to late 1990s and then around 2000 ratings start to increase
plot(ratings_by_year$movie_year, ratings_by_year$mean_rating)

#movie year effect
movie_year <- edx %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  group_by(year_extract) %>% 
  summarize(b_y = mean(rating - mu - b_i - b_u ))


#add movie year to test set
validation <- validation %>% 
  mutate( year_extract = 
            as.numeric( 
              str_sub( 
                substrRight(title,5)
                , end = -2)
            )  
  )

#let's calculate our new prediction incorporating all three effects (movie, user, movie year)
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  mutate(pred = mu_hat + b_i + b_u + b_y) %>% 
  pull(pred)


RMSE(predicted_ratings, validation$rating)
#.8650




#do we see some variation across ratings by review year?
review_year <- edx %>% 
  group_by(review_year = edx$review_year) %>% 
  summarise( mean_rating = mean(rating), count = n())

#yes, there does appear to be a bit of a cyclical pattern with scores fluctuating across years
plot(review_year$review_year, review_year$mean_rating)

#add in review year effect
year_group <- edx %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(movie_year, by = 'year_extract') %>%
  left_join(review_year, by = 'review_year') %>%
  group_by(review_year) %>% 
  summarize(b_ry = mean(rating - mu - b_i - b_u - b_y ))



#now examine our prediction including the year effect
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_ry) %>% 
  pull(pred)


RMSE(predicted_ratings, validation$rating)
#.8649285



#do we see some variation across ratings by review month?

ratings_by_reviewMonth <- edx %>% 
  group_by(review_month) %>% 
  summarise( mean_rating = mean(rating))

plot(ratings_by_reviewMonth$review_month, ratings_by_reviewMonth$mean_rating)

#add in review month effect to our model

month_group <- edx %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(movie_year, by = 'year_extract') %>%
  left_join(year_group, by = 'review_year') %>%
  left_join(ratings_by_reviewMonth, by = 'review_month') %>%
  group_by(review_month) %>% 
  summarize(b_rm = mean(rating - mu - b_i - b_u - b_y - b_ry ))

#now examine our prediction including the month effect
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_ry + b_rm) %>% 
  pull(pred)


RMSE(predicted_ratings, validation$rating)
#.8649263



#do we see an effect by the day # of the month?
#working from here
ratings_by_reviewMDay <- edx %>% 
  group_by(review_mday) %>% 
  summarise( mean_rating = mean(rating))

plot(ratings_by_reviewMDay$review_mday, ratings_by_reviewMDay$mean_rating)

#mDay effect

mDay_group <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  group_by(review_mday) %>% 
  summarize(b_rd = mean(rating - mu - b_i - b_u - b_y - b_ry - b_rm ))


#now examine our prediction including the mDay effect
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  left_join(mDay_group, by = 'review_mday') %>%
  mutate(pred = mu_hat + b_i + b_u + b_y + b_ry + b_rm + b_rd) %>% 
  pull(pred)

RMSE(predicted_ratings, validation$rating)
#.8649136



#do we see some variation across ratings by review day of week (1-7)?
ratings_by_reviewDay <- edx %>% 
  group_by(review_day) %>% 
  summarise( mean_rating = mean(rating))

#Reviews are definitely lower in the middle of the week.
plot(ratings_by_reviewDay$review_day, ratings_by_reviewDay$mean_rating)

#let's determine the day of week factor
day_group <- edx %>%
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  left_join(mDay_group, by = 'review_mday') %>%
  group_by(review_day) %>%
  summarize(b_rwd = mean(rating - mu - b_i - b_u - b_y - b_ry - b_rm - b_rd ))

#now examine our prediction including the day of week effect
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  left_join(mDay_group, by = 'review_mday') %>%
  left_join(day_group, by = 'review_day') %>%
  mutate(pred = mu_hat + b_i + b_u + b_y + b_ry + b_rm + b_rd + b_rwd) %>% 
  pull(pred)

RMSE(predicted_ratings, validation$rating)
#.8649125




#do we see some variation across ratings by the age at the time of review?  
ratings_by_reviewAge <- edx %>% 
  group_by(age_at_review = edx$age_at_review) %>% 
  summarise( mean_rating = mean(rating), n())

#interesting observation here, ratings seem to improve over time until ~60 years
plot(ratings_by_reviewAge$age_at_review, ratings_by_reviewAge$mean_rating)



#add in age at review effect

review_age <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  left_join(mDay_group, by = 'review_mday') %>%
  left_join(day_group, by = 'review_day') %>%
  group_by(age_at_review) %>%
  summarize(b_a = mean(rating - mu - b_i - b_u - b_y - b_ry - b_rm - b_rd - b_rwd ))

#examine the impact of age at review effect
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  left_join(mDay_group, by = 'review_mday') %>%
  left_join(day_group, by = 'review_day') %>%
  left_join(review_age, by = 'age_at_review') %>%
  mutate(pred = mu_hat + b_i + b_u + b_y + b_ry + b_rm + b_rd + b_rwd + b_a) %>% 
  pull(pred)


RMSE(predicted_ratings, validation$rating)
#.8646669



#do we see a pattern with regard to genre?
genre_reviews <- edx  %>% 
   group_by(genres) %>% 
  summarise( mean_rating = mean(rating), n())

#there is definitely variability by genres in the mean rating
plot(genre_reviews$mean_rating)

#determine the genres effect

genre_group <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  left_join(mDay_group, by = 'review_mday') %>%
  left_join(day_group, by = 'review_day') %>%
  left_join(review_age, by = 'age_at_review') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u - b_y - b_ry - b_rm - b_rd - b_rwd - b_a ))


#lets review the prediction loss after adding in the genre factor

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  left_join(mDay_group, by = 'review_mday') %>%
  left_join(day_group, by = 'review_day') %>%
  left_join(review_age, by = 'age_at_review') %>%
  left_join(genre_group, by = 'genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_y + b_ry + b_rm + b_rd + b_rwd + b_a + b_g) %>% 
  pull(pred)


RMSE(predicted_ratings, validation$rating)
#.8643152

#is there an effect based on the time of day (hour) of the review?
ratings_by_hour <- edx %>% 
  group_by(review_hour) %>% 
  summarise( mean_rating = mean(rating), n())

#average rating by hour
plot(ratings_by_hour$review_hour, ratings_by_hour$mean_rating)


time_group <- edx  %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  left_join(mDay_group, by = 'review_mday') %>%
  left_join(day_group, by = 'review_day') %>%
  left_join(review_age, by = 'age_at_review') %>%
  left_join(genre_group, by = 'genres') %>%
  group_by(review_hour) %>%
  summarize(b_h = mean(rating - mu - b_i - b_u - b_y - b_ry - b_rm - b_rd - b_rwd - b_a - b_g ))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year, by = 'year_extract') %>% 
  left_join(year_group, by = 'review_year') %>%
  left_join(month_group, by = 'review_month') %>%
  left_join(mDay_group, by = 'review_mday') %>%
  left_join(day_group, by = 'review_day') %>%
  left_join(review_age, by = 'age_at_review') %>%
  left_join(genre_group, by = 'genres') %>%
  left_join(time_group, by = 'review_hour') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_ry + b_rm + b_rd + b_rwd + b_a + b_g + b_h) %>% 
  pull(pred)

RMSE(predicted_ratings, validation$rating)
#.8643083

#add in review year factor


#In reviewing the data, I asked the question, is there a genre factor which we could attribute by splitting up all the distinct entries of genres
#I would like to revisit this in the future, but didn't want to delay completion of the project here
#split genres field into columns
#install.packages("tidyr")
#library(tidyr)
#install.packages("splitstackshape")
#library(splitstackshape)
#movie_genre_list <- edx %>% select( movieId, review, genres)
#movie_genre_list_unpacked <- movie_genre_list %>% separate_rows(genres, sep = "\\|", convert = FALSE)
#genre_list <- movie_genre_list %>% group_by(genre = movie_genre_list$genre) %>% summarise( mean_rating = mean(rating), n())
#plot(genre_list$genre, genre_list$mean_rating)




#Let's now use regularization to help improve our accuracy:
#We see there is significant variation across the number of reviews by each of these features (users, movies, etc), and this should be accounted for so that a movie with a very small number of reviews or a user with a small number of extreme scores does not have an overly weighted effect of the model.


#use cross validation to tune lambda
#the approach here is as follows:
#we will test for lambda values from 1-10, with a .25 increment


#for reference, this was our optimized lambda result:
#lambdas <- 5.1

#I also re-ran tuning with a slightly smaller 'step' to see if we could increase the accuracy by using lambdas <- seq(4, 6, .05)
#lambdas[which.min(rmses)] = 5.1

lambdas <- seq(1, 10, .25)


#the general approach I am using is bootstrap in order to avoid overfitting while optimizing the value for lambda
#we will :
#we will divide our training into 10 randomly split segments 
#each of these will be further divided into a train and test set 

#for each lambda and each subset of the population:
#train on edx_train
#validate on edx_test

#NOTE: THIS CODE WILL TAKE A LONG time to run (I let this run for an afternoon, overnight and into the next day on my machine)

#split the training data into 10 folds
bootstrapindex <- createFolds( y = edx$rating, k = 10, list = TRUE, returnTrain = FALSE)

#this is one single fold, 10% of the population
bootstrapindex[[1]]
#to see the top 10 rows bootstrapindex[[1]][1:100]

#this creates an index on a split of the data to split it into a training and test population
test_sets <- seq(1:10)
counter <- 0
lambda_run <- 0


cross_fold_validation <- sapply(test_sets, function(a){
  print(paste("working on test set",counter))
  counter <<- counter + 1
  
  #this creates an index to split each fold into a train (90% of the fold) and test set (10% of the fold)
  
  bootstrap_split <- createDataPartition(y = edx[bootstrapindex[[a]],]$rating, times = 1, p = .1, list = FALSE)

  #using the index created, take that fold out of the edx data and build the respective training and testing datasets.
  train_set <- edx[ bootstrapindex[[a]][-bootstrap_split],]
  validation_set <- edx[ bootstrapindex[[a]][bootstrap_split],]
  
    #now run this dataset through our algorithm for each possible value of lambda 
  rmses <- sapply(lambdas, function(l){
    print(paste("working on lambda run ",lambda_run))
    lambda_run <<- lambda_run + 1

  mu_train <- mean(train_set$rating)
  
  #movie effect with regularization
  movie_avgs_reg <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_train)/(n()+l))
  
  #user effect with regularization
  user_avgs_reg <- train_set %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_train)/(n()+l))
  #clean na
  user_avgs_reg[is.na(user_avgs_reg)] <- 0
  
  #movie year effect with regularization
  movie_year_reg <- train_set %>% 
    left_join(user_avgs_reg, by = 'userId') %>% 
    left_join(movie_avgs_reg, by = 'movieId') %>% 
    group_by(year_extract) %>% 
    summarize(b_y = sum(rating - b_i - b_u - mu_train )/(n()+l))
  #clean na
  movie_year_reg[is.na(movie_year_reg)] <- 0
  
  
  #age at review effect with regularization
  review_age_reg <- train_set %>% 
    left_join(movie_avgs_reg, by = 'movieId') %>% 
    left_join(user_avgs_reg, by = 'userId') %>% 
    left_join(movie_year_reg, by = 'year_extract') %>% 
    group_by(age_at_review) %>% 
    summarise(b_a = sum(rating - b_i - b_u - b_y - mu_train)/(n()+l))
  #clean na
  review_age_reg[is.na(review_age_reg)] <- 0
  
  #genres effect with regularization
  genre_group_reg <- train_set  %>% 
    left_join(movie_avgs_reg, by = 'movieId') %>% 
    left_join(user_avgs_reg, by = 'userId') %>% 
    left_join(movie_year_reg, by = 'year_extract') %>% 
    left_join(review_age_reg, by = 'age_at_review') %>% 
    group_by(genres) %>% 
    summarize(b_g = sum(rating -  b_i - b_u - b_y - b_a - mu_train)/(n()+l))
  #clean na
  genre_group_reg[is.na(genre_group_reg)] <- 0

  #review day effect with regularization
  day_group_reg <- train_set  %>% 
    left_join(movie_avgs_reg, by = 'movieId') %>% 
    left_join(user_avgs_reg, by = 'userId') %>% 
    left_join(movie_year_reg, by = 'year_extract') %>% 
    left_join(genre_group_reg, by = 'genres') %>% 
    left_join(review_age_reg, by = 'age_at_review') %>% 
    group_by(review_day) %>% 
    summarize(b_dy = sum(rating - b_i - b_u - b_y - b_a - b_g - mu_train)/(n()+l))
  #clean na
  day_group_reg[is.na(day_group_reg)] <- 0
  
  #review month effect with regularization
  month_group_reg <- train_set %>% 
    left_join(movie_avgs_reg, by = 'movieId') %>% 
    left_join(user_avgs_reg, by = 'userId') %>% 
    left_join(movie_year_reg, by = 'year_extract') %>% 
    left_join(genre_group_reg, by = 'genres') %>% 
    left_join(review_age_reg, by = 'age_at_review') %>% 
    left_join(day_group_reg, by = 'review_day') %>%
    group_by(review_month) %>% 
    summarize(b_m = sum(rating - b_i - b_u - b_y - b_a - b_g - b_dy - mu_train)/(n()+l))
  #clean na
  month_group_reg[is.na(month_group_reg)] <- 0
  
  
  #review hour effect with regularization
  time_group_reg <- train_set  %>% 
    left_join(movie_avgs_reg, by = 'movieId') %>% 
    left_join(user_avgs_reg, by = 'userId') %>% 
    left_join(movie_year_reg, by = 'year_extract') %>% 
    left_join(genre_group_reg, by = 'genres') %>% 
    left_join(review_age_reg, by = 'age_at_review') %>% 
    left_join(day_group_reg, by = 'review_day') %>%
    left_join(month_group_reg, by = 'review_month') %>% 
    group_by(review_hour) %>%
    summarize(b_h = sum(rating - b_i - b_u - b_y - b_a - b_g - b_dy - b_m - mu_train )/(n()+l))
  #clean na
  time_group_reg[is.na(time_group_reg)] <- 0
  
  #review year effect with regularization
  year_group_reg <- train_set  %>% 
    left_join(movie_avgs_reg, by = 'movieId') %>% 
    left_join(user_avgs_reg, by = 'userId') %>% 
    left_join(movie_year_reg, by = 'year_extract') %>% 
    left_join(genre_group_reg, by = 'genres') %>% 
    left_join(review_age_reg, by = 'age_at_review') %>% 
    left_join(day_group_reg, by = 'review_day') %>%
    left_join(month_group_reg, by = 'review_month') %>% 
    left_join(time_group_reg, by = 'review_hour') %>%
    group_by(review_year) %>%
    summarize(b_ry = sum(rating - b_i - b_u - b_y - b_a - b_g - b_dy - b_m - b_h - mu_train )/(n()+l))
  #clean na
  year_group_reg[is.na(year_group_reg)] <- 0
  
  #day of month effect with regularization
  mDay_group_reg <- train_set %>%
    left_join(movie_avgs_reg, by = 'movieId') %>% 
    left_join(user_avgs_reg, by = 'userId') %>% 
    left_join(movie_year_reg, by = 'year_extract') %>% 
    left_join(genre_group_reg, by = 'genres') %>% 
    left_join(review_age_reg, by = 'age_at_review') %>% 
    left_join(day_group_reg, by = 'review_day') %>%
    left_join(month_group_reg, by = 'review_month') %>% 
    left_join(time_group_reg, by = 'review_hour') %>%
    left_join(year_group_reg, by = 'review_year') %>%
    group_by(review_mday) %>%
    summarize(b_mdy = sum(rating - b_i - b_u - b_y - b_a - b_g - b_dy - b_m - b_h - b_ry - mu_train )/(n()+l))
  #clean na
  mDay_group_reg[is.na(mDay_group_reg)] <- 0
  
  predicted_ratings_reg <- validation_set %>% 
    left_join(movie_avgs_reg, by = 'movieId') %>% 
    left_join(movie_year_reg, by = 'year_extract') %>% 
    left_join(genre_group_reg, by = 'genres') %>% 
    left_join(user_avgs_reg, by = 'userId') %>% 
    left_join(review_age_reg, by = 'age_at_review') %>% 
    left_join(day_group_reg, by = 'review_day') %>% 
    left_join(month_group_reg, by = 'review_month') %>% 
    left_join(time_group_reg, by = 'review_hour') %>%
    left_join(year_group_reg, by = 'review_year') %>%
    left_join(mDay_group_reg, by = 'review_mday')
  
  #clean nas
  predicted_ratings_reg[is.na(predicted_ratings_reg)] <- 0
  
  #add prediction
  predicted_ratings_reg <- predicted_ratings_reg %>%
    mutate(pred = mu_train +  b_i + b_u + b_y + b_a + b_g + b_dy + b_m +  b_h + b_ry + b_mdy) %>% 
    pull(pred)

  #return the RMSEs from each lambda value we tested for this fold
  print(paste("lambda:",l,"RMSE:",RMSE(predicted_ratings_reg, validation_set$rating)))
  return(RMSE(predicted_ratings_reg, validation_set$rating))
})

#this stores the lambda value from each of the 10 folds which minimized the RMSE
  print(paste("min lambda:",lambdas[which.min(rmses)]))
  return(lambdas[which.min(rmses)])

})

#check values in cross_fold_validation and take the mean; this will be our lambda to then use in building the final model; which we will compare against the original validation set (not the test set from edx)

cross_fold_validation
final_lambda <- mean(cross_fold_validation)
final_lambda
#4.975

  
  #now that we have our tuned lambda, we will re-run our model with regularization on the original training set, edx
#movie effect with regularization

movie_avgs_reg_final <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+final_lambda))

#user effect with regularization
user_avgs_reg_final <- edx %>%
  left_join(movie_avgs_reg_final, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+final_lambda))
#clean na
#

user_avgs_reg_final[is.na(user_avgs_reg_final)] <- 0

#movie year effect with regularization
movie_year_reg_final <- edx %>% 
  left_join(user_avgs_reg_final, by = 'userId') %>% 
  left_join(movie_avgs_reg_final, by = 'movieId') %>% 
  group_by(year_extract) %>% 
  summarize(b_y = sum(rating - b_i - b_u - mu )/(n()+final_lambda))
#clean na
#movie_year_reg_final[is.na(movie_year_reg_final)] <- 0


#age at review effect with regularization
review_age_reg_final <- edx %>% 
  left_join(movie_avgs_reg_final, by = 'movieId') %>% 
  left_join(user_avgs_reg_final, by = 'userId') %>% 
  left_join(movie_year_reg_final, by = 'year_extract') %>% 
  group_by(age_at_review) %>% 
  summarise(b_a = sum(rating - b_i - b_u - b_y - mu)/(n()+final_lambda))
#clean na
#review_age_reg_final[is.na(review_age_reg_final)] <- 0

#genres effect with regularization
genre_group_reg_final <- edx  %>% 
  left_join(movie_avgs_reg_final, by = 'movieId') %>% 
  left_join(user_avgs_reg_final, by = 'userId') %>% 
  left_join(movie_year_reg_final, by = 'year_extract') %>% 
  left_join(review_age_reg_final, by = 'age_at_review') %>% 
  group_by(genres) %>% 
  summarize(b_g = sum(rating -  b_i - b_u - b_y - b_a - mu)/(n()+final_lambda))
#clean na
#genre_group_reg_final[is.na(genre_group_reg_final)] <- 0

#review day effect with regularization
day_group_reg_final <- edx  %>% 
  left_join(movie_avgs_reg_final, by = 'movieId') %>% 
  left_join(user_avgs_reg_final, by = 'userId') %>% 
  left_join(movie_year_reg_final, by = 'year_extract') %>% 
  left_join(genre_group_reg_final, by = 'genres') %>% 
  left_join(review_age_reg_final, by = 'age_at_review') %>% 
  group_by(review_day) %>% 
  summarize(b_dy = sum(rating - b_i - b_u - b_y - b_a - b_g - mu)/(n()+final_lambda))
#clean na
#day_group_reg_final[is.na(day_group_reg_final)] <- 0

#review month effect with regularization
month_group_reg_final <- edx %>% 
  left_join(movie_avgs_reg_final, by = 'movieId') %>% 
  left_join(user_avgs_reg_final, by = 'userId') %>% 
  left_join(movie_year_reg_final, by = 'year_extract') %>% 
  left_join(genre_group_reg_final, by = 'genres') %>% 
  left_join(review_age_reg_final, by = 'age_at_review') %>% 
  left_join(day_group_reg_final, by = 'review_day') %>%
  group_by(review_month) %>% 
  summarize(b_m = sum(rating - b_i - b_u - b_y - b_a - b_g - b_dy - mu)/(n()+final_lambda))
#clean na
#month_group_reg_final[is.na(month_group_reg_final)] <- 0


#review hour effect with regularization
time_group_reg_final <- edx  %>% 
  left_join(movie_avgs_reg_final, by = 'movieId') %>% 
  left_join(user_avgs_reg_final, by = 'userId') %>% 
  left_join(movie_year_reg_final, by = 'year_extract') %>% 
  left_join(genre_group_reg_final, by = 'genres') %>% 
  left_join(review_age_reg_final, by = 'age_at_review') %>% 
  left_join(day_group_reg_final, by = 'review_day') %>%
  left_join(month_group_reg_final, by = 'review_month') %>% 
  group_by(review_hour) %>%
  summarize(b_h = sum(rating - b_i - b_u - b_y - b_a - b_g - b_dy - b_m - mu )/(n()+final_lambda))
#clean na
#time_group_reg_final[is.na(time_group_reg_final)] <- 0

#review year effect with regularization
year_group_reg_final <- edx  %>% 
  left_join(movie_avgs_reg_final, by = 'movieId') %>% 
  left_join(user_avgs_reg_final, by = 'userId') %>% 
  left_join(movie_year_reg_final, by = 'year_extract') %>% 
  left_join(genre_group_reg_final, by = 'genres') %>% 
  left_join(review_age_reg_final, by = 'age_at_review') %>% 
  left_join(day_group_reg_final, by = 'review_day') %>%
  left_join(month_group_reg_final, by = 'review_month') %>% 
  left_join(time_group_reg_final, by = 'review_hour') %>%
  group_by(review_year) %>%
  summarize(b_ry = sum(rating - b_i - b_u - b_y - b_a - b_g - b_dy - b_m - b_h - mu )/(n()+final_lambda))
#clean na
#year_group_reg_final[is.na(year_group_reg_final)] <- 0

#day of month effect with regularization
mDay_group_reg_final <- edx %>%
  left_join(movie_avgs_reg_final, by = 'movieId') %>% 
  left_join(user_avgs_reg_final, by = 'userId') %>% 
  left_join(movie_year_reg_final, by = 'year_extract') %>% 
  left_join(genre_group_reg_final, by = 'genres') %>% 
  left_join(review_age_reg_final, by = 'age_at_review') %>% 
  left_join(day_group_reg_final, by = 'review_day') %>%
  left_join(month_group_reg_final, by = 'review_month') %>% 
  left_join(time_group_reg_final, by = 'review_hour') %>%
  left_join(year_group_reg_final, by = 'review_year') %>%
  group_by(review_mday) %>%
  summarize(b_mdy = sum(rating - b_i - b_u - b_y - b_a - b_g - b_dy - b_m - b_h - b_ry - mu )/(n()+final_lambda))
#clean na
#mDay_group_reg_final[is.na(mDay_group_reg_final)] <- 0

predicted_ratings_reg_final <- validation %>% 
  left_join(movie_avgs_reg_final, by = 'movieId') %>% 
  left_join(movie_year_reg_final, by = 'year_extract') %>% 
  left_join(genre_group_reg_final, by = 'genres') %>% 
  left_join(user_avgs_reg_final, by = 'userId') %>% 
  left_join(review_age_reg_final, by = 'age_at_review') %>% 
  left_join(day_group_reg_final, by = 'review_day') %>% 
  left_join(month_group_reg_final, by = 'review_month') %>% 
  left_join(time_group_reg_final, by = 'review_hour') %>%
  left_join(year_group_reg_final, by = 'review_year') %>%
  left_join(mDay_group_reg_final, by = 'review_mday')

#clean nas
predicted_ratings_reg_final[is.na(predicted_ratings_reg_final)] <- 0

#add prediction
predicted_ratings_reg_final <- predicted_ratings_reg_final %>%
  mutate(pred = mu +  b_i + b_u + b_y + b_a + b_g + b_dy + b_m +  b_h + b_ry + b_mdy)

#let's take a quick look at the final preds and mean values of our effects
head(predicted_ratings_reg_final)

mean(predicted_ratings_reg_final$b_i)
mean(predicted_ratings_reg_final$b_y)
mean(predicted_ratings_reg_final$b_g)
mean(predicted_ratings_reg_final$b_u)
mean(predicted_ratings_reg_final$b_a)
mean(predicted_ratings_reg_final$b_dy)
mean(predicted_ratings_reg_final$b_m)
mean(predicted_ratings_reg_final$b_h)
mean(predicted_ratings_reg_final$b_ry)
mean(predicted_ratings_reg_final$b_mdy)

final_preds <- predicted_ratings_reg_final %>% pull(pred)


#re-ran the model with regularization with the lambda value from above
RMSE(final_preds, validation$rating)
#this is our final model accuracy
.8638


#min(rmses)
#.8638128
#this satisfies the score we are looking to be below for the scope of this machine learning exercise

head(predicted_ratings_reg_final)

