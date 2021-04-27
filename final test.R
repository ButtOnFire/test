if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



head(edx)
anyNA(edx)
summary(edx)

edx %>% 
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.2)

validation %>% 
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.2)

edx %>% group_by(movieId)
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.2)
  
edx <- edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))
head(edx)
anyNA(edx)

movie_per_year <- edx%>% group_by(releaseyear) %>% summarize(n=n()) %>% arrange (desc(n))
movie_per_year %>% ggplot(aes(releaseyear, n))+geom_line()

edx%>% count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(binwidth = 0.2, show.legend = F, color= "Black", aes(fill = cut(n,100))) +
  scale_x_log10()

install.packages("highcharter")
library(ggplot2)
edx %>% group_by(releaseyear) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(releaseyear, rating)) +
  geom_point() +
  theme_hc() + 
  geom_smooth() +
  ggtitle("Release Year vs. Rating")

RMSE <-function(true_rating, predicted_rating){
  sqrt(mean((true_rating - predicted_rating)^2))
}

mu<-mean(edx$rating)
baseline_RMSE <- RMSE(edx$rating, mu)

naive_rmse<- RMSE(validation$rating, mu)
naive_rmse

edx%>% group_by(movieId) %>% filter (n()>1000) %>%
  summarize (avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bin=30, color = "Black", fill="Blue")

movie_means<-edx%>% group_by(movieId)%>%summarize(b_i=mean(rating -mu))
joined<- validation%>% left_join(movie_means, by="movieId")
any(is.na(joined$b_i))

predicted_ratings<- mu+joined$b_i
model2_rmse <- RMSE(validation$rating, predicted_ratings)
model2_rmse

residue <- validation %>% mutate (residue = predicted_ratings - rating, prediction = predicted_ratings) %>% top_n(10, abs(residue))
residue

lambda <- 5
movie_reg_means <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

joined<- validation %>% left_join(movie_reg_means)

predicted_ratings <- mu + joined$b_i
model3_rmse <- RMSE(validation$rating, predicted_ratings)
model3_rmse


lambda <-seq(0,8,0.25)
rmses <- sapply(lambda, function(lambda){
  movie_reg_means <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
  joined<-validation%>%left_join(movie_reg_means)
  predicted_ratings<-mu+joined$b_i
  RMSE(validation$rating, predicted_ratings)
})

plot(lambda, rmses)
lambda[which.min(rmses)]

lambda <- 2.5
movie_reg_means <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

joined<- validation %>% left_join(movie_reg_means)

predicted_ratings <- mu + joined$b_i
model3_rmse <- RMSE(validation$rating, predicted_ratings)
model3_rmse

a <- predicted_ratings
a <- ceiling(a / 0.5) * 0.5
a[a <= 0.5] <- 0.5
a[a >= 5] <- 5
a<-as.factor(a)
summarization = confusionMatrix(a, as.factor(validation$rating))
summarization$overall[1]

lambda <-seq(2.2,2.7,0.1)
rmses <- sapply(lambda, function(lambda){

b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda)) 

b_u<- edx%>% left_join(b_i, by = "movieId") %>% group_by(userId)%>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))

predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(predicted_ratings = mu + b_i +b_u) %>% pull (predicted_ratings)

RMSE(validation$rating, predicted_ratings)
})

min(rmses)




