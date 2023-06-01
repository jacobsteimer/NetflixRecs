library(tidyverse)
library(caret)
options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Training and Test Sets
test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
Training <- edx[-test_index2,]
Temp2 <- edx[test_index2,]
Testing <- Temp2 %>% 
  semi_join(Training, by = "movieId") %>%
  semi_join(Training, by = "userId")
removed2 <- anti_join(Temp2, Testing)
Training <- rbind(Training, removed2)

#Playing around starts here
#Accounting for movie effects...
y <- select(Training, movieId, userId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating) 
rnames <- y$userId
y <- as.matrix(y[,-1])
rownames(y) <- rnames
mu <- mean(y, na.rm = TRUE)
b_i <- colMeans(y - mu, na.rm = TRUE)
fit_movies <- data.frame(movieId = as.integer(colnames(y)), 
                         mu = mu, b_i = b_i)
left_join(Testing, fit_movies, by = "movieId") |> 
  mutate(pred = mu + b_i) |> 
  summarize(rmse = RMSE(rating, pred))
rmse
0.9437122

# y is huge. let's make it smaller?
set.seed(1, sample.kind="Rounding")
SmallIndex <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
Smaller <- edx[SmallIndex,]
#this didn't work for some reason