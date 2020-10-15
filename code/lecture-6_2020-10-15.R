# install the here package
# install.packages("here")

# load the here library
library("here")
# this is an equivalent to setwd()

# check out where "here" is
here()
# this is an equivalent to getwd()

# as you have probably guessed by now, the "#"" denotes comments and R leaves them alone when you run chunks of code

movie_metadata <- read.csv(here("data/movie_metadata.csv"))

# base R
# extract first 100 rows
movie_metadata_100 <- movie_metadata[1:100, ]

# look at existing variables in the dataset
# you can identify the variables by name
movie_metadata_100[, c("director_name", "movie_title")] 

# or you can identify the variables by their column index/number
movie_metadata_100[, c(2, 12)]

# you want to get the sum of facebook likes for the first 3 actors listed in the movie
movie_metadata_100$main_actors_fb_popularity <-
  movie_metadata_100$actor_1_facebook_likes +
  movie_metadata_100$actor_2_facebook_likes +
  movie_metadata_100$actor_3_facebook_likes

# view the dataset then scroll to the last column on the right to see the column you have just created!
View(movie_metadata_100)

# you want to extract the observations where there is a IMDB score of 5+ and 2 number of faces in the poster
subset(movie_metadata_100, imdb_score > 5 &
         facenumber_in_poster==2)

# descending order of movie title followed by ascending order of budget
movie_metadata_100[order(rev(movie_metadata_100$movie_title),
                         movie_metadata_100$budget) , ]

# Create a dataframe with mean and standard deviation information
# na.rm = TRUE removes all the NAs from the calculations
# in this case, if you include the NAs, it will yield a result of NA
# e.g., mean(movie_metadata_100$budget) # not what you want!

data.frame(budget.mean = mean(movie_metadata_100$budget, na.rm = TRUE),
           budget.sd = sd(movie_metadata_100$budget, na.rm = TRUE),
           gross.mean = mean(movie_metadata_100$gross, na.rm = TRUE),
           gross.sd = sd(movie_metadata_100$gross, na.rm = TRUE))

# Using aggregate
aggregate(formula = cbind(budget, gross) ~ country + genres, 
          data = movie_metadata_100, 
          FUN = function(x){
            c(mean = mean(x), sd = sd(x))
          })

# tidyverse

movie_metadata_100tidy <- movie_metadata %>%
  top_n(100)


# you can identify the variables by name
select(movie_metadata_100tidy, director_name, movie_title)

# or you can identify the variables by their column index/number
select(movie_metadata_100tidy, 2, 12)

# you want to get the sum of facebook likes for the first 3 actors listed in the movie

movie_metadata_100tidy <- mutate(movie_metadata_100tidy,
                                 main_actors_fb_popularity = actor_1_facebook_likes +
                                   actor_2_facebook_likes + actor_3_facebook_likes)


# you want to extract the observations where there is a IMDB score of 5+ and 2 number of faces in the poster
filter(movie_metadata_100tidy, imdb_score > 5 & facenumber_in_poster==2)

# descending order of movie title followed by ascending order of budget
arrange(movie_metadata_100tidy, desc(movie_title), budget) 

# Create a dataframe with mean and standard deviation information
summarise(movie_metadata_100tidy,
          budget.mean = mean(budget, na.rm = TRUE),
          budget.sd = sd(budget, na.rm = TRUE),
          gross.mean = mean(gross, na.rm = TRUE),
          gross.sd = sd(gross, na.rm = TRUE))

movie_metadata_100tidy %>% 
  group_by(country, genres) %>% 
  summarise(budget.mean = mean(budget),
            budget.sd = sd(budget),
            gross.mean = mean(gross),
            gross.sd = sd(gross)) %>% 
  ungroup()

# ungroup() removes any grouping changes you make from analyses beyond this section
# you should have created a brand new variable!
View(movie_metadata_100tidy)
