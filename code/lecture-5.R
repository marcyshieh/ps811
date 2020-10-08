# use R as a calculator
2 + 3
3 - 1
12 * 12
50 / 2

# R searches next line if incomplete
50 /
  2

# order of operations
5^(10/5)

# other commands
exp(1)
log(1)
pi
cos(pi)

# object creation
nine <- 8 + 1

nine

dog <- 8 + 1

dog

hello <- "hi"
hello

hello <- "hi, nice to meet you, what is your name"
hello
the_answer <- 8 + 1
the_answer
nine <- 50 + 4
nine # the numeric value does not have to be nine

# create vectors
vector_example <- c(5, 6, 7, 8, 9, 10, 100)

vector_example

results <- 2 * vector_example
results

vector_example * results

# matrix multiplication
vector_example %*% results

# matrix in functions
mean(vector_example)
median(vector_example)
mode(vector_example)
sd(vector_example) # standard deviation
sum(vector_example)

# Loops

for (n in 1:10) {
  print(n)
}

# Using loops in a dataset

df <- data.frame(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df

# create a container
output <- vector("double", ncol(df))  # 1. output
# double is in reference to a "double precision vector," which means that the variables has numbers with more numbers to the right of the decimal point (thus, more precise)

# create the actual function
# single vs. double brackets
# single brackets: variables names show up
# double brackets: variable names do NOT show up

for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}

# look at the output of the function
output

# do this with real data
data(iris)
View(iris)

# create a container
# your results will be numeric but you want to make sure R knows that
columns_loop <- numeric()

# create the function
for (row in 1:nrow(iris)) {
  # you want to put length of each "row" (i.e., column) into the container
  columns_loop[row] <- length(iris[row, ])
}

# look at the output of the function
columns_loop
# you will find that for every row, there are 5 columns, which checks out

# a better example because the above example almost doesn't make sense lol (sorry)
columns_loop <- numeric()

for (row in 1:nrow(iris)) {
  # for each row in the iris dataset, you want to find how many columns there are
  # once you find how many columns there are you, want to divide that number by 2
  # then you want to take that number and populate the container
  # the container will populate that number with the (number of columns/2)
  columns_loop[row] <- length(iris[row, ])/2
}
columns_loop

# of course these examples are weird, but functions make more sense when you are doing things with them for practical reasons which brings me to the apply() functions

#### BEGIN APPLY TUTORIAL ####
# https://www.guru99.com/r-apply-sapply-tapply.html

# apply: take values in dataframes and matrices output them into vectors
# you are APPLYING a function on rows or columns
# create fake data
m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
# X = data you are working with
# MARGIN = 2 (columns; if MARGIN = 1, then rows)
# FUN = sum (you want to sum all the columns (because MARGIN = 2))
a_m1 <- apply(X = m1, MARGIN = 2, FUN = sum)
a_m1

# lapply: takes values in list objects and output them as list objects
# you are applying the function to a list (apply -> l, thus lapply)

# create a list of movies
movies2000 <- c("GLADIATOR", "TRAFFIC", "ALMOST FAMOUS", "MEMENTO")
# you want to apply the function to make all the movie names lowercase (thus "tolower")
movies_lower <-lapply(X = movies2000, FUN = tolower)
# you also don't have to put "X =" and "FUN =" but you must put it in order
movies_lower <-lapply(movies2000, tolower)
str(movies_lower)

# sapply: takes values in list, matrix, or vector and output them as a vector or matrix
# if you use lappy
lmn_m1 <- lapply(m1, min)
lmn_m1

# compare to sapply
smn_m1 <- sapply(m1, min)
smn_m1

# tapply: applies a function to a subset for each factor level in a vector
# instead of applying the function to an entire dataset, you can apply the function to only a variable within the dataset

# example: use the iris data
data(iris)

# say you want to know the median sepal width for each species in the iris dataset
# notice that unlike the other apply() commands, you are applying the function to groups within the dataset, not to the entire dataset
tapply(iris$Sepal.Width, iris$Species, median)

# write function to find average
avg <- function(x) {  
  ( min(x) + max(x) ) / 2}
# insert function that you created into a sapply
fm1 <- sapply(m1, avg)
fm1
#### ENND APPLY TUTORIAL ####

# vectorizing
# a vector is a sequence of elements with the same type
columns <- apply(X = iris, MARGIN = 2, FUN = length)

# lapply()
# turn it into a list

iris_list <- as.list(iris)

# check out the list
iris_list
# check out the first five observations of the list
lapply(iris_list, head)

# create your own list!
my_list <- list(colors = c("red", "green", "blue"),
     food = c("pizza", "sushi", "burger"),
     drink = c("coffee", "tea", "juice"))

my_list

# look for the second group in your list
# one bracket tells you it's "food"
my_list[2]
# double bracket allows you to refer to the items directly
my_list[[2]]
# say you want to access "pizza"
# try doing this
my_list[2][1]
# it doesn't work!!! :-(
# the reason why it doesn't work is because you aren't accessing the list directly
# so you need to access it directly
my_list[[2]][[1]]

# index list
# str()
# structure of R objects
# yincludes type, the number of columns, the number of rows per column, the number of factors per column
str(iris_list)

typeof(iris$Sepal.Length)

# only look at the first column
str(iris_list[1])

# tapply()
# apply functions to groups of data
tapply(
  X = iris$Sepal.Length, 
  INDEX = list(iris$Species), 
  FUN = mean, 
  na.rm = TRUE  
)

# consider using tidyverse
# we will talk more about tidyverse & its differences from base R next week...
# but you can do tapply() stuff in tidyverse more easily
library(tidyverse)

iris %>%
  group_by(Species) %>%
  summarize(
    mean_nom = mean(Sepal.Length, na.rm = TRUE)
  ) 

# scoped verbs

# how to use if()

iris %>%
  mutate_if(
    .predicate = is.numeric,
    .funs = length
  )

# select variables with numeric values

select_if(iris, is.numeric)

# how to use at()

iris %>%
  mutate_at(
    .vars = vars(starts_with("Sepal")),
    .funs = function(x) x / 100
  ) %>% 
  select(starts_with("Sepal"))

# define your own functions (make your own!)

make_half <- function(x) {
  return(x / 2)
}

make_half(88)

# anonymous function
# sapply()
# create a function but don't give it a name
# it's like giving birth to a baby but not giving it a referent (I guess?)
sapply(88, function(x) x / 2)

# lambda function
select_if(
  iris,
  .predicate = ~ sum(is.numeric(.)) > 0
)

# more complex lambda function
# divide the numbers by 100
iris %>%
  mutate_at(
    .vars = vars(Sepal.Length, Sepal.Width),
    .funs = ~ . / 100
  )

# applying more than one function
summarize_all(
  iris, 
  .funs = list(uniques = ~ n_distinct(.), 
               obj_type = class)
)

# nesting dataframes
iris %>%
  group_by(Species) %>%
  nest() 

# group by species
# turn it into an object
nested_iris <- iris %>%
  group_by(Species) %>%
  nest() %>%
  print()

# "Does the width of the petal affect the length of the petal?"
nested_models <- nested_iris %>%
  mutate(
    model = map(
      .x = data, 
      .f = ~ lm(Petal.Length ~ Petal.Width, data = .x)
    )
  ) %>%
  print()

# crack open the nested data frame
nested_models$Species
nested_models$data
nested_models$model

# extract the coefficients
nested_coefs <- nested_models %>%
  mutate(coefs = map(model, coefficients)) %>%
  print()

nested_coefs$coefs

# unnest
coefs <- nested_coefs %>%
  unnest(coefs) %>%
  print()

# you cannot unnest non-dataframe objects but you can still extract them
nested_coefs$model
