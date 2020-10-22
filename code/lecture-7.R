# packages
library("here")
library("haven")
library("magrittr")
library("tidyverse")
library("tidyr")
library("dplyr")

# setup folders and directories
here("data")
here("code")

# read stata data in R
scotus <- read_dta(here("_ps811", "data", "SCDB_2020_01_justiceCentered_Citation.dta"))

# how does my data LOOK??
scotus
# that was a lot
head(scotus)
# that's a little better

# bottom six?
tail(scotus)

# i want to see more than six, like eight?
head(scotus, n = 8)
tail(scotus, n = 8)

# you can use the View() command
# this can be slow if it's a large dataset
View(scotus)

# if the file is, for some reason, NOT a dataframe, you can always force it become a dataframe:

as.data.frame(scotus)

# this doesn't do anything because it is already a dataframe

# if you are ever struggling with a function, go ahead and do ?function_name
# the help guide should load in the help pane

?as.data.frame

#####

# SIDEBAR: create your own dataframes, convert from tibble to dataframe

# take everything from the streaming service column
streaming <- c("Netflix", "Hulu", "Amazon Prime") 

# take everything from the approve column
approve <- c(50, 30, 15) 

# take everything from the disapprove column
disapprove <- c(50, 70, 85) 

# create variables from existing variables
difference <- approve-disapprove

# create your dataframe
streaming <- data_frame(streaming, approve, disapprove, difference) 
streaming

# this is a tibble
streaming_df <- as.data.frame(streaming_dataframe)

#####

# number of rows
nrow(scotus)

# number of columns
ncol(scotus)

# literally my favorite function: names()
# super helpful for other data manipulation tasks
names(scotus)

# you cannot simply reference the variable
justice
# Error: object 'justice' not found

# you have to refer to the dataset THEN variable
scotus$justice

# download or reference the codebook whenever possible
# sometimes variables are simply a string of numbers & letters (e.g., the ANES dataset has weird codes for their variables that can only be identified by looking them up in the codebook)
# thankfully, the scotus database is better than that, but you might still want to clarify a few things so it is easier for you (and your collaborators!) to work with
# the variables are case sensitive so you might want to change the cases, etc.

# rename

# rename(dataset, 
#        new_name = old_name,
#        new_name2 = old_name2)

anes <- rename(scotus, 
               case.id = caseId,
               docket.id = docketId, 
               case.issues.id = caseIssuesId, 
               vote.id = voteId)

# YOU CAN ALSO PIPE THIS (remember lesson 5??)
scotus <- scotus %>%
              rename(case.id = caseId,
               docket.id = docketId, 
               case.issues.id = caseIssuesId, 
               vote.id = voteId)
# this can make some of the code faster
# you will be asked to pipe on your homework
# refer back to lesson 5

# this overwrites everything, though
# you will no longer have the OLD variable name around anymore because you have changed it to a NEW variable name

# but what if you don't want to overwrite anything??

# let's look at decision directions
table(scotus$decisionDirection)

# we know from the code book that:
# 1: conservative
# 2: liberal
# 3: unspecifiable

# say you want to create a dummy variable named "liberal" that has all liberal cases coded "1" and all non-liberal cases coded "0"
# AND you want to create a dummy variable named "conservative that has all conservative cases coded "1" and all non-conservative cases coded "0"

# in pseudological speak, you want something like:
# new_variable <- ifelse(condition, if_true, if_false)
# liberal <- ifelse(decisionDirection==2, 1, 0)

scotus <- 
  mutate(scotus, 
         liberal = ifelse(decisionDirection == 2,
                          1,
                          0), 
         conservative = ifelse(decisionDirection == 1,
                               1,
                               0))

# maybe you want to do create a variable that identifies the decision dates by decade
# here's your chance to work with date values, which many of you will have to deal with

# take a look at the variable
summary(scotus$year)

# turn the dateDecision variable into a date variable with the date structure you see in the summary() results
# extract the year and put it in a variable called year
scotus$year <- as.numeric(format(as.Date(scotus$dateDecision, "%Y-%m-%d"), '%Y'))
summary(scotus$year)

# using the year variable, create the decade variable
scotus <- mutate(scotus,
                 decade = ifelse(
                   year %in% 1940:1949, 1940, NA),
                 decade = ifelse(
                   year %in% 1950:1959, 1950, decade),
                 decade = ifelse(
                   year %in% 1960:1969, 1960, decade),
                 decade = ifelse(
                   year %in% 1970:1979, 1970, decade),
                 decade = ifelse(
                   year %in% 1980:1989, 1980, decade),
                 decade = ifelse(
                   year %in% 1990:1999, 1990, decade),
                 decade = ifelse(
                   year %in% 2000:2009, 2000, decade),
                 decade = ifelse(
                   year %in% 2010:2019, 2010, decade),
                 decade = ifelse(
                   year %in% 2020:2029, 2020, decade))

# check out the number of votes per decade
table(scotus$decade)

# or create new variables: time trends

scotus <- mutate(scotus,
                 time_trends = 2020 - year)

# What if you want to do other logical operations with the variable?
# write this in R markdown so i don't have to go through converting it from the R script > markdown

# case_when function with decisionDirection

scotus <- mutate(scotus,
               decisionDirection = case_when(
                 decisionDirection == 1 ~ conservative,
                 decisionDirection == 2 ~ liberal,
                 decisionDirection == 3 ~ unspecified))

# factors and strings -- write about this in r markdown

# select columns

# say your question is whether the decade a case was decided influences whether it is liberal or not, so you might only care about two variables
# the select() structure is: select(dataset, var1, var2)
# var1, var2, etc. are the variables you want to select from the dataset
# think of it as highlighting these variables and deleting all the rest

select(scotus, decade, liberal)

# turn it into an object so you can work with it later
scotus_select <- select(scotus, decade, liberal)

# say you only want caseId and all background variables (consecutively ordered)
select(scotus, caseId, caseName:lcDispositionDirection)

# select caseid and any variable that contains "case" in the name
select(scotus, caseId, contains("case"))

# drop "docketId" variable and keep all other variables
select(scotus, -docketId)

# more specifically, you can do:
select(scotus, -docketId, matches("."))
# matches(".") keeps all remaining variables
# though not having also keeps all remaining variables


# filtering

# you know how you filter in excel?
# this is the R version

# say you only want to filter to decisions from before 1990
filter(scotus, year < 1990)

# say you only want to filter to decisions from 1990
filter(scotus, year == 1990)

# say you want to filter to decisions written by Ruth Bader Ginsburg in 2000
filter(scotus, year == 2000, justiceName == "RBGinsburg")

# summarize

# think of this as a more advanced summary() command
scotus %>%
  summarise(mean = mean(liberal, na.rm = TRUE), n = n())

scotus %>%
  group_by(decade) %>%
  summarise(mean = mean(liberal, na.rm = TRUE), n = n())

scotus %>%
  group_by(decade) %>%
  summarise(qs = quantile(liberal, na.rm = TRUE, c(0.25, 0.75)), prob = c(0.25, 0.75))

# sort data

# remember this object we created earlier??
scotus_select <- select(scotus, decade, liberal)

# sort decade by ascending order
arrange(scotus_select, decade, liberal)

# sort decade by descending order
arrange(scotus_select, desc(decade), liberal)

# there's an order to this
# arrange(dataset, var1, var2)
# this means you sort by var1, then by var2
# for example

sort_ex_df <- data_frame(x = c(1, 2, 3, 3, 5, 6), 
                y = c("A", "C", "D", "E", "B", "F"))

arrange(sort_ex_df, x, y)
arrange(sort_ex_df, y, x)

# fun part: merging!!

# you might have two datasets that you would like to merge
# for example, you might have grades for a few students from their English and math teachers

english <- data_frame(studentID = c(990055, 889765, 189245, 346789, 534098, 132938, 789012), 
                     grade = c(90, 85, 60, 75, 67, 93, 82))

math <- data_frame(studentID = c(990055, 889765, 189245, 346789, 534098, 345890), 
                   grade = c(80, 90, 50, 85, 95, 66))

# in base R
# english grade is "grade.x" and math grade is "grade.y"
merge(english, math, by="studentID")

# basic merge() function only merge variables that exist in BOTH datasets
# if you want to keep all variables in x or y, you can do it but it's WAY easier t remember in the dplyr package

# imitate merge()
inner_join(english, math, by = "studentID")

# join all, keep all variables, even if NA
full_join(english, math, by="studentID")

# keep all students in x
left_join(english, math, by = "studentID")

# keep all students in y
right_join(english, math, by = "studentID")

# figure out what doesn't match
# shows studentID that is in english but not in math
anti_join(english, math, by = "studentID")
# shows studentID that is in math but not in english
anti_join(math, english, by = "studentID")

# sometimes you just want to see a table of what's going on with a variable
table(scotus$decade)
table(scotus$liberal)
table(scotus$decisionDirection)
# turn it into a proportional table
prop.table(table(scotus$decisionDirection))
# round to 2 decimal points
round(prop.table(table(scotus$decisionDirection)), 2)

# you can even make a crosstab
table(scotus$decade, scotus$decisionDirection)
# and a prop crosstab
prop.table(table(scotus$decade, scotus$decisionDirection))
# and with rounding!
round(prop.table(table(scotus$decade, scotus$decisionDirection)), 2)

scotus_cross_example <- table(scotus$decade, scotus$decisionDirection)
# margin = 1 (by each row)
# margin = 2 (by each column)

# say i want to know (liberal decision in 1940s)/(all liberal decisions in every decade)
scotus_cross_prop_example_row <- prop.table(scotus_cross_example, margin = 1)

# say i want to know (liberal decision in 1940s)/(all decisions in the 1940s)
scotus_cross_prop_example_col <- prop.table(tab, margin = 2)

# say you want to count the number of cases per decade per decision type
# and you want it in a dataframe
count(scotus, decade, decisionDirection)

# maybe you want to turn this into an object
scotus_count <- count(scotus, decade, decisionDirection)

# group
# you want to see the proportion of type_of_decision per decade
# so you sum up ALL the decisions per decade
# divide the number of decision types per decade by the number of ALL decisions in decade
# then round them to 3 decimal places
mutate(group_by(scotus_count, decade), 
       n_in_decade = sum(n, na.rm = TRUE),
       p = n / n_in_decade,
       p = round(p, 3))

# let's create a dataset
scotus_df_example <- select(scotus, term, issueArea, decisionDirection)

# currently our decade variable is what you call a wide data
head(scotus_df_example)
scotus_df_example <- as.data.frame(scotus_df_example)

# reshape data (transpose data-ish)
scotus_df_example <- select(scotus, term, liberal, conservative)
scotus_df_example <- filter(scotus_df_example, term %in% c(2017, 2018, 2019))

scotus_df_example <- summarize(group_by(scotus_df_example, term), 
               liberal = mean(liberal, na.rm = TRUE),
               conservative = mean(conservative, na.rm = TRUE))

scotus_df_example

# gather()
scotus_df_example_gather <- gather(scotus_df_example, key = ideology, value = average, liberal, conservative)
scotus_df_example_gather

# spread()
spread(scotus_df_example_gather, key = ideology, value = average) 