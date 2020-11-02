# load packages
library(dplyr)
library(here)
library(tidyverse)
library(haven)

# folder locations
here()
here("data")

# Problem 1

Justice <- c("Clarence Thomas", "Ruth Bader Ginsburg",
             "Stephen Breyer", "John Robers", "Samuel Alito",
             "Sonia Sotomayor", "Elena Kagan", "Neil Gorsuch",
             "Brett Kavanaugh")

State <- c("GA","NY","MA","MD","NJ","NY","MA","CO","MD")

Position <- c("Associate Justice", "Associate Justice",
              "Associate Justice", "Chief Justice",
              "Associate Justice","Associate Justice",
              "Associate Justice","Associate Justice",
              "Associate Justice")

Replacing <- c("Thurgood Marshall", "Byron White",
               "Harry Blackmun", "William Rehnquist",
               "Sandra Day O'Connor", "David Souter",
               "John Paul Stevens", "Antonin Scalia",
               "Anthony Kennedy")

Year <- c(1991, 1993, 1994, 2005, 2006, 2009, 2010, 2017, 2018)

`Senate Confirmation Vote` <- c("52-48", "96-3", "87-9", "78-22",
                                "58-42", "68-31", "63-37",
                                "54-45", "50-48")

`Nominated by` <- c("George H.W. Bush", "Bill Clinton", 
                  "Bill Clinton", "George W. Bush",
                  "George W. Bush", "Barack Obama",
                  "Barack Obama", "Donald Trump", 
                  "Donald Trump")

SCJustices <- data.frame(Justice, State, Position, Replacing,
                         Year, `Senate Confirmation Vote`,
                         `Nominated by`)


# Problem 2
# load MQ scores dataset
justices <- read.csv(here("data", "justices.csv"))

# Problem 3
# load justice votes data
scotus <- read_dta(here("data", "SCDB_2020_01_justiceCentered_Citation.dta"))

# check variable names to see if they can be merged
names(scotus)
names(justices)

# check values
joined_justices <- full_join(scotus, justices, by=c("justiceName", "term"))
# check for repeats
table(joined_justices$justiceName)

# Problem 4
joined_justices_MQ <- filter(joined_justices, !is.na(post_mn))

# Problem 5
MQ_by_term <- joined_justices_MQ %>%
  group_by(term) %>%
  summarise(mean = mean(post_mn, na.rm = TRUE))

print(MQ_by_term, n = nrow(MQ_by_term))


# Problem 6
# 1 = conservative
# 2 = liberal
# 3 = unspecified
# You want to convert them into 1, -1, and 0
joined_justices_MQ <- mutate(joined_justices_MQ,
                          decisionDirection = case_when(
                            decisionDirection == 1 ~ 1,
                            decisionDirection == 2 ~ -1, 
                            decisionDirection == 3 ~ 0))


decision_by_term <- joined_justices_MQ %>%
  group_by(term) %>%
  summarise(mean = mean(decisionDirection, na.rm = TRUE))

print(decision_by_term, n = nrow(decision_by_term))

# Problem 7
mq_decision_compare <- inner_join(MQ_by_term, decision_by_term,
                                  by="term")


mq_decision_compare_diff <- mq_decision_compare %>%
  group_by(term) %>%
  summarise(difference = mean.y - mean.x)

# average difference is small...you can see this from looking at the mean or from a plot
mean(mq_decision_compare_diff$difference, na.rm = TRUE)
plot(mq_decision_compare_diff$term, mq_decision_compare_diff$difference)