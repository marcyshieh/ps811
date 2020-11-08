# load package
library("here")
library("haven")

# here are some useful data analysis packages
library("Rmisc")
library("magrittr")
library("tidyverse")
library("ggplot2")
library("broom")
library("MASS")
library("estimatr")
library("modelsummary")

# Descriptive statistics

# Imagine you went on an awesome data collecting expedition and you ended up with the ANES 2016 dataset (lucky you!). Very cool, good job.
# your RAs have created a nifty page for you to read all about the data you alone have collected: https://electionstudies.org/data-center/2016-time-series-study/

# setup folders and directories
here("code")
here("data")

# read stata data in R
anes2016 <- read_dta(here("data", "anes_timeseries_2016.dta"))

# DV: V162034: POST: Did R vote for President
# IV: V162256: PRE: R's interest in politics
# control: V161019: PRE: party of registration

# these names are hard to remember, so you can change them
anes2016$post.vote <- anes2016$V162034
anes2016$pre.interest <- anes2016$V162256
anes2016$pre.party <- anes2016$V161019
anes2016$pre.registered <- anes2016$V161011a
anes2016$pre.gender <- anes2016$V161342

# Non-regression analysis
# since we are not grouping post.vote, we just put "1" instead of an actual group
group.CI(post.vote ~ 1, data = anes2016)
# now grouping vote for president...
groupCI_anes <- group.CI(post.vote ~ pre.gender, data = anes2016)

# this is a dataframe so you can turn it into a graph pretty easily
ggplot(groupCI_anes, aes(x = as.factor(pre.gender), y = post.vote.mean)) + 
  geom_pointrange(aes(ymin = post.vote.lower, ymax = post.vote.upper)) +
  theme(panel.grid.minor = element_blank())

# data summary
# modelsummary() includes a powerful suite of data summary tools: https://vincentarelbundock.github.io/modelsummary/articles/datasummary.html
# it basically has all the summary tools you tend to see in published articles
# look at each variable by category
datasummary_skim(anes2016, type="categorical")
# there's a lot in the anes2016 data...so probably not the best demo
# let's look at the iris data...
datasummary_skim(iris)
# if you want to look at correlations between variables
datasummary_correlation(iris)

# Regression

# THE most amazing data analysis resource on the web:
# https://stats.idre.ucla.edu/other/dae/
# If you ever want to know whether you can use a particular regression or how to use a particular regression, go to the UCLA stats page!!
# If you don't know how to find what you are looking for, just type "[name of regression] UCLA R" into Google.
# If the options provided to you on the UCLA stats page are confusing, you should use it as a stepping stone to search for something that is easier to understand or suit your needs better.

# run a simple OLS regression
# you typically use an OLS when their dependent variable is continuous & unbounded
# there are some arguments that you can use OLS for binary dependent variables...search the internet and you'll find TONS
# some OLS assumptions: https://www.albert.io/blog/key-assumptions-of-ols-econometrics-review/
# you will learn more about this in 813, so I'm ont going to bog you down with these details
# this might be a good way to check yourself: https://www.theanalysisfactor.com/dependent-variables-never-meet-normality/

# for the sake of creating examples, I might be violating some assumptions and not thinking about the variables carefully...don't @ me
# but really, you should think about this carefully in your own work and if you do want to use something that's not the norm, you should make sure that you justify your case (one great way to do this is cite other people's work that you're following, or create some sort of validation check)

ols <-
  anes2016 %>%
  lm(post.vote ~ pre.interest + pre.party, data = .)

# look at the regression table by putting the regression object into summary()
summary(ols)

# look at diagnostic plots for residuals, fitted values, Cook’s distance, and leverage
# it is important to look at diagnostic plots so you can figure out if there are data points that might influence your results
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
par(opar)

# use base R to look at the data
# you can see where these respondents are from, etc. and see why their observations are weird
# for example, i want to see if they are all from the same state...
anes2016[c(1884, 1918, 2464, 2458), "V161010d"]
# you might want to look at other demographic information based on what you can find in the data using the codebook

# Robust standard errors are useful in social sciences where you don't know the structure of the variations
# you may want to use robust standard errors
# https://www.rdocumentation.org/packages/estimatr/versions/0.26.0/topics/lm_robust
# UCLA suggests rlm() but I think lm_robust is easier to use
# so to get unnbiased standard errors for OLS...
lm_robust(post.vote ~ pre.interest + pre.party, data = anes2016)

# GLM: logit regression
# this is what you *should* use for binary outcomes (0 or 1)
# you need to determine the "family" of distribution your outcome variable belongs in: https://www.statmethods.net/advstats/glm.html

# this is how using base R histograms to "check" the distribution of your outcome variable could come in useful
hist(anes2016$post.vote)
# say you just want to keep the respondents who did and did not vote
anes2016 <-
  anes2016 %>%
  filter(post.vote == 1 | post.vote == 2)

# let's look at the histogram again
hist(anes2016$post.vote)
# wahhh but i want it as a binary variable :-(
# well... you can use ifelse()
# i personally like to create new variables if i modify an existing variable
# 1 = yes voted for president, 2 = no did not vote for president
anes2016 <-
  anes2016 %>%
  mutate(
    post.vote.binary = ifelse(post.vote == 1, 1, 0))
# now it is 1 = voted, 0 = did not vote

hist(anes2016$post.vote.binary)

anes2016 <-
  anes2016 %>%
  mutate(
    pre.female = ifelse(pre.gender == 2, 1, 0))

# DV: voting for president
# IV: being female
logit <- glm(post.vote.binary ~ pre.female, family = binomial, data = anes2016)
# look at the summary regression table
summary(logit)
# for more info on how to interpret log odds, please check out this page: https://stats.idre.ucla.edu/r/dae/logit-regression/
# i will show you a tool that makes GLMs easier to interpret in a sec...

# how do i transfer my regression table into R markdown??

# welcome to the stargazer package: https://www.jakeruss.com/cheatsheets/stargazer/
# install.packages("stargazer")
library(stargazer)
# first off, stargazer makes your tables look NICE
stargazer(logit, type = "text",
          # you can customize the labels
          title            = "Effect of gender on voting for president",
          covariate.labels = c("Female"),
          dep.var.labels   = "Voted for president")
# how do you put this in R Markdown?

# ```{r, results='asis'}
# you want it to print as a latex because R Markdown has built-in tex options
# tex is nicer for formatting and typesetting, but R markdown is more convenient (I think!)
stargazer(logit, type='latex',
          title            = "Effect of gender on voting for president",
          covariate.labels = c("Female"),
          dep.var.labels   = "Voted for president",
          # you can cite the package in your References section, but it might be frustrating to see a citation every time you run stargazer...but you can just set it to header = FALSE and everything is okay
          header = FALSE)
# ```

# create a second regression with some controls
# V165510: age group
# V165511: education
# in reality, you should clean the variables like i did with the vote and gender variables (in fact, this is part of your homework assignment lol)
logit2 <- glm(post.vote.binary ~ pre.female + V165510 + V165511, family = binomial, data = anes2016)
summary(logit2)

# personally I like seeing the table in R first before i put it in R markdown, but you can abide by your own workflow
stargazer(logit, logit2, type='text',
          title            = "Effect of gender on voting for president",
          covariate.labels = c("Female", "Age", "Education"),
          dep.var.labels   = "Voted for president",
          header = FALSE)

# how to make interpreting log odds easier: https://cimentadaj.github.io/blog/2016-08-22-producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/

# I like tables but there are people out there who do not like tables. You can use coefplots or the tools in the modelsummary package.

# you can check out
# install.packages("modelsummary")
# library("modelsummary")
# read more about customizing it: https://vincentarelbundock.github.io/modelsummary/articles/modelplot.html
modelplot(ols)
# change variable names on coefplot
vars <- c('pre.interest' = 'Interest',
        'pre.party' = 'Party')
# ta-da!
modelplot(ols, coef_map = vars)
