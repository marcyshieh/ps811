# Download materials from dataverse
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/W5CV8E&version=1.0

# packages
library("magrittr")
library("tidyverse")
library("haven")
library("here")
library("stargazer")
library("modelsummary")
library("sjlabelled")

# load data for replication portion
here("data")
bork <- read_dta(here("data", "Bork.dta")) # download from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/W5CV8E&version=1.0

View(bork)

# table 1: descriptive stats - mean, deviation, min, max
# senate vote (vote), lack of qualifications (lackqual), ideological distance (eucldist), strong president (strngprs), same party (sameprty)
# https://vincentarelbundock.github.io/modelsummary/articles/datasummary.html
datasummary((`Senate Vote` = vote) +
              (`Lack of Qualifications` = lackqual) +
              (`Ideological Distance` = eucldist) +
              (`Strong President` = strngprs) +
              (`Same Party` = sameprty) ~
              Mean + SD + Min + Max,
            data = bork,
            output = 'markdown')
# see other display options: https://vincentarelbundock.github.io/modelsummary/articles/datasummary.html#save-and-display-1

# table 2: senate voting on supreme court nominees
# time frame: from black to bork
table2_ccs_nominees <- filter(bork, congress > 82 & congress < 101)
table2_ccs_nominees_model <- glm(vote ~ lackqual + eucldist + strngprs + sameprty,
                           family = binomial(link = "probit"), 
                           data = table2_ccs_nominees)
summary(table2_ccs_nominees_model)

# time frame error in the paper: it claims this is the time frame from black to roberts but it seems to be the time frame between black to alito (did not realize they also included alito in their analysis lol)
table2_add_nominees <- glm(vote ~ lackqual + eucldist + strngprs + sameprty,
                 family = binomial(link = "probit"), 
                 data = bork)
summary(table2_add_nominees)

stargazer(table2_ccs_nominees_model, table2_add_nominees,
          covariate.labels = c("Lack of Qualifications", "Ideological Distance",
                               "Strong President",
                               "Same Party"),
          column.labels = c("CCS Nominees", "Additional Nominees"),
          dep.var.caption = "Senate Vote",
          dep.var.labels.include = FALSE,
          type = "text")
# loglik error: notice that the log likelihood for the Additional Nominees are off in the paper

# add alito to the dataset

# so we need the common space score for each senator (dwnom1)
poole <- read_dta(here("data", "Weekly_DW-NOMINATE_31.DTA")) # download from https://legacy.voteview.com/Weekly_Constant_Space_DW-NOMINATE_Scores.htm

# there is leading whitespace in poole$name
# let's fix this so the names in both datasets merge properly
poole$name <- trimws(poole$name)

# rename congress
poole <- rename(poole, congress = cong)

View(poole)

# say you want to look at additional nominees
# extension: from black to alito

# you need to extend the dataset
# build alito votes (alito was confirmed on 1/31/2006)
# https://legacy.voteview.com/senate109.htm

# download all the roll call votes of the 109 congress
congress109 <- read_dta(here("data", "sen109kh.dta")) 

View(congress109)

# identify which column contains the alito votes by downloading the dictionary
# it seems like V368 is the alito vote column
congress109_alito <- congress109 %>%
  select(cong:name, V368) %>%
  filter(name != "BUSH") # filter out the president

# look at the breakdown
table(congress109_alito$V368)

# 1 = YES, 6 = NAY, 0 = not a member
# you want to change 6 to 0
# you want to change 0 to NA (CORZINE retired)
congress109_alito <- congress109_alito %>%
  mutate(
    vote = ifelse(V368 == 6,
                  0,
          ifelse(V368 == 0,
                 NA,
                 V368)
  ))

# check the breakdown again
table(congress109_alito$vote)

# you want to build the alito file to reflect the bork file
# so first, check the variable names

# bork
# "congress"(+)     "statenm"(+)      "name"(+)         "cs1"(+)          "nominee"(-) 
# "vote" (+)         "presprty"(-)     "sameprty"(-)     "strngprs"(-)     "nomid"(-)  
# "regime_dummy"(-) "lackqual"(-)     "csnom"(-)        "eucldist"(-)     "interaction"(-)

names(congress109_alito)
names(bork)

congress109_alito <- rename(congress109_alito, congress = cong)

alito <- congress109_alito %>%
  select(congress, id, state, name, vote)

# select relevant variables in the poole & rosenthal common space scores
poole_select <- poole %>%
  select(congress, idno, statenm, name, dwnom1, party)

# merge the senators' alito votes with the senators' common space scores
alito_poole <- left_join(alito, poole_select, by=c("id" = "idno", "congress", "name"))
names(alito_poole)
alito_poole <- rename(alito_poole, cs1 = dwnom1)
# now you want to build a dataframe with the following information:
# nominee, presprty, sameprty, strngprs, nomid, regime_dummy, lackqual, csnom, eucldist, interaction
alito_poole$nominee <- rep("ALITO") # name of nominee
alito_poole$presprty <- rep(1) # president held party majority in Senate

# whether senator is the same party as the president
alito_poole %>%
  mutate(sameprty = ifelse(party == 200,
                           1,
                           0))

alito_poole$nomid <- rep(0.100) # ideology score of the judicial nominee: https://en.wikipedia.org/wiki/Segal%E2%80%93Cover_score
alito_poole$regime_dummy <- rep(1) # whether the judicial nominee was pre- or post-bork (1 is post-bork, 0 is pre-bork)
alito_poole$lackqual <- rep(1-0.810) # you take how qualified someone is based on newspaper editorials and subtract it by 1 to get their "lack of qualifications" score #https://en.wikipedia.org/wiki/Segal%E2%80%93Cover_score

# you will need to calculate the following (later)
alito_poole$csnom <- NA # this is going to be the Segal-Cover score for each nominee "transformed" as the Common Space/Poole & Rosenthal/DW-Nominate (it goes by many names) score

alito_poole$eucldist <- NA # this is going to be the Euclidean distance between the senator and the nominee

alito_poole$interaction <-NA # this is the interaction between ideological distance and qualifications
  
# bind the dataset to the bork dataset
bork_alito_poole <- full_join(bork, alito_poole)
names(bork_alito_poole)
# the last two columns are just for binding datasets before
# you can just keep them there if they don't interfere with your analysis going forward

# from reading the article, you know that you need the common space scores for each president as well
# the president is coded with a congressional district (cd) number of 0, and a state number (state) of 99
poole_filter <- poole %>%
  filter(cd == 0 & state == 99) %>%
  select(congress, name, dwnom1)

# rename name
poole_filter <- rename(poole_filter, pres = name)

# rename dwnom1
poole_filter <- rename(poole_filter, cspres = dwnom1)

# join together the cspres scores
bork_alito_poole_full <- full_join(bork_alito_poole, poole_filter)

alito_poole_coeffs <- lm(cspres ~ nomid, data = bork_alito_poole_full)
alito_poole_coeffs$coefficients
# result:
# (Intercept)       nomid 
# 0.5816398  -0.9868674 

# scale the common space scores for alito
bork_alito_poole_full$cnom_alito <- 0.5816398 - 0.9868674*bork_alito_poole_full$nomid

# do the calculations for ideological distance
bork_alito_poole_full$eucldist_alito <- (bork_alito_poole_full$cnom_alito - bork_alito_poole_full$cs1)^2

# do the calculations for the interaction between ideological distance and qualifications
bork_alito_poole_full$interaction_alito <- bork_alito_poole_full$eucldist_alito*alito_poole$lackqual

View(bork_alito_poole_full)

# now you have the full dataset so you can run the analysis again from black to alito (and not just black to roberts)