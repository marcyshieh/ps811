# base R graphs
# if you need a quick & dirty way to check how things look

# load packages

library("here")
library("readxl") # read excel data in R
library("countrycode")

# setup folders and directories
here("data")
here("code")

# load your data
# read excel data in R: coup data
# http://www.systemicpeace.org/inscrdata.html
coups_polity <- read_excel(here("data", "CSPCoupsListv2018.xls"))
# read csv file in R: national trade data
# https://correlatesofwar.org/data-sets/bilateral-trade
power_cow <- read.csv(here("data", "NMC_v4_0.csv"))

coups_polity$p4c <- countrycode(coups_polity$scode, origin = "p4c", destination = "cown")

power_cow$cown <- countrycode(power_cow$ccode, origin = "cown", destination = "cown")

coups_power <- merge(coups_polity, power_cow, id="cown")

coups_power$country <- countrycode(coups_power$cown, origin = "cown", destination = "country.name")

# view the dataset
View(coups_power)

# look at the variable names
names(coups_power)

## Scatter plots
# http://www.sthda.com/english/wiki/scatter-plots-r-base-graphs
# Do more imports lead to more coups?

x <- coups_power$upop
y <- coups_power$tpop

# pch: plot symbols (http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r)
# frame = TRUE (create a box around the graph)
plot(x, y, main = "Effect of urban population on total population",
     xlab = "urban population", ylab = "total population",
     pch = 19, frame = TRUE)
# add regression line
abline(lm(y ~ x, data = coups_power), col = "blue")

## Box plots
# Box plot of one variable: success of coups
boxplot(coups_power$success)

## Bar plots
barplot(coups_power$success)
### well...that doesn't look that great...
### how you should do it instead...
### number of success coups
barplot(table(coups_power$success))
### number of successful coups per year
barplot(table(coups_power$success, coups_power$year))

## Line plots
plot(coups_power$year, coups_power$success, type = "l", lty = 1)

## Histogram and density plots
hist(coups_power$success, breaks = "Sturges")

## ggplot2
## best tutorials:
# http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
# https://mikedecr.github.io/courses/811/811-04-graphics/

## Load packages

library("magrittr")
library("tidyverse")
library("ggplot2")
library("scales")

## Load dataset
power_cow <- read.csv(here("data", "NMC_v4_0.csv"))
View(power_cow)

## Components

# data: the data source that you are using to grab variables from to put on the graph

# aesthetics: how your graph should look like (e.g., colors)

# geoms: how you want to plot your graph (e.g, line, points, bargraph)

# scale: how you want to present your graph (e.g., by color, which axes)

# coordinates: what kind of plane you are plotting (for the purpose of not being confusing, we are going to use the x and y coordinates)

# facets: split up your graph into different categories

# theme: change font size, font color, etc.

## CREATE plot

# milex: military expenditures
ggplot(data = power_cow, aes(x = year, y = milex))

## POPULATE plot

### Scatter/dot plot
### aes: what is the x and what is the y?
ggplot(data = power_cow, aes(x = milper, y = milex)) +
  geom_point()

### Fit a line
ggplot(data = power_cow, aes(x = milper, y = milex)) +
  geom_point() +
  geom_smooth(method="lm")

# turn ggplot() into an object
g <- ggplot(power_cow, aes(x = milper, y = milex)) + geom_point() + geom_smooth(method="lm")

# you can add stuff to the ggplot object "g", such as labels
g + labs(title="Military Personnel and Expenditures", subtitle="From power COW dataset", y="Expenditures", x="Personnel", caption="Military")

# change color/size of points
ggplot(power_cow, aes(x = milper, y = milex)) + 
  # Set color and size for points
  # here are some colors (starts from page 2): http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  geom_point(col="steelblue", size=3) + 
  geom_smooth(method="lm", col="firebrick") +  # change the color of line
  labs(title="Military Personnel and Expenditures", subtitle="From power COW dataset", y="Expenditures", x="Personnel", caption="Military")

# change colors to reflect category in another variable
gg <- ggplot(power_cow, aes(x = milper, y = milex)) + 
  geom_point(aes(col=year), size=3) + 
  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) +
  labs(title="Military Personnel and Expenditures", subtitle="From power COW dataset", y="Expenditures", x="Personnel", caption="Military")
gg

# notice how the year is a gradient
# you may want to change it into factors

gg <- ggplot(power_cow, aes(x = milper, y = milex)) + 
  geom_point(aes(col=factor(year)), size=3) + 
  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) +
  labs(title="Military Personnel and Expenditures", subtitle="From power COW dataset", y="Expenditures", x="Personnel", caption="Military") + theme(legend.position = "none") 
gg

# ahhh there are TOO MANY YEARS...graph is hard to read
# instead, filter it to the last five years...
# you can do this using your data manipulation skills :)

power_cow2 <- dplyr::filter(power_cow, year == 1995:2000)
power_cow2

gg2 <- ggplot(power_cow2, aes(x = milper, y = milex)) + 
  geom_point(aes(col=factor(year)), size=3) +  # Set color to vary based on state categories.
  # if you don't want to show the confidence intervals shadow, then do se = FALSE
  geom_smooth(method="lm", col="firebrick", size=2) +
  labs(title="Military Personnel and Expenditures", subtitle="From power COW dataset", y="Expenditures", x="Personnel", caption="Military")
gg2
# OR just gg2 works too :)

# change color schemes
library(RColorBrewer)
gg2 + scale_colour_brewer(palette = "Set1")
# different palettes:
# check out all the palettes in RColorBrewer: http://applied-r.com/rcolorbrewer-palettes/#:~:text=RColorBrewer%20is%20an%20R%20packages,data%2C%20dark%20for%20high%20data
# there are other palette packages that you can find on the webz

# people with colorblindness have a difficult time reading graphs with too many colors/too many similar colors (https://jfly.uni-koeln.de/color/)
# these palettes are supposedly colorblind-friendly: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
# you should try to use black and white as much as possible just because your published paper will likely be in black and white
# colors can be fun for presentations, but not always useful otherwise

# bar graph time
ggplot(power_cow2, aes(x=factor(year), y=milex, label=milex)) + 
  # you want to create a bar graph this time so you are going to use geom_bar
  # stat = 'identity' simply sums the number of military expenditures per year
  geom_bar(stat='identity', width=.5) +
  labs(title="Military Expenditures by Year", subtitle="Around the world", y="Expenditures ($)", x="Year") +
  scale_y_continuous(labels = scales::comma)

# facet: you want to split this up into categories
# by country (stateabb)

ggplot(power_cow2, aes(x=factor(year), y=milex, label=milex)) + 
  geom_bar(stat='identity', width=.5) +
  labs(title="Military Expenditures by Year", subtitle="Around the world", y="Expenditures ($)", x="Year") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ stateabb)

# ugh the x-axis looks ugly, as in, I can't read it at all!!
# let's rotate it!!

ggplot(power_cow2, aes(x=factor(year), y=milex, label=milex)) + 
  geom_bar(stat='identity', width=.5) +
  labs(title="Military Expenditures by Year", subtitle="Around the world", y="Expenditures ($)", x="Year") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ stateabb) +
  # angle = rotate it by 90 degrees
  # hjust = horizontal justification (right-justified); if hjust = 0, it is left-justified
  theme(axis.text.x=element_text(angle=90, hjust=1))

## hmm this is a lot to digest...
## maybe we can play around with our options so it looks easier to read...

ggplot(power_cow2, aes(x=stateabb, y=milex, label=milex)) + 
  geom_bar(stat='identity', width=.5) +
  labs(title="Military Expenditures by Year", subtitle="Around the world", y="Expenditures ($)", x="Year") +
  ## y-axis: you can do it by 0, 10, 20, 30 M...https://stackoverflow.com/questions/52602503/display-an-axis-value-in-millions-in-ggplot
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(~ year) +
  # rotate it by 90 degrees
  # make the x-axis text smaller so things can fit
  theme(axis.text.x=element_text(angle=90, hjust=1, size = 4))

# play around with the y-axis more
ggplot(power_cow2, aes(x=stateabb, y=milex, label=milex)) + 
  geom_bar(stat='identity', width=.5) +
  labs(title="Military Expenditures by Year", subtitle="Around the world", y="Expenditures ($)", x="Year") +
  # we can further rescale the y-axis to "zoom in" more
  # read more about scaling here: https://scales.r-lib.org/
  # this is when we take a log10 of the y values
  # many people handle expenditures in the millions
  # logarithms are an easy way to express large numbers
  scale_y_log10(labels = scales::label_number_si()) +
  # you can use facet_grid(var1 ~ var2) if you have data and want to split the graph up into more categories (e.g., year ~ gender)
  facet_wrap(~ year) +
  # rotate it by 90 degrees
  # make the x-axis text smaller so things can fit
  theme(axis.text.x=element_text(angle=90, hjust=1, size = 4))

# the best way to learn ggplot2 is through experience :)