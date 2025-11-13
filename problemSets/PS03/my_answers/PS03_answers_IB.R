#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
library(ggplot2)

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

#part 1, run a regression
VoteDifReg <- lm(voteshare~difflog, inc.sub)

summary(VoteDifReg)

#part 2 make a scatterplot
ggplot(inc.sub, aes(x= difflog, y = voteshare)) + 
  geom_point() + 
  geom_smooth(method='lm') 

#part 3 get the residuals
VoteDifResid <- VoteDifReg$residuals

#prediction equation


#Question 2
PresVoteReg <-lm(presvote~difflog, inc.sub)
summary(PresVoteReg)
#scatterplot

ggplot(inc.sub, aes(x= presvote, y = difflog)) + 
  geom_point() + 
  geom_smooth(method='lm') 

PresVoteResid <- PresVoteReg$residuals



#question 3
VotePresReg <- lm(voteshare~presvote, inc.sub)
summary(VotePresReg)
#scatterplot
ggplot(inc.sub, aes(x= presvote, y = voteshare)) + 
  geom_point() + 
  geom_smooth(method='lm') 


VotePresResid <- VotePresReg$residuals

#part 4
summary(lm(VoteDifResid~PresVoteResid))

ggplot(inc.sub, aes(x= PresVoteResid, y = VoteDifResid)) + 
  geom_point() + 
  geom_smooth(method='lm') 


#part 5
summary(lm(voteshare~difflog+presvote,inc.sub))
