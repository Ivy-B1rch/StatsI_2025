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
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr

lapply(c("car"),  pkgTest)
library(car)
library(dplyr)
data(Prestige) #note to self: run this whenever you need to reset the dataframe
help(Prestige)


Prestige
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



Prestige$type

#part 1

#using some stuff i learned in Comp Sci for Recoding without needing to use ifelse
professional <- recode(Prestige$type, 'prof'=1, 'bc'=0, 'wc' = 0)
Prestige <- cbind(Prestige, professional)

Prestige

#outcome, continuous, dummy, continuous*Dummy
model = lm(prestige~income + professional + income*professional, data = Prestige)

summary(model)

#just so I don't have to do math, question 1.f
(0.0031709*1000 + 37.7812800*1 + 1000*1*0.0023257)-(0.0031709*0 + 37.7812800*1 + 0*1*0.0023257)

#1.g
(0.0031709*6000 + 37.7812800*1 + 6000*1*0.0023257)-(0.0031709*6000 + 37.7812800*0 + 6000*0*0.0023257)


#2a
ts <- 0.042/0.016
ts

#130-2 is n-k, n is 130 and k, the number of variables, is 2 
p_value <- 2*pt(abs(ts),130-2,lower.tail = F) 
p_value



#2b
ts2 <- 0.042/0.013
ts2

p_value2 <- 2*pt(abs(ts2),130-2,lower.tail = F) 
p_value2



#2d
