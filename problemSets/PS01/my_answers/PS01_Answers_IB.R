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
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

demeanedSum <- y-mean(y)
n = length(y)
#heres the big ugly formula to figure out Standard Deviation
SD <- sqrt((sum(demeanedSum^2))/ (n-1))


estimatedPopDeviation <- SD/sqrt(n) #estimate population deviation using sample deviation
#We CANT use Qnorm because N < 30 so we're gonna be using t scores instead  

tscore = qt((1-.9)/2, n-1) 

#making the answer as a vector
CI <- c(mean(y)+tscore*estimatedPopDeviation, mean(y)-tscore*estimatedPopDeviation)
CI


#question 1.2

#is the average >100? That's a one sided test. Null hypothesis: yMean =< 100
# lets start with our t statistic

# alpha = .05 and we're looking at if the value is "lower", so we're gonna use a Coefficent of .95. Our t score is 
tscore <- qt((1-.95)/2, n)

#our N_0 is 100
#test statistic, using the estimatedPopDeviation we figured out earlier
TestStat <- (mean(y)-100)/estimatedPopDeviation

TestStat

# pvalue = the probability of the test statistic being <100
#only getting one tail and not multiplying by 2 because its a single sided table
pvalue = pt(abs(TestStat), df = n-1, lower.tail = F)

pvalue

#cannot reject the null hypothesis, p value too large to be statistically significant 



#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)

plot(expenditure$X1, expenditure$Y)
#possible weak positive correlation

plot(expenditure$X2, expenditure$Y)
#possible weak positive correlation

plot(expenditure$X3, expenditure$Y)
#possible weak positive correlation

plot(expenditure$X1, expenditure$X2)
#possible positive correlation, hard to say just by looking

plot(expenditure$X1, expenditure$X3)
#possible weak positive correlation

plot(expenditure$X2, expenditure$X3)
#no clear correlation, hard to say

plot(expenditure$Region, expenditure$Y)
#northe-east region has the highest average per-capita expenditure on shelters/housing assistance

png(file="fileplot.png")

plot(expenditure$X1, expenditure$Y, 
     col = expenditure$Region, 
     pch = expenditure$Region,
     xlab = "Per capita personal income",
     ylab = "Per capita expenditure on shelters/housing assistance")
#There is a vague positive correlation, with the majority of the sample clustered around the "middle"

legend(950, 130, # x and y position of legend
              legend=c("Northeast","North Central", "South","West"),
              col=c(1,2,3,4),
              pch=c(1,2,3,4))
dev.off()


