#chi squared = sum of (f0-fe) squared / fe
#f1e = (row total / grand total) * column total

# 14, 6, 7 : 27
# 7,  7, 1  : 16

# 21, 13,8  

#like all good computer scientists i do my best to never do anything in my head ever
NSTotal <- sum(14,7) #column total
BRTotal <- sum(6,7) #column 2 total
STotal <- sum(7,1) #column 3 total
UCTotal <-sum(14,6,7) #row total
LCTotal <-sum(7,7,1) #row total

grandtotal <- sum(UCTotal,LCTotal)

f1e <-(NSTotal/grandtotal)*UCTotal
f2e <- (BRTotal/grandtotal)*UCTotal
f3e <- (STotal/grandtotal)*UCTotal
f4e <- (NSTotal/grandtotal)*LCTotal
f5e <- (BRTotal/grandtotal)*LCTotal
f6e <- (STotal/grandtotal)*LCTotal

chisquare <- sum(((14-f1e)^2)/f1e, 
    ((6-f2e)^2)/f2e, 
    ((7-f3e)^2)/f3e, 
    ((7-f4e)^2)/f4e, 
    ((7-f5e)^2)/f5e,
    ((1-f4e)^2)/f6e
  )
chisquare


#part b
pvalue <- pchisq(chisquare,3,lower.tail= FALSE)
pvalue

#if Alpha is 0.1, we can reject the null hypothesis due to the P value being below the Alpha


#part c

z1 <- ((14-f1e)^2)/sqrt(f1e*(1-NSTotal/grandtotal)*(1-UCTotal/grandtotal))
z2 <- ((6-f2e)^2)/sqrt(f2e*(1-BRTotal/grandtotal)*(1-UCTotal/grandtotal))
z3 <- ((7-f3e)^2)/sqrt(f3e*(1-STotal/grandtotal)*(1-UCTotal/grandtotal)) 
z4 <- ((7-f4e)^2)/sqrt(f4e*(1-NSTotal/grandtotal)*(1-LCTotal/grandtotal)) 
z5 <- ((7-f5e)^2)/sqrt(f5e*(1-BRTotal/grandtotal)*(1-LCTotal/grandtotal))
z6 <- ((1-f4e)^2)/sqrt(f6e*(1-STotal/grandtotal)*(1-LCTotal/grandtotal))

z1
z2
z3
z4
z5
z6
#Standardized residuals would help us determine potential outliers in the data, such as in LowerClass/Stopped




water <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)
str(water)


#null hypothesis: presence of female GP leader had no effect on drinking water facilities
#p = 0
#Alternative hypothesis: presence of female GP leader had an effect on drinking water facilities
# p != 0


#check null hypothesis
watertest <- cor.test(water$female, water$water)
watertest
#P value is below .05, we can reject null hypothesis

#correlation coefficient is positive, meaning that there is on average a positive increase in the amount of repaired water facilities in GP with female leaders- however, as it is a small number, it may not be a very large correlation.

