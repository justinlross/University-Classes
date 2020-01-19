#Open Libraries

#library(e1071)

library(dplyr)
library(car)

library(lattice)
library(BSDA)
library(psych)
library(PerformanceAnalytics)


#skew(data$scores, na.rm = TRUE,type=2) #this is closes to SPSS 
#kurtosi(data$scores, na.rm = TRUE,type=2)#this is closes to SPSS

#Declaring variables Section

twin<- read.table(file="C:/Users/Justin/Desktop/STA8190 Non-Parametrics/Assignments to Students 2019/Data Files/twin.txt", header=TRUE, sep="")
twinDf<-data.frame(twin)
controlVar=twinDf %>% pull(Control)
treatmentVar=twinDf %>% pull(Treatment)


goats<- read.table(file="C:/Users/Justin/Desktop/STA8190 Non-Parametrics/Assignments to Students 2019/Data Files/goats.txt", header=TRUE, sep="")
goatDf<-data.frame(goats)
goat_split <- split(goatDf, goatDf$Treatment)
goatControl <- goat_split$`1` %>% pull(Judgement)
goatTreat <-goat_split$`2` %>% pull(Judgement)


# Display output Section

print('#####################')
print('##### TWINS.txt #####')
print('#####################')

print('*** Control Group ***')
print(controlVar)
print(summary(controlVar))
print(paste0("Standard Deviation = ",sd(controlVar)))
hist(controlVar,main="Histogram of Twins Control Group",xlab="Ewe Height (Inches)")
print(kurtosi(controlVar,na.rm = TRUE,type=2))
print(skew(controlVar,na.rm = TRUE,type=2))
print(ks.test(controlVar,"pnorm",mean = mean(controlVar), sd= sd(controlVar)))


print('*** Treatment Group ***')
print(treatmentVar)
hist(treatmentVar,main="Histogram of Twins Treatment Group",xlab="Ewe Height (Inches)")
print(summary(treatmentVar))
print(paste0("Standard Deviation = ",sd(treatmentVar)))
print(paste0("Kurtosis = ", kurtosi(treatmentVar,na.rm = TRUE,type=2)))
print(paste0("Skewness = ", skew(treatmentVar,na.rm = TRUE,type=2)))
print(ks.test(treatmentVar,"pnorm", mean = mean(treatmentVar), sd= sd(treatmentVar)))

print('*** Treatment vs Control Group ***')
print(wilcox.test(controlVar,treatmentVar,paired=TRUE,conf.int = TRUE,conf.level = 0.95)) #paired=TRUE results in wilcox signed rank test for paired data, defaults to two tailed.


print(SIGN.test(x = controlVar, y = treatmentVar, alternative = "two.sided",conf.level = 0.95)) # Two sided means two tailed

print(ks.test(controlVar,treatmentVar))
print(t.test(controlVar,treatmentVar,paired=TRUE,conf.level=0.95)) # Students t-test for dependant samples

print('#####################')
print('##### GOATS.txt #####')
print('#####################')

#print(goatControl)
#print(goatTreat)

print('*** Control Group ***')
print(goatControl)
print(summary(goatControl))
print(paste0("Standard Deviation = ",sd(goatControl)))
hist(goatControl,main="Histogram of Goats Control Group",xlab="Overall Condition (between 40 and 130)")
print(kurtosi(goatControl,na.rm = TRUE,type=2))
print(skew(goatControl,na.rm = TRUE,type=2))
print(ks.test(goatControl,"pnorm", mean = mean(goatControl), sd= sd(goatControl)))


print('*** Treatment Group ***')
print(goatTreat)
hist(goatTreat,main="Histogram of Goats Treatment Group",xlab="Overall Condition (between 40 and 130)")
print(summary(goatTreat))
print(paste0("Standard Deviation = ",sd(goatTreat)))
print(paste0("Kurtosis = ", kurtosi(goatTreat,na.rm = TRUE,type=2)))
print(paste0("Skewness = ", skew(goatTreat,na.rm = TRUE,type=2)))
print(ks.test(goatTreat,"pnorm", mean = mean(goatTreat), sd= sd(goatTreat)))


print('***Treatment vs Control Group***')
#print(wilcox.test(goatControl,goatTreat,paired=FALSE))
print(wilcox.test(Judgement ~ Treatment,data=goatDf)) # Judgement by treatment. Mann Whitney U-test, paired = false is default, defaults to two tailed.
print(ks.test(goatControl,goatTreat)) # Two sample Kolmogorov-Smirnoff Test
print(t.test(goatControl,goatTreat,paired=FALSE,conf.level=0.95, var.equal=TRUE)) # Students t-test for independant samples

print(wilcox.test(Judgement ~ Treatment,data=goatDf))

#print(cat("Treatment Group Skewness = ", skewness(treatmentVar)))

print('***Levene Test of Homogeneity of Variance***')

y <- c(goatControl, goatTreat)

group <- as.factor(c(rep(1, length(goatControl)), rep(2, length(goatTreat))))

print(leveneTest(y, group))


#print(leveneTest(Judgement ~ Treatment, data=goatDf, center=mean))



print('#######################################')
print('##### Hand Based Sign Calculation #####')
print('#######################################')

print(goatControl)
print(goatTreat)

n = 20
x = 20
#factorial = 0

topHalf<-factorial(n)
bottomHalf<-(factorial(n-x)) * (factorial(x))
middle<-(0.5^x)
end<-((1-0.5)^(n-x))

print(topHalf)
print(bottomHalf)
print(middle)
print(end)

#print((factorial(n)/factorial(n-x)*factorial(x)))
#print((0.5^x))
#print(((1-0.5)^(n-x)))

#answer = (factorial(n)/(factorial(n-x)*factorial(x))) * (0.5^x) * ((1-0.5)^(n-x))

answer = (topHalf/bottomHalf) * (middle) * (end)

print(answer)


#for (i in 1:n){
#  print((factorial(i)/(factorial(i-x)*factorial(x))) * (0.5^x) * ((1-0.5)^(i-x)))
#}


print('##########################################')
print('##### Wilcoxon signed rank test Sums #####')
print('##########################################')


# controlVar,treatmentVar

diff <- c(controlVar - treatmentVar) #calculating the vector containing the differences
diff <- diff[ diff!=0 ] #delete all differences equal to zero
diff.rank <- rank(abs(diff)) #check the ranks of the differences, taken in absolute
diff.rank.sign <- diff.rank * sign(diff) #check the sign to the ranks, recalling the signs of the values of the differences
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0]) #calculating the sum of ranks assigned to the differences as a positive, ie greater than zero
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0]) #calculating the sum of ranks assigned to the differences as a negative, ie less than zero
print(ranks.pos) #it is the value V of the wilcoxon signed rank test
print(ranks.neg)



print(goatControl)
print(goatTreat)
