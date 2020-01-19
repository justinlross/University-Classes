#Open Libraries

#library(e1071)

library(dplyr)
library(car)

library(lattice)
library(BSDA)
library(psych)
library(PerformanceAnalytics)
library(tidyr)
library(ltm)

#skew(data$scores, na.rm = TRUE,type=2) #this is closes to SPSS 
#kurtosi(data$scores, na.rm = TRUE,type=2)#this is closes to SPSS

#Declaring variables Section

weevil<- read.table(file="C:/Users/Justin/Desktop/STA8190 Non-Parametrics/Assignment 2/Data Files/Weevil.txt", header=TRUE, sep="")
weevilDf<-data.frame(weevil)
weevil_variety_split <- split(weevilDf, weevilDf$Variety)
weevil_treatment_split <- split(weevilDf, weevilDf$Treatment)
#weevil_treatment_split <- split(weevil_variety_split, weevil_variety_split$Treatment)


weevilNoneSplit <- weevil_treatment_split$`1`
weevilOrganicSplit <- weevil_treatment_split$`2`
weevilIPMSplit <- weevil_treatment_split$`3`
weevilConventionalSplit <- weevil_treatment_split$`4`


weevilNone <- weevilNoneSplit %>% pull(Larvae)
weevilOrganic <- weevilOrganicSplit %>% pull(Larvae)
weevilIPM <- weevilIPMSplit %>% pull(Larvae)
weevilConventional <- weevilConventionalSplit %>% pull(Larvae)


#weevilNoneVar=weevil_treatment_split %>% pull(Control)

#print(weevilNone)



systolic<-read.table(file="C:/Users/Justin/Desktop/STA8190 Non-Parametrics/Assignment 2/Data Files/systolic.txt", header=TRUE, sep="")
systolicSexSplit <- split(systolic, systolic$Sex)
systolicRaceSplit <- split(systolic, systolic$Race)


systolicMales <- systolicSexSplit$`1`
systolicFemales <- systolicSexSplit$`2`

systolicMales <- systolicMales[ -c(1) ]
systolicFemales <- systolicFemales[ -c(1) ]


systolicBlackSplit <- systolicRaceSplit$`1`
systolicHispanicSplit <- systolicRaceSplit$`2`
systolicWhiteSplit <- systolicRaceSplit$`3`
systolicOtherSplit <- systolicRaceSplit$`4`


systolicBlack <- systolicBlackSplit %>% pull(SBP)
systolicHispanic <- systolicHispanicSplit %>% pull(SBP)
systolicWhite <- systolicWhiteSplit %>% pull(SBP)
systolicOther <- systolicOtherSplit %>% pull(SBP)




gender<-read.table(file="C:/Users/Justin/Desktop/STA8190 Non-Parametrics/Assignment 2/Data Files/GenderTrait.txt", header=TRUE, sep="")
#print(gender)

#twin<- read.table(file="C:/Users/Justin/Desktop/STA8190 Non-Parametrics/Assignments to Students 2019/Data Files/twin.txt", header=TRUE, sep="")
#twinDf<-data.frame(twin)
#controlVar=twinDf %>% pull(Control)
#treatmentVar=twinDf %>% pull(Treatment)

#goatDf<-data.frame(goats)
#goat_split <- split(goatDf, goatDf$Treatment)
#goatControl <- goat_split$`1` %>% pull(Judgement)
#goatTreat <-goat_split$`2` %>% pull(Judgement)


# Display output Section

print('######################')
print('##### Question 1 #####')
print('######################')

print('a)Create a side-by-side box and whisker plot showing the distribution of weevil incidence in each treatment.
Describe the distributions.')

boxplot(Larvae ~ Treatment, data = weevilDf,
        xlab = "Treatment", ylab = "Larvae",
        main = "Distribution of Weevil Incidence by Treatment",
        xaxt = "n"
)

raceTicks <- c("None", "Organic", "IPM", "Conventional")

axis(1, at=1:4, labels=raceTicks)






weevilNone <- weevilNoneSplit %>% pull(Larvae)
weevilOrganic <- weevilOrganicSplit %>% pull(Larvae)
weevilIPM <- weevilIPMSplit %>% pull(Larvae)
weevilConventional <- weevilConventionalSplit %>% pull(Larvae)



print('*** None Group ***')

print(summary(weevilNone))
print(paste0("Standard Deviation = ",sd(weevilNone)))
print(kurtosi(weevilNone,na.rm = TRUE,type=2))
print(skew(weevilNone,na.rm = TRUE,type=2))


print('*** Organic Group ***')

print(summary(weevilOrganic))
print(paste0("Standard Deviation = ",sd(weevilOrganic)))
print(kurtosi(weevilOrganic,na.rm = TRUE,type=2))
print(skew(weevilOrganic,na.rm = TRUE,type=2))


print('*** IPM Group ***')

print(summary(weevilIPM))
print(paste0("Standard Deviation = ",sd(weevilIPM)))
print(kurtosi(weevilIPM,na.rm = TRUE,type=2))
print(skew(weevilIPM,na.rm = TRUE,type=2))


print('*** Conventional Group ***')

print(summary(weevilConventional))
print(paste0("Standard Deviation = ",sd(weevilConventional)))
print(kurtosi(weevilConventional,na.rm = TRUE,type=2))
print(skew(weevilConventional,na.rm = TRUE,type=2))



print('b)Perform a Friedman test to determine if there is any difference in weevil incidence among treatments. State the hypotheses (Ho and Ha) and interpret you results.')

#friedman.test(data1$late, data1$month, data1$Employee)

#friedman.test(Likert ~ Instructor | Rater, data = Data)

print(friedman.test(Larvae ~ Treatment | Variety, data = weevilDf)) # Larvae is the dependant variable, Treatment is the Independent, Variety is blocking


#friedman.test(weevil_treatment_split$late, weevil_treatment_split$month, data1$Employee)

print('c)If your analysis in part b) was significant use Wilcoxon signed rank tests to identify which treatments are different to each other. State the hypotheses (Ho and Ha) for the first comparison only. Interpret these results.')

#weevil_treatment_split

treatment1 <- weevil_treatment_split$`1` %>% pull(Larvae)
treatment2 <- weevil_treatment_split$`2` %>% pull(Larvae)
treatment3 <- weevil_treatment_split$`3` %>% pull(Larvae)
treatment4 <- weevil_treatment_split$`4` %>% pull(Larvae)

#print(treatment1)
#print(treatment2)
#print(treatment3)
#print(treatment4)

#print(paste('treatment','1',sep=""))


#wilcox.test(Larvae ~ Treatment, data=weevilDf)




for (i in 1:4) {
  treatmentA <- c(get(paste('treatment',i,sep="")))
  for (j in 1:4) {
    if (j > i){
      treatmentB <- c(get(paste('treatment',j,sep="")))
      print(treatmentA)
      print(treatmentB)
      print(wilcox.test(treatmentA,treatmentB))
      #print(wilcox.test(treatmentA,treatmentB,paired=TRUE, alternative = "two.sided", mu = 0.0, 
      #                  exact = TRUE, correct = FALSE, conf.int = TRUE, conf.level = 0.95)) #paired=TRUE results in wilcox signed rank test for paired data, defaults to two tailed.
      #print(wilcox.test(treatmentA, treatmentB, paired = TRUE, alternative = "two.sided", mu = 0.0, 
      #                  exact = TRUE, correct = TRUE, conf.int = TRUE, conf.level = 0.95))
      #print(pairwise.wilcox.test(treatmentA, treatmentB, alternative = c("two.sided")))
      
      diff <- c(treatmentA - treatmentB) #calculating the vector containing the differences
      diff <- diff[ diff!=0 ] #delete all differences equal to zero
      diff.rank <- rank(abs(diff)) #check the ranks of the differences, taken in absolute
      diff.rank.sign <- diff.rank * sign(diff) #check the sign to the ranks, recalling the signs of the values of the differences
      ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0]) #calculating the sum of ranks assigned to the differences as a positive, ie greater than zero
      ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0]) #calculating the sum of ranks assigned to the differences as a negative, ie less than zero
      print(ranks.pos) #it is the value V of the wilcoxon signed rank test # Sum of Positive Difference Ranks
      print(ranks.neg) # Sum of Negative Difference Ranks
    }
  }
}


print('#### new tests ####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')


A <- c(121, 132, 148, 101, 153)

B <- c(88, 79, 105, 52, 49)


print(wilcox.test(A, B, paired = TRUE, alternative = "two.sided", mu = 0.0, 
            exact = TRUE, correct = TRUE, conf.int = TRUE, conf.level = 0.95))


print(pairwise.wilcox.test(weevilDf$Larvae, weevilDf$Treatment, p.adjust.method = "none"))

print(pairwise.wilcox.test(weevilDf$Larvae, weevilDf$Treatment,p.adjust.method = "bonferroni"))



#print(pairwise.wilcox.test(Larvae ~ Treatment, data = weevilDf, paired=TRUE, alternative = c("two.sided")))

#print(friedman.test(Larvae ~ Treatment | Variety, data = weevilDf))

#print(wilcox.test(Larvae ~ Treatment, data = weevilDf, paired = TRUE, alternative = c( "two.sided"))) #agrees with SPSS


print('d)Show the Bonferroni correction calculation by hand. How does this correction change your results and interpretation in part c)? How does this correction affect the probability of a type-I error?')





print('e)Calculate the Friedman test by hand using equation 5.2 from the text book. Clearly define Ri, n, k and CF as part of your answer. Does this match your result in part a)? (10 marks)')






print('######################')
print('##### Question 2 #####')
print('######################')

print('a)Create a side-by-side box and whisker plot showing the distribution of SBP for each Race. Describe these distributions.')



boxplot(SBP ~ Race, data = systolic,
        xlab = "Race", ylab = "Systolic Blood Pressure (SBP)",
        main = "Distribution of \n Systolic Blood Pressure (SBP) by Race",
        xaxt = "n",
        ylim=c(100, 150)
)

raceTicks <- c("Black", "Hispanic", "White", "Other")

axis(1, at=1:4, labels=raceTicks)



print('*** Black Group ***')

print(summary(systolicBlack))
print(paste0("Standard Deviation = ",sd(systolicBlack)))
print(kurtosi(systolicBlack,na.rm = TRUE,type=2))
print(skew(systolicBlack,na.rm = TRUE,type=2))


print('*** Hispanic Group ***')

print(summary(systolicHispanic))
print(paste0("Standard Deviation = ",sd(systolicHispanic)))
print(kurtosi(systolicHispanic,na.rm = TRUE,type=2))
print(skew(systolicHispanic,na.rm = TRUE,type=2))


print('*** White Group ***')

print(summary(systolicWhite))
print(paste0("Standard Deviation = ",sd(systolicWhite)))
print(kurtosi(systolicWhite,na.rm = TRUE,type=2))
print(skew(systolicWhite,na.rm = TRUE,type=2))


print('*** Other Group ***')

print(summary(systolicOther))
print(paste0("Standard Deviation = ",sd(systolicOther)))
print(kurtosi(systolicOther,na.rm = TRUE,type=2))
print(skew(systolicOther,na.rm = TRUE,type=2))




print('b)Perform a Kruskal-Wallis H-test to determine if there is any difference in SBP among the four races. State the hypotheses (Ho and Ha) and interpret you results (Do not include pairwise-comparison analysis).')

#data1$treat<-as.factor(data1$treat) #must convert to factor or test will not work
#kruskal.test(data1$strength, data1$treat) #agrees with SPSS

#kruskal.test(systolic$Race, systolic$SBP) #agrees with SPSS

print(kruskal.test(SBP ~ Race, data = systolic))


#(data1<-systolic %>% gather(Race, SBP, 1:3))
#data1$group<-as.factor(data1$Race) #must convert to factor or test will not work
#kruskal.test(data1$Race, data1$SBP) #agrees with SPSS




print('c)Perform a correlation analysis between SBP and sex and interpret your results. Include an appropriate plot showing the correlation between the variables, the correlation coefficient and p-value in your answer. ')

# https://www.statmethods.net/advgraphs/axes.html

#corr.test(data[,2:3], use = "pairwise", method = "spearman", adjust = "none")


#sexVar<- systolic$Sex
#SBPVar<- systolic$SBP

# print(corr.test(x = sexVar, y = SBPVar, use = "pairwise", method = "spearman", adjust = "none"))

# print(systolic)


#(systolic)


#axis(1, xaxp=c(1, 2, 5), las=0)

###axis(1, at=c(1,2,3,4), las=1)

###abline(lm(systolic$SBP~systolic$Sex), col="blue") # regression line (y~x)

#legend(location, title, legend, ...)

#lines(lowess(systolic$Race, systolic$SBP), col = "blue")


#print(corr.test(systcoli$Race, systolic$SBP, use = "pairwise", method = "spearman", adjust = "none"))


print(biserial.cor(systolic$SBP,systolic$Sex, level = 2))

print(cor.test(systolic$SBP, systolic$Sex))

print(mean(systolicMales$SBP))
print(mean(systolicFemales$SBP))

par(xpd=FALSE)

plot(systolic$Sex, systolic$SBP, main = "Scatter Plot of \n Systolic Blood Pressure (SBP) vs Sex",
     xlab = "Sex", ylab = "Systolic Blood Pressure (SBP)",
     pch = 19, frame = TRUE,
     xaxt = "n",
     ylim=c(100, 150),
     xlim=c(0, 3)
)

sexTicks <- c("Male", "Female")

axis(1, at=1:2, labels=sexTicks)


par(xpd=FALSE)
abline(lm(systolic$SBP~systolic$Sex), col="black") # regression line (y~x)

#abline(lm(systolic$SBP~systolic$Sex), col="black") # regression line (y~x)

boxplot(SBP ~ Sex, data = systolic,
        xlab = "Sex", ylab = "Systolic Blood Pressure (SBP)",
        main = "Box Plot of Distribution of \n Systolic Blood Pressure (SBP) by Sex",
        xaxt = "n",
        ylim=c(100, 150)
)

sexTicks <- c("Male", "Female")

axis(1, at=1:2, labels=sexTicks)

plot(systolic$Sex,systolic$SBP, xlab = "Sex", ylab = "SBP", main = "Distribution of SBP by Sex")
abline(lm(systolic$SBP~systolic$Sex), col="blue") # regression line (y~x)

#plot(x = sexVar, y = SBPVar)


#plot(systolic$Race,systolic$SBP)
#plot(systolic$Race,systolicMales$SBP)
#points(systolicMales$SBP, col=2)
#print(systolicMales)
#print(systolicFemales)

plot(systolicMales$Race,systolicMales$SBP, col="Blue", xlab = "Sex", ylab = "SBP", main = "Distribution of SBP by Sex")
points(systolicFemales$SBP, col="Pink")


abline(lm(systolicMales$SBP~systolicMales$Race), col="blue") # regression line (y~x)
abline(lm(systolicFemales$SBP~systolicFemales$Race), col="Pink") # regression line (y~x)

barplot(systolicMales$SBP,systolicMales$Race)
barplot(systolicFemales$SBP,systolicFemales$Race)

#margin.table(systolic,1)

#barplot(systolic,
#        main = "Survival of Each Class",
#        xlab = "Class",
#        col = c("blue","pink")
#)
#legend("topleft",
#       c("Not survived","Survived"),
#       fill = c("red","green")
#)


print('######################')
print('##### Question 3 #####')
print('######################')

print('a)Perform a Chi-square Goodness of Fit test. State the hypotheses (Ho and Ha) as part of your answer and interpret your results. You do not need to import a data file into SPSS or R to perform this analysis.')

#goodness of fit
#chisq.test(x = count, p = expected, rescale.p = TRUE) #rescale turns expected counts into proportions
#?chisq.test



# printing the p-value
#chisq$p.value
# printing the mean
#chisq$estimate


print('b)Perform a Chi-square Test of Independence. State the hypotheses (Ho and Ha) and interpret your results.')

# Not by Gender

#$genderMorphA <- xtabs(Trait~Gender, data=gender)
#chisqVarA <- chisq.test(genderMorphA)
#print(chisqVarA)
#print(chisqVarA$expected)





#print(gender)

y = count(gender, 'Trait')
print(y)

z <- gender %>% group_by(Gender) %>% count(Trait)

z[1:2,1:1] <- 'Female'
z[3:4,1:1] <- 'Male'

z[1:1,2:2] <- 'No Trait'
z[2:2,2:2] <- 'Trait'
z[3:3,2:2] <- 'No Trait'
z[4:4,2:2] <- 'Trait'

z <-data.frame(z)

colnames(z) <- c("Gender", "Presence","Count")

print(z)

##test of independence
tblGender = table(gender$Trait, gender$Gender) # trait is row, gender is column
print(tblGender)

chiVar <- chisq.test(tblGender,correct=F)

print(chiVar)
print(chiVar$expected)


##test of independence 2
(data <- z)
data1<-xtabs(Count~Gender + Presence, data=data)
(a<-chisq.test(data1,correct=F))
print(a)
print(a$expected)


#genderMorph <- xtabs(Trait~Gender, data=gender)
#genderMorphTest<-xtabs(Trait~Gender+Subject, data=gender) # Unsure, page 178 of textbook do more reading.

#print(genderMorph)
#print(genderMorphTest) # Unsure

#chisqVar <- chisq.test(genderMorph)

#print(chisqVar)

#print(chisqVar$expected)

effectSize <- sqrt(2.0109/60)

print(effectSize)




#cramers <- apply(newdat, 2, cramersV)



##test of independence
#(data <- read.table("table8.10.txt",header=TRUE))
#data1<-xtabs(Count~Type + Behaviour, data=data)
#(a<-chisq.test(data1,correct=F))
#a$expected



print('######################')
print('##### Question 4 #####')
print('######################')





