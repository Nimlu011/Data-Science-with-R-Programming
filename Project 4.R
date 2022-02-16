# Project 4 || Insurance factors identification || Niladri Sekhar Sardar

# Insurance factors identification
# import library and Dataset

library(dplyr)
library(caTools)
library(randomForest)

dataifi = read.csv('Insurance_factor_identification.csv')

# VIEW Dataset

str(dataifi)
head(dataifi,10)

#******************************************************************************************************************#
#-------------------------------------------------- ANALYSIS ------------------------------------------------------#

dataifi$Claims = as.factor(dataifi$Claims)
str(dataifi)

dataifi %>% group_by(Claims) %>% dplyr::summarise(number = n())

set.seed(486)
split = sample.split(dataifi$Claims, SplitRatio = 0.8)
training_data = dataifi[split,]
testing_data = dataifi[!split,]

classifier = randomForest(Claims ~ Bonus + Zone + Make + Insured, data = training_data, ntree = 100)

training_predict = predict(classifier, newdata = training_data)
training_confusion = table(training_predict, training_data$Claims)
training_confusion

sum(diag(training_confusion))/ sum(training_confusion)


testing_predict = predict(classifier, newdata = testing_data)
testing_confusion = table(testing_predict, testing_data$Claims)
testing_confusion

sum(diag(testing_confusion))/ sum(testing_confusion)

#******************************************************************************************************************#
#-------------------------------------------- ANALYSIS  PROTOCOL --------------------------------------------------#
data_ifi=read.csv('Insurance_factor_identification.csv')
dim(data_ifi)
#the dimensions of the dataset are 2182 x 7 

#through summary we can see that the mean of kilometre is 2.986 ie, 10,000-15,000
#similarly the maximum number of claims are 3338 and mean is 51.97 
##we perform corelation to find out the relation between Claims and Payment

b=cor(data_ifi$Claims,data_ifi$Payment)
b*100
#percentage of corelation between claims and payments
#hence we can see that the corelation between claims and payment are 0.9954 ie, 99% corelated , 

plot(data_ifi$Claims,data_ifi$Payment)
c=cor(data_ifi$Claims,data_ifi$Insured)
c*100
#claims and insured have corelation of 91% 

d=cor(data_ifi$Insured,data_ifi$Payment)
d*100
##Payment and Insured has Corelation of 93%

plot(data_ifi$Claims,data_ifi$Insured)
plot(data_ifi$Payment,data_ifi$Insured)

##we can see that the plot between claims and payment was linear
##we now use linear regression for Payments vs the rest of the data

reg=lm(data_ifi$Payment~data_ifi$Claims+data_ifi$Insured+data_ifi$Make+data_ifi$Bonus+data_ifi$Zone+data_ifi$Kilometres)
summary(reg)
plot(reg)

aggkm=apply(data_ifi[,c(5,6,7)],2,function(x) tapply(x,data_ifi$Kilometres,mean))
aggkm
##in these 5 zones we could see 1st km dist has been more insured than others with 2nd being close to it
##the amount of claims in the 2nd km distribution is higher hence more payments

aggzone=apply(data_ifi[,c(5,6,7)],2,function(x) tapply(x,data_ifi$Zone,mean))
aggzone
##zone number 4 has higher number of insured vehicle and more amount of claims and payment is more due to claims

aggbonus = apply(data_ifi[,c(5,6,7)],2,function(y) tapply(y,data_ifi$Bonus,mean))
aggbonus
##here we can see that variations are low with 7th group having more insured count

#to find if claims is affected by km,zone,bonusand make

regcl = lm(data_ifi$Claims~data_ifi$Zone+data_ifi$Bonus+data_ifi$Kilometres+data_ifi$Make)
summary(regcl)
plot(regcl)
##here we can see that all the factors affect the Claims they are significant (pvalues)



#******************************************************************************************************************#
#----------------------------------------------- ANALYSIS  TASK ---------------------------------------------------#


# 1. The committee is interested to know each field of the data collected through descriptive analysis to gain basic insights into the data set and to prepare for further analysis. 

summary(data_ifi)

# 2. The total value of payment by an insurance company is an important factor to be monitored. So the committee has decided to find whether this payment is related to the number of claims and the number of insured policy years. They also want to visualize the results for better understanding. 

lm1<-lm(data_ifi$Payment~data_ifi$Claims+data_ifi$Insured)
lm1
summary(lm1)

cor(data_ifi$Claims,data_ifi$Payment)

cor(data_ifi$Insured,data_ifi$Payment)

plot(data_ifi$Claims,data_ifi$Payment)

plot(data_ifi$Insured,data_ifi$Payment)


# 3. The committee wants to figure out the reasons for insurance payment increase and decrease. So they have decided to find whether distance, location, bonus, make, and insured amount or claims are affecting the payment or all or some of these are affecting it. 


lm2<-lm(data_ifi$Payment~.,data=data_ifi)
summary(lm2)


# 4. The insurance company is planning to establish a new branch office, so they are interested to find at what location, kilometre, and bonus level their insured amount, claims, and payment gets increased. (Hint: Aggregate Dataset) 


grupzone<-apply(data_ifi[,c(5,6,7)], 2, function(x) tapply(x, data_ifi$Zone, mean))
grupzone

grupkil<-apply(data_ifi[,c(5,6,7)],2,function(x)tapply(x,data_ifi$Kilometres,mean))
grupkil

grupbon<-apply(data_ifi[,c(5,6,7)],2,function(x)tapply(x,data_ifi$Bonus,mean))
grupbon


# 5. The committee wants to understand what affects their claim rates so as to decide the right premiums for a certain set of situations. Hence, they need to find whether the insured amount, zone, kilometre, bonus, or make affects the claim rates and to what extent. 

reg<-lm(Claims~Kilometres+Zone+Bonus+Make+Insured,data=data_ifi)
summary(reg)
plot(reg)



