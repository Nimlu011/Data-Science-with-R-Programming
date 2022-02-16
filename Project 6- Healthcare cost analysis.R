#importing file
library(readr)
library("readxl")
setwd("/home/labsuser")
data_hca <- read_xlsx("1555054100_hospitalcosts.xlsx")
head(data_hca)

summary(data_hca)

summary(as.factor(data_hca$AGE))


hist(data_hca$AGE, main="Histogram of Age category of people who frequently visit the Hospital",
     xlab="Age group", border="black", col=c("#008080", "#995c00","#e6005c","#e6b800","#b300b3",
                                             "#E9967A","#264d73","#996633","#7575a3"), xlim=c(0,20), ylim=c(0,350))

#Now we will analyse the maximum expediture using the aggregate function and to find out the maximum we will use the max function.
max(aggregate(TOTCHG~AGE,FUN = sum,data = data_hca))

ExpenseBasedOnAge = aggregate(TOTCHG ~ AGE, FUN=sum, data=data_hca)

which.max(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$TOTCHG, FUN=sum))

barplot(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$AGE, FUN=sum))





summary(as.factor(data_hca$APRDRG))

DiagnosisCost = aggregate(TOTCHG ~ APRDRG, FUN = sum, data = data_hca)

DiagnosisCost[which.max(DiagnosisCost$TOTCHG), ]





summary(as.factor(data_hca$RACE))

data_hca = na.omit(data_hca)
summary(as.factor(data_hca$RACE))

raceInfluence=lm(TOTCHG~ RACE, data=data_hca)
summary(raceInfluence)

raceInfluenceAOV <- aov(TOTCHG ~ RACE, data=data_hca)
raceInfluenceAOV


summary(raceInfluenceAOV)





summary(data_hca$FEMALE)

ageGenderInflModel <- lm(formula = TOTCHG ~ AGE + FEMALE, data = data_hca)
summary(ageGenderInflModel)



ageGenderRaceInflModel <- lm(formula = LOS ~ AGE + FEMALE + RACE, data = data_hca)
summary(ageGenderRaceInflModel)



hospitalCostModel <- lm(formula = TOTCHG ~ ., data = data_hca)
summary(hospitalCostModel)


hcm1 <- lm(formula = TOTCHG ~ AGE + FEMALE + LOS + APRDRG, data = data_hca)
summary(hcm1)


hcm2 <- lm(formula = TOTCHG ~ AGE + LOS + APRDRG, data = data_hca)
summary(hcm2)

hcm3 <- lm(formula = TOTCHG ~ AGE + LOS, data = data_hca)
summary(hcm3)


