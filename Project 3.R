# Web Data Analysis
# import library and dataset
library(readxl)
library(plyr)
library(dplyr)
library(readr)

#read Dataset
wda <- read_excel('internet_dataset.xlsx')


# 1. The team wants to analyze each variable of the data collected through data summarization to get a basic understanding of the dataset and to prepare for further analysis.

summary(wda)




# 2. As mentioned earlier, a unique page view represents the number of sessions during which that page was viewed one or more times. A visit counts all instances, no matter how many times the same visitor may have been to your site. So the team needs to know whether the unique page view value depends on visits.

cor(wda$Uniquepageviews,wda$Visits)

plot(wda$Uniquepageviews,wda$Visits)

anly2<-aov(Uniquepageviews~Visits, data=wda)
summary(anly2) 




# 3. Find out the probable factors from the dataset, which could affect the exits. Exit Page Analysis is usually required to get an idea about why a user leaves the website for a session and moves on to another one. Please keep in mind that exits should not be confused with bounces.

anly3<-aov(Exits~.,data = wda)
summary(anly3)



# 4. Every site wants to increase the time on page for a visitor. This increases the chances of the visitor understanding the site content better and hence there are more chances of a transaction taking place. Find the variables which possibly have an effect on the time on page.

anly4<-aov(Timeinpage~.,data = wda)
summary(anly4)



# 5. A high bounce rate is a cause of alarm for websites which depend on visitor engagement. Help the team in determining the factors that are impacting the bounce.

wda$Bounces=wda$Bounces*0.01
rmm<-glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = wda,family = "binomial")

summary(rmm)





