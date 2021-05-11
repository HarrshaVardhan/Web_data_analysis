library(readxl)
Internet <- read_excel("D:/Simplilearn/Project Data Sets/3/Internet.xlsx")
View(Internet)

# To analyze each variable of the data collected through
 # data summarization for further analysis

summary(Internet)

# To know whether the unique page view value 
 # depends on visits

cor(Internet$Uniquepageviews,Internet$Visits)

annova <- aov(Uniquepageviews~Visits, data = Internet)

summary(annova)

# To find the probable factors from the dataset
 # which could effect the visits

affect <- aov(Exits~.,data = Internet)

summary(affect)

# To find the variables which possibly have an 
 # effect on the time on a page

effect <- aov(Timeinpage~., data = Internet)

summary(effect)

# To determine the factors the effect that are 
 # impacting the bounce 

Internet$Bounces = Internet$Bounces*0.01

# Due to more count of value = 0.01 in the column
 # of bounces new the same value is multiplied 

?glm

Impact <- glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = Internet,family = "binomial")

summary(Impact)

# As we are having different values in bounces new so trying to understand the values

library(readxl)
Internet <- read_excel("D:/Simplilearn/Project Data Sets/3/Internet.xlsx")
View(Internet)

Internet$Bounces = Internet$Bounces*0.02

Impact <- glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = Internet,family = "binomial")

summary(Impact)


library(readxl)
Internet <- read_excel("D:/Simplilearn/Project Data Sets/3/Internet.xlsx")
View(Internet)

Internet$Bounces = Internet$Bounces*0.03

Impact <- glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = Internet,family = "binomial")

summary(Impact)

