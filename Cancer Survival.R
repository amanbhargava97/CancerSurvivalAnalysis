library(survival)
library(readxl)
library(forecast)
setwd("C:/Users/Aman Bhargava/OneDrive - purdue.edu/Desktop/Purdue/Module 3/672 - Advanced Business Analytics/Data")
cancer.data <- read_excel("CancerData.xlsx")

library(mice)    # call the mice library so we can use the functions in this library
?mice            # tells you a bit about the mice() function

## Change data types
cancer.data$inst <- as.factor(cancer.data$inst)
cancer.data$sex <- as.factor(cancer.data$sex)
cancer.data$ph.ecog<-as.factor(cancer.data$ph.ecog)

##Meal-cal subsets
summary(cancer.data$meal.cal)
meal.cal1 <- subset(cancer.data, meal.cal < mean(cancer.data$meal.cal))
meal.cal2 <- subset(cancer.data, meal.cal < 1150)
meal.cal3 <- subset(cancer.data, meal.cal >= 1150)
meal.cal4 <- subset(cancer.data, meal.cal < 625)
meal.cal5 <- subset(cancer.data, meal.cal >= 625 & meal.cal < 1150)

meal.cal1 <- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal1)
meal.cal2<- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal2)
meal.cal3<- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal3)
meal.cal4<- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal4)
meal.cal5<- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal5)

meal.cal1
meal.cal2
meal.cal3
meal.cal4
meal.cal5

# here we use a decision tree predictive model approach to predict the missing
# values for LotFrontage and Electrical. The algorithm used is "cart" which stands 
# for "Classification And Regression Trees".
imputedValues <- mice(data=cancer.data
                      , seed=2016     # keep to replicate results
                      , method="cart" # model you want to use
                      , m=1           # Number of multiple imputations
                      , maxit = 1     # number of iterations
)
# impute the missing values in our tr data.frame
cancer.data <- mice::complete(imputedValues,1) # completely fills in the missing

View(cancer.data)

fit.all<-survfit(Surv(time,status==2)~1,data=cancer.data) # analyze w/ all data
plot(fit.all, col=1:2:3, ylab = 'Survival Rate', xlab = 'Days', main='Overall Survival Rate')

fit.sex<-survfit(Surv(time,status==2)~sex,data=cancer.data)
plot(fit.sex,col=1:2, lty=1:3, ylab = 'Survival Rate', xlab = 'Days', main="Survival Rate by sex")
llabel<-gsub("x=","",names(fit.sex$strata))
legend("top",legend=llabel,col=1:2,lty=1:3,bty='n')

##All variables
fit.cox<-coxph(Surv(time,status==2)~sex+age+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss,data=cancer.data)
fit.cox

##Meal-cal subsets
summary(cancer.data$meal.cal)
meal.cal1 <- subset(cancer.data, meal.cal >= mean(cancer.data$meal.cal))
meal.cal2 <- subset(cancer.data, meal.cal < 1150)
meal.cal3 <- subset(cancer.data, meal.cal >= 1150)
meal.cal4 <- subset(cancer.data, meal.cal < 625)
meal.cal5 <- subset(cancer.data, meal.cal >= 625 & meal.cal < 1150)

meal.cal1 <- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal1)
meal.cal2<- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal2)
meal.cal3<- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal3)
meal.cal4<- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal4)
meal.cal5<- coxph(Surv(time, status == 2) ~ sex + ph.ecog + age + ph.karno + pat.karno + meal.cal + wt.loss, data = meal.cal5)

meal.cal1
meal.cal2
meal.cal3
meal.cal4
meal.cal5
