library(MASS)
library(caret)
library(car)
##importing the dataset##
hospital=read.csv('index.txt',sep='\t')
#cleaning up the dataframe for analysis
hospital$MedSchool[hospital$MedSchool==1]<-'MedSchool1'
hospital$MedSchool[hospital$MedSchool==2]<-'MedSchool2'
hospital$Region[hospital$Region==1]<-'Region1'
hospital$Region[hospital$Region==2]<-'Region2'
hospital$Region[hospital$Region==3]<-'Region3'
hospital$Region[hospital$Region==4]<-'Region4'
# partioning the dataset into training and testing 
ind<-sample(2,nrow(hospital),replace=TRUE,prob=c(0.8,0.2))
training_data<-hospital[ind==1,]
testing_data<-hospital[ind==2,]
## correlation matrix for the predictors
new_numerical_dataframe<-data.frame(training_data$InfctRsk,training_data$Stay,training_data$Culture,training_data$Xray,training_data$Census,training_data$Nurses,training_data$Facilities)
res<-cor(new_numerical_dataframe)
print(res)
## selecting the predictors using backward selection
Fitall<-lm(InfctRsk~ID + Stay + Age + Culture + Xray + Beds + MedSchool + 
             Region + Census*Nurses + Census*Facilities,data=training_data)
step(Fitall,direction='backward')
##fitting the model
lm.fit<-lm(formula = InfctRsk ~ Stay + Culture + Xray + MedSchool + Region + 
             Census + Nurses + Census:Nurses,data=training_data)
summary(lm.fit)
new_test_data<-data.frame(testing_data$InfctRsk,testing_data$Stay,testing_data$Culture,testing_data$Xray,testing_data$MedSchool,testing_data$Region,testing_data$Census,testing_data$Nurses)
model<-lm(InfctRsk ~ Stay + Culture + Xray + MedSchool + Region + 
            Census + Nurses + Census:Nurses,data=training_data)
pred<-predict(model,testing_data)
## checking the performance of the model
output<-cbind(testing_data,pred)
actuals<-testing_data$InfctRsk
scores<-(abs(pred-actuals))/actuals
summary(scores)
lm.fit=lm(formula = InfctRsk~,data=training_data)
summary(lm.fit)
## checking for potential problems i)non - linearity ii) correlation of error terms iii) non-constant variance of error terms 
## iv) outliers v) high leverage points 

#i) checked and linear
res<-resid(model)
plot(fitted(model),res)
abline(0,0)

#ii) no outliers as studentresiduals values are within -3 and 3
studentres<-studres(model)
plot(fitted(model),studentres)
abline(0,0)

#calculating collinearity 
vif_values<-vif(model)
print(vif_values)

##dealing with the problems
model2<-lm(InfctRsk ~ Stay + Culture + Xray + MedSchool + Region +  Nurses,data=training_data)
summary(model2)
vif(model2)
pred2<-predict(model2,testing_data)
output<-cbind(testing_data,pred2)
actuals2<-testing_data$InfctRsk
scores2<-(abs(pred2-actuals2))/actuals2
summary(scores2)
print(output)

