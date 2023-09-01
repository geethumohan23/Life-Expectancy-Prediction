setwd('C:/Users/geeth/Desktop/Modelling_Coursework')
dataLife <- read.csv('Life_Expectancy_Data1.csv')
library('ISLR')
library('tidyverse')
library('ggplot2')
library('corrplot')
library('dplyr')
library(corrplot)
library(mice) 
library("faraway")
library('tidyr')
library('olsrr')
str(dataLife)
head(dataLife)
summary(dataLife)
#ggplot(dataLife,aes(DensityPop,LifeExp))+geom_point(size=1)+geom_line(color="red")
#view(dataLife)

#Attaching  
dim(dataLife)
dataLife <- dataLife[,c(-1,-2,-3,-10,-11,-12,-14,-25,-28)]

#renaming
dataLife<-data.frame(dataLife)
colnames(dataLife)[1:20]<-c("LifeExp","AccessElectricity","IncomePerCapita","NationalIncome","HIVKids","NoSchool","Infant_Mor","PrimaryComp","Interest","GrowthPop","DensityPop","ExpenseGDP","Health","Unemploy","GDPGrowth","GDPPerCapita","BirthRate","HIVAdult","DrinkWater","Education")
view(dataLife)

data$LifeExp[is.na(data$LifeExp)]<-mean(data$LifeExp,na.rm = TRUE)
data$AccessElectricity[is.na(data$AccessElectricity)]<-mean(data$AccessElectricity,na.rm = TRUE)
data$IncomePerCapita[is.na(data$IncomePerCapita)]<-mean(data$IncomePerCapita,na.rm = TRUE)
data$NationalIncome[is.na(data$NationalIncome)]<-mean(data$NationalIncome,na.rm = TRUE)
data$HIVKids[is.na(data$HIVKids)]<-mean(data$HIVKids,na.rm = TRUE)
data$NoSchool[is.na(data$NoSchool)]<-mean(data$NoSchool,na.rm = TRUE)
data$Infant_Mor[is.na(data$Infant_Mor)]<-mean(data$Infant_Mor,na.rm = TRUE)
data$PrimaryComp[is.na(data$PrimaryComp)]<-mean(data$PrimaryComp,na.rm = TRUE)
data$Interest[is.na(data$Interest)]<-mean(data$Interest,na.rm = TRUE)
data$GrowthPop[is.na(data$GrowthPop)]<-mean(data$GrowthPop,na.rm = TRUE)
data$DensityPop[is.na(data$DensityPop)]<-mean(data$DensityPop,na.rm = TRUE)
data$Health[is.na(data$Health)]<-mean(data$Health,na.rm = TRUE)
data$ExpenseGDP[is.na(data$ExpenseGDP)]<-mean(data$ExpenseGDP,na.rm = TRUE)
data$Unemploy[is.na(data$Unemploy)]<-mean(data$Unemploy,na.rm = TRUE)
data$GDPGrowth[is.na(data$GDPGrowth)]<-mean(data$GDPGrowth,na.rm = TRUE)

data$GDPPerCapita[is.na(data$GDPPerCapita)]<-mean(data$GDPPerCapita,na.rm = TRUE)
data$BirthRate[is.na(data$BirthRate)]<-mean(data$BirthRate,na.rm = TRUE)
data$HIVAdult[is.na(data$HIVAdult)]<-mean(data$HIVAdult,na.rm = TRUE)
data$DrinkWater[is.na(data$DrinkWater)]<-mean(data$DrinkWater,na.rm = TRUE)

data$Education[is.na(data$Education)]<-mean(data$Education,na.rm = TRUE)



#Imputation Start
data_Impute<-mice(dataLife, method="cart",seed=2318);
complete(data_Impute) #imputated variable
data_Impute$imp

#analysis of Imputed Value with the filtered dataset
Model_Imputed<- with(data_Impute, lm(LifeExp~AccessElectricity+IncomePerCapita+NationalIncome+HIVKids+NoSchool+Infant_Mor+PrimaryComp+Interest+GrowthPop+ExpenseGDP+Health+Unemploy+GDPGrowth+GDPPerCapita+BirthRate+HIVAdult+DrinkWater+Education))
summary(Model_Imputed)


#ploting with imputed values
xyplot(data_Impute,LifeExp  ~ NationalIncome| .imp, pch = 20, cex = 1.4)
xyplot(data_Impute,LifeExp  ~ Infant_Mor| .imp, pch = 20, cex = 1.4)
#md.pattern(Model_Imputed)

#pooling
pooled.model<-pool(Model_Imputed)
summary(pooled.model) #completed data imputation with data in pooled.model



data_impute_complete<-complete(data_Impute,2)
#collinearity plot
collinear_model<-lm(LifeExp~.,data=data_impute_complete)
summary(collinear_model)
summary(data_impute_complete)
Data_relate<-cor(data_impute_complete)
summary(Data_relate)
corrplot(Data_relate,tl.pos ='lt', tl.cex=0.55,method='circle')

#testing multicollinearity
ols_vif_tol(collinear_model)
evif(collinear_model)
ols_plot_resid_fit_spread(collinear_model)

#Forward and Backward Selection
data_impute_complete<- data_impute_complete[,c(-2,-3,-4,-7,-10,-11,-12,-16,-17,-19)]
view(data_impute_complete)
dt<-data.frame(data_impute_complete)
#write.csv(dt,"C:/Users/geeth/Desktop/Modelling_Coursework/final_data.csv", row.names=FALSE)

BestModelBuild<-lm(LifeExp~., data=data_impute_complete) #linear regression for the model()
summary(BestModelBuild) #Read details which all fields needs to be rejected

ModelForward<-lm(LifeExp~1,data=data_impute_complete)
step1<-step(BestModelBuild,ModelForward=~ PrimaryComp+Health+Unemploy+GDPGrowth, method='forward')
AIC(step1)

#backward Elimination
forward_list<-step(BestModelBuild,direction="forward")
AIC(forward_list)

#Backward feature selection
ModelBackward<-lm(LifeExp~.,data=data_impute_complete)
step2<-step(BestModelBuild,ModelBackward=~.,data=data_impute_complete)
AIC(step2)


backward_list<-step(BestModelBuild,direction="backward")
AIC(backward_list)

ModelFit_Backward<-lm(LifeExp ~ AccessElectricity + PrimaryComp + Interest + DensityPop + 
                       Health + GDPGrowth + HIVAdult + DrinkWater, data=data_impute_complete)
AIC(ModelFit_Backward)
summary(ModelFit_Backward)

ModelFit_Backward<-lm(LifeExp ~ AccessElectricity + HIVKids + NoSchool + PrimaryComp + 
                        Interest + DensityPop + Health + Unemploy + GDPGrowth + HIVAdult + 
                        DrinkWater + Education, data=data_impute_complete)
AIC(ModelFit_Backward)
summary(ModelFit_Backward)
summary(step1)
Final_linear_model<-lm(LifeExp ~ NoSchool+HIVAdult+Interest+Unemploy + GDPGrowth +Health+PrimaryComp)
