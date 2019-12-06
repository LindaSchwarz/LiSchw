
####HOME ASSINGMENT 1 #####
#by Linda Schwarz
rm(list=ls(all=TRUE)) # clears the workspace
graphics.off()

setwd("~/Documents/Uni/Psychologie /Master Uni Lund/PSY13")

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")
view(data_sample_1)

library(psych) 
library(gsheet) 
library(tidyverse) 
library(stats)

#checking the data:
str(data_sample_1)	

data_sample_1 %>% 	
  summary()	
#there is a problem with the STAI-trait since the minimum is at 3.5 and it ranges from 20 to 80
#houshold income is negative
#IQ of 60 is intellectually disabled and cannot be a participant in the study


describe(data_sample_1)
#the describe function shows that there is no missing data

#cleaning the data:
data_sample_1clean = data_sample_1
data_sample_1clean =data_sample_1clean[!data_sample_1clean$STAI_trait <= 20, ]
data_sample_1clean =data_sample_1clean[!data_sample_1clean$household_income <= 0, ]
view(data_sample_1clean)

#checking if they are clean
View(data_sample_1clean)
data_sample_1clean %>% 	
  summary()	


#exploring data
data_sample_1clean %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram()


#computing the model
model1 <- lm(pain ~ sex + age, data = data_sample_1clean)	
model1
model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_sample_1clean)
model2

#checking for outliers for every variable via boxplots to visualize it
boxplot(data_sample_1clean$age, xlab="age")
boxplot(data_sample_1clean$STAI_trait, xlab="STAI")
boxplot(data_sample_1clean$pain_cat, xlab="pain_cat")
boxplot(data_sample_1clean$mindfulness, xlab="mindfulness")
boxplot(data_sample_1clean$cortisol_saliva, xlab="cortisol saliva")
boxplot(data_sample_1clean$cortisol_serum, xlab="cortisal serum")
boxplot(data_sample_1clean$pain, xlab="pain")
boxplot(data_sample_1clean$weight, xlab="weight")
boxplot(data_sample_1clean$IQ, xlab="IQ")
boxplot(data_sample_1clean$household_income, xlab="household income")



########CHECKING MODEL 1###########
#MODEL 1:checking for outliers
cooksd <- cooks.distance(model1)
sample_size <- nrow(data_sample_1clean)
plot(cooksd, pch="°", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 4/sample_size, col="blue")  
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="blue") 
view(cooksd)

#MODEL 1: deleting the outliers from the cooks distance to be able to test the regression with and without
data_sample_1nooutlier_model1=data_sample_1clean
data_sample_1nooutlier_model1=data_sample_1nooutlier_model1[!cooksd> (4/sample_size),]
view(data_sample_1nooutlier_model1)

#creating a model on the dataset without outliers
model1_nooutliers <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_sample_1nooutlier)
model1_nooutliers

#model summary of both models to check if they differ significantly
summary(model1)
summary(model1_nooutliers)

########checking for assumptions MODEL 1 WITH AND WITHOUT OUTLIERS
#1. normality with outliers
?shapiro.test
residuals_model1 <- residuals(model1)
shapiro.test(residuals_model1)
hist(residuals_model1)
#without outliers
residuals_nooutliers_model1 <- residuals(model1_nooutliers)
shapiro.test(residuals_nooutliers_model1)
hist(residuals_nooutliers_model1)

#2. homoscedasticity
#with residuals
plot(model1,1)
plot(model1,3)
library(lmtest)
bptest(model1)
#without residuals
plot(model1_nooutliers,1)
plot(model1_nooutliers,3)
bptest(model1_nooutliers)

#3.linearity
#with residuals
residualPlots(model1)
#without residuals
residualPlots(model1_nooutliers)
#the residuals in follow the linearity assumption

#4.multicolinearity
library(carData)
library(car)
#without outliers:
vif(model1)
#with outliers: #rule of thumb is about 5 so everything under is ok. 
vif(model1_nooutliers)
#the outliers do not seem to impact the assumptions so the outliers stay in the dataset

################################---------------################
##########CHECKING MODEL 2##########
#MODEL 2: calculation Cooks Distance and visualizing it in a plot 
cooksd <- cooks.distance(model2)
sample_size <- nrow(data_sample_1clean)
plot(cooksd, pch="°", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 4/sample_size, col="blue")  
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="blue") 
view(cooksd)

#MODEL 2:deleting the outliers from the cooks distance to be able to test the regression with and without
data_sample_1nooutlier=data_sample_1clean
data_sample_1nooutlier=data_sample_1nooutlier[!cooksd> (4/sample_size),]
view(data_sample_1nooutlier)

#creating a model on the dataset without outliers
model2_nooutliers <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_sample_1nooutlier)
model2_nooutliers

########checking for assumptions FOR MODEL 2 WITH AND WITHOUT OUTLIERS
#1. normality with outliers
residuals_model2 <- residuals(model2)
shapiro.test(residuals_model2)
hist(residuals_model2)
#without outliers
residuals_nooutliers_model2 <- residuals(model2_nooutliers)
shapiro.test(residuals_nooutliers_model2)
hist(residuals_nooutliers_model2)

#2. homoscedasticity
#with residuals
plot(model2,1)
plot(model2,3)
library(lmtest)
bptest(model2)
#without residuals
plot(model2_nooutliers,1)
plot(model2_nooutliers,3)
bptest(model2_nooutliers)

#3.linearity
#with residuals
residualPlots(model2)
#without residuals
residualPlots(model2_nooutliers)
#the residuals in follow the linearity assumption

#4.multicolinearity
#without outliers:
vif(model2)
#with outliers: #rule of thumb is about 5 so everything under is ok. 
vif(model2_nooutliers)

#model summary of both models to check if they differ significantly
summary(model2)
summary(model2_nooutliers)

#correlation table to check multicolinearity via correlation matrix:
datamatrix=data.matrix(data_sample_1clean)
cor(datamatrix[,c("pain","sex", "age", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum", "cortisol_saliva", "weight", "IQ", "household_income")])
#because saliva has such a strong correlation with eachother i will exclude one of them. By checking the variance explained by each of the variables i will decide which one to delete.
#the assumptions are similar for the dataset with or without outliers so i will keep the outliers in the dataset
#checking one ´model with cortisol saliva vs. one model with cortisol serum
model2_saliva=lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_1clean)
summary(model2_saliva)
model2_serum=lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1clean)
summary(model2_serum)
####since the model with saliva explaines more variance than the model with serum, i will decide to leave saliva in the model and delete serum
model2_final=model2_saliva


#checking all the assumptions again for the new model without serum:
#1.normality
residuals_model2_final <- residuals(model2_final)
shapiro.test(residuals_model2_final)
hist(residuals_model2_final)
#2.linerity
residualPlots(model2_final)

#3.multicolinearity
#rule of thumb is about 5 so everything under is ok. 
vif(model2_final)
#4.homoscedasticity
#with residuals
plot(model2_final,1)
plot(model2_final,3)
bptest(model2_final)


#AIC for model 1 and 2
AIC(model1,model2_final) #lower score idicates which model is better
Anova (model1,model2_final) #no significant difference between the models
anova(model1, model2_final)
#confidence intervals:
confint(model1) #
confint(model2_final)

#model summaries
model2_final
summary(model2_final)
summary(model1)
model1
#model statistics Anova
anova(model2_final)

# loading the coefficient table for model 2

coef_table = function(model2_final){
  require(lm.beta)
  mod_sum = summary(model2_final)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3)) 
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"])) 
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model2_final), confint(model2_final), c(0, lm.beta(model2_final)$standardized.coefficients[c(2:length(model2_final$coefficients))])), 2)), mod_sum_p_values) 
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value") 
  mod_sum_table["(Intercept)","Std.Beta"] = "0" 
  return(mod_sum_table)
}

coef_table(model2_final)

#loading the coefficient table for model 1
coef_table1 = function(model1){
  require(lm.beta)
  mod_sum = summary(model1)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3)) 
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"])) 
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model1), confint(model1), c(0, lm.beta(model1)$standardized.coefficients[c(2:length(model1$coefficients))])), 2)), mod_sum_p_values) 
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value") 
  mod_sum_table["(Intercept)","Std.Beta"] = "0" 
  return(mod_sum_table)
}
coef_table1(model1)
