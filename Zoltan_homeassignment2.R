###### Home assignemnt 2#####
#by Linda Schwarz

rm(list=ls(all=TRUE)) # clears the workspace
graphics.off()


data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")
view(data_sample_1)

library(psych) 
library(gsheet) 
library(tidyverse) 
library(carData)
library(car)

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
view(data_sample_1)

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
model3 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness  + cortisol_serum + weight +IQ + household_income, data = data_sample_1clean)
model3

#calculation Cooks Distance and visualizing it in a plot
cooksd <- cooks.distance(model3)
sample_size <- nrow(data_sample_1clean)
plot(cooksd, pch="°", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 4/sample_size, col="blue")  
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="blue") 
view(cooksd)

#deleting the outliers from the cooks distance to be able to test the regression with and without
data_sample_1nooutlier3=data_sample_1clean
data_sample_1nooutlier3=data_sample_1nooutlier3[!cooksd> (4/sample_size),]
view(data_sample_1nooutlier3)

#creating a model on the dataset without outliers
model3_nooutliers <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1nooutlier3)
model3_nooutliers

#model summary of both models to check if they differ significantly
summary (model3)
summary(model3_nooutliers)
#Rsquared does not differ a lot for the two models, if the outliers are included or not.We will check the assumptions
#p values vary slightly

########CHECKING FOR ASSUMPTIONS########
#normality with outliers
residuals_model3 <- residuals(model3)
shapiro.test(residuals_model3)
hist(residuals_model3)
#without outliers
residuals_nooutliers_model3 <- residuals(model3_nooutliers)
shapiro.test(residuals_nooutliers_model3)
hist(residuals_nooutliers_model3)

#homoscedasticity
#with outliers
plot(model3,1)
plot(model3,3)
library(lmtest)
bptest(model3)
#without outliers
plot(model3_nooutliers,1)
plot(model3_nooutliers,3)
bptest(model3_nooutliers)

#linearity
#with outliers
residualPlots(model3)
#without outliers
residualPlots(model3_nooutliers)
#Tukey needs to be non-sign. to be fine

#multicolinearity
#without outliers:
#correlation table:
vif(model3)

#with outliers: #rule of thumb is about 5 so everything under is ok
#correlation table:
vif(model3_nooutliers)
#keeping the outliers because the assumptiuns don't differ too much, but there are slight differences
#although it ia very close i will still keep the outliers

#####RUNNING THE BACKWARDS REGRESSION######
step(model3, direction= "backward")
#get the model from the output:
backward_model3 =lm(formula = pain ~ sex + age + pain_cat + mindfulness + cortisol_serum + 
     weight, data = data_sample_1clean)
summary(backward_model3)

######COMPARING the initial model of the backward regression with backward model 3########
#initial model:
model3 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness  + cortisol_serum + weight +IQ + household_income, data = data_sample_1clean)
summary(model3)
#backward model:
summary(backward_model3)

#anova model3 and backwardmodel compared
anova(model3, backward_model3)

#AIC
AIC(model3, backward_model3)

######COMPARING the final model of assignment 2 with backward model 3########
#model 2
model2_final=lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_1clean)
summary(model2_final)
#backward model 3 
summary(backward_model3)
#getting coefficients for backward model
coef_table_backward = function(backward_model3){
  require(lm.beta)
  mod_sum = summary(backward_model3)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3)) 
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"])) 
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(backward_model3), confint(backward_model3), c(0, lm.beta(backward_model3)$standardized.coefficients[c(2:length(backward_model3$coefficients))])), 2)), mod_sum_p_values) 
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value") 
  mod_sum_table["(Intercept)","Std.Beta"] = "0" 
  return(mod_sum_table)
}
coef_table_backward(backward_model3)

#ANOVA TO COMPARE BOTH MODELS
anova(model2_final, backward_model3)

#AIC for both models
AIC(model2_final, backward_model3)

########-getting data set 2-########
data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
data_sample_2

###Testing new Dataset on Model 2
sex =c("female","female","male", "female")
age = c(45,50,43, 36)
STAI_trait = c(45,44,41,40)
pain_cat = c(35,33,32,30)
cortisol_saliva= c(5.4,4.1,4.1, 5.5)
mindfulness= c(1.9,4.0,2.4, 2.6)

new_data = as_tibble(sex,age,STAI_trait,pain_cat,cortisol_saliva, mindfulness)
predictions = predict(model2_final, newdata = new_data)

new_data_with_predicted = cbind(new_data, predictions)
new_data_with_predicted

####Testing new Dataset on Model 3#####
backward_model3 =lm(formula = pain ~ sex + age + pain_cat + mindfulness + cortisol_serum + weight, data = data_sample_1clean)

######NEW PREDICTIONS:
#for backward model:
predictions = predict(backward_model3, newdata = data_sample_2, allow.new.levels=TRUE)
predictions
#for model 2 final:
predictions = predict(model2_final, newdata = data_sample_2, allow.new.levels=TRUE)
predictions
#create a theory model to compare it to!!!
#for these 4 values both models are off by a lot


######calculationg differences in the Models
#Model 2
RSS = sum((data_sample_2$pain - predict(model2_final))^2)
RSS
RAD = sum(abs(data_sample_2$pain - predict(model2_final)))
RAD
#Model 3
RSS = sum((data_sample_2$pain - predict(backward_model3))^2)
RSS
RAD = sum(abs(data_sample_2$pain - predict(backward_model3)))
RAD
#because the backwards model has lower errors it is better


