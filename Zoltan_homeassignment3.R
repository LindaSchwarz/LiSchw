######--HOMEASSIGNMENT 3--#######

rm(list=ls(all=TRUE)) # clears the workspace
graphics.off()

library(psych) 
library(tidyverse) 
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(influence.ME)
library(lattice) 
library(dplyr)
#get data3
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")
View(data_sample_3)

#get data4
data_sample_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")
View(data_sample_4)

#checking the data:
str(data_sample_3)	
str(data_sample_4)	

data_sample_3 %>% 	
  summary()	

data_sample_4 %>% 	
  summary()	

describe(data_sample_3)
describe(data_sample_4)

#R doesn't recognize the sex and hospital names, so i need to change them
#changing sex:
data_sample_3clean1 <- data_sample_3 %>% 
  mutate(
    sex = droplevels(replace(sex,sex == "Female", "female")), 
    sex = droplevels(replace(sex,sex == "Male", "male"))
    )
#viewing and making sure it changed
str(data_sample_3)	

#change hospital names to numeric
data_sample_3clean2 <- data_sample_3clean1 %>%
  mutate(hospital = recode(hospital,	
                           "hospital_1" = 1,	
                           "hospital_2" = 2,	
                           "hospital_3" = 3,	
                           "hospital_4" = 4,	
                           "hospital_5" = 5,	
                           "hospital_6" = 6,
                           "hospital_7" = 7,
                           "hospital_8" = 8,
                           "hospital_9" = 9,
                           "hospital_10" =10,
  ))

#
#creating a model - clustering it for hospital 
mod_ind_hosp =lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_saliva 
                   + mindfulness + (1|hospital), data = data_sample_3clean2)
summary(mod_ind_hosp)
?mutate


#TESTING THE ASSUMPTIONS and FOR OUTLIERS

#creating residuals for it
data_sample_3clean2 = data_sample_3clean2 %>% mutate(resid = residuals(mod_ind_hosp))

#TESTING FOR OUTLIERS
influence_observation = influence(mod_ind_hosp, obs = T)$alt.fixed
influence_group = influence(mod_ind_hosp, group = "hospital")$alt.fixed
View(influence_group)
View(influence_observation)

#plotting the outliers
data_plot_influence = as_tibble(influence_group) %>% gather(colnames(influence_group),
value = coefficient, key = predictor)
data_plot_influence %>% ggplot() + aes(x = 1, y = coefficient,
group = predictor) + geom_violin() + facet_wrap(~predictor,
scales = "free")

#####checking for NORMALITY######
#normality of fixed effects
qqmath(mod_ind_hosp, id = 0.05)

#quecking normality for random effects
qqmath(ranef(mod_ind_hosp))

#######checking for LINEARITY######
plot(mod_ind_hosp, arg = "pearson")
#look at the scatterplot of the residuals and the fixed predictors separately
data_sample_3clean2 %>% ggplot() + aes(x = hospital, y = resid) +
  geom_point()
data_sample_3clean2 %>% ggplot() + aes(x = STAI_trait, y = resid) +
  geom_point()
data_sample_3clean2 %>% ggplot() + aes(x = sex, y = resid) +
  geom_point()
data_sample_3clean2 %>% ggplot() + aes(x = age, y = resid) +
  geom_point()
data_sample_3clean2%>% ggplot() + aes(x = mindfulness, y = resid) +
  geom_point()
data_sample_3clean2%>% ggplot() + aes(x = cortisol_saliva, y = resid) +
  geom_point()
data_sample_3clean2 %>% ggplot() + aes(x = pain_cat, y = resid) +
  geom_point()


#######checking for HOMOSCEDASTICITY######
#plot
plot(mod_ind_hosp, arg = "pearson")
#run a significancde test on the model


IQR_of_residuals_by_hospital = sapply(split(data_sample_3clean2,
f = data_sample_3clean2$hospital), function(x) IQR(x$resid))
# rank ordering them
rank = rank(IQR_of_residuals_by_hospital)
# adding rank to the dataframe containing the residuals
data_sample_3clean2$rank = rep(rank, each = length(c("hospital_1", "hospital_2",
"hospital_3", "hospital_4", "hospital_5", "hospital_6", "hopsital_7", "hospital_8","hospital_9", "hospital_10")))

# creating a vector of participant IDs ordered based on the
# rank, this will be used as labels
hospitalforplot = unique(data_sample_3clean2$hospital[order(data_sample_3clean2$rank)])
ggplot(data_sample_3clean2, aes(y = resid, x = factor(rank), labels = hospital)) +
  geom_boxplot() + scale_x_discrete(labels = hospitalforplot) + coord_flip()

######checking for MULTICOLINEARITY#######
pairs.panels(data_sample_3clean2[, c("sex","hospital", "age",
"STAI_trait", "pain_cat", "cortisol_saliva", "mindfulness")], col = "red", lm = T)

#library(car)
vif(mod_ind_hosp)



###### ----CONDUCTIONG THE ANALYSIS----########
#random intercept model:
mod_ind_hosp =lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_saliva 
                   + mindfulness + (1|hospital), data = data_sample_3clean2)
#summary to get coefficients
summary(mod_ind_hosp)

#confidence intervals
confint(mod_ind_hosp)
#standardized beta 
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}
stdCoef.merMod(mod_ind_hosp)

#comparing coefficients and CI to the ones from model 1

#compute the variance explained by the fixed effect predictors using marginal R2:
r2beta(mod_ind_hosp, method = "nsj", data = data_sample_3clean2)	

#variance explained by the fixed and random effect terms combined using conditional R:
# marginal and conditional R squared values	
r.squaredGLMM(mod_ind_hosp)	
####checking the dataset 4#######
describe(data_sample_4)
summary(data_sample_4)
str(data_sample_4)

#changing the hospital number to numeric
data_sample_4 <- data_sample_4 %>%
  mutate(hospital = recode(hospital,	
                           "hospital_11" = 11,	
                           "hospital_12" = 12,	
                           "hospital_13" = 13,	
                           "hospital_14" = 14,	
                           "hospital_15" = 15,	
                           "hospital_16" = 16,
                           "hospital_17" = 17,
                           "hospital_18" = 18,
                           "hospital_19" = 19,
                           "hospital_20" =20,
  ))
#use the regression equation obtained on data file 3 to predict pain in data file 4:
####Testing new Dataset on Model 4#####

#predictions:
predictions = predict(mod_ind_hosp, newdata = data_sample_4, allow.new.levels=TRUE)
predictions

#creatining a meanmodel for RSS
meanmodel= lmer(pain ~ 1 + (1|hospital), data = data_sample_3clean2, REML=FALSE)

#compute the variance explained by the model on data file 4: 1-(RSS/TSS)
RSS = sum((data_sample_4$pain - predict(mod_ind_hosp, data_sample_4, allow.new.levels = TRUE))^2)	
TSS = sum((data_sample_4$pain - predict(meanmodel, data_sample_4, allow.new.levels = TRUE ))^2)
1-(RSS/TSS) #the result is basically an R^2 and then compare it to the other one

#Compare this R2 to the marginal and conditional R2 values computed for the model on data file 3

######################-----########################
#Build a new linear mixed effects model on dataset 3 predicting pain
mod_ind_hosp =lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_saliva 
                   + mindfulness + (1|hospital), data = data_sample_3clean2)
summary(mod_ind_hosp)

#looking at standardized coefficients
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}
stdCoef.merMod(mod_ind_hosp) # i pick cortisol saliva because it is higher in the coefficient table

#instead of including all predictors, you should only include the most influential predictor from the previous model
#Allow for both random intercept and random slope
#model allwoing for random intercept
mod_hosp_intercept =lmer(pain ~ cortisol_saliva + (1|hospital), data = data_sample_3clean2)
mod_hosp_intercept
#model allowing for random slope and intercep
mod_hosp_slope =lmer(pain ~ cortisol_saliva + (cortisol_saliva|hospital), data = data_sample_3clean2)
mod_hosp_slope
summary(mod_hosp_slope)

#visualize the fitted regression lines for each hospital separately
#create the data set to visualize it
data_hosp_slope = data_sample_3clean2 %>% mutate(pred_slope = predict(mod_hosp_slope))

#visualizing the data: for hospital
data_hosp_slope %>% ggplot() + aes(y = pain, x = cortisol_saliva,
group = hospital) + geom_point(aes(color = hospital), size = 4) +
geom_line(color = "red", aes(y = pred_slope, x = cortisol_saliva)) +
facet_wrap(~hospital, ncol = 2)

#only allwoing for random intercep
data_hosp_intercept = data_sample_3clean2 %>% mutate(pred_int = predict(mod_hosp_intercept))
data_hosp_intercept %>% ggplot() + aes(y = pain, x = cortisol_saliva,
group = hospital) + geom_point(aes(color = hospital), size = 4) +
  geom_line(color = "red", aes(y = pred_int, x = cortisol_saliva)) +
  facet_wrap(~hospital, ncol = 2)

cAIC(mod_hosp_intercept)$caic
cAIC(mod_hosp_slope)$caic

