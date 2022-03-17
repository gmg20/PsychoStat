# Regression Tutorial: Predicting Turnover Intentions for Employees

# Original data downloaded from Yuan et al (2021) OSF page -> https://osf.io/yx2us/#!

## Amended data used here (and codebook) are uploaded to my GH page -> https://github.com/gmg20/PsychoStat
## Amended data contain both raw and mean-centered versions of all predictors

#################################################################################
# Housekeeping

options(scipen=99)

library(tidyverse)
library(effectsize)
library(olsrr)
library(rockchalk)
library(car)
library(rgl)
library(psych)
library(ppcor)
library(rwa)
library(MBESS)
library(flextable)
library(lmtest)
library(dominanceanalysis)

# Load and View Data
dat1<-read.csv("turnover.csv") # Read Data
View(dat1) 

# Convert categorical variables to factors
dat1$gender_cent<-as.factor(dat1$gender_cent) # Gender 
levels(dat1$gender_cent)<-c("Men","Women")

dat1$gender_raw<-as.factor(dat1$gender_raw) # Gender 
levels(dat1$gender_raw)<-c("Men","Women")

dat1$cont_type_cent<-as.factor(dat1$cont_type_cent) # Temp vs Full-Time
levels(dat1$cont_type_cent)<-c("Temp","Perm")

dat1$cont_type_raw<-as.factor(dat1$cont_type_raw) # Temp vs Full-Time
levels(dat1$cont_type_raw)<-c("Temp","Perm")

dat1$manager_cent<-as.factor(dat1$manager_cent) # Manager Role?
levels(dat1$manager_cent)<-c("No","Yes")

dat1$manager_raw<-as.factor(dat1$manager_raw) # Manager Role?
levels(dat1$manager_raw)<-c("No","Yes")

dat1$educ_raw<-as.factor(dat1$educ_raw) # Education Level
dat1$educ_cent<-as.factor(dat1$educ_cent)

# Check to make sure all variables are as they should be
str(dat1) 

################################################################################
# Simple Regression -> Turnover Intent Predicted from Pay Satisfaction
##############################################################################
# check to see the univariate descriptive stats
describe(dat1$paysatis_raw)
describe(dat1$turnover)

# Correlation between  Numeric Variables (disregarding cat. vars)
corPlot(dat1[,-c(3,4,5,6,7,8,10,11)]) 
cor(dat1$turnover, dat1$paysatis_raw)

# Scatterplot of turnover intentions and pay satisfaction
p1<-ggplot(dat1, aes(paysatis_raw, dat1)) + geom_point() 
p1
# Add Regression Line
p1<-p1 + geom_smooth(method="lm",se=FALSE) 
p1 + ggtitle("Turnover Predicted From Pay Satisfaction")


# Create Regression Model
m1<-lm(turnover ~ paysatis_raw,data=dat1) # Fit model
summary(m1) # Get summary
confint(m1) # CI for unstd (raw) regression coefficient
effectsize(m1) # CI for standardized regression coeff.
ci.R2(R2=.1159, df.1=1, df.2=1452)

outreg(m1, type="html") # Makes HTML table for model summary

# Regression Diagnostics/Check Assumptions
plot(m1) # Base R

ols_plot_resid_qq(m1) # Prettier Diagnostics
ols_plot_resid_fit(m1)
ols_plot_resid_hist(m1)
ols_plot_cooksd_chart(m1)
ols_plot_resid_stud(m1)

##################################################################################
#####################################################################################

# Multiple Regression -> Turnover Intent Predicted from Pay Satis + Career Opportunity

######################################################################################
###################################################################################

# Correlation Plot of Numeric Variables
corPlot(dat1[,-c(3,4,5,6,7,8,10,11)]) 

# Scatterplot of Turnover ~ CareerOpp by itself
p2<-ggplot(dat1, aes(careeropp_raw, turnover)) + geom_point()
p2<-p2 + geom_smooth(method="lm",se=FALSE)
p2 + ggtitle("Turnover Predicted from Career Opportunity")

# Multiple Regression Model Starts
m2<-lm(turnover ~ paysatis_raw + careeropp_raw,data=dat1) # Fit Model
summary(m2) # Summary 

confint(m2) # Raw/Unstd Regression Coefficients
effectsize(m2) # Standardized Regression Coefficients

ci.R2(R2=.1885, df.1=2, df.2=1451) # 95% CI for R2

# Effect Sizes
# (Squared semi-partial correlation)

# Method 1 for semipartial square (EASIEST)
aovm2<-Anova(m2, type=3)
eta_squared(aovm2, partial=FALSE)

# Method 2 for semipartial square
semicorr<-spcor(dat1[,c("turnover","paysatis_raw","careeropp_raw")]) 
semicorr # semicorr matrix
semicorrsq<-semicorr$estimate^2 
semicorrsq["turnover",] # semicorr squared matrix


# Get 3D Regression Plane -> Super Cool!
scatter3d(turnover ~ paysatis_raw + careeropp_raw, data=dat1)

outreg(m2, type="html") # Get model output to table

## Does Career Opportunity predict Turnover Intentions above and beyond Pay Satisfaction?
anova(m2, m1)

# Relative Weights 
dat1 %>% rwa(outcome = "turnover",predictors = c("paysatis_raw", "careeropp_raw"))

# Regression Diagnostics

plot(m2) # Base R Diagnostics
 
ols_plot_resid_qq(m2) # Prettier Diagnostics
ols_plot_resid_fit(m2)
ols_plot_resid_hist(m2)
ols_plot_cooksd_chart(m2)
ols_plot_resid_stud(m2)

ncvTest(m2) # Test homogeneity of error variance assumption
coeftest(m2) # sandwich estimator if error variance non-constant

vif(m2) #Multicollinearity

#################################################################################
##################################################################################

# Adding Categorical Predictors 

#################################################################################
#################################################################################

# Example 1: 1 Categorical Predictor Only
# Turnover Predicted From ManagerStatus

# Descriptives by Manager Group
describeBy(dat1$turnover, dat1$manager_raw) 

# Side-by-side boxplot of Turnover Intent by Manager Group
p3a<-ggplot(dat1, aes(manager_raw, turnover, color=manager_raw, fill=manager_raw))+geom_boxplot(alpha=0.3)
p3a

# Regression model for T/O intent Predicted From Manager Status

m3a<-lm(turnover ~ manager_raw, data=dat1) # Fit Model

contrasts(dat1$manager_raw) # Dummy Coding Check

summary(m3a) # Summary

describeBy(dat1$turnover, dat1$manager_raw) # Compare mean diffs to model output

confint(m3a) # raw slope (unstandardized)

effectsize(m3a) # standardized slope and CI

###################################################################################

# Example 1b -> 1 Categorical Predictor Only with more than 2 groups/factor levels
# Turnover Predicted From Education Level

m3b<-lm(turnover ~ educ_raw, data=dat1) # Fit Model

contrasts(dat1$educ_raw) # Check dummy coding

summary(m3b) # Model Summary

describeBy(dat1$turnover, dat1$educ_raw) # Compare mean diffs to model output

###################################################################################
# Example 1c -> Relevel Factor to Change Reference Group

# In Ex.1b above, the reference group is Education Level Zero (No College Degree)
# We can relevel this factor to make Year 7 (PhD) the reference group

dat1$educ_raw<-relevel(dat1$educ_raw, ref="7")

contrasts(dat1$educ_raw) # Check Contrast/Dummy Code

m3b2<-lm(turnover ~ educ_raw, data=dat1) # Fit model again

summary(m3b2) # Summary Results

describeBy(dat1$turnover, dat1$educ_raw) # Compare mean diffs to model output

dat1$educ_raw<-relevel(dat1$educ_raw, ref="0") # Set back to ref group 0

contrasts(dat1$educ_raw) # Check Contrast/Dummy Code went back
################################################################################

# Example 2: -> 1 Categorical Predictor and 1 Continuous Predictor
# Turnover Predicted From Pay Sat + ManagerStatus


# Scatterplot of both predictors
p3c<-ggplot(dat1, aes(paysatis_raw, turnover, color=manager_raw)) + geom_point()
p3c + geom_smooth(method="lm",se=FALSE)


# Regression Model with both categorical and continuous
m3c<-lm(turnover ~ paysatis_raw + manager_raw, data=dat1) # Fit Model
summary(m3c) # Summary

confint(m3c) # Unstandardized Confidence Intervals 
effectsize(m3c) # Standardized CI

# Effect Sizes (Squared Semi-partial correlations)
aovm3c<-Anova(m3c, type=3)
eta_squared(aovm3c, partial=FALSE)

outreg(m3c, type="html") # Get model output to table

## Does ManagerStatus predict T/O Intent above & beyond PaySatis?
anova(m3c, m1) # No it doesn't!

# Dominance Analysis (Similar to Relative Weights, which only take numeric)
# Note: This really isn't necessary for a situation with only 2 predictors,
# but this is how you would perform it

da_m3c<-dominanceAnalysis(m3c)
print(da_m3c)

# Regression Diagnostics

plot(m3c, ask=FALSE) # Base R Diagnostics

ols_plot_resid_qq(m3c) # Prettier Diagnostics
ols_plot_resid_fit(m3c)
ols_plot_resid_hist(m3c)
ols_plot_cooksd_chart(m3c)
ols_plot_resid_stud(m3c)

#################################################################################
#################################################################################

# Mean-Centering Predictors

#################################################################################
#################################################################################
# How to mean-center a predictor if you don't already have it centered in your data
dat1$ageCENTER<-scale(dat1$age, scale=FALSE)
describe(dat1$ageCENTER)

# Example 3: -> Simple Regression with 1 Mean Centered Continuous Predictor 
# Turnover Intent Predicted from Pay Satisfaction

# Scatterplot of turnover intentions and mean-centered pay satisfaction
p1cent<-ggplot(dat1, aes(paysatis_cent, turnover)) + geom_point() 

# Add Regression Line
p1cent<-p1cent + geom_smooth(method="lm",se=FALSE) 
p1cent + ggtitle("Turnover Predicted From Pay Satisfaction (Mean-Centered")


# Create Regression Model
m1cent<-lm(turnover ~ paysatis_cent,data=dat1) # Fit model

summary(m1cent) # Get summary

confint(m1cent) # CI for unstd (raw) regression coefficient

effectsize(m1cent) # CI for standardized regression coeff.

ci.R2(R2=.1159, df.1=1, df.2=1452)

outreg(m1cent, type="html") # Makes HTML table for model summary

# Regression Diagnostics/Check Assumptions
plot(m1cent, ask=FALSE)

ols_plot_resid_qq(m1cent) # Prettier Diagnostics
ols_plot_resid_fit(m1cent)
ols_plot_resid_hist(m1cent)
ols_plot_cooksd_chart(m1cent)
ols_plot_resid_stud(m1cent)

##################################################################################
###################################################################################
# Interaction Terms
##################################################################################
################################################################################


# Example 2b: -> 1 Categorical Predictor and 1 Continuous Predictor
# Fairness Predicted from CareerOpp + ManagerStatus

# Scatterplot 
p4a<-ggplot(dat1, aes(turnover, fairness_raw, color=manager_raw))+geom_point()
p4a + geom_smooth(method="lm",se=FALSE)

m4<-lm(fairness_raw ~ careeropp_raw*manager_raw,dat1) # Fit Model
summary(m4) # Summary

confint(m4) # Unstd CI
effectsize(m4) # Std CI

# Diagnostics
plot(m4)

##################################################################################

# Example 2c: -> 1 Categorical Predictor and 1 Continuous Predictor
# Fairness Predicted from PaySatis + Gender

p5a<-ggplot(dat1, aes(paysatis_raw, fairness_raw, color=gender_cent)) + geom_point()
p5a + geom_smooth(method="lm",se=FALSE)

m5<-lm(fairness_raw ~ paysatis_raw*gender_raw, data=dat1)
summary(m5)

confint(m5)
effectsize(m5)