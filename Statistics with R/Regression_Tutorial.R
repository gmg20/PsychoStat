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
turnover<-read.csv("turnover.csv") # Read Data
View(turnover) 

# Convert categorical variables to factors
turnover$gender_cent<-as.factor(turnover$gender_cent) # Gender 
levels(turnover$gender_cent)<-c("Men","Women")

turnover$gender_raw<-as.factor(turnover$gender_raw) # Gender 
levels(turnover$gender_raw)<-c("Men","Women")

turnover$cont_type_cent<-as.factor(turnover$cont_type_cent) # Temp vs Full-Time
levels(turnover$cont_type_cent)<-c("Temp","Perm")

turnover$cont_type_raw<-as.factor(turnover$cont_type_raw) # Temp vs Full-Time
levels(turnover$cont_type_raw)<-c("Temp","Perm")

turnover$manager_cent<-as.factor(turnover$manager_cent) # Manager Role?
levels(turnover$manager_cent)<-c("No","Yes")

turnover$manager_raw<-as.factor(turnover$manager_raw) # Manager Role?
levels(turnover$manager_raw)<-c("No","Yes")

turnover$educ_raw<-as.factor(turnover$educ_raw) # Education Level
turnover$educ_cent<-as.factor(turnover$educ_cent)

# Check to make sure all variables are as they should be
str(turnover) 

################################################################################
# Simple Regression -> Turnover Intent Predicted from Pay Satisfaction
##############################################################################
# check to see the univariate descriptive stats
describe(turnover$paysatis_raw)
describe(turnover$turnover)

# Correlation between  Numeric Variables (disregarding cat. vars)
corPlot(turnover[,-c(3,4,5,6,7,8,10,11)]) 
cor(turnover$turnover, turnover$paysatis_raw)

# Scatterplot of turnover intentions and pay satisfaction
p1<-ggplot(turnover, aes(paysatis_raw, turnover)) + geom_point() 
p1
# Add Regression Line
p1<-p1 + geom_smooth(method="lm",se=FALSE) 
p1 + ggtitle("Turnover Predicted From Pay Satisfaction")


# Create Regression Model
m1<-lm(turnover ~ paysatis_raw,data=turnover) # Fit model
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
#####################################################################################
# Multiple Regression -> Turnover Intent Predicted from Pay Satis + Career Opportunity
######################################################################################
# Correlation Plot of Numeric Variables
corPlot(turnover[,-c(3,4,5,6,7,8,10,11)]) 

# Scatterplot of Turnover ~ CareerOpp by itself
p2<-ggplot(turnover, aes(careeropp_raw, turnover)) + geom_point()
p2<-p2 + geom_smooth(method="lm",se=FALSE)
p2 + ggtitle("Turnover Predicted from Career Opportunity")

# Multiple Regression Model Starts
m2<-lm(turnover ~ paysatis_raw + careeropp_raw,data=turnover) # Fit Model
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
semicorr<-spcor(turnover[,c("turnover","paysatis_raw","careeropp_raw")]) 
semicorr # semicorr matrix
semicorrsq<-semicorr$estimate^2 
semicorrsq["turnover",] # semicorr squared matrix


# Get 3D Regression Plane -> Super Cool!
scatter3d(turnover ~ paysatis_raw + careeropp_raw, data=turnover)

outreg(m2, type="html") # Get model output to table

## Does Career Opportunity predict Turnover Intentions above and beyond Pay Satisfaction?
anova(m2, m1)

# Relative Weights 
turnover %>% rwa(outcome = "turnover",predictors = c("paysatis_raw", "careeropp_raw"))

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

##################################################################################
# Adding Categorical Predictors 
#################################################################################

# Example 1: -> Turnover Predicted From Pay Sat + ManagerStatus
# Let's get descriptive statistics by group first

describeBy(turnover$turnover, turnover$manager_raw)
describeBy(turnover$paysatis_raw, turnover$manager_raw)

# Side-by-side boxplot

p3a<-ggplot(turnover, aes(manager_raw, turnover, color=manager_raw, fill=manager_raw))+geom_boxplot(alpha=0.3)
p3a

p3b<-ggplot(turnover, aes(manager_raw, paysatis_raw, color=manager_raw, fill=manager_raw))+geom_boxplot(alpha=0.3)
p3b

# Scatterplot 
p3c<-ggplot(turnover, aes(paysatis_raw, turnover, color=manager_raw)) + geom_point()
p3c + geom_smooth(method="lm",se=FALSE)


# Regression model with just categorical
m3a<-lm(turnover ~ manager_raw, data=turnover) # Fit Model
contrasts(turnover$manager_raw) # Dummy Coding
summary(m3a) # Summary
describeBy(turnover$turnover, turnover$manager_raw) # Compare mean diffs to model output

# Example model with more than 2 groups/factor levels
m3b<-lm(turnover ~ educ_raw, data=turnover)
contrasts(turnover$educ_raw) # Illustrate dummy coding
summary(m3b)
describeBy(turnover$turnover, turnover$educ_raw) # Compare mean diffs to model output

# Regression Model with both categorical and continuous
m3c<-lm(turnover ~ paysatis_raw + manager_raw, data=turnover) # Fit Model
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
da_m3c<-dominanceAnalysis(m3c)
print(da_m3c)

# Regression Diagnostics

plot(m3c) # Base R Diagnostics

ols_plot_resid_qq(m3c) # Prettier Diagnostics
ols_plot_resid_fit(m3c)
ols_plot_resid_hist(m3c)
ols_plot_cooksd_chart(m3c)
ols_plot_resid_stud(m3c)

#################################################################################
# Adding a Categorical Predictor
# Example 2: -> Fairness Predicted from CareerOpp + ManagerStatus

# Scatterplot 
p4a<-ggplot(turnover, aes(careeropp, fairness, color=manager))+geom_point()
p4a + geom_smooth(method="lm",se=FALSE)

m4<-lm(fairness ~ careeropp*manager,turnover) # Fit Model
summary(m4) # Summary

confint(m4) # Unstd CI
effectsize(m4) # Std CI

# Diagnostics
plot(m4)
#################################################################################
# Adding a Categorical Predictor
# Example 3: -> Fairness Predicted from PaySatis + Gender
p5a<-ggplot(turnover, aes(paysatis, fairness, color=gender)) + geom_point()
p5a + geom_smooth(method="lm",se=FALSE)

m5<-lm(fairness ~ paysatis*gender, data=turnover)
summary(m5)

confint(m5)
effectsize(m5)

#################################################################################
# Mean-Centering Predictors
#################################################################################

################################################################################
# Simple Regression -> Turnover Intent Predicted from Pay Satisfaction
##############################################################################


# Scatterplot of turnover intentions and pay satisfaction
p1cent<-ggplot(turnover, aes(paysatis_cent, turnover)) + geom_point() 

# Add Regression Line
p1cent<-p1cent + geom_smooth(method="lm",se=FALSE) 
p1cent + ggtitle("Turnover Predicted From Pay Satisfaction (Mean-Centered")


# Create Regression Model
m1cent<-lm(turnover ~ paysatis_cent,data=turnover) # Fit model
summary(m1cent) # Get summary
confint(m1cent) # CI for unstd (raw) regression coefficient
effectsize(m1cent) # CI for standardized regression coeff.
ci.R2(R2=.1161, df.1=1, df.2=1452)

outreg(m1cent, type="html") # Makes HTML table for model summary

# Regression Diagnostics/Check Assumptions
plot(m1cent)
ols_plot_resid_qq(m1cent) # Prettier Diagnostics
ols_plot_resid_fit(m1cent)
ols_plot_resid_hist(m1cent)
ols_plot_cooksd_chart(m1cent)
ols_plot_resid_stud(m1cent)