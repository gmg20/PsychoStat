################################################################################
################################################################################

# Adding Interaction Terms (Moderation)

################################################################################
################################################################################

# Housekeeping

library(readr)
library(interactions)
library(sandwich)
library(knitr)
library(effectsize)
library(car)
library(olsrr)

dat1 <- read_csv("childiq.txt")

View(childiq)

# Make Categorical Var a Factor

dat1$mom_hs <- factor(dat1$mom_hs)


# Mean Center Continuous Predictors if not already

dat1$mom_work_cent <- scale(dat1$mom_work, scale = FALSE)

dat1$mom_work_cent <- scale(dat1$mom_work, scale = FALSE)

################################################################################

# Categorical Interaction: Child IQ ~ Mom HS + Mom IQ + Mom IQ*Mom HS

# Main Effects First

m1 <- lm(kid_score ~ mom_hs + mom_iq_cent, data = dat1) # Fit

summary(m1) # Summary

effectsize(m1) # Effect Size


# Add Interaction

m1b <- lm(kid_score ~ mom_hs*mom_iq_cent, data = dat1) # Fit


# Probe Interaction/Simple Slopes

interact_plot(m1b, pred = mom_iq_cent, 
              modx = mom_hs)


# Diagnostics

ols_plot_diagnostics(m1b)


################################################################################

# Continuous Predictors: Child IQ ~ Mom Work Hrs + Mom IQ + Mom IQ*Mom Work


# Main Effects First

m2<-lm(kid_score ~ mom_work_cent + mom_iq_cent, data=dat1) # Fit

summary(m2) # Summary

effectsize(m2) # Effect Size


# Add Interaction Term

m2b<-lm(kid_score ~ mom_work_cent*mom_iq_cent, data=dat1) # Fit

summary(m2b) # Summary

effectsize(m2b) # Effect Size


# Probe Interaction/Simple Slopes

sim_slopes(m2b, pred = mom_iq_cent, 
           modx = mom_work_cent, jnplot = TRUE)

interact_plot(m2b, pred = mom_iq_cent, 
              modx = mom_work_cent)


# Simple slopes with specified values for the moderator


interact_plot(m1, pred = mom_iq_cent, modx = mom_work_cent, 
              modx.values = c(-.896, 1.104))


# Diagnostics

ols_plot_diagnostics(m2b)
