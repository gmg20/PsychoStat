################################################################################
################################################################################

# One-way ANOVA & Multiple Comparisons

################################################################################
################################################################################

# Housekeeping & Data Prep

options(scipen = 999)

library(effect)
library(readr)
library(Rmisc)
library(car)
library(effectsize)
library(tidyverse)
library(lsr)
library(olsrr)
library(knitr)

# Load Data

dat1 <- read_delim("Age_recall_2.dat",
  delim = "\t",
  escape_double = FALSE, trim_ws = TRUE
)

View(dat1)

# Set Factors

dat1$Age <- factor(dat1$Age, levels = c(1, 2), labels = c("Young", "Old"))

dat1$Condition <- factor(dat1$Condition)

str(dat1)


# Crosstab Age ~ Condition

xtabs(~ dat1$Condition + dat1$Age)

################################################################################

# Visualization of Recall by Condition

# Overall ANOVA Visualization: One population mean or multiple?

par(mfrow = c(1, 2))

boxplot(dat1$Recall ~ dat1$Condition,
  xlab = "Task Condition", ylab = "Word Recall",
  main = "Word Recall by Condition"
)

boxplot(dat1$Recall, ylab = "Word Recall", main = "Overall Word Recall")

# Mean Recall by Condition with 95% confidence interval

ggplot(aov1_sumry, aes(x = Condition, y = Recall, colour = Condition, group = 1)) +
  geom_errorbar(aes(ymin = Recall - ci, ymax = Recall + ci), width = .1) +
  geom_line(color = "black") +
  geom_point() +
  ggtitle("Word Recall by Task Condition and 95% CI for Mean")

# Prettier Boxplots of Recall by Condition

ggplot(dat1, aes(Condition, Recall, color = Condition, fill = Condition)) +
  geom_boxplot(alpha = 0.3) +
  ggtitle("Word Recall by Task Condition")

################################################################################

# Initial Descriptive Statistics

# Means and SD by Condition Level with SE and CI

aov1_sumry <- summarySE(dat1, "Recall", "Condition", conf.interval = 0.90)

aov1_sumry

# Means and Variances (No SE or CI)

t1 <- aggregate(Recall ~ Condition, dat1, mean)
t2 <- aggregate(Recall ~ Condition, dat1, var)
names(t2)[2] <- "Var"
names(t1)[2] <- "Mean"
aov1_means_var <- join(t1, t2, by = "Condition")

aov1_means_var[2:3] <- round(aov1_means_var[2:3], digits = 2) # round to 2 digits

aov1_means_var

kable(aov1_means_var) # prettier output

################################################################################

# One-Way ANOVA Model Fit, Results, Effect Sizes

# Word Recall ~ Task Condition

aov1 <- aov(Recall ~ Condition, data = dat1) # Fit ANOVA object

fit1 <- Anova(mod = aov1, type = "III")

fit1 # View summary

kable(round(fit1, 2)) # Prettier summary

eta_squared(fit1, partial = FALSE) # Eta Squared

################################################################################

# Fitting the ANOVA as a Linear Regression Model

m1 <- lm(Recall ~ Condition, dat1) # Fit Regression Mod

summary(m1) # Summary of Results

aov1_sumry # Compare results to differences in means

fitlm1 <- Anova(m1, type = "III")

eta_squared(fitlm1, partial = FALSE) # Partial eta squared

################################################################################

# Multiple Comparisons with Posthoc Tests

posthocPairwiseT(x = aov1, p.adjust.method = "holm") # Holm Adjust

TukeyHSD(aov1) # Tukey HSD

#################################################################################

# Model Assumptions & Diagnostics

plot(aov1, ask = FALSE) # Have to use Base R (not fancy)

resid1 <- residuals(aov1) # pull residuals

hist(resid1) # histogram of residuals


# If heterogeneity of variance is strong, we can use Welch

aggregate(Recall ~ Condition, data = dat1, var) # Check homogeneity of variance

oneway.test(Recall ~ Condition, dat1) # Welch's separate-variances test


# If normality assumption severely violated, we can use Kruskal-Wallis

kruskal.test(Recall ~ Condition, data = dat1)

################################################################################
################################################################################

# Two-Way Factorial ANOVA (Equal Sample Sizes aka Balanced Design)

# Recall ~ Condition + Age -> (5 x 2 ANOVA)

################################################################################
################################################################################

# Boxplot of Main Effects

par(mfrow = c(1, 3))

boxplot(dat1$Recall ~ dat1$Age,
  xlab = "Age", ylab = "Word Recall",
  main = "Word Recall by Age"
)

boxplot(dat1$Recall, ylab = "Word Recall", main = "Overall Word Recall")

boxplot(dat1$Recall ~ dat1$Condition,
  xlab = "Task Condition", ylab = "Word Recall",
  main = "Word Recall by Condition"
)


# Side-by-side Boxplot 1

ggplot(dat1, aes(Condition, Recall, color = Age, fill = Age)) +
  geom_boxplot(alpha = 0.3) +
  ggtitle("Recall by Task Condition and Age")

# Side-by-side Boxplot 2

ggplot(dat1, aes(Age, Recall, color = Condition, fill = Condition)) +
  geom_boxplot(alpha = 0.3) +
  ggtitle("Recall by Task Condition and Age")

# Multi-factor Line Plots

interaction.plot(dat1$Condition, dat1$Age, dat1$Recall, mean)

################################################################################

# Descriptive Statistics

# Cell/Group Means and Variances

t1 <- aggregate(Recall ~ Condition + Age, data = dat1, mean)

t2 <- aggregate(Recall ~ Condition + Age, data = dat1, var)

aov2_means_var <- join(t1, t2, by = c("Condition", "Age"))

names(aov2_means_var)[3] <- "Mean"
names(aov2_means_var)[4] <- "Var"

aov2_means_var[3:4] <- round(aov2_means_var[3:4], digits = 2)

kable(aov2_means_var) # Fancier Summary


# Marginal Means

age_marg <- aggregate(Recall ~ Age, data = dat1, mean)
age_marg

cond_marg <- aggregate(Recall ~ Condition, data = dat1, mean)
cond_marg

# Grand Mean

mean(dat1$Recall)

################################################################################

# Fit 2-way Factorial ANOVA with Interaction Term

# Recall ~ Condition + Age

aov2 <- aov(Recall ~ Condition * Age,
  data = dat1,
  contrasts = list(Condition = "contr.sum", Age = "contr.sum")
) # Fit Model


fit2 <- Anova(aov2, type = "III")

fit2 # Model Summary

kable(round(fit2, digits = 2)) # Fancier Summary

eta_squared(fit2, partial = FALSE) # Eta Squared

################################################################################

# Test simple main effects because interaction significant

# Subset data by group level

Age.O <- subset(dat1, dat1$Age == "Old") # Old subset

Age.Y <- subset(dat1, dat1$Age == "Young") # Young subset



# Simple Main Effect of Condition for Old Folks

SME_cond_old <- aov(Recall ~ Condition, Age.O) # Fit SME old

# Simple Main Effect of Condition for Young Folks

SME_cond_young <- aov(Recall ~ Condition, Age.Y) # Fit SME young



# Compare Simple Main Effects & Eta squared

fit_SME_old <- Anova(SME_cond_old, type = "III") # Fit for Old Group

fit_SME_old # Summary

eta_squared(fit_SME_old, partial = FALSE) # Eta Squared


fit_SME_young <- Anova(SME_cond_young, type = "III") # Fit for Young Group

fit_SME_young

eta_squared(fit_SME_young, partial = FALSE) # Eta Squared


################################################################################

# Estimated Group Means

eff <- effect(term = "Condition*Age", mod = aov2)

print(eff) # Estimated Group Means

summary(eff) # Estimated Group Means with 95% CI

###############################################################################

# Posthoc Pairwise Tests if Desired

TukeyHSD(SME_cond_old)

TukeyHSD(SME_cond_young)

################################################################################

# Model Assumptions & Diagnostics

var2 <- aggregate(Recall ~ Condition + Age, data = dat1, var) # Homogeneity of Variance

names(var2)[3] <- "Var"

kable(var2) # Fancier variance table

plot(aov2, ask = FALSE) # Base R plot

resid2 <- residuals(aov2) # pull  residuals

hist(resid2) # histogram of residuals

################################################################################

# 2-way ANOVA as Regression Model

dat1$Condition2 <- as.numeric(dat1$Condition)

dat1$Age2 <- as.numeric(dat1$Age)

m2 <- lm(Recall ~ Condition * Age, data = dat1) # Fit Regression Model

summary(m2) # Summary of Results

print(eff) # Compare Model Summary to Estimated Group Means

eta_squared(m2, partial = FALSE) # semi-partial eta squared

################################################################################
################################################################################

# 2-way Factorial ANOVA Unbalanced Design -> Violations ~ State and Intoxication

################################################################################
################################################################################

# Load Data

dat2 <- read_delim("state_drunk_uneq.dat",
  delim = "\t",
  escape_double = FALSE, trim_ws = TRUE
)

View(dat2)

# Set Factors

dat2$State <- factor(dat2$State, levels = c(1, 2), labels = c("NC", "VA"))

dat2$Condition <- factor(dat2$Condition, levels = c(1, 2), labels = c("Sober", "Drunk"))

str(dat2) # Check Factors set Correctly

xtabs(~ State + Condition, dat2) # See Imbalance!

################################################################################

# Boxplot of Main Effects

par(mfrow = c(1, 3))

boxplot(dat2$DV ~ dat2$State,
  xlab = "State", ylab = " DV",
  main = " DV by State"
)

boxplot(dat2$DV, ylab = " DV", main = "Overall  DV")

boxplot(dat2$DV ~ dat2$Condition,
  xlab = "Task Condition", ylab = " DV",
  main = " DV by Condition"
)


# Side-by-side Boxplot 1

ggplot(dat2, aes(Condition, DV, color = State, fill = State)) +
  geom_boxplot(alpha = 0.3) +
  ggtitle("DV by Task Condition and State")

# Side-by-side Boxplot 2

ggplot(dat2, aes(State, DV, color = Condition, fill = Condition)) +
  geom_boxplot(alpha = 0.3) +
  ggtitle("DV by Task Condition and State")

# Multi-factor Line Plots

interaction.plot(dat2$State, dat2$Condition, dat2$DV, mean)

################################################################################

# Descriptive Statistics

# Cell/Group Means and Variances

t1 <- aggregate(DV ~ Condition + State, data = dat2, mean)

t2 <- aggregate(DV ~ Condition + State, data = dat2, var)

aov3_means_sd <- join(t1, t2, by = c("Condition", "State"))

names(aov3_means_sd)[3] <- "Mean"
names(aov3_means_sd)[4] <- "Var"

aov3_means_sd

# Marginal Means

State_marg <- aggregate(DV ~ State, data = dat2, mean)
State_marg

cond_marg <- aggregate(DV ~ Condition, data = dat2, mean)
cond_marg

# Grand Mean

mean(dat2$DV)

################################################################################

# Fit 2-way Factorial ANOVA with Interaction Term

# DV ~ Condition + State

aov3 <- aov(DV ~ Condition * State,
  data = dat2,
  contrasts = list(Condition = "contr.sum", State = "contr.sum")
) # Fit Model

fit3 <- Anova(aov3, type = "III")

fit3 # Results Summary

eta_squared(fit3, partial = FALSE) # Eta Squared

################################################################################

# Estimated Group Means

eff <- effect(term = "Condition*State", mod = aov3)

print(eff) # Estimated Group Means

State_marg # Compare to Marginal Means

summary(eff) # Estimated Group Means with 95% CI

###############################################################################

# Posthoc Pairwise Tests if Desired

TukeyHSD(aov3)

################################################################################

# Model Assumptions & Diagnostics

aggregate(DV ~ Condition + State, data = dat2, var) # Homogeneity of Variance

plot(aov3, ask = FALSE) # Base R plot

resid3 <- residuals(aov3) # pull  residuals

hist(resid3) # histogram of residuals

################################################################################
################################################################################
