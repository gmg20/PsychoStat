# Installing packages for the first time
################################################################################
# Housekeeping

install.packages("psych")
install.packages("tidyverse")

# Loading packages
library(psych)
library(tidyverse)

# Loading dataset 1
dat1<-read.csv("gender_study.csv")
View(dat1)

# Denote categorical variables as factor/grouping variables for dataset1
dat1$Gender<-as.factor(dat1$Gender)
levels(dat1$Gender)<-c("Men","Women")
dat1$Gender

# Loading dataset 1
dat2<-read.csv("C:/Users/gmg30/Desktop/R/R Work/PsychoStat/psych.csv")
View(dat2)

# Denote categorical variables as factor/grouping variables for dataset2
dat2$year<-as.factor(dat2$year)
levels(dat2$year)<-c("Freshman","Sophomores","Juniors","Seniors")
dat2$year

###############################################################################
# Descriptives for dataset1
describe(dat1) # Descriptives for all variables in dataset1
summary(dat1) # Additional descriptives

summary(dat1$StudyHrs)
describe(dat1$StudyHrs) # Descriptives for just the single variable

describeBy(dat1$StudyHrs, dat1$Gender) # Descriptives by group level (gender)
describeBy(dat2, dat2$year)

# Descriptives for dataset2
describe(dat2) # Descriptives for all variables in dataset2
describe(dat2$neurot) # Descriptives for just one variable (neuroticism)
describeBy(dat2$neurot, dat2$year) # Descriptives for neuroticism by group level (year)

###############################################################################
# Histograms

# Study Hours Overall 
hist(dat1$StudyHrs, main = "Study Hours Histogram")

# Study Hours Overall
p1<-ggplot(dat1, aes(StudyHrs)) + geom_histogram() 
p1
p1 + ggtitle("Histogram of Study Hrs")


# Study Hours by Group Level (Gender)
p1b<-ggplot(dat1,aes(StudyHrs, fill=Gender)) + geom_histogram()
p1b
p1b + ggtitle("Histogram of Study Hrs by Gender")


p1c<-ggplot(dat1, aes(StudyHrs,fill=Gender)) + geom_histogram() + facet_grid(~Gender)
p1c
p1c + ggtitle("Histogram of Study Hours by Gender")

################################################################################
# Boxplots of Study Hrs

# Simple Boxplots
boxplot(dat1$StudyHrs, main = "Study Hours Boxplot") # Study Hrs Overall
boxplot(dat1$StudyHrs ~ dat1$Gender, main = "Study Hours by Gender Boxplot") # By Gender

# Slightly Fancier Boxplots
p2<-ggplot(dat1,aes(StudyHrs)) + geom_boxplot() # Study Hrs Overall
p2
p2 + ggtitle("Study Hours Boxplot")

p2b<-ggplot(dat1, aes(Gender, StudyHrs, fill=Gender)) + geom_boxplot() # By Gender
p2b
p2b + ggtitle("Study Hours by Gender Boxplot")

p2c<-ggplot(dat1, aes(Gender, StudyHrs, fill=Gender)) + geom_boxplot() + facet_grid(~Gender)
p2c
p2c + ggtitle("Study Hours by Gender Boxplot")

#################################################################################
# Scatter Plots 

# Neuroticism by Stress Scatterplot without Groups
# Simple
plot(dat2$neurot ~ dat2$stress, main = "Neuroticism by Stress ScatterPlot")

# Slightly fancier
p3<-ggplot(dat2, aes(stress, neurot)) + geom_point()
p3
p3 + ggtitle("Neuroticism by Stress Scatterplot") # Scatterplot without groups

# Neuroticism by Stress by Group (School Year)

p3b<-ggplot(dat2, aes(stress, neurot, color=year)) + geom_point()
p3b
p3b + ggtitle("Neuroticism by Stress by School Year Scatterplot")

p3c<-ggplot(dat2, aes(stress, neurot, color=year)) + geom_point() + facet_grid(~year)
p3c
p3c + ggtitle("Neuroticism by Stress by School Year Scatterplot")

p3d<-p3b<-ggplot(dat2, aes(stress, neurot, color=year)) + geom_point() + geom_smooth(method="lm",se=FALSE)
p3d
p3d + ggtitle("Neuroticism by Stress by School Year with Regression Line")

p3e<-ggplot(dat2, aes(stress, neurot, color=year)) + geom_point() + geom_smooth(method="lm",se=FALSE) + facet_grid(~year)
p3e
################################################################################