# Housekeeping
options(scipen = 999)
library(lsr)
library(psych)
library(effsize)

# Set grouping variable as factor (tells R it's a grouping variable)
gender_study$Gender <- as.factor(gender_study$Gender)
levels(gender_study$Gender) <- c("Women", "Men")
gender_study$Gender

# Descriptives by Group Level
describeBy(gender_study$StudyHrs, gender_study$Gender)

# 2-sided independent samples t-test with separate variances
t.test(gender_study$StudyHrs ~ gender_study$Gender)

# 2-sided Indepndent samples t-test with equal variances (pooled variances)
t.test(gender_study$StudyHrs ~ gender_study$Gender, var.equal = TRUE)

# Paired-samples t-test
t.test(gender_study$StudyHrs ~ gender_study$Gender, paired = TRUE)

###############################################################################

# One-sided tests (just add alternative = "greater" or "less")
t.test(gender_study$StudyHrs ~ gender_study$Gender, alternative = "greater")

################################################################################

# Effect Sizes

# Independent samples pooled-variance Cohens D
cohensD(gender_study$StudyHrs ~ gender_study$Gender)

# Independent samples separate-variance Cohens D
cohensD(gender_study$StudyHrs ~ gender_study$Gender, method = "unequal")

# Paired-samples Cohens D
cohensD(gender_study$StudyHrs[gender_study$Gender == "Women"], gender_study$StudyHrs[gender_study$Gender == "Men"])
