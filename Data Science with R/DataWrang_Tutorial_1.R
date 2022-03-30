library(tidyverse)

df <- read.csv("turnover_full.csv")
df_b <- read.csv("hr.csv")

################################################################################
################################################################################

# Basics: Specify specific columns and/or rows using Base R (tidyverse shown later)

################################################################################
################################################################################

# df[row, column] is how we index/specify rows and/or columns

# First column of dataset (all rows included)

df[, 1]

# Multiple columns (all rows)
df[, 1:3]
df[, c(1, 2, 3)]

# Column by name (all rows)
df[, "code"]
df$code

# Multiple columns by name (all rows)
df[, c("code", "age")]

# All columns EXCEPT first column

df3 <- df[, -1]
head(df3)

# All columns EXCEPT first, second, and fourth columns
df3 <- df[, -c(1, 2, 4)]
head(df3)

# First Row and all Columns
df[1, ]

# First and ninth row, all columns
df[c(1, 9), ]

# First and Fifth row, third and fourth columns
df[c(1, 5), c(3, 4)]

# Rows 1:10, age and gender variables
df[c(1:10), c("age", "gender")]

################################################################################
################################################################################

# Basic Housekeeping

################################################################################
################################################################################

# Examine structure of data

head(x = df) # Gives us the first few rows
tail(x = df) # Last few rows

str(object = df) # Structure of variables in the dataset -> Important!

# Convert any categorical variables to factors
df$gender <- factor(df$gender)
df$manager <- factor(df$manager)

# If your categorical variables are coded numerically and you want to change that
levels(df$gender) <- c("Male", "Female")
levels(df$manager) <- c("No", "Yes")

# rename any specific variables
names(df)[1] <- "turnover"
head(df)

# make variable names lower case/upper case
names(df) <- toupper(names(df))
names(df)

names(df) <- tolower(names(df))
names(df)

# remove a variable from the dataframe
df <- df[, -38] # Single Variable removed (the 38th variable in this example)
head(df)

df <- df[, -c(37, 38, 39)] # Multiple Variables removed
head(df)

# Relocate variable columns

df <- df %>% relocate(caropp, .before = educ)
head(df)

################################################################################
################################################################################

# Tables/Percentages/Crosstabs of observations and proportions by groups/category

################################################################################
################################################################################

# Single Variable Table-> Employee Counts by Education level
table(df$educ)

# Single Variable Percentages -> Percentage Employees by Education Level
prop.table(table((df$educ))) * 100

# Contingency Table 2 Variables (Crosstabulation)-> Gender by Manager Crosstab
xtabs(~ gender + manager, data = df)

# 3 Variable Crosstab/Array -> Gender by Manager by Education Level
xtabs(~ gender + manager + educ, data = df)

# Proportions/Percentages for Multiple variables BY CELL
prop.table(xtabs(~ gender + manager, data = df))

# Proportions/Percentages for Multiple variables BY ROW (MARGINAL)
prop.table(xtabs(~ gender + manager, data = df), margin = 1)

# Proportions/Percentages for Multiple variables BY COLUMN (MARGINAL)
prop.table(xtabs(~ gender + manager, data = df), margin = 2)

# Proportions/Percentages for Multiple variables BY ARRAY ELEMENT (MARGINAL)
prop.table(xtabs(~ gender + manager + educ, data = df), margin = 3)

################################################################################
################################################################################

# Missing Data Basics

################################################################################
################################################################################

# Total number of missing values
sum(is.na(df))

# Check Missingness by Variable

df  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(), names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank()) + 
  ggtitle("Data Missingness Proportions by Variable")


# Delete all observations with missing values (listwise deletion)
df <- na.omit(df)
sum(is.na(df)) # No missing values anymore

################################################################################
################################################################################

# Selecting and Filtering with Tidyverse

################################################################################
################################################################################

# Simple filter by single condition -> Extract all employees
df_filt <- df %>% filter(gender == "Male")

# Simple filter by single condition and sort results
df_filt <- df %>%
              filter(gender == "Male") %>%
              arrange(desc(turnover)) # Descending order
head(df_filt)


df_filt <- df %>%                        # Ascending order
              filter(gender == "Male") %>%
               arrange(turnover) 

head(df_filt)

# Filter by multiple conditions where ALL must be true
df_filt <- df %>% 
              filter(gender == "Male" & manager == "Yes" & turnover < 3)
head(df_filt)

# Filter by multiple conditions where ONE or more must be true
df_filt <- df %>% 
              filter(manager == "Yes" | gender == "Male" | turnover < 3)
head(df_filt)

################################################################################
################################################################################

# Making new variables or altering current ones

################################################################################
################################################################################

# Alter current variable
df <- df %>% 
        mutate(age = age + 10)


# Make new numeric variable as function of old one (Age squared)
df <- df %>% 
        mutate(age_sq = age^2)


# Making a new variable based on text patterns in current variable
df <- df %>% 
        mutate(facility = substr(x = code, start = 1, stop = 2))

df <- df %>% 
        relocate(facility, .before = age)

df$facility <- factor(df$facility)

# Make a new categorical variable based on a continuous range -> This takes the
# continuous turnover variable and creates 4 risk categories based on whether the
# employees fall within certain ranges for their continuous turnover scores

df <- df %>%
        mutate(flight_risk = case_when(
                turnover <= 1.999 ~ "Very-Low-Risk",
                turnover > 1.999 & turnover <= 2.999 ~ "Low-Risk",
                turnover > 2.999 & turnover < 4 ~ "Moderate-Risk",
                TRUE ~ "High-Risk"
  ))

################################################################################
################################################################################

# Combining filtering, mutating, and arranging

################################################################################
################################################################################

# This filters for only employees at facility "LV" and then makes a new variable
# called Turnover Risk, where if they have a turnover intent level of 3 or greater,
# they are classified as High Risk. Then those results are sorted in descending
# order, such that the High Risk employees are listed first

highrisk_LV <- df %>%
                  filter(facility == "LV") %>%
                  mutate(TI_risk = ifelse(turnover >= 3, "HighRisk", "LowRisk")) %>%
                  arrange(desc(TI_risk))

head(highrisk_LV) 

# This filters for only Males over Age 10 and then makes a new variable called age
# squared by squaring the original age values. Then those results are sorted in
# descending order by the new age squared variable

oldman_agesq <- df %>%
                  filter(gender == "Male" & age > 10) %>%
                  mutate(age_sq = age^2) %>%
                   arrange(desc(age_sq))

head(oldman_agesq)

################################################################################
################################################################################

# Summarize, Filter/Summarize, GroupBy/Summarize/Mutate, Filter/GroupBy/Summarize

################################################################################
################################################################################

# Summarize -> Mean Turnover and Median Age

df %>% 
  summarize(mean_turnover = mean(turnover))

df %>% 
  summarize(middleage = median(age))


# Filter then Summarize -> mean turnover for just men
df %>%
  filter(gender == "Male") %>%
  summarize(men_avg_turnover = mean(turnover))

# Filter then Summarize -> mean turnover for only those over 10 years old
df %>%
  filter(age > 10) %>%
  summarize(mean_paysatisfaction = mean(paysatis))

# GroupBy then Summarize -> median pay satisfaction by gender
df %>%
  group_by(gender) %>%
  summarize(middle_paysat = median(paysatis))

# GroupBy then Multiple Summary Stats -> mean turnover and max age by facility
df %>%
  group_by(facility) %>%
  summarize(mean_turnover = mean(turnover), maxage = max(age))

# GroupBy then Multiple Summary -> mean turnover, max age, median pay satisf. by facility
df %>%
  group_by(facility) %>%
  summarize(
    mean_turnover = mean(turnover),
    maxage = max(age),
    middlepay = median(paysatis))


# Filter then Group then Summary -> Filter for TO > 3, calculate average paysatis by gender

df %>%
  filter(turnover >= 3) %>%
  group_by(gender) %>%
  summarize(mean_paysatis = mean(paysatis))

# Filter for TO > 3, calc. avg paysatis & avg fair by facility
df %>%
  filter(turnover >= 3) %>%
  group_by(facility) %>%
  summarize(avg_pay = mean(paysatis), avg_fair = mean(fair))


# Multiple GroupBys then summarize -> median pay satisfaction by facility and gender

df %>%
  group_by(facility, gender) %>%
  summarize(midpay = median(paysatis))

# GroupBy -> Summary Stats -> Filter based on those summary stats

# Calculate number of employees and average pay satisfaction by facility, then
# filter the results for only those facilities with over 10 employees and an
# average pay satisfaction above 2.0

df %>%
  group_by(facility) %>%
  summarise(n = n(), avgpaysat = mean(paysatis)) %>%
  filter(n > 10, avgpaysat > 2)

#################################################################################
################################################################################

# Use Group_By and summarize as way of plotting group summary stats

#################################################################################
################################################################################

# dataframe with avg pay satisfaciton for each facility
by_facility <- df %>%
  group_by(facility) %>%
  summarize(avgpay = mean(paysatis))

# Plot average pay satisfaction by facility
ggplot(data = by_facility, aes(x = facility, 
                               y = avgpay, 
                               color = facility, 
                               size = avgpay)) +
                                geom_point()

# Same thing but with avg turnover
by_facility_2 <- df %>%
                    group_by(facility) %>%
                    summarize(median_turnover = median(turnover))

ggplot(data = by_facility_2, aes(x = facility, 
                                 y = median_turnover, 
                                 color = facility, 
                                 size = median_turnover)) +
                                  geom_point()


by_fac_gender <- df %>%
                    group_by(facility, gender) %>%
                    summarize(midpay = median(paysatis))

ggplot(data = by_fac_gender, aes(x = facility, 
                                 y = midpay, 
                                 color = gender, 
                                 size = midpay)) +
                                  geom_point()

################################################################################
################################################################################

# Pivot Longer and Pivot Wider

################################################################################
################################################################################

# HR1:HR9 represent 9 different responses to an HR survey question
# Currently each row represents one employee, and there are 9 columns for each
# employee row representing that single employee's 9 different HR survey responses

# Use Pivot longer to have a row for each HR response, so now each employee will
# have 9 different rows. The other columns for that employee will be duplicated
# across the 9 rows.

df2 <- df %>%
          pivot_longer(cols = hr1:hr9,
                      names_to = "HR",
                      values_to = "rating")

head(df2)

# Now we decide this is awful. Let's go back to 1 row per employee. We need to tell
# R how we want to handle the duplicated values in the other columns. We resolve this
# by using value_fn = mean. Note: You might not need to specify this for your data.

df2 <- df2 %>% 
            pivot_wider(names_from = HR, 
                        values_from = rating, 
                         values_fn = mean)


# Example 2 Using HR dataset -> Identify columns containing the phrase "Rate"

head(df_b) # Just to see what it currently looks like

a <- colnames(df_b)[grepl("Rate", colnames(df_b))]
a

# All three of these columns are pay rates, so we decide to use long form.
# After doing so, each employee will have three rows (all the other cols are dupes)

df_b2 <- df_b %>% 
              pivot_longer(cols = all_of(a), 
                           names_to = "PayFrequency", 
                           values_to = "PayAmount")
head(df_b2)

# Let's go back to wide form because we want each employee to have one row after all

df_b2 <- df_b2 %>% 
                pivot_wider(names_from = PayFrequency, 
                            values_from = PayAmount)

#################################################################################
#################################################################################

# Getting Substrings and/or matching patterns

#################################################################################
#################################################################################

# Get first two letters of company ID code and make new facility variable

df <- df %>% 
        mutate(facility = str_sub(code, start = 1, stop = 2))

# Subset for only the rows where the facility matches "RK"
df[grep(pattern = "RK", df$facility), ]

# Select only the columns that start with 'HR'
df3 <- df %>% 
          select(starts_with("HR"))

head(df3)


# Select only the columns that end with 'over'
df3 <- df %>% 
          select(ends_with("over"))

head(df3)

#################################################################################
#################################################################################

# Alternate Methods Using Base R

################################################################################
################################################################################

# Group and Summarize -> Average Daily Pay by Department
aggregate(x = df_b$DailyRate, by = list(df_b$Department), FUN = mean)

# Group and Summarize -> Median Turnover Intent by Facility
aggregate(x = df$turnover, by = list(df$facility), FUN = median) # Method 1

tapply(X = df$turnover, INDEX = df$facility, FUN = median) # Method 2

# Sort in ascending/descending order
sort(x = df$turnover, decreasing = FALSE)


# Make new column as function of old column -> Mean Center Original Turnover Variable
df$turnover_centered <- df$turnover - mean(df$turnover)
head(df$turnover_centered)

# Make new column as function of old column -> Age Squared
df$age <- df$age^2
head(df$age)

# Filtering Data -> employees age over 10, all columns
a <- df[df$age > 10, ] # Method 1
a

a <- subset(x = df, subset = age > 10) # Method 2
a

# Filtering Data 0=-> all employees, just the age and turnover columns
a <- df[, c("age", "turnover")]
a

# Filtering  -> employees age over 10, just the age and turnover columns
a <- df[df$age > 10, c("age", "turnover")] # Method 1
a

a <- subset(df, subset = age > 10, select = c("age", "turnover")) # Method 2
a

# Filtering and Summarize -> Mean pay satisfaction for employees whose turnover intent > 3

mean(df[df$turnover > 3, ]$paysatis) # Method 1

mean(subset(df$paysatis, df$turnover > 3)) # Method 2
