# add libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(extrafont)
library(reshape2)

# read csv
gunDeath <- read.csv('/Users/hwpark/workspace/R-projects/guns-R/data/guns.csv', na.strings = c("", "NA"), stringsAsFactors = F)

# settings
gunDeath$intent <- factor(gunDeath$intent)
gunDeath$year <- factor(gunDeath$year)
gunDeath$age_group <- factor(findInterval(gunDeath$age, c(0, 20, 40, 60, 80, 110)))
levels(gunDeath$age_group) <- c("0-20","20-39", "40-59", "60-79", "80-109")

# 1. Intent of Gun death year wise
ggplot(gunDeath, aes(x = year)) +
  geom_bar(aes(fill = intent), position = "stack", alpha = 0.9) +
  labs(title = "Intent of Gun Deaths", x = "Year", y = "Deaths")
# Suicide Leads the intent of Gun death, followed by Homicide, and Accidental. Over the years there has been increase in the number of suicide deaths

# 2. Gender wise Gun death between 2012 - 2014
ggplot(gunDeath, aes(x = year)) +
  geom_bar(aes(fill = sex), position = "stack", alpha = 0.9) +
  labs(title = "Gender wise Gun deaths", x = "Year", y = "Deaths")

# 3. Intent of Gun death categorized by Age
ggplot(subset(gunDeath, !is.na(age_group)), aes(x = reorder(age_group, age_group, function(x) - length(x)))) +
  geom_bar(aes(fill = intent), position = "dodge") +
  labs(title = "Intent by age category", x = "Age groups", y = "Number of deaths")
# Suicide and Homicide are the primary reasons behind Gun deaths

# 4. Race of Dead(Gun death) categorized by Age
ggplot(subset(gunDeath, !is.na(age_group)), aes(x = reorder(age_group, age_group, function(x) - length(x)))) +
  geom_bar(aes(fill = race), position = "dodge") +
  labs(title = "Race by age category", x = "Age groups", y = "Number of deaths")
# In Age group(0-20)Blacks lead while Whites are catching up fast. For rest of the age groups leader is White

# 5. Gender of Dead(Gun death) categorized by Age
ggplot(subset(gunDeath, !is.na(age_group)), aes(x = reorder(age_group, age_group, function(x) - length(x)))) +
  geom_bar(aes(fill = sex), position = "dodge") +
  labs(title = "Gender by age category", x = "Age groups", y = "Number of deaths")

# 6. Place of Dead(Gun death) categorized by Age
ggplot(subset(gunDeath, !is.na(age_group)), aes(x = reorder(age_group, age_group, function(x) - length(x)))) +
  geom_bar(aes(fill = place), position = "dodge") +
  labs(title = "Place by age category", x = "Age groups", y = "Number of deaths")

# 7. Finding the leading cause of Gun deaths
ggplot(gunDeath, aes(x = reorder(intent, intent, function(x) length(x)))) +
  geom_bar(aes(fill = intent), stat = "count") +
  labs(title = "Leading cause of gun death? Suicide", x = "Causes of death", y = "Number of deaths") +
  coord_flip() +
  guides(fill = F)

# 8. Finding the gender most vulnerable to Gun deaths
ggplot(gunDeath, aes(x = factor(1), fill = factor(sex))) +
  geom_bar(width = 3) +
  coord_polar(theta = "y") +
  labs(title = "Distribution (of deaths) by Gender", x = NULL, y = NULL)

# 9. Finding the race most vulnerable to Gun deaths
ggplot(gunDeath, aes(x = factor(1), fill = factor(race))) +
  geom_bar(width = 3) +
  coord_polar(theta = "y") +
  labs(title = "Distribution (of deaths) by Race", x = NULL, y = NULL)

# 10. Finding the age group most vulnerable to Gun deaths
ggplot(gunDeath, aes(x = factor(1), fill = factor(age_group))) +
  geom_bar(width = 3) +
  coord_polar(theta = "y") +
  labs(title = "Distribution (of deaths) by Age Group", x = NULL, y = NULL)

# Age group(20-40) is most vulnerable to Gun Death followed by Age group 40-60 years

# 1 2 3 4 6 / 8 9 10
