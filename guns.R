# add libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(extrafont)
library(reshape2)

# read csv
gunDeath <- read.csv('/Users/hwpark/workspace/R-projects/guns-R/data/guns.csv', na.strings = c("", "NA"), stringsAsFactors = F)

# settings
text_theme <- theme(text = element_text(size = 10, 
                                        family = "Verdana", 
                                        face = "italic"),
                    plot.title = element_text(hjust = 0.5))
color_theme <- scale_fill_brewer(palette = "RdYlBu", 
                                 na.value = "black")

gunDeath$intent <- factor(gunDeath$intent)
gunDeath$year <- factor(gunDeath$year)

gunDeath$age_group <- factor(findInterval(gunDeath$age, 
                                          c(0,20, 40, 60, 80, 110)))

levels(gunDeath$age_group) <- c("0-20","20-39", "40-59", "60-79", "80-109")

# Intent of Gun death year wise
ggplot(gunDeath, aes(x = year)) +
  geom_bar(aes(fill = intent), 
           position = "stack",
           alpha = 0.9) +
  text_theme +
  color_theme +
  labs(title = "Intent of Gun Deaths", 
       x = "Year", 
       y = "Deaths")
# Suicide Leads the intent of Gun death, followed by Homicide, and Accidental. Over the years there has been increase in the number of suicide deaths


# Gender wise Gun death between 2012 - 2014
ggplot(gunDeath, aes(x = year)) +
  geom_bar(aes(fill = sex), 
           position = "stack",
           alpha = 0.9) +
  text_theme +
  color_theme +
  labs(title = "Gender wise Gun deaths", 
       x = "Year", 
       y = "Deaths")
# Intent of Gun death categorized by Age
ggplot(subset(gunDeath, !is.na(age_group)), 
       aes(x = reorder(age_group,
                       age_group,
                       function(x) -length(x)))) +
  geom_bar(aes(fill = intent),
           position = "dodge") +
  text_theme +
  color_theme +
  labs(title = "Intent by age category", 
       x = "Age groups", 
       y = "Number of deaths")
# Suicide and Homicide are the primary reasons behind Gun deaths

# Race of Dead(Gun death) categorized by Age
ggplot(subset(gunDeath, !is.na(age_group)), 
       aes(x = reorder(age_group,
                       age_group,
                       function(x) -length(x)))) +
  geom_bar(aes(fill = race),
           position = "dodge") +
  text_theme +
  color_theme +
  labs(title = "Race by age category", 
       x = "Age groups", 
       y = "Number of deaths")
# In Age group(0-20)Blacks lead while Whites are catching up fast. For rest of the age groups leader is White

# Gender of Dead(Gun death) categorized by Age
ggplot(subset(gunDeath, !is.na(age_group)), 
       aes(x = reorder(age_group,
                       age_group,
                       function(x) -length(x)))) +
  geom_bar(aes(fill = sex),
           position = "dodge") +
  text_theme +
  color_theme +
  labs(title = "Gender by age category", 
       x = "Age groups", 
       y = "Number of deaths")

# Place of Dead(Gun death) categorized by Age
ggplot(subset(gunDeath, !is.na(age_group)), 
       aes(x = reorder(age_group,
                       age_group,
                       function(x) -length(x)))) +
  geom_bar(aes(fill = place),
           position = "dodge") +
  text_theme +
  color_theme +
  labs(title = "Place by age category", 
       x = "Age groups", 
       y = "Number of deaths")

# Finding the leading cause of Gun deaths
ggplot(gunDeath, aes(x = reorder(intent, 
                                 intent, 
                                 function(x) length(x)))) +
  geom_bar(aes(fill = intent), 
           stat = "count") +
  text_theme +
  color_theme +
  labs(title = "Leading cause of gun death? Suicide", 
       x = "Causes of death", 
       y = "Number of deaths") +
  coord_flip() +
  guides(fill = F)

# Finding the gender most vulnerable to Gun deaths
ggplot(gunDeath, aes(x = factor(1), fill = factor(sex)))  + geom_bar(width = 3) +coord_polar(theta = "y")+
  text_theme + labs(title = "Distribution (of deaths) by Gender", x=NULL, y=NULL)

# Finding the race most vulnerable to Gun deaths
ggplot(gunDeath, aes(x = factor(1), fill = factor(race)))  + geom_bar(width = 3) +coord_polar(theta = "y")+
  text_theme + labs(title = "Distribution (of deaths) by Race", x=NULL, y=NULL)

# Finding the age group most vulnerable to Gun deaths
ggplot(gunDeath, aes(x = factor(1), fill = factor(age_group)))  + geom_bar(width = 3) +coord_polar(theta = "y")+
  text_theme + labs(title = "Distribution (of deaths) by Age Group", x=NULL, y=NULL)

# Age group(20-40) is most vulnerable to Gun Death followed by Age group 40-60 years
