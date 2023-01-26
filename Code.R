# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(lmtest)
library(sandwich)
library(haven)
library(stargazer)
library(caret)
library(grid)
library(modelsummary)
library(scales)
library(lspline)
library(ggcorrplot)
# Data import
df <- read.csv('morg-2014-emp.csv')
# Filter the data to surgeons and physicians
df <- df %>% filter(occ2012 == '3060')

# Check the data
glimpse(df)

unique(df$class)
unique(df$ownchild)
unique(df$grade92)
unique(df$race)
unique(df$lfsr94)
unique(df$prcitshp)

fct_count(df$class)
fct_count(df$lfsr94)
fct_count(df$prcitshp)
# Drop unnecessary columns
df <- df %>% select(-c("X", "hhid", "unioncov", "intmonth", "occ2012", "ind02"))

# Create hourly earnings column
df$earnhourly <- df$earnwke / df$uhours


P01 <- function(x){quantile(x, 0.01)}
P99 <- function(x){quantile(x, 0.99)}
datasummary( earnhourly~ P0 + P01 + P25 + P50 + P75 +  P99 + P100 + Mean + SD,  
             data = df )
# Drop the unrealistically high values in earnhourly column
df <- df %>% filter(!earnhourly > 96.15)

# Drop the unrealistically low hourly earnings
df <- df %>% filter(!earnhourly < 8.06)

# Filter out the ones who doesn't have at least bachelor's degree, 
# since you can only work in this field if you have a diploma.
df <- df %>% filter(!grade92 %in% c(33, 39, 40, 41, 42))


# Check frequency by class
df %>%
  group_by(class) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# Check frequency by education level
df %>%
  group_by(grade92) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# Check frequency by ethnic
df %>%
  group_by(ethnic) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)
# Drop ethnic way too many missing values
df <- df %>% select(-c("ethnic"))

# Check frequency by race
df %>%
  group_by(race) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# Check frequency by citizenship status
df %>%
  group_by(prcitshp) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# Correlation between variables
ggcorrplot(cor(df %>% select_if(is.numeric)), type = 'lower',lab = TRUE, lab_size = 3.5)

# Data summary
df %>%
  dplyr::select(age, grade92, ownchild, marital, uhours, sex, race,earnhourly ) %>%
  summary()
