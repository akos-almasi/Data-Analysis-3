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
################################################################################
# DATA GENERATION & DESCRIPTIVES

# Create new column based on marital column, takes 1 if married 0 if not 
df <- df %>% 
  mutate(married = ifelse(marital < 4, 1, 0))

# Modify ownchild column, takes 1 if have a child 0 if not
df <- df %>%
  mutate(ownchild = ifelse(ownchild >= 1, 1,0))

# Create ln hourly earnings column
df$lnearnhourly <- log(df$earnhourly)

# Create age squared column
df$agesq <- df$age^2

# Create new columns based on the class column
df <- df %>%
  mutate(priv_profit = ifelse(class == "Private, For Profit", 1,0),
         priv_nonprofit = ifelse(class == "Private, Nonprofit", 1,0),
         gov_state = ifelse(class == "Government - State", 1,0),
         gov_local = ifelse(class == "Government - Local", 1,0),
         gov_fed = ifelse(class == "Government - Federal", 1,0))

# Summary statistics based on hourly earning in each class
df %>% 
  group_by(class) %>% 
  summarise(
    frequency=n(),
    min = min(earnhourly),
    P1 = quantile(earnhourly, 0.01), 
    D1 = quantile(earnhourly, 0.1), 
    Q1 = quantile(earnhourly, 0.25), 
    Me = quantile(earnhourly, 0.5), 
    Q3 = quantile(earnhourly, 0.75), 
    D9 = quantile(earnhourly, 0.9), 
    P99 = quantile(earnhourly, 0.99),
    max = max(earnhourly),
    mean = mean(earnhourly))

# Summary statistics based on hourly earning in each education level
df %>% 
  group_by(grade92) %>% 
  summarise(
    frequency=n(),
    min = min(earnhourly),
    P1 = quantile(earnhourly, 0.01), 
    D1 = quantile(earnhourly, 0.1), 
    Q1 = quantile(earnhourly, 0.25), 
    Me = quantile(earnhourly, 0.5), 
    Q3 = quantile(earnhourly, 0.75), 
    D9 = quantile(earnhourly, 0.9), 
    P99 = quantile(earnhourly, 0.99),
    max = max(earnhourly),
    mean = mean(earnhourly))



# Check how much they earn in each sex, 1 = Male, 2 = Female
df %>%
  group_by(sex) %>%
  dplyr::summarize(frequency=n(), mean=mean(earnhourly))

# Check how much they earn hourly on average based on if they have a child or not: 1 = yes, 0 = no
df %>%
  group_by(ownchild) %>%
  dplyr::summarize(frequency=n(), mean=mean(earnhourly))

# Check how much they earn hourly on average based on the race
df %>%
  group_by(race) %>%
  dplyr::summarize(frequency=n(), mean=mean(earnhourly))

#### GRAPHS ####
# Hourly earnings
ggplot(data = df, aes(earnhourly)) + 
  geom_histogram(bins = 20, fill = 'navyblue', col = 'white', size = 0.25, alpha = 0.8) + 
  theme_bw() + 
  labs(title = "Hourly earning distribution of surgeons and physicians", x = "Hourly earnings in USD", y = "frequency") +
  theme(plot.title = element_text(size = rel(1))) + 
  scale_x_continuous(expand = c(0.01,0.01))

# Hourly earnings
ggplot(data = df, aes(x = earnhourly)) + 
  geom_histogram(
    aes(y = (..count..)/sum(..count..)),
    bins = 20, fill = 'navyblue', col = 'white', size = 0.25, alpha = 0.8) + 
  theme_bw() + 
  labs(
    title = "Hourly earning distribution of surgeons and physicians", 
    x = "ln price in USD", y = "percent of total"
  ) +
  scale_y_continuous(
    expand = c(0.01,0.01),
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  theme(plot.title = element_text(size = rel(1))) +
  scale_x_continuous(expand = c(0.01,0.01))

# Check the loess of their hourly earnings based on age
ggplot(data = df, aes(x=age, y=earnhourly)) +
  geom_point( color = "#330099", size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="loess", se=F, colour="#000000", size=1, span=0.9, formula = 'y ~ x') +
  labs(x = "Age (years)",y = "Hourly earnings (US dollars)", title = ' Age and hourly earnings, loess method') +
  theme_bw()
# Check with lspline at the age of 42
spline_regression <- lm(earnhourly ~ lspline(age, 42), data = df)
df$splinepredict <- predict(spline_regression)

ggplot(df, aes(x = age, y = earnhourly)) + 
  geom_point( color = "#330099", size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  theme_bw() + 
  labs(title = "Age and hourly earnings, spline regression", x = "Age (years)", y = "Hourly earnings (US dollars)") +
  theme(plot.title = element_text(size = rel(1))) + 
  geom_line(data = df, aes(x = age, y = splinepredict), size = 1)

gp1 <- ggplot(df, aes(x = factor(ownchild), y =earnhourly, fill = factor(ownchild))) +
  geom_boxplot( color = '#000000', alpha = 0.8) +
  stat_summary(fun.y = 'mean',geom = "point", fill='white', shape = 21, size = 4) +
  labs(title ='Hourly wage based on children', x = '', y = 'Hourly earnings (US dollars)') +
  scale_x_discrete(labels = c('Without children','With children')) +
  theme_bw() +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(size = 6))


gp2 <- ggplot(df, aes(x = factor(grade92), y =earnhourly, fill = factor(grade92))) +
  geom_boxplot( color = '#000000', alpha = 0.8) +
  stat_summary(fun.y = 'mean',geom = "point", fill='white', shape = 21, size = 4) +
  labs(title ='Hourly wage based on schooling', x = '', y = 'Hourly earnings (US dollars)') +
  scale_x_discrete(labels = c('Bachelors degree','Masters degree', 'Professional school degree', 'Doctorate degree')) +
  theme_bw() +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 6))


gp3 <- ggplot(df, aes(x = factor(married), y =earnhourly, fill = factor(married))) +
  geom_boxplot( color = '#000000', alpha = 0.8) +
  stat_summary(fun.y = 'mean',geom = "point", fill='white', shape = 21, size = 4) +
  labs(title ='Hourly wage based on marital status', x = '', y = 'Hourly earnings (US dollars)') +
  scale_x_discrete(labels = c('Single',"Married")) +
  theme_bw() +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 6))


ggarrange(gp1, gp2, gp3,
          hjust = 1,
          ncol = 3, nrow = 1)

### Models ###

model1 <- as.formula(earnhourly ~ age + agesq)
model2 <- as.formula(earnhourly ~ age + agesq + married)
model3 <- as.formula(earnhourly ~ age + agesq + married + ownchild + as.factor(grade92))
model4 <- as.formula(earnhourly ~ age + agesq + married*age + ownchild + as.factor(grade92) + 
                       sex + priv_profit + 
                       priv_nonprofit + gov_state + 
                       gov_local + gov_fed)     
reg1 <- lm(model1, data=df)
reg2 <- lm(model2, data=df)
reg3 <- lm(model3, data=df)
reg4 <- lm(model4, data=df)


summary(reg3, vcov = 'sandwich')

# evaluation of the models
models <- c("reg1", "reg2","reg3", "reg4")
AIC <- c()
BIC <- c()
RMSE <- c()
RSquared <- c()
regr <- c()
k <- c()

for ( i in 1:length(models)){
  AIC[i] <- AIC(get(models[i]))
  BIC[i] <- BIC(get(models[i]))
  RMSE[i] <- RMSE(predict(get(models[i])), get(models[i])$model$earnhourly)
  RSquared[i] <-summary(get(models[i]))$r.squared
  regr[[i]] <- coeftest(get(models[i]), vcov = sandwich)
  k[i] <- get(models[i])$rank -1
}

# All models
eval <- data.frame(models, k, RSquared, RMSE, BIC)

# gsub(pattern, replacement, x) 
eval <- eval %>%
  mutate(models = paste0("(",gsub("reg","",models),")")) %>%
  rename(Model = models, "R-squared" = RSquared, "Training RMSE" = RMSE, "N predictors" = k)



#################################################################
# Cross-validation

# set number of folds
k <- 4

set.seed(13505)
cv1 <- train(model1, df, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv2 <- train(model2, df, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv3 <- train(model3, df, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv4 <- train(model4, df, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")

# calculate average RMSE
cv <- c("cv1", "cv2", "cv3", "cv4")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2)/4)
}


# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                     rbind(cv1$resample[1], rmse_cv[1]),
                     rbind(cv2$resample[1], rmse_cv[2]),
                     rbind(cv3$resample[1], rmse_cv[3]),
                     rbind(cv4$resample[1], rmse_cv[4])
)

colnames(cv_mat)<-c("Resample","Model1", "Model2", "Model3", "Model4")
cv_mat



