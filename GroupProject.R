# ---
# jupyter:
#   jupytext:
#     formats: R:percent
#     text_representation:
#       extension: .R
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.17.2
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# %%
### Install missing packages
install.packages("leaps") # Run this if an installation of leaps is required
install.packages("patchwork") # Run this if an installation of Patchwork is required

# %%
library(leaps)
library(tidyverse)
library(dplyr)
library(car)
library(MASS)
library(ggplot2)
library(patchwork)
options(repr.plot.width=12, repr.plot.height=5)
set.seed(1234)

# %%
## Import dataset
studentData <- read.table("student-mat.csv", sep=";", header=TRUE)
head(studentData)

# %%
# Extract the explanatory terms and the response
studentData <- studentData[, c('sex', 'age', 'Pstatus', 'Medu', 'Fedu', 'activities', 'freetime', 'absences', 'G1', 'G2', 'G3')]

# %%
# Check for missing data to handle
sum(is.na(studentData))

# %%
studentData <- studentData %>%
    mutate(sex = as.factor(sex), 
           age = as.numeric(age),
           Pstatus = as.factor(Pstatus), 
           Medu = as.factor(Medu),
           Fedu = as.factor(Fedu),
           activities = as.factor(activities),
           freetime = as.numeric(freetime), 
           absences = as.numeric(absences), 
           G1 = as.numeric(G1),
           G2 = as.numeric(G2),
           G3 = as.numeric(G3))
head(studentData)

## generate initial summary of data
summary(studentData)

## Number of observations
n <- nrow(studentData)
cat("There are ", n, " observations.")


# Examine variables
str(studentData)

# Distribution of G3
ggplot(studentData, aes(x = G3)) +
  geom_histogram(binwidth=1) +
  ggtitle("Histogram of G3")

# Boxplots for binary explanatory variables
p1 <- ggplot(studentData, aes(x = sex, y = G3)) +
  geom_boxplot() +
  labs(title = "G3 by Sex")
p2 <- ggplot(studentData, aes(x = Pstatus, y = G3)) +
  geom_boxplot() +
  labs(title = "G3 by Parent Status")
p3 <- ggplot(studentData, aes(x = activities, y = G3)) +
  geom_boxplot() +
  labs(title = "G3 by Activities")
(p1 | p2 | p3)

# Multi-level boxplots for Medu and Fedu factor variables
p4 <- ggplot(studentData, aes(x = Medu, y = G3)) +
  geom_boxplot() +
  labs(title = "G3 by Mother's Education")
p5 <- ggplot(studentData, aes(x = Fedu, y = G3)) +
  geom_boxplot() +
  labs(title = "G3 by Father's Education")
(p4 | p5)

## Scatterplots for numeric data
# Age vs G3
p6 <- ggplot(studentData, aes(x = age, y = G3)) +
  geom_point() +
  ggtitle("Age vs G3")

## Freetime vs G3b
p7 <- ggplot(studentData, aes(x = freetime, y = G3)) +
  geom_point() +
  ggtitle("Freetime vs G3")

## absences vs G3
p8 <- ggplot(studentData, aes(x = absences, y = G3)) +
  geom_point() +
  ggtitle("Absences vs G3")

## visualize scatterplots for numeric data
p6
p7
p8

## create dataframe for numeric variables only
numeric_vars <- c("age", "freetime", "absences", "G1", "G2")
df_numeric <- studentData[, numeric_vars]

## Correlation check, round to 4 decimals
cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")
round(cor_matrix, 4)

## Initial Model without G1 and G2
model_initial <- lm(G3 ~ . -G1 -G2, data = studentData)
summary(model_initial)

## VIF check
vif_values <- round(vif(model_initial), 4)
vif_values_table <- data.frame(vif_values)
vif_values_table

# Residual plot and QQ plot of model_initial
res_init <- residuals(model_initial)
fitted_vals_init <- fitted(model_initial)

plot(fitted_vals_init, res_init, xlab = "Fitted values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red")

qqnorm(res_init, main = "Normal Q-Q Plot")
qqline(res_init, col = "red")

## Full Model including G1 and G2
model_full <- lm(G3 ~., data = studentData)
summary(model_full)

# Residual plot and QQ plot of model_full
res <- residuals(model_full)
fitted_vals <- fitted(model_full)

plot(fitted_vals, res, xlab = "Fitted values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red")

qqnorm(res, main = "Normal Q-Q Plot")
qqline(res, col = "red")
