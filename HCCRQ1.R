#install the packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("car")
install.packages("carData")
install.packages("dunn.test")
install.packages("MASS")
install.packages("lmtest")
install.packages("gridExtra")
#load the packages
library(tidyverse)
library(ggplot2)
library(car)
library(carData)
library(dunn.test)
library(MASS)
library(lmtest)
library(gridExtra)

#load the table
df <- read.csv("/Users/franziskanicolaus/Downloads/Empower-U Aufgabe/dataset-cleaned.csv")

#first statistics overvew
summary(df)

#calculate DV 
df$VDS_Diff <- df$VDS_after - df$VDS_before

#show VDS_Diff
print(df$VDS_Diff)

#visualize DV with ggplot for each group
ggplot(df, aes(x = factor(Groups), y = VDS_Diff)) +
  geom_boxplot() +
  labs(title = "VDS Difference for each type of Interface", x = "type of Interface", y = "VDS-Difference")

#normality test
shapiro.test(df$VDS_Diff)

#Kruskal-Wallis-Test, not ANOVA because of non-normal distribution
kruskal.test(VDS_Diff ~ factor(Groups), data = df)

#Dunn's test for pairwise comparisons, after non-sgnificant KW 
dunn.test(df$VDS_Diff, g = df$Groups, method = "bonferroni")

#robust regression, due to not-fitting data for GLM
robust_model <- rlm(VDS_Diff ~ factor(Groups) + Knowledge_score + Age + factor(Gender) + factor(Education), data = df)

#results
summary(robust_model)

#testing for normality of the residuals
shapiro.test(residuals(robust_model))

#testing for heteroscedasticity
bptest(robust_model)


#plot diagnostics for robust regression model
par(mfrow=c(2, 2))
plot(robust_model)
par(mfrow=c(1, 1))

#robust Regression with interactions
robust_model_interaction <- rlm(VDS_Diff ~ factor(Groups) * (Knowledge_score + Age + factor(Gender) + factor(Education)), data = df)

#results of robust interaction model
summary(robust_model_interaction)

#check the tests for comparison to robust model
shapiro.test(residuals(robust_model_interaction))
bptest(robust_model_interaction)


#plot diagnostics for robust interaction model
par(mfrow=c(2, 2))
plot(robust_model_interaction)
par(mfrow=c(1, 1))

#further analysis of the control variables for each interface against VDS_Diff
#define a custom theme for consistent legend placement
custom_theme <- theme(legend.position = "bottom",
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 9))

#create individual plots for each control variable
plot1 <- ggplot(df, aes(x = Knowledge_score, y = VDS_Diff, color = factor(Groups))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Knowledge-Interface interaction against VDS-Diff",
       x = "Knowledge", y = "VDS-Diff", color = "Interface") +
  custom_theme

plot2 <- ggplot(df, aes(x = Age, y = VDS_Diff, color = factor(Groups))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Age-Interface interaction against VDS-Diff",
       x = "Age", y = "VDS-Diff", color = "Interface") +
  custom_theme

plot3 <- ggplot(df, aes(x = Education , y = VDS_Diff, color = factor(Groups))) +
  geom_point() +
  labs(title = "Education-Interface interaction against VDS-Diff",
       x = "Education", y = "VDS-Diff", color = "Interface") +
  custom_theme

plot4 <- ggplot(df, aes(x = factor(Gender), y = VDS_Diff, color = factor(Groups))) +
  geom_boxplot() +
  labs(title = "Gender-Interface interaction against VDS-Diff",
       x = "Gender", y = "VDS-Diff", color = "Interface") +
  custom_theme

#combine plots with legend
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
