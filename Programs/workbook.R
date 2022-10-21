# -----------------------------------
# Setting up the IDE and environment
# -----------------------------------


# ----------------------------------
# Clear the console and environment
# ----------------------------------

remove(list=ls())
cat("\f")


# ---------
# Packages
# ---------

library(tidyverse)
library(stargazer)
library(magrittr)
library(forcats)
library(olsrr)
library(corrplot)
library (lm.beta)


# ----------------------------------------------------
# Set working directory and define folder's structure
# ----------------------------------------------------
setwd("C:/STUDIA - materiały/RSM-MASTERS/COURSES/BLOCK 1/Advanced Statistics and Programming/Assignment 1")

dir <- "C:/STUDIA - materiały/RSM-MASTERS/COURSES/BLOCK 1/Advanced Statistics and Programming/Assignment 1/"

dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Results/")


# ----------------------------
# Import CSV data - train.csv
# ----------------------------

housingDataAll <- read.csv(file = paste0(dirData, "train.csv"), header = TRUE)


# Performing checks on the data to see whether it loaded correctly (that is as expected)

head(housingDataAll)
tail(housingDataAll)
summary(housingDataAll$SalePrice)
str(housingDataAll$OverallQual10)


# Checking n/a per variable 
# linreg model will not handle missing values;
# One must investigate the nature of missing values 
# and whether decision to drop them is reasonable
# Otherwise, such variables cannot be considered for the model
# Investigate the pattern of n/a

dfMissingValues <- as.data.frame(colSums(is.na(housingDataAll)))
colnames(dfMissingValues) <- c("# Missing Values")
columnsNA <- as.data.frame(housingDataAll[, dfMissingValues[,1] != 0])
 

# -----------------------------------
# Variables of choice for the model
# -----------------------------------

# LotArea
# LotShape
# GarageArea
# OverallQual


# -------------------------------------------------------------------------------------------
# Plot scatter plots/bar charts/box plots to support the story and initial look at associations
# -------------------------------------------------------------------------------------------

# 0) Investigating possible non-linear terms among quantitative variables
scatterLinearTestLotArea <- ggplot(housingDataAll, aes(LotArea, SalePrice))

scatterLinearTestLotArea + geom_point() + 
  geom_smooth(method = "lm", colour = "Red") + 
  labs(x = "LotArea", y = "SalePrice") +
  scale_x_continuous(limits = c(0, 30000)) +
  scale_y_continuous(limits = c(0, 755000), breaks = seq(30000, 755000, by = 120000))

scatterLinearTestLotArea + geom_point() + 
  geom_smooth() + 
  labs(x = "LotArea", y = "SalePrice") +
  scale_x_continuous(limits = c(0, 30000)) +
  scale_y_continuous(limits = c(0, 755000), breaks = seq(30000, 755000, by = 120000))


scatterLinearTestGarageArea <- ggplot(housingDataAll, aes(GarageArea, SalePrice))

scatterLinearTestGarageArea + geom_point() + 
  geom_smooth(method = "lm", colour = "Red") + 
  labs(x = "GarageArea", y = "SalePrice") +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 755000), breaks = seq(30000, 755000, by = 120000))


scatterLinearTestGarageArea + geom_point() + 
  geom_smooth() + 
  labs(x = "GarageArea", y = "SalePrice") +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 755000), breaks = seq(30000, 755000, by = 120000))


#  1) Possible interaction effect LotArea*LotShape against SalePrice
scatterLotShapeInteraction <- ggplot(data = housingDataAll, aes(x = LotArea, y = SalePrice, colour = LotShape))

scatterLotShapeInteraction +
  geom_point() +
  geom_smooth(method = 'lm', aes(fill=LotShape), alpha = 0.5) +
  labs(x = "LotArea", y = "SalePrice") +  
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 120000))


# 2) Bar chart with LotShape showing that merging irregular shapes is desired
table(housingDataAll$LotShape)

barBaseShapeAggregate <- ggplot(data = housingDataAll, aes(x = fct_infreq(LotShape)))

barBaseShapeAggregate + 
  geom_bar(aes(fill = LotShape), stat = "count") +
  geom_text(aes(LotShape, label = signif((..count..)/sum(..count..), 2)), stat = "count", nudge_y = 30) +
  labs(x = "LotShape", y = "Count") +
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 250), name = "Count of instances of each lot shape", sec.axis = sec_axis(~ . / 1460, name = "Ratio of instances of lot shape to total # of lots"))

# 3) Scatter plot with fitted line for GarageArea vs. SalePrice
scatterBaseGarageArea <- ggplot(data = housingDataAll, aes(x = GarageArea, y = SalePrice))

scatterBaseGarageArea +
  geom_point() +
  geom_smooth(method = 'lm', alpha = 0.5) +
  labs(x = "GarageArea", y = "SalePrice") +
  theme(legend.position="bottom") +
  scale_x_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300)) +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 120000))

# 4) Overall material and finish quality plotted against SalePrice
#' run the regression of categorical quality on saleprice; if there is a constant increase in coefficiant then it is an indication of linearity
scatterBaseQualityLinear <- ggplot(data = housingDataAll, aes(x = OverallQual, y = SalePrice, colour = OverallQual))

scatterBaseQualityLinear +
  geom_point() +
  geom_smooth(method = 'lm', aes(fill=OverallQual), alpha = 0.5) +
  labs(x = "OverallQual", y = "SalePrice") +
  xlim(1, 10) +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 120000))

#  For saving the latest plot 
ggsave(paste0(dirRslt, "scatterBaseQualityLinear.png"), width=8, height=6)

# ------------------------------------------------------------------
# Transform variables to render them ready for analysis
# categorical variables into factors
# dummy variables transformation - reflect on the choice being made 
# ------------------------------------------------------------------

# Reflection on whether the OverallQual has to remain categorical or can be expressed numerically in the model
#'What has to be done is the following:
#'Prepare a temporary object for a regression analysis: Id, SalePrice, OverallQual, dummies for OverallQual
#'Two regression models: SalePrice on OverallQual as discrete, SalePrice on OverallQual dummies
#'In dummy model - if there is a constant increase in coefficiants then it is an indication of linearity
#'In discrete model - does the coefficient hint at an rather steep positive effect of OverallQual on SalePrice? 

overallQual_lintest.df <-
  housingDataAll %>% 
  select(Id, SalePrice, OverallQual)

# Necessary manipulation for dummies

overallQual_lintest.df$dOverallQual2 <- ifelse(overallQual_lintest.df$OverallQual == "2", 1, 0)
overallQual_lintest.df$dOverallQual3 <- ifelse(overallQual_lintest.df$OverallQual == "3", 1, 0)
overallQual_lintest.df$dOverallQual4 <- ifelse(overallQual_lintest.df$OverallQual == "4", 1, 0)
overallQual_lintest.df$dOverallQual5 <- ifelse(overallQual_lintest.df$OverallQual == "5", 1, 0)
overallQual_lintest.df$dOverallQual6 <- ifelse(overallQual_lintest.df$OverallQual == "6", 1, 0)
overallQual_lintest.df$dOverallQual7 <- ifelse(overallQual_lintest.df$OverallQual == "7", 1, 0)
overallQual_lintest.df$dOverallQual8 <- ifelse(overallQual_lintest.df$OverallQual == "8", 1, 0)
overallQual_lintest.df$dOverallQual9 <- ifelse(overallQual_lintest.df$OverallQual == "9", 1, 0)
overallQual_lintest.df$dOverallQual10 <- ifelse(overallQual_lintest.df$OverallQual == "10", 1, 0)

# Factoring dummies
overallQual_lintest.df$dOverallQual2 <- factor(overallQual_lintest.df$dOverallQual2, levels = c(0, 1))
overallQual_lintest.df$dOverallQual3 <- factor(overallQual_lintest.df$dOverallQual3, levels = c(0, 1))
overallQual_lintest.df$dOverallQual4 <- factor(overallQual_lintest.df$dOverallQual4, levels = c(0, 1))
overallQual_lintest.df$dOverallQual5 <- factor(overallQual_lintest.df$dOverallQual5, levels = c(0, 1))
overallQual_lintest.df$dOverallQual6 <- factor(overallQual_lintest.df$dOverallQual6, levels = c(0, 1))
overallQual_lintest.df$dOverallQual7 <- factor(overallQual_lintest.df$dOverallQual7, levels = c(0, 1))
overallQual_lintest.df$dOverallQual8 <- factor(overallQual_lintest.df$dOverallQual8, levels = c(0, 1))
overallQual_lintest.df$dOverallQual9 <- factor(overallQual_lintest.df$dOverallQual9, levels = c(0, 1))
overallQual_lintest.df$dOverallQual10 <- factor(overallQual_lintest.df$dOverallQual10, levels = c(0, 1))

# Fit the models
model_overallQual_lin_discrete <- lm(SalePrice ~ OverallQual, data = overallQual_lintest.df)
model_overallQual_lin_dummy <- lm(SalePrice ~ dOverallQual2 + dOverallQual3 + dOverallQual4 + dOverallQual5 + dOverallQual6 + dOverallQual7 + dOverallQual8 + dOverallQual9 + dOverallQual10, data = overallQual_lintest.df)

stargazer(model_overallQual_lin_discrete, model_overallQual_lin_dummy, type = 'text')

# Conclusion - we can work with OverallQual as a numerical variable instead of a scale (to be transformed to dummy) 
# because of the linear relationship of effects of consecutive increase in quality (seen in lm result and the graph). 
# The direct interpretation of beta will have  no value, but the general association and standardized strength of the effect 
# can now be measured and interpreted more easily. 


# Dummy transformation

# Given a proportion of regular shaped homes to those irregularly shaped under IR1, IR2 and IR3 - group irregulars together
# Merging IR1, IR2, IR3 into IR
housingDataAll$LotShape <- ifelse(housingDataAll$LotShape == "Reg", "Regular", "Irregular")
housingDataAll$dLotShapeRegular <- ifelse(housingDataAll$LotShape == "Regular", "Regular", "Irregular")

# OverallQual and LotShape into 10 dummy variables; for overallqual dummy value is either as given (1) or average (0)
housingDataAll$dOverallQual1 <- ifelse(housingDataAll$OverallQual == "1", 1, 0)
housingDataAll$dOverallQual2 <- ifelse(housingDataAll$OverallQual == "2", 1, 0)
housingDataAll$dOverallQual3 <- ifelse(housingDataAll$OverallQual == "3", 1, 0)
housingDataAll$dOverallQual4 <- ifelse(housingDataAll$OverallQual == "4", 1, 0)
housingDataAll$dOverallQual6 <- ifelse(housingDataAll$OverallQual == "6", 1, 0)
housingDataAll$dOverallQual7 <- ifelse(housingDataAll$OverallQual == "7", 1, 0)
housingDataAll$dOverallQual8 <- ifelse(housingDataAll$OverallQual == "8", 1, 0)
housingDataAll$dOverallQual9 <- ifelse(housingDataAll$OverallQual == "9", 1, 0)
housingDataAll$dOverallQual10 <- ifelse(housingDataAll$OverallQual == "10", 1, 0)
housingDataAll$dLotShapeRegular <- ifelse(housingDataAll$dLotShapeRegular == "Regular", 1, 0)

# Perform checks on the transformation
select(housingDataAll, c("OverallQual", "dOverallQual1", "dOverallQual2", "dOverallQual3", "dOverallQual4", "dOverallQual6", "dOverallQual7", "dOverallQual8", "dOverallQual9", "dOverallQual10"))
select(housingDataAll, c("LotShape", "dLotShapeRegular"))

# Combine relevant variables into a df that is gonna be an input to model 
housingData <- 
  housingDataAll %>% 
  select(Id, SalePrice, LotArea, GarageArea, LotShape, OverallQual, dLotShapeRegular, dOverallQual1, dOverallQual2, dOverallQual3, dOverallQual4, dOverallQual6, dOverallQual7, dOverallQual8, dOverallQual9, dOverallQual10)


# ------------------------------------------------------------------------------------
# Present summary statistics of the data set with the variables of your choice 
# using function stargazer and present a description of the main characteristics of variables.
# ------------------------------------------------------------------------------------

stargazer(housingData, type = "text", summary.stat = c("n", "mean", "sd", "min", "max" ,"median", "p25", "p75"), omit = "Id", digits = 2)


# --------
# Factors
# --------

# Categorical values (LotShape, OverallQual) to factors
housingData$LotShape <- factor(housingData$LotShape, levels = c("Regular", "Irregular"))
housingData$OverallQual <- factor(housingData$OverallQual, levels = c(1:10), labels = c("Very Poor", "Poor", "Fair", "Below Average", "Average", "Above Average", "Good", "Very Good", "Excellent", "Very Excellent"))


housingData$dLotShapeRegular <- factor(housingData$dLotShapeRegular, levels = c(1, 0))
housingData$dOverallQual1 <- factor(housingData$dOverallQual1, levels = c(1, 0))
housingData$dOverallQual2 <- factor(housingData$dOverallQual2, levels = c(1, 0))
housingData$dOverallQual3 <- factor(housingData$dOverallQual3, levels = c(1, 0))
housingData$dOverallQual4 <- factor(housingData$dOverallQual4, levels = c(1, 0))
housingData$dOverallQual6 <- factor(housingData$dOverallQual6, levels = c(1, 0))
housingData$dOverallQual7 <- factor(housingData$dOverallQual7, levels = c(1, 0))
housingData$dOverallQual8 <- factor(housingData$dOverallQual8, levels = c(1, 0))
housingData$dOverallQual9 <- factor(housingData$dOverallQual9, levels = c(1, 0))
housingData$dOverallQual10 <- factor(housingData$dOverallQual10, levels = c(1, 0))



# -----------------------------------------------------------------------------------
# Draw a causal relationship scheme and develop a theory/story with chosen variables
# -----------------------------------------------------------------------------------




# ----------------------------------------------------------
# Formulate three research hypotheses as was in class
# ----------------------------------------------------------

# H1 Lot size has a positive influence on the sale price of the house.
# H2 The expected sale price of the house is higher for houses with a better overall material and finish of the house.
# H3 The positive influence of lot size on sale price of the house is expected to be larger for regularly shaped houses than for irregularly shaped houses.


# ----------------------------------------------------------
# Formulate a population regression model 
# (include dummy.categorical variables specifications, 
# interactions and non-linear terms if applicable)
# ----------------------------------------------------------

# Should quadratic terms be introduced for quantitative variables in the model in terms of model fit?

# LotArea goodness-of-fit
linear_test_LotArea <- lm(SalePrice ~ LotArea, data = housingDataAll)
quadratic_test_LotArea <- lm(SalePrice ~ LotArea + I(LotArea^2), data = housingDataAll)
stargazer(linear_test_LotArea, quadratic_test_LotArea, type = 'text')

# GarageArea goodness-of-fit
linear_test_GarageArea <- lm(SalePrice ~ GarageArea, data = housingDataAll)
quadratic_test_GarageArea <- lm(SalePrice ~ GarageArea + I(GarageArea^2), data = housingDataAll)
stargazer(linear_test_GarageArea, quadratic_test_GarageArea, type = 'text')

# Conclusion : R2 improvement and significant change in coefficiants suggest that introducing quadratic terms for LotArea will improve the model. 

# Population regression models

# Population regression model:
  SalePrice=\ \beta_0\ +\ \beta_1LotArea\ +\ \beta_2GarageArea\ +\ \beta_3LotShape\ +\ \beta_4OverallQual\ +\ \varepsilon,\ \ \varepsilon\ ~\ n(0,\ \sigma)


# Population regression model introducing non-linear specifications:
  SalePrice=\ \beta_0\ +\ \beta_1LotArea\ +\ \beta_2GarageArea\ +\ \beta_3LotShape\ +\ \beta_4OverallQual\ +\ \beta_5LotArea2\ +\ \varepsilon,\ \ \varepsilon\ ~\ n(0,\ \sigma)


# Population regression model introducing dummy variables specifications:
  SalePrice=\ \beta_0\ +\ \beta_1LotArea\ +\ \beta_2GarageArea\ +\ \beta_3dLotShapeRegular\ +\ \beta_{41}OverallQual\ +\ \beta_{42}OverallQual\ +\ \beta_{43}OverallQual\ +\ \beta_{44}OverallQual\ +\ \beta_{46}OverallQual\ +\ \beta_{47}OverallQual\ +\ \beta_{48}OverallQual\ +\ \beta_{49}OverallQual\ +\ \beta_{40}OverallQual\ OverallQual\ +\ \varepsilon,\ \ \varepsilon\ ~\ n(0,\ \sigma)\ 

# Population regression model introducing interactions:
  SalePrice=\ \beta_0\ +\ (\beta_{11}+\beta_{12}dLotShapeRegular)LotArea\ +\ \beta_2GarageArea\ +\ \beta_3dLotShapeRegular\ +\ \beta_{41}OverallQual\ +\ \beta_{42}OverallQual\ +\ \beta_{43}OverallQual\ +\ \beta_{44}OverallQual\ +\ \beta_{46}OverallQual\ +\ \beta_{47}OverallQual\ +\ \beta_{48}OverallQual\ +\ \beta_{49}OverallQual\ +\ \beta_{40}OverallQual\ OverallQual\ +\ \varepsilon,\ \ \varepsilon\ ~\ n(0,\ \sigma)



# ----------------------------------------------------------------------------
# Elaborate on each of the six linear regression model
# assumptions discussed in class and explain why which assumptions could be
# violated; use appropriate example variables to explain why you think which
# assumptions might be violated.
# ----------------------------------------------------------------------------




# ----------------------------------------------------------------------------
# Estimate the model developed in the preceding exercise, both with and without
# interaction and non-linear terms. 
# ----------------------------------------------------------------------------

model_baseline <- lm(SalePrice ~ LotArea + GarageArea + dLotShapeRegular + dOverallQual1 + dOverallQual2 + dOverallQual3 + dOverallQual4 + dOverallQual6 + dOverallQual7 + dOverallQual8 + dOverallQual9 + dOverallQual10, data = housingData)
  
model_interactions <- lm(SalePrice ~ LotArea + LotArea*dLotShapeRegular + GarageArea + dLotShapeRegular + dOverallQual1 + dOverallQual2 + dOverallQual3 + dOverallQual4 + dOverallQual6 + dOverallQual7 + dOverallQual8 + dOverallQual9 + dOverallQual10, data = housingData)
  
model_nonlinear <- lm(SalePrice ~ LotArea + I(LotArea^2) + GarageArea + dLotShapeRegular + dOverallQual1 + dOverallQual2 + dOverallQual3 + dOverallQual4 + dOverallQual6 + dOverallQual7 + dOverallQual8 + dOverallQual9 + dOverallQual10, data = housingData)
  
model_interactions_nonlinear <- lm(SalePrice ~ LotArea + I(LotArea^2) + LotArea*dLotShapeRegular + GarageArea + dLotShapeRegular + dOverallQual1 + dOverallQual2 + dOverallQual3 + dOverallQual4 + dOverallQual6 + dOverallQual7 + dOverallQual8 + dOverallQual9 + dOverallQual10, data = housingData)


# ----------------------------------------------------------------------------
# Generate the report of linreg estimates using stargazer function. 
# Interpret results of the estimation and determine which independent variable 
# has the largest effect on explained variable. Motivate criterion to pick the largest effect
# ----------------------------------------------------------------------------

stargazer(model_baseline, model_interactions, model_nonlinear, model_interactions_nonlinear, type = "text")


# -----------------------------------------------------------------------------------
# Perform a diagnostic analysis of the estimated model, 
# i.e., systematically analyze if OLS model assumptions have been violated, 
# and if remedies for these violations seriously affect model outcomes.
# -----------------------------------------------------------------------------------

# ------------------------
# Normality assumption
# ------------------------
  
#LotArea

# Normal QQ plot
ols_plot_resid_qq(housingData$LotArea)

# Boxplot for normality
normalLotArea_boxplot <- ggplot(data = housingData, aes(x = LotArea)))

normalLotArea_boxplot +
  geom_boxplot()

# Histogram for normality
ols_plot_resid_hist(housingData$LotArea)

#Shapiro-Wilk test
shapiro.test(housingData$LotArea)


#GarageArea

# Normal QQ plot
ols_plot_resid_qq(housingData$GarageArea)

# Boxplot for normality
normalGarageArea_boxplot <- ggplot(data = housingData, aes(x = GarageArea)))

normalGarageArea_boxplot +
  geom_boxplot()

# Histogram for normality
ols_plot_resid_hist(housingData$GarageArea)

#Shapiro-Wilk test
shapiro.test(housingData$GarageArea)

# dLotShapeRegular

# Normal QQ plot
ols_plot_resid_qq(housingData$dLotShapeRegular)

# Boxplot for normality
normaldLotShapeRegular_boxplot <- ggplot(data = housingData, aes(x = dLotShapeRegular)))

normaldLotShapeRegular_boxplot +
  geom_boxplot()

# Histogram for normality
ols_plot_resid_hist(housingData$dLotShapeRegular)

#Shapiro-Wilk test
shapiro.test(housingData$dLotShapeRegular)


# -----------------------------------------------------------------------------------
# Analyze the (standardized) residuals to assess whether their behavior is consistent 
# with the assumptions of the linear regression model, and check if  multicollinearity 
# problems adversely affect the interpretation of the estimation results.
# -----------------------------------------------------------------------------------

# Inspecting residuals
resids_baseline <- residuals(model_baseline)
resids_interactions <- residuals(model_interactions)
resids_nonlinear <- residuals(model_nonlinear)
resids_interactions_nonlinear <- residuals(model_interactions_nonlinear)

residuals <- data.frame(resids_baseline, resids_interactions, resids_nonlinear, resids_interactions_nonlinear)


# if the assumption of OLS holds, we should see 0 as the mean (it is an empirical mean, can be NOT EXACTLY 0, like -0)
# also looking at the max-min, we want residuals to be normally distribute, so the min-max should be of same magnitute
stargazer(residuals, type = 'text')

# Normal QQ plot
ols_plot_resid_qq(model_baseline)
ols_plot_resid_qq(model_interactions)
ols_plot_resid_qq(model_nonlinear)
ols_plot_resid_qq(model_interactions_nonlinear)

# Residual vs Fitted Values Plot
ols_plot_resid_fit(model_baseline)
ols_plot_resid_fit(model_interactions)
ols_plot_resid_fit(model_nonlinear)
ols_plot_resid_fit(model_interactions_nonlinear)


# Histogram for normality
residual_histogram <- ggplot(data = residuals, aes(resids_baseline))
residual_histogram + 
  geom_histogram(aes(y = ..density..), binwidth = 1170) +
  geom_density(lwd = 1.2, colour = 2)
  labs(x = "Residual Value", y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 3)
# or
ols_plot_resid_hist(model_baseline)  
ols_plot_resid_hist(model_interactions)
ols_plot_resid_hist(model_nonlinear)
ols_plot_resid_hist(model_interactions_nonlinear)
  
    
# Boxplot for normality
residual_boxplot <- ggplot(data = residuals, aes(x = resids_baseline)))

residual_boxplot +
  geom_boxplot()

# Tests for normality
ols_test_normality(model_baseline)
ols_test_normality(model_interactions)
ols_test_normality(model_nonlinear)
ols_test_normality(model_interactions_nonlinear)

# Correlation between observed residuals and expected residuals under normality.
ols_test_correlation(model_baseline)
ols_test_correlation(model_interactions)
ols_test_correlation(model_nonlinear)
ols_test_correlation(model_interactions_nonlinear)


# Checking for multicollinearity

# Correlation matrix
corrplot(cor(housingData[,-c(1, 5, 6)]), method = "number")

# -----------------------------------------------------------------------------------
# Re-run updated models to determine if any remedies have been 
# effective to counter the observed modeling issues; discuss consequences of your model
# interventions by comparing the results with the originally estimated models
# -----------------------------------------------------------------------------------


