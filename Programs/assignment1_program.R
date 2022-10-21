
############################################################################
############################################################################
###                                                                      ###
###                ALEKSANDER ODZIEMKOWSKI, BAM01 AS&P                   ###
###                INDIVIDUAL ASSIGNMENT 1, 16.09.2022                   ###
###                                                                      ###
############################################################################
############################################################################


##################################################################
##              Setting up the IDE and environment              ##
##################################################################

# ----------------------------------
# Clear the console and environment
# ----------------------------------
remove(list=ls())
cat("\f")

# ---------
# Packages
# ---------
library(bannerCommenter)
library(stargazer)
library(magrittr)
library(forcats)
library(olsrr)
library(corrplot)
library(lm.beta)
library(knitr)
library(plyr)
library(MASS)
library(gridExtra)
library(tidyverse)
library(sandwich)
library(car)

# ----------------------------------------------------
# Set working directory and define folder's structure
# ----------------------------------------------------

# setwd()

dir <- "C:/STUDIA - materiały/RSM-MASTERS/COURSES/BLOCK 1/Advanced Statistics and Programming/Assignment 1/"

dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Results/")

#################################################################
##              Import and inspect the train data              ##
#################################################################

# ----------------------------
# Import CSV data - train.csv
# ----------------------------

housingDataAll <- read.csv(file = paste0(dirData, "train.csv"), header = TRUE)


# Performing checks on the data to see whether it loaded correctly (that is - as expected)

head(housingDataAll)
tail(housingDataAll)
summary(housingDataAll$SalePrice)
str(housingDataAll$OverallQual10)


# Checking n/a per variable as linear regression model will not handle missing values;
# One must investigate the missing values and whether decision to drop them is reasonable;
# Otherwise, such variables cannot be considered for the model;
# Consequently, there is a need to nvestigate the pattern of n/a;

dfMissingValues <- as.data.frame(colSums(is.na(housingDataAll)))
colnames(dfMissingValues) <- c("# Missing Values")
columnsNA <- as.data.frame(housingDataAll[, dfMissingValues[,1] != 0])

#################################################################
##              Variables of choice for the model              ##
#################################################################

# LotArea
# LotShape
# GarageArea
# OverallQual

##############################################################################
##  Plots to support the story and to have an initial look at associations  ##
##############################################################################

# 1) Investigating possible non-linear terms among quantitative variables
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

#  2) Possible interaction effect LotArea*LotShape against SalePrice
scatterLotShapeInteraction <- ggplot(data = housingDataAll, aes(x = LotArea, y = SalePrice, colour = LotShape))

scatterLotShapeInteraction +
  geom_point() +
  geom_smooth(method = 'lm', aes(fill=LotShape), alpha = 0.5) +
  labs(x = "LotArea", y = "SalePrice") +  
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 120000))

# 3) Bar chart with LotShape showing that merging irregular shapes is desired
barBaseShapeAggregate <- ggplot(data = housingDataAll, aes(x = fct_infreq(LotShape)))

barBaseShapeAggregate + 
  geom_bar(aes(fill = LotShape), stat = "count") +
  geom_text(aes(LotShape, label = signif((..count..)/sum(..count..), 2)), stat = "count", nudge_y = 30) +
  labs(x = "LotShape", y = "Count") +
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 250), name = "Count of instances of each lot shape", sec.axis = sec_axis(~ . / 1460, name = "Ratio of instances of lot shape to total # of lots"))

# 4) Scatter plot with fitted line for GarageArea vs. SalePrice
scatterBaseGarageArea <- ggplot(data = housingDataAll, aes(x = GarageArea, y = SalePrice))

scatterBaseGarageArea +
  geom_point() +
  geom_smooth(method = 'lm', alpha = 0.5) +
  labs(x = "GarageArea", y = "SalePrice") +
  theme(legend.position="bottom") +
  scale_x_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300)) +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 120000))

# 5) Overall material and finish quality plotted against SalePrice
scatterBaseQualityLinear <- ggplot(data = housingDataAll, aes(x = OverallQual, y = SalePrice, colour = OverallQual))

scatterBaseQualityLinear +
  geom_point() +
  geom_smooth(method = 'lm', aes(fill=OverallQual), alpha = 0.5) +
  labs(x = "OverallQual", y = "SalePrice") +
  xlim(1, 10) +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 120000))

#  For saving the latest plot 
# ggsave(paste0(dirRslt, "scatterBaseQualityLinear.png"), width=8, height=6)

##################################################################
##           Data manipulation; dummy transformations           ##
##################################################################

# Reflection on whether the OverallQual has to remain categorical or can be expressed numerically in the model;
#'What has to be done is the following:
#'Prepare a temporary object for a regression analysis: Id, SalePrice, OverallQual, dummies for OverallQual
#'Two regression models: SalePrice on OverallQual as discrete, SalePrice on OverallQual dummies
#'In dummy model - if there is a constant increase in coefficiants then it is an indication of linearity
#'In discrete model - does the coefficient hint at an rather steep positive effect of OverallQual on SalePrice? 

overallQual_lintest.df <-
  housingDataAll %>% 
  dplyr::select(Id, SalePrice, OverallQual)

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

stargazer(model_overallQual_lin_discrete, model_overallQual_lin_dummy)

# Conclusion - we can work with OverallQual as a numerical variable instead of a scale (to be transformed to dummy) 
# because of the linear relationship of effects of consecutive increase in quality (seen in lm result and the graph). 
# The direct interpretation of beta will have no precise meaning, but the general association and standardized strength of the effect 
# can now be measured and interpreted more easily. 

# Dummy transformation
# Given a proportion of regular shaped homes to those irregularly shaped under IR1, IR2 and IR3 - group irregulars together
# Merging IR1, IR2, IR3 into IR
housingDataAll$LotShape <- ifelse(housingDataAll$LotShape == "Reg", "Regular", "Irregular")
housingDataAll$dLotShapeRegular <- ifelse(housingDataAll$LotShape == "Regular", "Regular", "Irregular")

# LotShape into dummy variable - 1 describes regular shape, 0 describes irregular shape
housingDataAll$dLotShapeRegular <- ifelse(housingDataAll$dLotShapeRegular == "Regular", 1, 0)

# Perform a check on the transformation
dplyr::select(housingDataAll, c("LotShape", "dLotShapeRegular"))

# Combine relevant variables into a df that is gonna be an input to model 
housingData <- 
  housingDataAll %>% 
  dplyr::select(Id, SalePrice, LotArea, GarageArea, OverallQual, LotShape, dLotShapeRegular)

############################################################
##      Summary statistics of variables in the model      ##
############################################################

stargazer(housingData, type = "text", summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"), omit = "Id", digits = 2)

# Latex
stargazer(housingData, summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"), omit = "Id", digits = 2)

# Coefficient of variance
cov_results <- round(cbind(SalePriceCV = sd(housingData$SalePrice)/mean(housingData$SalePrice),
      LotAreaCV = sd(housingData$LotArea)/mean(housingData$LotArea),
      GarageAreaCV = sd(housingData$GarageArea)/mean(housingData$GarageArea),
      OverallQualCV = sd(housingData$OverallQual)/mean(housingData$OverallQual),
      dLotShapeRegularCV = sd(housingData$dLotShapeRegular)/mean(housingData$dLotShapeRegular)
), 4)

# --------
# Factors
# --------
# Categorical variables (LotShape and dLotShapeRegular) to factors
housingData$LotShape <- factor(housingData$LotShape, levels = c("Irregular", "Regular"))
housingData$dLotShapeRegular <- factor(housingData$dLotShapeRegular, levels = c(0, 1))

####################################################################
##  Drawing a causal relationship scheme and developing a theory  ##
####################################################################

# Done outside of the code

############################################################################
##      Formulating research hypotheses and population regression models  ##
############################################################################

# H1: Garage area has a positive influence on the sale price of the house.
# H2: The expected sale price of the house is higher for houses with a better overall material and finish of the house.
# H3: The positive influence of lot size on sale price of the house is expected to be larger for regularly shaped houses than for irregularly shaped houses.


# Should quadratic terms be introduced for quantitative variables in the model in terms of model fit? And for which?
# LotArea goodness-of-fit
linear_LotArea <- lm(SalePrice ~ LotArea, data = housingDataAll)
quadratic_LotArea <- lm(SalePrice ~ LotArea + I(LotArea^2), data = housingDataAll)
stargazer(linear_LotArea, quadratic_LotArea, type = 'text')

# GarageArea goodness-of-fit
linear_GarageArea <- lm(SalePrice ~ GarageArea, data = housingDataAll)
quadratic_GarageArea <- lm(SalePrice ~ GarageArea + I(GarageArea^2), data = housingDataAll)
stargazer(linear_GarageArea, quadratic_GarageArea, type = 'text')
# Conclusion : R2 improvement suggest that introducing quadratic terms for LotArea will improve the model fit better. 

# -----------------------------
# Population regression models
# -----------------------------
# Introduced outside of the code

##################################################################
##   Estimating the baseline model and further specifications   ##
##################################################################

model_baseline <- lm(SalePrice ~ LotArea + GarageArea + dLotShapeRegular + OverallQual, data = housingData)

model_interactions <- lm(SalePrice ~ LotArea + LotArea*dLotShapeRegular + GarageArea + dLotShapeRegular + OverallQual, data = housingData)

model_nonlinear <- lm(SalePrice ~ LotArea + I(LotArea^2) + GarageArea + dLotShapeRegular + OverallQual, data = housingData)

model_interactions_nonlinear <- lm(SalePrice ~ LotArea + I(LotArea^2) + LotArea*dLotShapeRegular + GarageArea + dLotShapeRegular + OverallQual, data = housingData)

stargazer(model_baseline, model_interactions, model_nonlinear, type = "text")

stargazer(model_interactions_nonlinear, type = "text")

##################################################################
##                     Standardized effects                     ##
##################################################################

# Calculate standardized effects
lm_baseline_beta <- lm.beta(model_baseline)
lm_interactions_beta <- lm.beta(model_interactions)
lm_nonlinear_beta <- lm.beta(model_nonlinear)
lm_interactions_nonlinear_beta <- lm.beta(model_interactions_nonlinear)

# Manipulation of results to arrive at a final df to be used for reporting

# Set up data frames for standardized coefficients
baselina_beta_df <- as.data.frame(lm_baseline_beta[["standardized.coefficients"]], col.names = names(c("Standardized coefficients")))
interactions_beta_df <- as.data.frame(lm_interactions_beta[["standardized.coefficients"]], col.names = names(c("Standardized coefficients")))
nonlinear_beta_df <- as.data.frame(lm_nonlinear_beta[["standardized.coefficients"]], col.names = names(c("Standardized coefficients")))
interactions_nonlinear_beta_df <- as.data.frame(lm_interactions_nonlinear_beta[["standardized.coefficients"]], col.names = names(c("Standardized coefficients")))

# First generate original data.frames
building_stand_coeff1 <- merge(baselina_beta_df, interactions_beta_df, by = 0, all = TRUE)
building_stand_coeff2 <- merge(nonlinear_beta_df, interactions_nonlinear_beta_df, by = 0, all = TRUE)

# Then use first column for row names
building_stand_coeff1 <- data.frame(building_stand_coeff1, row.names = 1)
building_stand_coeff2 <- data.frame(building_stand_coeff2, row.names = 1)

# Merge again 
standardized_coefficients <- merge(building_stand_coeff1, building_stand_coeff2, by = 0, all = TRUE)

# Use first column for row names and change remaining columns' names to arrive at a final df
standardized_coefficients <- data.frame(standardized_coefficients, row.names = 1)
colnames(standardized_coefficients) <- c("Baseline", "Interactions", "Non-linear", "Interactions and non-linear")

# Output 
kable(round(standardized_coefficients, 4), "latex")

#################################################################
##         Diagnostic analyses of the estimated models         ##
#################################################################

# -----------------------------------
# Normality of independent variables
# -----------------------------------
#LotArea
lotarea_hist <- ggplot(data = housingData, aes(x = LotArea))
bp <- lotarea_hist + 
  geom_histogram(aes(y = ..density..), fill = "turquoise3") + 
  labs(x = "LotArea", y = "Density") + 
  scale_x_continuous(limits = c(0, 50000)) +
  scale_y_continuous(breaks = seq(0, 0.2, by = 0.005)) +
  stat_function(
    fun = dnorm, 
    args = with(housingData, c(mean = mean(LotArea), sd = sd(LotArea)))) +
  ggtitle("Histogram and density curve for LotArea")

#GarageArea
garagearea_hist <- ggplot(data = housingData, aes(x = GarageArea))
dp <- garagearea_hist + 
  geom_histogram(aes(y = ..density..), fill = "turquoise3") + 
  labs(x = "GarageArea", y = "Density") + 
  scale_y_continuous(breaks = seq(0, 0.2, by = 0.005)) +
  stat_function(
    fun = dnorm, 
    args = with(housingData, c(mean = mean(GarageArea), sd = sd(GarageArea)))) +
  ggtitle("Histogram and density curve for GarageArea")

# OverallQual
overallqual_hist <- ggplot(data = housingData, aes(x = OverallQual))
vp <- overallqual_hist + 
  geom_histogram(aes(y = ..density..), fill = "turquoise3") + 
  labs(x = "OverallQual", y = "Density") + 
  stat_function(
    fun = dnorm, 
    args = with(housingData, c(mean = mean(OverallQual), sd = sd(OverallQual)))) +
  ggtitle("Histogram and density curve for OverallQual")

# Remedy for the non-normality of LotArea - log transformation
housingData$ln_LotArea <- log(housingData$LotArea)
ln_lotarea_hist <- ggplot(data = housingData, aes(x = ln_LotArea))
sc <- ln_lotarea_hist + 
  geom_histogram(aes(y = ..density..), fill = "turquoise3") + 
  labs(x = "ln_LotArea", y = "log Density") + 
  stat_function(
    fun = dnorm, 
    args = with(housingData, c(mean = mean(ln_LotArea), sd = sd(ln_LotArea)))) +
  ggtitle("Histogram and density curve for ln_LotArea")

# Combine graphs with gridExtra
grid.arrange(bp, sc, ncol=2, nrow =1)

# Shapiro-Wilk test - aggregation for the reporting
shap_lotarea <- shapiro.test(housingData$LotArea)
shap_garagearea <- shapiro.test(housingData$GarageArea)
shap_overallqual <- shapiro.test(housingData$OverallQual)
shap_ln_lotarea <- shapiro.test(housingData$ln_LotArea)

var_name <- c("LotArea", "ln_LotArea", "GarageArea", "OverallQual")
SW_test_statistic <- c(shap_lotarea[["statistic"]], shap_ln_lotarea[["statistic"]], shap_garagearea[["statistic"]], shap_overallqual[["statistic"]])
SW_p_value <- c(shap_lotarea[["p.value"]], shap_ln_lotarea[["p.value"]], shap_garagearea[["p.value"]], shap_overallqual[["p.value"]])

shapiro_results <- data.frame(var_name,
                              SW_test_statistic,
                              SW_p_value,
                              row.names = var_name)

shapiro_results <-
  shapiro_results %>%
  dplyr::select(SW_test_statistic, SW_p_value)

kable(shapiro_results, "latex")

# Run simple regression model with LotArea and lnLotArea
model_lotarea_norm <- lm(SalePrice ~ LotArea, data = housingData)
model_ln_lotarea_norm <- lm(SalePrice ~ ln_LotArea, data = housingData)

stargazer(model_lotarea_norm, model_ln_lotarea_norm, type = 'text')

# -----------------------------------
# Homoscedasticity of the error term
# -----------------------------------

# Plot Residuals vs. Fitted for graphical evidence

# par(mfrow=c(1,2))
ols_plot_resid_fit(model_baseline)
ols_plot_resid_fit(model_interactions_nonlinear)
# par(mfrow=c(1,1))

# Perform Breusch-Pagan test for test-based evidence
lmtest::bptest(model_baseline)
lmtest::bptest(model_interactions_nonlinear)

# Remedy for heteroscedasticity - robust standard errors (they are more conservative)
seBasic <- sqrt(diag(vcov(model_baseline)))
seWhite <- sqrt(diag(vcovHC(model_baseline, type = 'HC0')))
seClust <- sqrt(diag(vcovHC(model_baseline, cluster = 'dLotShapeRegular')))

stargazer(model_baseline, model_baseline, model_baseline, se = list(seBasic, seWhite, seClust), type = 'text')

# -----------------------------
# Is multicollinearity a problem?
# -----------------------------

# Using VIF to assess whether multicollinearity exists in the model

vif_base <- vif(model_baseline)

stargazer(vif_base, type = 'text')

# There are no objective threshold, but vif = 5 may be used as a proxy for an existence of multicollinearity in the data

# Correlation matrix
subset_cor <- 
  housingData %>%
  select(SalePrice, LotArea, GarageArea, OverallQual)

corrplot(cor(subset_cor), method = "number")

# Remedy for multicollinearity - problem non-existent in the model

# -----------------------------------------------------------------------------------
# Analyzing the (standardized) residuals to assess whether their behavior is consistent 
# with the assumptions of the linear regression model
# -----------------------------------------------------------------------------------

# Inspecting residuals
resids_baseline <- residuals(model_baseline)
resids_interactions <- residuals(model_interactions)
resids_nonlinear <- residuals(model_nonlinear)
resids_interactions_nonlinear <- residuals(model_interactions_nonlinear)

residuals <- data.frame(resids_baseline, resids_interactions, resids_nonlinear, resids_interactions_nonlinear)

# Inspecting studentized residuals
studresids_baseline <- studres(model_baseline)
studresids_interactions <- studres(model_interactions)
studresids_nonlinear <- studres(model_nonlinear)
studresids_interactions_nonlinear <- studres(model_interactions_nonlinear)

stud_residuals <- data.frame(studresids_baseline, studresids_interactions, studresids_nonlinear, studresids_interactions_nonlinear)

# if the assumption of OLS holds, we should see 0 as the mean (it is an empirical mean, can be NOT EXACTLY 0, like -0)
# also looking at the range, we want residuals to be normally distribute, so the min-max should be of same magnitute
stargazer(residuals, stud_residuals, type = 'text')

# Residual vs Fitted Values Plot
#par(mfrow=c(2,2))
plot(fitted(model_baseline), resids_baseline, abline(0,0), main="Residuals vs Fitted Baseline")
plot(fitted(model_interactions), resids_interactions, abline(0,0), main="Residuals vs Fitted Interactions")
plot(fitted(model_nonlinear), resids_nonlinear, abline(0,0), main="Residuals vs Fitted Nonlinear")
plot(fitted(model_interactions_nonlinear), resids_interactions_nonlinear, abline(0,0), main="Residuals vs Fitted Interactions and Nonlinear")
#par(mfrow=c(1,1))

# Studentized Residual vs Fitted Values Plot
# par(mfrow=c(2,2))
plot(fitted(model_baseline), studresids_baseline, abline(0,0), main="Stu_Residuals vs Fitted Baseline")
plot(fitted(model_interactions), studresids_interactions, abline(0,0), main="Stu_Residuals vs Fitted Interactions")
plot(fitted(model_nonlinear), studresids_nonlinear, abline(0,0), main="Stu_Residuals vs Fitted Nonlinear")
plot(fitted(model_interactions_nonlinear), studresids_interactions_nonlinear, abline(0,0), main="Stu_Residuals vs Fitted Interactions and Nonlinear")
# par(mfrow=c(1,1))

#################################################################
##                       Subset analyses                       ##
#################################################################

# ---------------------------------------------
# Does the model differ for AC unit in houses?
# ---------------------------------------------

# Familiarize with the data
table(housingDataAll$CentralAir)
joinAC <- housingDataAll %>%
  dplyr::select(Id, CentralAir)

# Adjust housingData and run the models
housingDataAC <- merge(housingData, joinAC, by = "Id")
housingDataAC_N <- subset(housingDataAC, CentralAir == "N")
housingDataAC_Y <- subset(housingDataAC, CentralAir == "Y")
submodel_AC_N <- lm(SalePrice ~ LotArea + GarageArea + dLotShapeRegular + OverallQual, data = housingDataAC_N)
submodel_AC_Y <- lm(SalePrice ~ LotArea + GarageArea + dLotShapeRegular + OverallQual, data = housingDataAC_Y)

stargazer(submodel_AC_N, submodel_AC_Y, type = 'text')

# ---------------------------------------------------------------------------------------------
# Does the Age (YearRemodAdd - YearBuilt) affect the SalePrice and other effects in the model?
# ---------------------------------------------------------------------------------------------

# Set up new df with special variable "Age"
housingDataAge <- 
  housingDataAll %>%
  dplyr::select(Id, SalePrice, LotArea, GarageArea, OverallQual, LotShape, dLotShapeRegular, YearRemodAdd, YearBuilt)

housingDataAge$Age <- case_when(
  housingDataAge$YearRemodAdd == housingDataAge$YearBuilt ~ as.integer(format(Sys.Date(), "%Y")) - housingDataAge$YearBuilt,
  housingDataAge$YearRemodAdd != housingDataAge$YearBuilt ~ as.integer(format(Sys.Date(), "%Y")) - housingDataAge$YearRemodAdd
)

# Inspection of the Age distribution
age_histogram <- ggplot(data = housingDataAge, aes(Age))
age_histogram + geom_histogram(binwidth = 5) + labs(x = "Age", y = "Frequency")

#Splitting houses into "New" and "Old" based on the median of the Age (28)
median(housingDataAge$Age)

housingDataAge$NewOld <- if_else(housingDataAge$Age > 28, "Old", "New")
# table(housingDataAge$NewOld) 728 732

#Subset of New and Old for the model
housingDataAgeNew <- subset(housingDataAge, NewOld == "New")
housingDataAgeOld <- subset(housingDataAge, NewOld == "Old")

# Build a model and compare results
submodel_age_new <- lm(SalePrice ~ LotArea + GarageArea + dLotShapeRegular + OverallQual, data = housingDataAgeNew)
submodel_age_old <- lm(SalePrice ~ LotArea + GarageArea + dLotShapeRegular + OverallQual, data = housingDataAgeOld)

stargazer(submodel_age_new, submodel_age_old, type = 'text')

# Graphical evidence for the models' results
ad <- 
  ggplot(data = housingDataAge, aes(x = Age, y = SalePrice)) +
  geom_point(colour = "magenta4") +
  geom_smooth(method = 'lm', alpha = 0.5, colour = "black") +
  labs(x = "Age", y = "SalePrice") +
  theme(legend.position="bottom") +
  scale_x_continuous(limits = c(10, 75), breaks = seq(0, 75, by = 15)) +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 240000))

ko <- 
  ggplot(data = housingDataAge, aes(x = LotArea, y = SalePrice, colour = NewOld)) +
  geom_point(colour = "magenta4") +
  geom_smooth(method = 'lm', aes(fill=NewOld), alpha = 0.5) +
  labs(x = "LotArea", y = "SalePrice") +  
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 240000))

nl <- 
  ggplot(data = housingDataAge, aes(x = GarageArea, y = SalePrice, colour = NewOld)) +
  geom_point(colour = "magenta4") +
  geom_smooth(method = 'lm', aes(fill=NewOld), alpha = 0.5) +
  labs(x = "GarageArea", y = "SalePrice") +  
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 240000))

rt <-
  ggplot(data = housingDataAge, aes(x = OverallQual, y = SalePrice, colour = NewOld)) +
  geom_point(colour = "magenta4") +
  geom_smooth(method = 'lm', aes(fill=NewOld), alpha = 0.5) +
  labs(x = "OverallQual", y = "SalePrice") +  
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(30000, 755000), breaks = seq(30000, 755000, by = 240000))

# Combine graphs with gridExtra
grid.arrange(ad, ko, nl, rt, ncol=2, nrow =2)
