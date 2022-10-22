
############################################################################
############################################################################
###                                                                      ###
###                ALEKSANDER ODZIEMKOWSKI, BAM01 AS&P                   ###
###                INDIVIDUAL ASSIGNMENT 2, 28.09.2022                   ###
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
library(stargazer)
library(corrplot)
library(lm.beta)
library(knitr)
library(gridExtra)
library(tidyverse)
library(plyr)
library(reshape2)
library(ggplot2)
library(olsrr)
library(lm.beta)
library(sandwich)
library(car)
library(bannerCommenter)
library(AER)
library(xtable)
library(ivreg)

# ----------------------------------------------------
# Set working directory and define folder's structure
# ----------------------------------------------------
setwd("C:/STUDIA - materiały/RSM-MASTERS/COURSES/BLOCK 1/Advanced Statistics and Programming/Assignment 2")

dir <- "C:/STUDIA - materiały/RSM-MASTERS/COURSES/BLOCK 1/Advanced Statistics and Programming/Assignment 2/"

dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Results/")


##################################################################
##        Difference-in-Difference analysis: female labor       ##
##                    force participation                       ##
##################################################################


#################################################################
##              Import and inspect the DiD data                ##
#################################################################

# -----------------------------------
# Import CSV data - DiD_dataset.csv
# -----------------------------------
options(digits=7)

dfDiDAll <- read.csv(file = paste0(dirData, "DiD_dataset.csv"), header = TRUE, stringsAsFactors = FALSE)


# Checking n/a per variable as linear regression model will not handle missing values;
dfMissingValues <- as.data.frame(colSums(is.na(dfDiDAll)))
colnames(dfMissingValues) <- c("# Missing Values")
# Conclusion - no missing values, dataset is complete

str(dfDiDAll)
# Conclusion - all variables seem to have a correct data type


# -----------------------------------
# Data manipulation and cleaning
# -----------------------------------

# Introduce the period indicator
dfDiDAll$dPeriod <- ifelse(dfDiDAll$year < 1993, 0, 1)

# Introduce a dummy variable for a presence of children
dfDiDAll$dChildren <- ifelse(dfDiDAll$children == 0, 0, 1)

# Take a look at possible duplicates 
dfDiDDuplicates <- as.data.frame(dfDiDAll[duplicated(dfDiDAll),])
# Conclusion- there are 20 duplicates but their nature cannot be conveniently assessed



#################################################################
##              Equations and their adjustment                 ##
#################################################################


#' Y <- the effect; dfDiDAll$work
#' T0 <- pre-effect-period; dfDiDAll$year in 1991, 1992
#' T1 <- post-effect-period; dfDiDAll$year in 1993 and beyond
#' D0 <- no children; dfDiDAll$dChildren = 0
#' D1 <- had children; dfDiDAll$dChildren = 1
#' 
#' State id or state's unemployment rate can be added as a control variable to account for local labor market effects



##################################################################################
##              Plots as visual evidence of the DiD effect of the EITC          ##
##       by means of annual earnings, annual family and working/non-working     ##
##################################################################################

# 1) Annual earnings
lineEarnEffect <- ggplot(dfDiDAll, aes(x = year, y = earn, group = dChildren, color = dChildren))

lineEarnEffect + 
  stat_summary(geom = "line", fun = mean, lwd=1.2) +
  labs(x = "Year", y = "Annual earnings") +
  geom_vline(xintercept=as.numeric(1993), linetype = 4) +
  scale_colour_gradient(limits = c(0, 1), 
                        breaks = c(0, 1),
                        labels = c(0, 1)) +
  theme(legend.position="right")

# ggsave(paste0(dirRslt, "lineEarnEffect.png"), width = 11, height = 6)

# 2) Family income
lineFincEffect <- ggplot(dfDiDAll, aes(x = year, y = finc, group = dChildren, color = dChildren))

lineFincEffect + 
  stat_summary(geom = "line", fun = mean, lwd=1.2) +
  labs(x = "Year", y = "Family earnings") +
  geom_vline(xintercept=as.numeric(1993), linetype = 4) +
  scale_colour_gradient(limits = c(0, 1), 
                        breaks = c(0, 1),
                        labels = c(0, 1)) +
  theme(legend.position="right")

# ggsave(paste0(dirRslt, "lineFincEffect.png"), width = 11, height = 6)

# 3) Work indicator
lineWorkEffect <- ggplot(dfDiDAll, aes(x = year, y = work, group = dChildren, color = dChildren))

lineWorkEffect + 
  stat_summary(geom = "line", fun = mean, lwd=1.2) +
  labs(x = "Year", y = "Indicator of work status (proportion)") +
  geom_vline(xintercept=as.numeric(1993), linetype = 4) +
  scale_colour_gradient(limits = c(0, 1), 
                        breaks = c(0, 1),
                        labels = c(0, 1)) +
  theme(legend.position="right")

# ggsave(paste0(dirRslt, "lineWorkEffect.png"), width = 11, height = 6)

##################################################################################
##              Stargazer to provide a table with summary statistics            ##
##                and concise description of the data in the table              ##
##################################################################################

order = which(names(dfDiDAll)%in%c("urate", "finc", "earn", "unearn", "age", "ed", "work", "children", "dChildren", "nonwhite", "dPeriod"))

stargazer(dfDiDAll, 
          summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"), 
          omit = c("state", "year"), 
          digits = 2,
          order = order,
          type = 'text'
          )


##################################################################################
##              Provide matrices of three difference-in-differences             ##
##                effects of EITC introduction per each variable                ##
##################################################################################

# -----------------------------------
# Indicator of work status
# -----------------------------------

# Prepare the means
avgEmpl <- ddply(dfDiDAll, .(dPeriod, dChildren), summarise,
                 avgEmploy = mean(work))

# Make table for the outcomes
tmp <- dcast(avgEmpl, dPeriod ~ dChildren, value.var = 'avgEmploy')
tmp <- rbind(tmp, tmp[2,]-tmp[1,])

# Rename the rows and columns
rownames(tmp) <- c("Pre-effect", "Post-effect", "Difference")
colnames(tmp) <- c("dPeriod", "No children", "Had children")
tmp[3, "dPeriod"] <- NA

# Table with the results
stargazer(tmp, summary = FALSE, align = TRUE, type = 'text')


# -----------------------------------
# Annual earnings
# -----------------------------------

# Prepare the means
avgEarn <- ddply(dfDiDAll, .(dPeriod, dChildren), summarise,
                 avgEarn = mean(earn))

# Make table for the outcomes
tmpEarn <- dcast(avgEarn, dPeriod ~ dChildren, value.var = 'avgEarn')
tmpEarn <- rbind(tmpEarn, tmpEarn[2,]-tmpEarn[1,])

# Rename the rows
rownames(tmpEarn) <- c("Pre-effect", "Post-effect", "Difference")
colnames(tmpEarn) <- c("dPeriod", "No children", "Had children")
tmpEarn[3, "dPeriod"] <- NA


# Table with the results
stargazer(tmpEarn, summary = FALSE, align = TRUE, type = 'text')


# -----------------------------------
# Family annual income
# -----------------------------------

# Prepare the means
avgFinc <- ddply(dfDiDAll, .(dPeriod, dChildren), summarise,
                  avgFinc = mean(finc))

# Make table for the outcomes
tmpFinc <- dcast(avgFinc, dPeriod ~ dChildren, value.var = 'avgFinc')
tmpFinc <- rbind(tmpFinc, tmpFinc[2,]-tmpFinc[1,])

# Rename the rows
rownames(tmpFinc) <- c("Pre-effect", "Post-effect", "Difference")
colnames(tmpFinc) <- c("dPeriod", "No children", "Had children")
tmpFinc[3, "dPeriod"] <- NA


# Table with the results
stargazer(tmpFinc, summary = FALSE, align = TRUE, type = 'text')

##################################################################################
##              Regression models for the three dependent variables.            ##
##  What is the effect of the policy introduction on the dependent variables?   ##
##            How does the effect change when adding control variables?         ##
##                      Are robust standard errors necessary?                   ##
##################################################################################

# ------------------------------------------------------------------------
# Running the actual difference-in-difference model and getting estimates
# ------------------------------------------------------------------------

# Define models
mdlWork <- work ~ dChildren + dPeriod +dChildren:dPeriod
mdlEarn <- earn ~ dChildren + dPeriod +dChildren:dPeriod
mdlFinc <- finc ~ dChildren + dPeriod +dChildren:dPeriod

# Estimate models
rstlOLSWork <- lm(mdlWork, data = dfDiDAll)
rstlOLSEarn <- lm(mdlEarn, data = dfDiDAll)
rstlOLSFinc <- lm(mdlFinc, data = dfDiDAll)


# The summary table
stargazer(rstlOLSWork, rstlOLSEarn, rstlOLSFinc,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')


# -------------------------
# Adding control variables
# -------------------------

mdlWorkControl <- work ~ dChildren + dPeriod +dChildren:dPeriod + urate + nonwhite
mdlEarnControl <- earn ~ dChildren + dPeriod +dChildren:dPeriod + urate + nonwhite
mdlFincControl <- finc ~ dChildren + dPeriod +dChildren:dPeriod + urate + nonwhite

rstlOLSWorkControl <- lm(mdlWorkControl, data = dfDiDAll)
rstlOLSEarnControl <- lm(mdlEarnControl, data = dfDiDAll)
rstlOLSFincControl <- lm(mdlFincControl, data = dfDiDAll)

stargazer(rstlOLSWorkControl, rstlOLSEarnControl, rstlOLSFincControl,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')


# --------------
# Robust errors
# --------------

# Remedy for heteroscedasticity - robust standard errors (they are more conservative)
# Check if introducing robust errors increase the value of standard errors

# Work indicator
seBasicWork <- sqrt(diag(vcov(rstlOLSWork)))
seWhiteWork <- sqrt(diag(vcovHC(rstlOLSWork, type = 'HC0')))
seClustWork <- sqrt(diag(vcovHC(rstlOLSWork, cluster = 'state')))

stargazer(rstlOLSWork, rstlOLSWork, rstlOLSWork, se = list(seBasicWork, seWhiteWork, seClustWork),
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE, 
          type = 'text')

# Earnings
seBasicEarn <- sqrt(diag(vcov(rstlOLSEarn)))
seWhiteEarn <- sqrt(diag(vcovHC(rstlOLSEarn, type = 'HC0')))
seClustEarn <- sqrt(diag(vcovHC(rstlOLSEarn, cluster = 'state')))

stargazer(rstlOLSEarn, rstlOLSEarn, rstlOLSEarn, se = list(seBasicEarn, seWhiteEarn, seClustEarn),
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE, 
          type = 'text')


# Family income
seBasicFinc <- sqrt(diag(vcov(rstlOLSFinc)))
seWhiteFinc <- sqrt(diag(vcovHC(rstlOLSFinc, type = 'HC0')))
seClustFinc <- sqrt(diag(vcovHC(rstlOLSFinc, cluster = 'state')))

stargazer(rstlOLSFinc, rstlOLSFinc, rstlOLSFinc, se = list(seBasicFinc, seWhiteFinc, seClustFinc),
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE, 
          type = 'text')


##################################################################################
##     Do high-education mothers react differently to the EITC policy measure   ##
##                than low education mothers? 9 years as a threshold            ##
##                          in terms of length of education                     ##
##################################################################################

# Estimate models:

# Single mothers with high education and with children. Compare
# them with single mothers with low education and with children.

#Introducing a dummy variable for differentiating between high ed and low ed
dfDiDAll$dHighEd <- ifelse(dfDiDAll$ed < 9, 0, 1)

mdlWorkHaveChild <- work ~ dHighEd + dPeriod + dHighEd:dPeriod
mdlEarnHaveChild <- earn ~ dHighEd + dPeriod + dHighEd:dPeriod
mdlFincHaveChild <- finc ~ dHighEd + dPeriod + dHighEd:dPeriod

rstlOLSWorkHaveChild <- lm(mdlWorkHaveChild, data = dfDiDAll, subset = (dfDiDAll$children == 1))
rstlOLSEarnHaveChild <- lm(mdlEarnHaveChild, data = dfDiDAll, subset = (dfDiDAll$children == 1))
rstlOLSFincHaveChild <- lm(mdlFincHaveChild, data = dfDiDAll, subset = (dfDiDAll$children == 1))

a1 <- rstlOLSWork
a2 <- rstlOLSWorkHaveChild
b1 <- rstlOLSEarn
b2 <- rstlOLSEarnHaveChild
c1 <- rstlOLSFinc
c2 <-rstlOLSFincHaveChild

stargazer(a2, b2, c2,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')

stargazer(a1, a2,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')

stargazer(b1, b2,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')

stargazer(c1, c2,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')


# Single women with low education, without children. Compare them
# with single women with low education, with children.

mdlWorkLowEd <- work ~ dChildren + dPeriod +dChildren:dPeriod
mdlEarnLowEd <- earn ~ dChildren + dPeriod +dChildren:dPeriod
mdlFincLowEd <- finc ~ dChildren + dPeriod +dChildren:dPeriod

rstlOLSWorkLowEd <- lm(mdlWorkLowEd, data = dfDiDAll, subset = (dfDiDAll$dHighEd == 0))
rstlOLSEarnLowEd <- lm(mdlEarnLowEd, data = dfDiDAll, subset = (dfDiDAll$dHighEd == 0))
rstlOLSFincLowEd <- lm(mdlFincLowEd, data = dfDiDAll, subset = (dfDiDAll$dHighEd == 0))

stargazer(rstlOLSWorkLowEd, rstlOLSEarnLowEd, rstlOLSFincLowEd,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')



##################################################################
##                Instrumental Variable analysis:               ##
##            effect of compulsory schooling on wages           ##
##################################################################


#################################################################
##              Import and inspect the IV data                 ##
#################################################################

# -----------------------------------
# Import CSV data - IV_dataset.csv
# -----------------------------------

dfIVAll <- read.csv(file = paste0(dirData, "IV_dataset.csv"), header = TRUE, stringsAsFactors = FALSE)

# Select only the variables that are mentioned in the task
dfIVAll <- 
  dfIVAll %>%
  select(age, educ, lnwage, married, qob, SMSA, yob)

# Checking n/a per variable as linear regression model will not handle missing values;
dfMissingValuesIV <- as.data.frame(colSums(is.na(dfIVAll)))
colnames(dfMissingValuesIV) <- c("# Missing Values")
# Conclusion - no missing values, dataset is complete

str(dfIVAll)
# Conclusion - structure is correct


#################################################################
##        Task 2: Summary statistics and concise descrp        ##
#################################################################
order1 = which(names(dfIVAll)%in%c("lnwage", "age", "educ", "married", "qob", "yob", "SMSA"))

stargazer(dfIVAll, 
          summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"), 
          digits = 2,
          order = order1,
          type = 'text')

#################################################################
##                            Task 3:                          ##
##    Explore if variable qob meets the relevance criterion    ##
##        as a good instrument for years of education          ##
#################################################################

# Relevance: need to have sufficiently strong correlation 
# between the instrument and the instrumentalized variables

# -------------
# Regressions 
# -------------

# partial F-test - run the iv regression and perform diagnostics 
# to assess the strength of the instrument
rsltSLS <- ivreg(lnwage ~ educ | qob,
                 data = dfIVAll)

stargazer(rsltSLS, 
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')

summ.ivreg <- summary(rsltSLS, diagnostics = TRUE)

xtable(summ.ivreg$diagnostics)

# -----------------------
# Plot of QOB relevance
# -----------------------

scatterQobEduc <- ggplot(data = dfIVAll, aes(x = qob, y = educ))

ad <- scatterQobEduc +
  stat_summary(geom = "line", fun = mean, lwd=1.2, color = "turquoise3") +
  labs(x = "Quarter of birth", y = "Average years of education") +
  theme(legend.position="bottom") +
  ggtitle(label = "Association between average years of education \nand quarter of birth on a compressed y-axis.")

bo <- scatterQobEduc +
  stat_summary(geom = "line", fun = mean, lwd=1.2, color = "turquoise3") +
  labs(x = "Quarter of birth", y = "Average years of education") +
  theme(legend.position="bottom") +
  ylim(0,20) +
  ggtitle(label = "Association between average years of education \nand quarter of birth on a full range of educ.")

# Combine graphs with gridExtra
grid.arrange(ad, bo, ncol=2, nrow =1)

# ggsave(paste0(dirRslt, "scatterQobEduc.png"), width = 11, height = 8)


scatterQobLnwage <- ggplot(data = dfIVAll, aes(x = qob, y = lnwage))

ca <- scatterQobLnwage +
  stat_summary(geom = "line", fun = mean, lwd=1.2, color = "turquoise3") +
  labs(x = "Quarter of birth", y = "Average log-transformed wages") +
  theme(legend.position="bottom") +
  ggtitle(label = "Association between quarter of birth \nand average log wages on a compressed y-axis.")

hu <- scatterQobLnwage +
  stat_summary(geom = "line", fun = mean, lwd=1.2, color = "turquoise3") +
  labs(x = "Quarter of birth", y = "Average log-transformed wages") +
  theme(legend.position="bottom") +
  ylim(0,10) +
  ggtitle(label = "Association between quarter of birth \nand average log wages on a full range of lnwage.")

# Combine graphs with gridExtra
grid.arrange(ca, hu, ncol=2, nrow =1)

# ggsave(paste0(dirRslt, "scatterQobLnwage.png"), width = 11, height = 8)

# Correlation matrix
cor(dfIVAll$educ, dfIVAll$qob)

subset_cor <- 
  dfIVAll %>%
  select(educ, lnwage, qob)

corrplot(cor(subset_cor), method = "number")
# ggsave(paste0(dirRslt, "corrplot.png"), width = 11, height = 8)

# Correlation coefficient - too low to display in the matrix
cor(dfIVAll$educ, dfIVAll$qob)


#######################################################################
##                               Task 4:                             ## 
##  IV regression analysis of the effect of education on log wages,  ##
##             using quarter of birth as the instrument.             ##
##  What is the effect of an additional year of education on wages?  ##
#######################################################################

rsltSLS <- ivreg(lnwage ~ educ | qob,
                      data = dfIVAll)

stargazer(rsltSLS, 
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')

summary(rsltSLS, diagnostics = TRUE)

####################################################################################################
##                   How does the effect change when adding control variables?                    ##
##  Examine if the use of robust standard errors critically affects your statistical inferences.  ##
####################################################################################################

# Adding control variables / exogenous regressors 
rsltSLSControl <- ivreg(lnwage ~ educ | qob + married + SMSA,
                         data = dfIVAll)

rsltSLSControl2 <- ivreg(lnwage ~ educ | qob + married + SMSA + age,
                        data = dfIVAll)

stargazer(rsltSLS, rsltSLSControl, rsltSLSControl2,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')

# Robust standard errors
seBasicSLS <- sqrt(diag(vcov(rsltSLSControl)))
seWhiteSLS <- sqrt(diag(vcovHC(rsltSLSControl, type = 'HC0')))
seClustSLS <- sqrt(diag(vcovHC(rsltSLSControl, cluster = 'age')))

stargazer(rsltSLSControl, rsltSLSControl, rsltSLSControl, se = list(seBasicSLS, seWhiteSLS, seClustSLS),
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE, 
          type = 'text')

# Notice a slight difference that can be omitted due to rounding in stargazer output
print(seBasicSLS)
print(seWhiteSLS)
print(seClustSLS)


###########################################################################################
##  Following the previous analysis, conduct the same regressions with OLS as with IV.   ##
##      Apply a formal test to decide between the two OLS and IV-estimated models.       ##
##          Examine if over-identification can be an issue in the IV analysis.           ##
###########################################################################################

rsltOLS <- lm(lnwage ~ educ,
              data = dfIVAll)

stargazer(rsltOLS, rsltSLS, 
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE,
          type = 'text')

# Formal test - OLS or IV? 
summ.hausman <- summary(rsltSLS, diagnostics = TRUE)

xtable(summ.hausman$diagnostics)

# Formal test - is over-identification an issue? 

summ.sagan <- summary(rsltSLSControl, diagnostics = TRUE)

xtable(summ.sagan$diagnostics)
