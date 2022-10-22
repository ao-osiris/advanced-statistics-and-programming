############################################################################
############################################################################
###                                                                      ###
###                ALEKSANDER ODZIEMKOWSKI, BAM01 AS&P                   ###
###                INDIVIDUAL ASSIGNMENT 3, 12.10.2022                   ###
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

library(wbstats)
library(stargazer)
library(MASS)
library(plm)
library(tidyverse)
library(plyr)
library(ggplot2)
library(bannerCommenter)
library(xtable)
library(broom)
library(sandwich)
library(extraDistr)
library(knitr)

# ----------------------------------------------------
# Set working directory and define folder's structure
# ----------------------------------------------------
setwd("C:/STUDIA - materiały/RSM-MASTERS/COURSES/BLOCK 1/Advanced Statistics and Programming/Assignment 3")

dir <- "C:/STUDIA - materiały/RSM-MASTERS/COURSES/BLOCK 1/Advanced Statistics and Programming/Assignment 3/"

dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Results/")


##################################################################
##             Panel data modeling: time to export              ##
##################################################################

# Download selected data from the portal and store the data in dataframe dfTime2Export

# document_vars <- wb_search(pattern = "document")
# export_vars <- wb_search(pattern = "export")
# customs_vars <- wb_search(pattern = "customs")

# Variables of choice:
# IC.EXP.TMBC - Time to export, border compliance (hours)
# IC.EXP.CSBC.CD - Cost to export, border compliance (US$)
# GC.TAX.EXPT.ZS - Taxes on exports (% of tax revenue)
# NE.EXP.GNFS.KD.ZG - Exports of goods and services (annual % growth)
# need to multiply % by a 100 so that a unit change is in fact a percentage point change
# TX.VAL.INSF.ZS.WT - Insurance and financial services (% of commercial service exports)

dfTime2Export <-
  wb_data(indicator = c("IC.EXP.TMBC",
                        "IC.EXP.CSBC.CD",
                        "GC.TAX.EXPT.ZS", 
                        "NE.EXP.GNFS.KD.ZG"),
          country = "countries_only",
          start_date = 1960,
          end_date = 2021)

# Rename the columns 
colnames(dfTime2Export)[colnames(dfTime2Export) == "iso2c"]             <- "Country.iso2"
colnames(dfTime2Export)[colnames(dfTime2Export) == "iso3c"]             <- "Country.iso3"
colnames(dfTime2Export)[colnames(dfTime2Export) == "country"]           <- "Country"
colnames(dfTime2Export)[colnames(dfTime2Export) == "date"]              <- "Year"
colnames(dfTime2Export)[colnames(dfTime2Export) == "IC.EXP.TMBC"]       <- "Time2Export"
colnames(dfTime2Export)[colnames(dfTime2Export) == "GC.TAX.EXPT.ZS"]    <- "TaxOnExport"
colnames(dfTime2Export)[colnames(dfTime2Export) == "IC.EXP.CSBC.CD"]    <- "Cost2Export"
colnames(dfTime2Export)[colnames(dfTime2Export) == "NE.EXP.GNFS.KD.ZG"] <- "ExportGrowthRate"


# ------------------------------------------
# Formulate a population regression model
# ------------------------------------------

# Time2Export=\ \beta_0\ +\ \beta_1Cost2Export\ +\ \beta_2TaxOnExport\ +\ \beta_3ExportGrowthRate+\ \varepsilon,\ \ \varepsilon\ ~\ n(0,\ \sigma)

# ------------------------------------------
# Balance the data
# ------------------------------------------

# Remove missing values from the dependent variable
dfTime2Export <- subset(dfTime2Export, !is.na(dfTime2Export$Time2Export))

# Select only complete cases for the period 2014-2019
dfTime2Export.sub <- dfTime2Export[complete.cases(dfTime2Export),]
dfTime2Export.sub <- dfTime2Export.sub[dfTime2Export.sub$Year >= 2014 & dfTime2Export.sub$Year <= 2019,]


# ------------------------------------------
# Summary statistics
# ------------------------------------------

data.frame(colnames(dfTime2Export.sub))
order1 = c(7, 5, 6, 8)

stargazer(as.data.frame(dfTime2Export.sub), 
          summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"), 
          omit = c("Country.iso2", "Country.iso3", "Country", "Year"), 
          digits = 3,
          order = order1,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE
          , type = 'text'
          )

# ---------------------------------------------
# Estimate 4 models - pooled, between, FE, RE
# ---------------------------------------------

modelBase <- Time2Export ~ Cost2Export + TaxOnExport + ExportGrowthRate

# pooled
results.Pooling <- plm(modelBase, data = dfTime2Export.sub, model = "pooling")

# between
dfTime2Export.avg <- 
  ddply(dfTime2Export.sub, .(Country), summarise,
        avg.Time2Export        = mean(Time2Export, na.rm=TRUE),
        avg.Cost2Export        = mean(Cost2Export, na.rm=TRUE),
        avg.TaxOnExport        = mean(TaxOnExport, na.rm=TRUE),
        avg.ExportGrowthRate   = mean(ExportGrowthRate, na.rm=TRUE),
        numValid               = length(Country))

dfTime2Export.sub <- merge(dfTime2Export.sub, dfTime2Export.avg, by="Country")

attach(dfTime2Export.sub)
dfTime2Export.sub$diff.Time2Export       <- Time2Export      - avg.Time2Export
dfTime2Export.sub$diff.Cost2Export       <- Cost2Export      - avg.Cost2Export
dfTime2Export.sub$diff.TaxOnExport       <- TaxOnExport      - avg.TaxOnExport
dfTime2Export.sub$diff.ExportGrowthRate  <- ExportGrowthRate - avg.ExportGrowthRate
detach(dfTime2Export.sub)

# Balancing again
dfTime2Export.sub <- dfTime2Export.sub[dfTime2Export.sub$numValid == 6,]
dfTime2Export.avg <- dfTime2Export.avg[dfTime2Export.avg$numValid == 6,]


# ... find the variables of interest
modelVars      <- all.vars(modelBase)
modelVars.avg  <- paste0("avg.", modelVars)
modelVars.diff <- paste0("diff.", modelVars)

# ... select variables from the data frames
dfTime2Export.between <- dfTime2Export.avg[modelVars.avg]
dfTime2Export.within  <- dfTime2Export.sub[modelVars.diff]

# ... rename column names
colnames(dfTime2Export.within) <- 
  gsub("diff\\.", "", colnames(dfTime2Export.within))
colnames(dfTime2Export.between) <- 
  gsub("avg\\.", "", colnames(dfTime2Export.between))

results.Between <- lm(modelBase, data=dfTime2Export.between)

# Fixed effects
results.FE.Country <- 
  plm(modelBase, data = dfTime2Export.sub, 
      index=c("Country", "Year"), model = "within")

# Random effects
results.RE.Country <- 
  plm(modelBase, data = dfTime2Export.sub, 
      index=c("Country", "Year"), model = "random")

# Tabulating the results
stargazer(results.Pooling, results.Between, results.FE.Country, results.RE.Country,
          column.labels = c("Pooled", "Between", "Fixed", "Random"),
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE
          , type="text"
          )


# ------------------------------
# Formal tests to pick a model 
# ------------------------------

# First of all, evaluate the fixed effects model versus the pooled regression model

# Which one should we go with? 
partialF_test <- pFtest(results.FE.Country, results.Pooling)

partialF_test <- tidy(partialF_test)
partialF_test$statistic <- round(as.numeric(partialF_test$statistic),3)
names(partialF_test)[1:6] <- c('df1', 'df2', 'Statistic', 'p-value','df', 'Method', 'Alternative')

stargazer(partialF_test,
          summary = FALSE
          #, type = 'text'
          )

#' insignificant pool test would say that both models are consistent which nodes slightly in favor of pooled model
#' significant pool test rejects null in favor of fixed effect model


# Should fixed effects model be picked over the random effects model?
hausman_test <- phtest(results.FE.Country, results.RE.Country)

hausman_test <- tidy(hausman_test)
hausman_test$statistic <- round(as.numeric(hausman_test$statistic),3)
hausman_test$parameter <- as.numeric(hausman_test$parameter)
names(hausman_test)[1:5] <- c('Statistic', 'p-value','df', 'Method', 'Alternative')

stargazer(hausman_test,
          summary = FALSE
          , type = 'text'
          )


#################################################################
##           Counts data modeling: the Mashable case           ##
#################################################################

# Read the data
dfMashAll <- read.csv(file = paste0(dirData, "OnlineNewsPopularity.csv"), header = TRUE)


# Pick 9 most important variables that explain the sharing of online news articles

# shares - Number of shares (target)
# is_weekend - Was the article published on the weekend?
# n_tokens_content - Number of words in the content
# num_hrefs - Number of links
# num_imgs - Number of images
# num_videos - Number of videos
# num_keywords - Number of keywords in the metadata
# n_tokens_title - Number of words in the title
# global_sentiment_polarity - Text sentiment polarity (**to include as index, *100)
# global_subjectivity - Text subjectivity (**to include as index, *100)


dfMash.sub <- 
  dfMashAll %>%
  mutate(
  index_sentiment_polarity = global_sentiment_polarity * 100,
  index_subjectivity = global_subjectivity * 100
  ) %>%
  dplyr::select(shares, n_tokens_title, n_tokens_content, num_hrefs, num_imgs, num_videos, num_keywords, is_weekend, index_sentiment_polarity, index_subjectivity)

# -----------------------------
# Summary statistics
# -----------------------------

stargazer(dfMash.sub, 
          summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"), 
          digits = 2,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE
          , type = 'text'
)

# ------------------------------------------
# Formulate a population regression model
# ------------------------------------------

# shares=\ \beta_0\ +\ \beta_1n_tokens_title+\ \beta_2n_tokens_content\ +\ \beta_3num_hrefs+\beta_4num_imgs\ +\beta_5num_videos\ +\ \beta_6num_hrefs+\beta_7num_keywords+\beta_8is_weekend\ +\beta_9index_sentiment_polarity\ +\ \beta_{10}index_subjectivity\ +\varepsilon,\ \ \varepsilon\ ~\ n(0,\ \sigma)


# -----------------------------------------------------------------------------
# Estimate unobserved parameters with the most appropriate statistical method
# -----------------------------------------------------------------------------

# Model specification
modelA <- shares ~ n_tokens_title + n_tokens_content + num_hrefs + num_imgs + num_videos + num_keywords + is_weekend + index_sentiment_polarity + index_subjectivity

# Model estimation
rsltCountOLS     <- lm(modelA, data = dfMash.sub)
rsltCountPoisson <- glm(modelA, data = dfMash.sub, family=c("poisson"))
rsltCountQuasi   <- glm(modelA, data=dfMash.sub, family=c("quasipoisson"))
rsltCountNegBin  <- glm.nb(modelA, data=dfMash.sub)

modelAPlus <- modelA

modelAPlus[[3]][[1]] <- quote(`|`)
modelAPlus[[3]][[2]] <- modelA[[3]]
modelAPlus[[3]][[3]] <- quote(1)

# Make table of the results
seWhite <- sqrt(diag(vcovHC(rsltCountPoisson, type="HC0")))

C.OLS <- rsltCountOLS
C.Poisson <- rsltCountPoisson
C.Quasi <- rsltCountQuasi
C.NegBin <- rsltCountNegBin


stargazer(C.OLS, C.Poisson, C.Poisson, C.Quasi, C.NegBin,
          align=TRUE, no.space = TRUE, intercept.bottom = FALSE,
          se = list(NULL, NULL, seWhite, NULL, NULL)
          , type="text"
          )

# new version - split
stargazer(C.OLS, C.Poisson, C.Poisson,
          align=TRUE, no.space = TRUE, intercept.bottom = FALSE,
          se = list(NULL, NULL, seWhite)
          #star.cutoffs = NA,
          #omit.table.layout = "n"
          #, type="text"
)


stargazer(C.OLS, C.Quasi, C.NegBin,
          align=TRUE, no.space = TRUE, intercept.bottom = FALSE
          # star.cutoffs = NA,
          # omit.table.layout = "n"
          #, type="text"
)


# ----------------------------------------------------------------
# Partial effects for a preferred model and comparison with OLS
# ----------------------------------------------------------------

# Find parameter estimates 
estBeta <- coef(rsltCountNegBin)

# Calculate the average partial effects, APE
Count.APE <- mean(exp(predict.glm(rsltCountNegBin, type="link")))*estBeta
round(Count.APE, 3)

# Calculate the average partial effects for dummy, APE
tmp.c <- dfMash.sub

tmp.c$is_weekend[tmp.c$is_weekend == 0] <- 1
tmpAPE.1 <- mean(exp(predict.glm(rsltCountNegBin, newdata=tmp.c, type="link")))
round(tmpAPE.1, 3)

tmp.c$is_weekend[tmp.c$is_weekend == 1] <- 0
tmpAPE.0 <- mean(exp(predict.glm(rsltCountNegBin, newdata=tmp.c, type="link")))
round(tmpAPE.0, 3)

Count.APE.weekend <- tmpAPE.1 - tmpAPE.0
round(cbind(tmpAPE.1, tmpAPE.0, Count.APE.weekend), 3)


# Prepare the final table 
Count.partial_eff <- data.frame(Count.APE)
names(Count.partial_eff) <- c("Partial effects")
Count.partial_eff_tmp <- data.frame(tmpAPE.1, tmpAPE.0, Count.APE.weekend)
Count.partial_eff_tmp <- as.data.frame(t(Count.partial_eff_tmp))
names(Count.partial_eff_tmp) <- c("Partial effects")
Count.partial_eff <- rbind(Count.partial_eff, Count.partial_eff_tmp)

rownames(Count.partial_eff)[11] <- c("When weekend")
rownames(Count.partial_eff)[12] <- c("When not weekend")
Count.partial_eff[8, 1] <- NA
Count.partial_eff <- na.omit(Count.partial_eff)
rownames(Count.partial_eff)[12] <- c("is_weekend")

Count.OLS_eff <- data.frame(rsltCountOLS$coefficients)
Count.OLS_eff <- rbind(Count.OLS_eff,
                       "When weekend" = NA,
                       "When not weekend" = NA)

Count.OLS_eff <-
  Count.OLS_eff %>%
  slice(c(1:7, 9:12, 8))

Count.OLS.partial_eff <- cbind(Count.OLS_eff, Count.partial_eff)
colnames(Count.OLS.partial_eff) <- c("OLS coefficients", "Partial effects negbin")

Count.OLS.partial_eff <- as.data.frame(Count.OLS.partial_eff)
stargazer(Count.OLS.partial_eff, summary = FALSE, align=TRUE, no.space = TRUE, flip = FALSE)
kable(Count.OLS.partial_eff, format = "latex")


#################################################################
##        Ordinal logistic data modeling: the Yelp case        ##
#################################################################


# -----------------------------
# Read and prepare the data
# -----------------------------

dfYelpAll <- read.csv2(file = paste0(dirData, "online_ratings_travel.csv"), header = TRUE)

# Construct additional dependent variable for highly skewed ratings of 5 stars
dfYelpAll$dFiveStars <- ifelse(dfYelpAll$review_stars == 5, 1, 0)

# Construct a variable for how long someone has been on Yelp (in months)
dfYelpAll$yelping_since <- as.POSIXct(dfYelpAll$yelping_since, format = "%d.%m.%Y")

today <- Sys.Date()
dfYelpAll$current_month_year <- format(today, format = "%d.%m.%Y")
dfYelpAll$current_month_year <- as.POSIXct(dfYelpAll$current_month_year, format = "%d.%m.%Y")

dfYelpAll$yelping_for_months <- round(as.numeric(difftime(dfYelpAll$current_month_year, dfYelpAll$yelping_since, units ="days"))/(365.25/12), 0)

# Missing values? Notice which variables are missing in terms of variables for the model later on
missingValuesYelp <- as.data.frame(colSums(is.na(dfYelpAll)))


# -----------------------------
# Summary statistics
# -----------------------------

dfYelpAll.sub <- 
  dfYelpAll %>%
  dplyr::select(review_stars, dFiveStars, reviews_in_city_so_far, numb_friends, travel, length, yelping_for_months)


data.frame(colnames(dfYelpAll.sub))
order1 = c(1, 2, 5, 3, 4, 6, 7)

stargazer(dfYelpAll.sub, 
          summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"), 
          digits = 2,
          order = order1,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE
          #, type = 'text'
)


# -----------------------------
# Formal specifications
# -----------------------------

# review_stars=\ \beta_0\ +\ \beta_1travel+\ \beta_2fans\ +\ (\beta_{31}+\beta_{32}travel)length+\beta_4years_elite\ +\beta_5yelping_for_months\ +\ \varepsilon,\ \ \varepsilon\ ~\ n(0,\ \sigma)

# dFiveStars=\ \beta_0\ +\ \beta_1travel\ +\ \beta_2fans\ +\ (\beta_{31}+\beta_{32}travel)length+\beta_4years_elite\ +\beta_5yelping_for_months+\ \ \varepsilon,\ \ \varepsilon\ ~\ n(0,\ \sigma)



# -----------------------------------------------------------------
# Estimating binary-choice and ordered-response regression models 
# -----------------------------------------------------------------

# 1. Binary-choice model:

# Define the model
mdlBi <- dFiveStars ~ travel + numb_friends + length + yelping_for_months + reviews_in_city_so_far + reviews_in_city_so_far*travel

# Estimate the model
rsltBiOLS    <-  lm(mdlBi, data = dfYelpAll.sub)
rsltBiLogit  <- glm(mdlBi, data = dfYelpAll.sub, 
                    family=binomial(link = "logit"))
rsltBiProbit <- glm(mdlBi, data = dfYelpAll.sub, 
                    family=binomial(link = "probit"))

# Summarize the results
summary(rsltBiOLS)
summary(rsltBiLogit)
summary(rsltBiProbit)

# Make table of the results
stargazer(rsltBiOLS, rsltBiLogit, rsltBiProbit,
          align = TRUE, no.space = TRUE, 
          intercept.bottom = FALSE
          , type="text"
          )

# Odds ratio as a way to interpret coefficients for nonlinear models
stargazer(rsltBiOLS$coefficients, exp(rsltBiLogit$coefficients), exp(rsltBiProbit$coefficients)
          , type="text"
          )


#' However, those results/effects/coeffs of probit and logit models (nonlinear) 
#' can be better understood by computing partial effects instead of odds ratios

#------------------
# PEA: partial effect at the average
#------------------

# Collect estimated coefficients, and make subsets of the
# data frame (for convenience)
betaBiLogit  <- coefficients(rsltBiLogit)
betaBiProbit <- coefficients(rsltBiLogit)

myVar       <- all.vars(mdlBi) # picking up all variables
dfYelpAll.avg <- dfYelpAll.sub[myVar[-1]] # Exclude explained variables as we do not its average
dfYelpAll.avg <- as.data.frame(t(colMeans(dfYelpAll.avg))) # for partial effect at average we need averages of explanatories


# Determine partial effects at the average for all
# explanatory variables
# For non-dummy variables --- Predict (type="link") gives avg-x'beta, function dlogis/dnorm
# calculates the density f(avg-x'beta)
PEAlogit.1 <- round(dlogis(predict(rsltBiLogit, newdata=dfYelpAll.avg,
                             type="link"))*betaBiLogit, 6)
PEAprobit.1<- round(dnorm(predict(rsltBiProbit, newdata=dfYelpAll.avg,
                            type="link"))*betaBiProbit, 6)

# The partial effect for dummy variables is a simple difference
# between success probabilities if the dummy event does occur
# and does not occur;
tmp <- dfYelpAll.avg

# Predict (type="response") gives P(Y=1|avg-x'beta)
tmp$travel <- 0

tmpPEAlogit.0   <- 
  predict(rsltBiLogit, newdata=tmp, type="response")
tmpPEAprobit.0  <- 
  predict(rsltBiProbit, newdata=tmp,type="response")

tmp$travel <- 1

tmpPEAlogit.1   <- 
  predict(rsltBiLogit, newdata=tmp, type="response")
tmpPEAprobit.1  <- 
  predict(rsltBiProbit, newdata=tmp,type="response")

# Difference between success of response variable when dummy is 1 and success of Y when dummy is 0
PEAlogit.2 <- PEAlogit.1
PEAlogit.2["travel"] <- tmpPEAlogit.1 - tmpPEAlogit.0

PEAprobit.2 <- PEAprobit.1
PEAprobit.2["travel"] <- tmpPEAprobit.1 - tmpPEAprobit.0

df.PEAlogit <- as.data.frame(PEAlogit.2)
df.PEAprobit <- as.data.frame(PEAprobit.2)

df.PEA_Bi <- cbind(df.PEAlogit, df.PEAprobit)
colnames(df.PEA_Bi) <- c("PEA Logit", "PEA Probit")

df.PEA_Bi <- round(df.PEA_Bi, 6)


#------------------
# APE: average partial effects
#------------------

# Predict (type="link) gives avg-x'beta, function dlogis/dnorm
# calculates the density f(avg-x'beta);
APElogit.1   <- mean(dlogis(predict(rsltBiLogit, type="link")))*betaBiLogit
APEprobit.1  <- mean(dnorm(predict(rsltBiProbit, type="link")))*betaBiProbit

# The average partial effect for dummy variables is the average
# of the differences between success probabilities if the dummy 
# event does occur and does not occur for each observations;
tmp <- dfYelpAll.avg

# Predict (type="response") gives P(Y=1|avg-x'beta)
tmp$travel  <- 0

tmpAPElogit.0 <- 
  mean(predict(rsltBiLogit, newdata=tmp, type="response"))
tmpAPEprobit.0<- 
  mean(predict(rsltBiProbit,newdata=tmp, type="response"))

tmp$travel  <- 1

tmpAPElogit.1 <- 
  mean(predict(rsltBiLogit, newdata=tmp, type="response"))
tmpAPEprobit.1<- 
  mean(predict(rsltBiProbit,newdata=tmp, type="response"))

APElogit.2 <- APElogit.1
APElogit.2["travel"] <- tmpAPElogit.1 - tmpAPElogit.0

APEprobit.2 <- APEprobit.1
APEprobit.2["travel"]<- tmpAPEprobit.1 -tmpAPEprobit.0

df.APElogit <- as.data.frame(APElogit.2)
df.APEprobit <- as.data.frame(APEprobit.2)

df.APE_Bi <- cbind(df.APElogit, df.APEprobit)
colnames(df.APE_Bi) <- c("APE Logit", "APE Probit")

df.APE_Bi <- round(df.APE_Bi, 6)

# Present the results
df.APE.PEA.Bi <- cbind(df.PEA_Bi, df.APE_Bi)

stargazer(df.APE.PEA.Bi, 
          summary = FALSE, flip = FALSE, 
          no.space = TRUE, align = TRUE
          , type="text"
          )

#------------------
# R2 for binary logit
#------------------

lnL.fitted <- -0.5*rsltBiLogit$deviance
lnL.null   <- -0.5*rsltBiLogit$null.deviance

# Degrees of freedom of both models
df.fitted  <- rsltBiLogit$df.residual
df.null    <- rsltBiLogit$df.null

# Number of predictors and sample size
K <- df.null - df.fitted
N <- df.null + 1

# Predicted and observed value of the target
probLogit.Bi <- predict.glm(rsltBiLogit, type = "response")
yvalue.Bi    <- dfYelpAll.sub$dFiveStars


#------------------
# Define pseudo R2 measures
#------------------
McFadden.R2.Bi    <- 1 - (lnL.fitted/lnL.null)
McFadden.R2adj.Bi <- 1 - ((lnL.fitted - K)/lnL.null)

# 2. Cox & Snell
CoxSnell.R2.Bi <- 1 - (exp(lnL.null)/exp(lnL.fitted))^(2/N)

# 3. Nagelkerke
Nagelkerke.R2.Bi <- CoxSnell.R2.Bi/(1 - exp(lnL.null)^(2/N))

# 4. Efron
Efron.R2.Bi <- 1 - 
  sum((yvalue.Bi-probLogit.Bi)^2)/sum((yvalue.Bi-mean(yvalue.Bi))^2)

# 5. Count R2 and adjusted count R2
Count.R2.Bi <- sum(yvalue.Bi == (probLogit.Bi>0.5))/length(yvalue.Bi)

maxFreq.Bi     <- max(table(yvalue.Bi))
Count.R2adj.Bi <- (sum(yvalue.Bi == (probLogit.Bi>0.5)) - maxFreq.Bi)/
  (length(yvalue.Bi) - maxFreq.Bi)

df.R2.Bi <- t(round(
        cbind(McFadden.R2.Bi, McFadden.R2adj.Bi, CoxSnell.R2.Bi, 
        Nagelkerke.R2.Bi, Efron.R2.Bi, 
        Count.R2.Bi, Count.R2adj.Bi), 3))

colnames(df.R2.Bi) <- c("pseudo R-squared")

df.R2.Bi[is.nan(df.R2.Bi)] <- 0.000

stargazer(df.R2.Bi,
          summary = FALSE, flip = FALSE, 
          no.space = TRUE, align = TRUE
          , type="text"
          )

# --------------------------
# 2. Ordinal-choice model:
# --------------------------

# Dependent as factor
dfYelpAll.sub$review_stars <- factor(dfYelpAll.sub$review_stars, levels = c(1, 2, 3, 4, 5))

# Specify the model
mdlOrd <- review_stars ~ travel + numb_friends + length + yelping_for_months + reviews_in_city_so_far + reviews_in_city_so_far*travel

# Estimate the model
rsltOrd.Logit  <- polr(mdlOrd, data = dfYelpAll.sub, method = "logistic")
rsltOrd.Probit <- polr(mdlOrd, data = dfYelpAll.sub, method = "probit")

stargazer(rsltOrd.Logit, rsltOrd.Probit, type="text")

# Stargazer does not extract log likelihood and AIC 
# values from the polr objects, which are therefore
# added with an extra step. The add.lnL and add.Aic
# vectors contain the infomration that will be 
# added with stargazer's add.lines parameter
add.lnL <- c("lnL", round(logLik(rsltOrd.Logit),3), 
             round(logLik(rsltOrd.Probit),3))
add.Aic <- c("AIC", round(AIC(rsltOrd.Logit),3), 
             round(AIC(rsltOrd.Probit),3))

# Marginal effects are again needed

est.Logit  <- summary(rsltOrd.Logit)$coefficients
est.Probit <- summary(rsltOrd.Probit)$coefficients

prb.Logit  <- as.data.frame(predict(rsltOrd.Logit,  type="probs"))
prb.Probit <- as.data.frame(predict(rsltOrd.Probit, type="probs"))


# Calculate the cumulative probabilities (as an intermediate
# step, for code transparency)
cdf.Logit.1 <- prb.Logit[, 1]
cdf.Logit.2 <- prb.Logit[, 1] + prb.Logit[, 2]
cdf.Logit.3 <- prb.Logit[, 1] + prb.Logit[, 2] + prb.Logit[, 3]
cdf.Logit.4 <- prb.Logit[, 1] + prb.Logit[, 2] + prb.Logit[, 3] + prb.Logit[, 4]
cdf.Logit.5 <- prb.Logit[, 1] + prb.Logit[, 2] + prb.Logit[, 3] + prb.Logit[, 4] + prb.Logit[, 5]

cdf.Probit.1 <- prb.Probit[, 1]
cdf.Probit.2 <- prb.Probit[, 1] + prb.Probit[, 2]
cdf.Probit.3 <- prb.Probit[, 1] + prb.Probit[, 2] + prb.Probit[, 3]
cdf.Probit.4 <- prb.Probit[, 1] + prb.Probit[, 2] + prb.Probit[, 3] + prb.Probit[, 4]
cdf.Probit.5 <- prb.Probit[, 1] + prb.Probit[, 2] + prb.Probit[, 3] + prb.Probit[, 4] + prb.Probit[, 5]
# ... cdf.Logit.5 and cdf.Probit.5 should be equal to 1

# Calculate density parts of the effects
prb.Logit$pdf.1 <- 
  -dlogis(qlogis(cdf.Logit.1))
prb.Logit$pdf.2 <- 
  dlogis(qlogis(cdf.Logit.1)) - dlogis(qlogis(cdf.Logit.2))
prb.Logit$pdf.3 <- 
  dlogis(qlogis(cdf.Logit.2)) - dlogis(qlogis(cdf.Logit.3))
prb.Logit$pdf.4 <- 
  dlogis(qlogis(cdf.Logit.3)) - dlogis(qlogis(cdf.Logit.4))
prb.Logit$pdf.5 <- 
  dlogis(qlogis(cdf.Logit.4))

prb.Probit$pdf.1 <- 
  -dnorm(qnorm(cdf.Probit.1))
prb.Probit$pdf.2 <- 
  dnorm(qnorm(cdf.Probit.1)) - dnorm(qnorm(cdf.Probit.2))
prb.Probit$pdf.3 <- 
  dnorm(qnorm(cdf.Probit.2)) - dnorm(qnorm(cdf.Probit.3))
prb.Probit$pdf.4 <- 
  dnorm(qnorm(cdf.Probit.3)) - dnorm(qnorm(cdf.Probit.4))
prb.Probit$pdf.5 <- 
  dnorm(qnorm(cdf.Probit.4))

# Determine the average effects (apart from the estimated 
# effects)
avgAPE.Logit  <- colMeans(prb.Logit[c("pdf.1", "pdf.2", "pdf.3", "pdf.4", "pdf.5")])
avgAPE.Probit <- colMeans(prb.Probit[c("pdf.1", "pdf.2", "pdf.3", "pdf.4", "pdf.5")])

# Extract the estimated effects from the logit
# and probit objects
est.Logit  <- coef(rsltOrd.Logit)
est.Probit <- coef(rsltOrd.Probit)

# Determine the APE
dfAPE.Logit  <- as.data.frame(round(avgAPE.Logit %*% t(est.Logit), 7))
dfAPE.Probit <- as.data.frame(round(avgAPE.Probit %*% t(est.Probit), 7))

rownames(dfAPE.Logit) <- rownames(dfAPE.Probit) <-
  c("P(y=1)", "P(y=2)", "P(y=3)", "P(y=4)", "P(y=5)")

df.APE.Logit.Probit <- rbind(dfAPE.Logit, dfAPE.Probit)
df.APE.Logit.Probit <- cbind(
  "Type of analysis" = c("Logit", "Logit", "Logit", "Logit", "Logit", "Probit", "Probit", "Probit", "Probit", "Probit"),
  df.APE.Logit.Probit
)

df.APE.Logit.Probit <- tibble::rownames_to_column(df.APE.Logit.Probit, "PDF")
rownames(df.APE.Logit.Probit) <- df.APE.Logit.Probit[,2]
df.APE.Logit.Probit[,2] <- NULL

# Make the table
stargazer(df.APE.Logit.Probit, summary = FALSE,
          align = TRUE, no.space = TRUE
          , type="text"
          )
stargazer(dfAPE.Probit, summary = FALSE,
          align = TRUE, no.space = TRUE, type="text")
