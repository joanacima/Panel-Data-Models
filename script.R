# 1. Data Preparation and Import

  # 1.1 Clean the work environment
rm(list = ls())

  # 1.2 Set the working directory
setwd("C:\\Users\\Joana Cima\\Documents\\GitHub\\Panel_models_R")

  # 1.3 Import the data
# install.packages("haven")
library(haven)
data <- read_dta("mus08psidextract.dta")

# 2. Data Description:

  # 2.1 View tha variables
names(data)

  # 2.2 Descriptive statistics
summary(data)

# install.packages("plm")
library(plm)

# 3. Panel data setup

  # 3.1 Define data as panel data
pdata <- pdata.frame(data, index = c("id", "t"))


# 4. Econometric analysis

  # 4.1 POLS with Robust Standard Errors:
#install.packages("lmtest")
#install.packages("sandwich")
library(lmtest)
library(sandwich)

pols1 <- plm(lwage ~ fem + blk + ed + ms + exp + exp2 + occ + ind + south + smsa + union, data = pdata, model = "pooling")

summary(pols1)

  # 4.2 Random effects

re1 <- plm(lwage ~ fem + blk + ed + ms + exp + exp2 + occ + ind + south + smsa + union, data = pdata, model = "random")

summary(re1)

re1 <- plm(lwage ~ ms + exp + exp2 + occ + ind + south + smsa + union, data = pdata, model = "random")

  # 4.3 Fixed effects

fe1 <- plm(lwage ~ fem + blk + ed + ms + exp + exp2 + occ + ind + south + smsa + union, data = pdata, model = "within")

summary(fe1)

fe1 <- plm(lwage ~ ms + exp + exp2 + occ + ind + south + smsa + union, data = pdata, model = "within")

summary(fe1)

  # 4.4 LSDV - Least Squares Dummy Variables

LSDV <- lm(lwage ~ ms + exp + exp2 + occ + ind + south + smsa + union + factor(id), data = pdata)
summary(LSDV)


# 5. Specification Tests for Panel Data

  # 5.1 LM Test for Unobserved Effects (OLS vs random effects)

plmtest(pols1, type = "bp")

  # 5.2 Test the significance of the fixed effects for 'id' (OLS vs. LSDV(FE))

anova(LSDV, pols1)

  # 5.3 Hausman test (RE vs FE)

phtest(fe1, re1)

  # 5.4 Test for Heteroscedasticity:

library(lmtest)
bptest(fe1)

  # 5.5 Serial Correlation Test:

pbgtest(fe1)

# 6. Final specification
fe_robust <- coeftest(fe1, vcovHC(fe1, method = "arellano"))


# 7. Export

library(stargazer)

stargazer(fe_robust, type = "text", column.labels = c("FE Robust"))


