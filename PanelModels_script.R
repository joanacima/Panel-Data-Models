# Panel Data Econometrics using R
# EEG Data Pro
# 1. Data Preparation and Import

  # 1.1 Clean the work environment
rm(list = ls())

  # 1.2 Set the working directory
#setwd("C:\\Users\\Joana Cima\\Documents\\GitHub\\Panel-Data-Models")

  # 1.3 Import the data
#install.packages("haven")
library(haven)
data <- as.data.frame(read_dta("mus08psidextract.dta"))

# 2. Data Description:

  # 2.1 View the variables
names(data)

  # 2.2 Analyse the missing values

library(naniar)
vis_miss(data)

# 3. Panel data setup

# install.packages("plm")
library(plm)

  # 3.1 Define data as panel data
pdata <- pdata.frame(data, index = c("id", "t"))

  # 3.2 Exploring panel data

library(gplots)

plotmeans(lwage ~ id, main="Heterogeineity across individuals", data=pdata)
  # 3.2 Descriptive statistics
summary(data)
library(stargazer)
library(dplyr)

data %>%
  dplyr::select(exp, ed, lwage, occ, south, smsa, ms, fem, union, blk) %>% 
  stargazer(title="",
            type= "text", out = "Descriptive_Statistics.html",
            digits = 2)

# 3.2 Descriptive statistics - change columns/rows
data %>%
  dplyr::select(exp, ed, lwage, occ, south, smsa, ms, fem, union, blk) %>% 
  stargazer(title="",
            type= "text", flip=TRUE, out = "Descriptive_Statistics2.html",
            digits = 2)


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

summary(LSDV)

stargazer(fe1, LSDV, 
          type = "html", 
          out = "FE.html", 
          column.labels = c("FE", "LSDV"), 
          title = "Model estimation - Fixed effects")

# 5. Specification Tests for Panel Data

  # 5.1 LM Test for Unobserved Effects (OLS vs random effects)

plmtest(pols1, type = "bp")

  # 5.2 Test the significance of the fixed effects for 'id' (OLS vs. LSDV(FE))

LSDV_reduced <- lm(lwage ~ ms + exp + exp2 + occ + ind + south + smsa + union, data = pdata)

anova(LSDV, LSDV_reduced)

  # 5.3 Hausman test (RE vs FE)

phtest(fe1, re1)

  # 5.4 Test for Heteroscedasticity:

library(lmtest)
bptest(fe1)

  # 5.5 Cross-sectional dependence test:

pcdtest(fe1, test = "lm")

  # 5.6 Serial Correlation Test:

pbgtest(fe1)

# 6. Final specification
fe_robust <- coeftest(fe1, vcovHC(fe1, method = "arellano"))

# 7. Export

library(stargazer)

stargazer(fe_robust, type = "text", column.labels = c("FE Robust"))


