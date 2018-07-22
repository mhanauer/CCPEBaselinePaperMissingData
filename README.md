---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load all releveant packages

```{r}
library(effects)
require(sandwich)
require(foreign)
library(plyr)
library(Amelia)
library(MASS)
library(psych)
library(ggplot2)
library(dplyr)      # for data manipulation
library(tidyr)      # for reshaping data
library(descr)
library(pscl)

```
Just loading the data and getting rid of missing values.  Getting fid of missing data here and calculating the percentage of missing data.
```{r}

setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/CCPEPaperData")
CCPEBaseline = read.csv("CCPEBaselineFull.csv", header = TRUE)
dim(CCPEBaseline)
#

CCPEBaseline = CCPEBaseline[c("RSKCIG","CIG30D","MJ30D","RSKMJ", "BINGE530D",	"RSKALC",	"R_WHITE_N",	"REL_IMP", "HINCOMEO_N","SEX_PR", "GENDER",	"YOB")]

dim(CCPEBaseline)

CCPEBaseline = CCPEBaseline[1:744,]
write.csv(CCPEBaseline, "CCPEBaseline.csv", row.names = FALSE)
CCPEBaseline = read.csv("CCPEBaseline.csv", header = TRUE, na.strings = c(NA, 98, 99, 77, 97, " "))


dim(CCPEBaseline)
#CCPEBaseline = data.frame(na.omit(CCPEBaseline))
#dim(CCPEBaseline)

592/745
```
Drop anyone who is not male or female.  So subset the data where gender equals 1 or 2
Lose 3 total people.  1 equals male and 2 equals female.  Need to read and write the dataset to get the variables to be factors.  Also, changing gender to be 1 for male and 0 for female.

So I need to change the values greater than 2 to -999 so I can subset those values and figure out which rows I need to delete.  Remember to find the deleted rows, I need to create a new data set and subset which values are -999 so I can find the rows and delete them below
```{r}
CCPEBaseline$GENDER = ifelse(CCPEBaseline$GENDER > 2, -999, CCPEBaseline$GENDER)

#CCPEBaseline = subset(CCPEBaseline, GENDER  == -999)
CCPEBaseline

CCPEBaseline = CCPEBaseline[-c(287,472,590,648),]
```
Now change AGE to AGE by subtracting 2018 from YOB.  
```{r}
CCPEBaseline$AGE = 2018-CCPEBaseline$YOB
```
Change home income to split on something 30,000 or lower is low income.  We choose $30,000 because it was in the middle of the scale for the GPRA.  Ok so 1 and 2 are 30,000 and below so they are 0 and everything else is 1, because options 3,4,5 and higher than 30,000.

Change sex orientation to straight or non-straight where is straight and zero is non-straight
```{r}
CCPEBaseline$INCOME = ifelse(CCPEBaseline$HINCOMEO_N == 1, 0, ifelse(CCPEBaseline$HINCOMEO_N == 2, 0, 1))
CCPEBaseline$SEX_PR = ifelse(CCPEBaseline$SEX_PR ==1, 1, 0)
```

Get descriptives.  Break them down by continous and non-continous.  Continous just get the mean and sd, but for ordinal get the count and percentage.
```{r}
CCPEBaselineCount = data.frame(CIG30D = CCPEBaseline$CIG30D, MJ30D=CCPEBaseline$MJ30D, BINGE530D=CCPEBaseline$BINGE530D, CCPEBaseline$RSKCIG, CCPEBaseline$RSKMJ, CCPEBaseline$RSKALC)
round(apply(CCPEBaselineCount, 2, mean),2)
round(apply(CCPEBaselineCount, 2, sd),2)

## Now create for binary and ordinal
library(prettyR)
describeCounts = data.frame(R_WHITE_N = CCPEBaseline$R_WHITE_N, REL_IMP = CCPEBaseline$REL_IMP, INCOME=CCPEBaseline$INCOME,SEX_PR= CCPEBaseline$SEX_PR,GENDER= CCPEBaseline$GENDER)
describeCounts = apply(describeCounts, 2, function(x){describe.factor(x)})
describeCounts

round(mean(CCPEBaseline$AGE),2)
round(sd(CCPEBaseline$AGE),2)

```

If we get rid of sexual orientation, then we have no missing data patterns. 
```{r}
CCPEBaseline = CCPEBaseline[c("RSKCIG", "CIG30D", "MJ30D", "RSKMJ", "BINGE530D", "RSKALC", "R_WHITE_N", "REL_IMP", "SEX_PR", "GENDER", "AGE", "INCOME")]

library(MissMech)
```

Test of MCAR is statistically significant at .05, (.03), so we will impute the missing values.

Because it is predicting negative missing values for the count variables with both logs and linear, we need to say anything lower than zero is zero.  Nothing in this data set should be lower than zero, so we can be lazy and do an lapply across everything that says this. 

Also, we need to round everything to 0 decimals, which should be fine nothing in the data set needs decimal points.

So sqrts works for count variables; however, we still have decimal points.
```{r}
head(CCPEBaseline)

m = 20
bds = matrix(c(2,0,30, 3, 0, 30, 5, 0 , 30), byrow =TRUE, nrow = 3, ncol = 3); bds

CCPEBaseline.out = amelia(m = m,CCPEBaseline, noms = c("R_WHITE_N", "SEX_PR", "GENDER", "INCOME"), ords = c("RSKCIG", "RSKMJ", "RSKALC", "REL_IMP"), sqrts = c( "CIG30D", "MJ30D", "BINGE530D"))
summary(CCPEBaseline.out)

compare.density(CCPEBaseline.out, var = "CIG30D")
compare.density(CCPEBaseline.out, var = "MJ30D")
compare.density(CCPEBaseline.out, var = "BINGE530D")


CCPEBaselineLin.out = amelia(m = m,CCPEBaseline, noms = c("R_WHITE_N", "SEX_PR", "GENDER", "INCOME"), ords = c("RSKCIG", "RSKMJ", "RSKALC", "REL_IMP"))
compare.density(CCPEBaselineLin.out, var = "CIG30D")
compare.density(CCPEBaselineLin.out, var = "MJ30D")
compare.density(CCPEBaselineLin.out, var = "BINGE530D")

```
Ok supply your own missing data here
```{r}
CCPEBaselineMissing = lapply(1:m, function(x){CCPEBaseline.out$imputations[[x]]})
CCPEBaselineMissing = na.omit(CCPEBaselineMissing)
CCPEBaselineMissing = lapply(1:m, function(x){round(CCPEBaselineMissing[[x]], 0)})
head(CCPEBaselineMissing[[1]])
```



Now we need to mean center all ordinal and continuous variables, so use the scale function, with scale equals false, because that creates z-scores by dividing by the standard deviation.  Creating a new name for the new variable data set. And adding all of the centered variables to the original data set.

Renaming the variables, because they are now centered so I don't want to confuse them with other variables that are not centered.

Creating interaction variables, because they are easier to include in the code.  See cigarette model below the interaction terms that I created here produce the same results as including the actual interaction term in the model.

RSKCIG CIG30D MJ30D RSKMJ BINGE530D RSKALC  R_WHITE_N REL_IMP HINCOMEO_N SEX_PR GENDER YOB AGE INCOME

Ok need to grab all five data sets and put them into one.
```{r}
CCPEBaselineMissingMean = lapply(1:m, function(x){scale(CCPEBaselineMissing[[x]], scale = FALSE)})

CCPEBaselineMissing = lapply(1:m, function(x){(data.frame(CCPEBaselineMissing[[x]], CCPEBaselineMissingMean[[x]]))})
CCPEBaselineMissing[[1]]
head(CCPEBaselineMissing)


```
Now run the analysis
```{r}
cigMissing = lapply(1:m, function(x){hurdle(CIG30D ~   RSKCIG.1*R_WHITE_N + RSKCIG.1*AGE.1 + RSKCIG.1*REL_IMP.1 + RSKCIG.1*INCOME + RSKCIG.1*SEX_PR+ RSKCIG.1*GENDER , data = CCPEBaselineMissing[[x]], dist = "poisson", zero.dist = "binomial")})

marMissing = lapply(1:m, function(x){hurdle(MJ30D ~   RSKMJ.1*R_WHITE_N + RSKMJ.1*AGE.1 + RSKMJ.1*REL_IMP.1 + RSKMJ.1*INCOME + RSKMJ.1*SEX_PR+ RSKMJ.1*GENDER , data = CCPEBaselineMissing[[x]], dist = "poisson", zero.dist = "binomial")})


output = NULL
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = hurdle(CIG30D ~   RSKCIG.1*R_WHITE_N + RSKCIG.1*AGE.1 + RSKCIG.1*REL_IMP.1 + RSKCIG.1*INCOME + RSKCIG.1*SEX_PR+ RSKCIG.1*GENDER , data = CCPEBaselineMissing[[i]], dist = "poisson", zero.dist = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients$count[[1]]
  se_output[[i]] = output[[i]]$coefficients$count[[2]]
  }

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = t(x)
}

coef_output = quickTrans(coef_output)
coef_output
  
```
Now run it through this function
```{r}
meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 111)),3)
  return(p)
}


```




