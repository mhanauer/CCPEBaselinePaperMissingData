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
CCPEBaseline$GENDER
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

Instead try a cross tab of gender and substance misuse
I want the average number of use for each of the categories.
Use the compmeans function to the means and numbers for each category
Do this for every categorical variable and then split age on the mean and do it for that as well.

Need to repeat this process for each substance.

Then figure out how to create a table from this.
```{r}
attach(CCPEBaseline)
R_WHITE_Marj = round(compmeans(MJ30D, R_WHITE_N), 2)
REL_IMPMarji = round(compmeans(MJ30D, REL_IMP),2)
INCOMEMarj = round(compmeans(MJ30D, INCOME),2)
SEX_PRMarj = round(compmeans(MJ30D, SEX_PR),2)
genderMarj = round(compmeans(MJ30D, GENDER),2)

## Change age to split in the mean
ageMean = mean(AGE)
ageMean
AGECross = ifelse(AGE > ageMean, 1, 0)
ageMarj = round(compmeans(MJ30D, AGECross),2)

#### Cig  #### #### #### #### #### #### #### 

R_WHITE_CIG = round(compmeans(CIG30D, R_WHITE_N),2)
REL_IMPCIG = round(compmeans(CIG30D, REL_IMP),2)
INCOMECIG = round(compmeans(CIG30D, INCOME),2)
SEX_PRCIG = round(compmeans(CIG30D, SEX_PR),2)
genderCIG = round(compmeans(CIG30D, GENDER),2)

ageMean = mean(AGE)
ageMean
AGECross = ifelse(AGE > ageMean, 1, 0)
ageCIG = round(compmeans(CIG30D, AGECross),2)




#### Binge #### #### #### #### #### #### #### 
R_WHITE_BINGE = round(compmeans(BINGE530D, R_WHITE_N),2)
REL_IMPBINGE = round(compmeans(BINGE530D, REL_IMP),2)
INCOMEBINGE = round(compmeans(BINGE530D, INCOME),2)
SEX_PRBINGE = round(compmeans(BINGE530D, SEX_PR),2)
genderBINGE = round(compmeans(BINGE530D, GENDER),2)

## Change age to split in the mean
ageMean = mean(AGE)
ageMean
AGECross = ifelse(AGE > ageMean, 1, 0)
ageBINGE = round(compmeans(BINGE530D, AGECross),2)



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


Now we need to mean center all ordinal and continuous variables, so use the scale function, with scale equals false, because that creates z-scores by dividing by the standard deviation.  Creating a new name for the new variable data set. And adding all of the centered variables to the original data set.

Renaming the variables, because they are now centered so I don't want to confuse them with other variables that are not centered.

Creating interaction variables, because they are easier to include in the code.  See cigarette model below the interaction terms that I created here produce the same results as including the actual interaction term in the model.

RSKCIG CIG30D MJ30D RSKMJ BINGE530D RSKALC  R_WHITE_N REL_IMP HINCOMEO_N SEX_PR GENDER YOB AGE INCOME
```{r}
CCPEBaselineMeanCenter = CCPEBaseline
head(CCPEBaselineMeanCenter)


CCPEBaselineMeanCenter = scale(CCPEBaselineMeanCenter, scale = FALSE)
head(CCPEBaselineMeanCenter)
colnames(CCPEBaselineMeanCenter) = c("CenterRSKCIG", "CenterCIG30D", "CenterMJ30D", "CenterRSKMJ", "CenterBINGE530D", "CenterRSKALC", "CenterR_WHITE_N", "CenterREL_IMP", "CenterHINCOMEO_N",  "CenterSEX_PR", "CenterGENDER", "CenterYOB", "CenterAGE", "CenterINCOME")

# Maybe I don't need this. I think I can just add in the previous data frame
CCPEBaseline = data.frame(CCPEBaselineMeanCenter, CCPEBaseline)
head(CCPEBaseline)

# Now create interaction terms
# CenterRSKCIG
CenterRSKCIG_CenterR_WHITE_N = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterR_WHITE_N
CenterRSKCIG_CenterREL_IMP = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterREL_IMP
CenterRSKCIG_CenterINCOME = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterINCOME
CenterRSKCIG_CenterSEX_PR = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterSEX_PR
CenterRSKCIG_CenterGENDER = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterGENDER
CenterRSKCIG_CenterAGE = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterAGE

# Now mar interaction 
CenterRSKMJ_CenterR_WHITE_N = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterR_WHITE_N
CenterRSKMJ_CenterREL_IMP = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterREL_IMP
CenterRSKMJ_CenterINCOME = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterINCOME
CenterRSKMJ_CenterSEX_PR = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterSEX_PR
CenterRSKMJ_CenterGENDER = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterGENDER
CenterRSKMJ_CenterAGE = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterAGE

# Now alcohol interaction
CenterRSKALC_CenterR_WHITE_N = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterR_WHITE_N
CenterRSKALC_CenterREL_IMP = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterREL_IMP
CenterRSKALC_CenterINCOME = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterINCOME
CenterRSKALC_CenterSEX_PR = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterSEX_PR
CenterRSKALC_CenterGENDER = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterGENDER
CenterRSKALC_CenterAGE = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterAGE



CCPEBaseline = cbind(CCPEBaseline, CenterRSKCIG_CenterR_WHITE_N, CenterRSKCIG_CenterREL_IMP, CenterRSKCIG_CenterINCOME, CenterRSKCIG_CenterSEX_PR, CenterRSKCIG_CenterGENDER, CenterRSKCIG_CenterAGE, CenterRSKMJ_CenterR_WHITE_N, CenterRSKMJ_CenterREL_IMP, CenterRSKMJ_CenterINCOME, CenterRSKMJ_CenterSEX_PR, CenterRSKMJ_CenterGENDER, CenterRSKMJ_CenterAGE, CenterRSKALC_CenterR_WHITE_N, CenterRSKALC_CenterREL_IMP, CenterRSKALC_CenterINCOME, CenterRSKALC_CenterSEX_PR, CenterRSKALC_CenterGENDER, CenterRSKALC_CenterAGE)

summary(CCPEBaseline)
dim(CCPEBaseline)[1]


```
Now we are imputing the data.  So get rid of HOMEincome and YOB befor you impute.  Only using the variables that were statistically significant in any of the models.  Can drop those variables from the models if you want later.
```{r}
CCPEBaseline$CenterHINCOMEO_N = NULL
CCPEBaseline$CenterYOB = NULL
library(BaylorEdPsych)
CCPEBaselineMissing = CCPEBaseline[,1:12]

CCPEBaselineMissing2 = CCPEBaseline[c("RSKCIG", "CIG30D", "MJ30D", "RSKMJ", "BINGE530D", "RSKALC", "R_WHITE_N", "REL_IMP", "SEX_PR", "GENDER", "AGE", "INCOME")]

CCPEBaselineMissing3 = CCPEBaseline[c("RSKCIG", "CIG30D", "MJ30D", "RSKMJ", "BINGE530D", "RSKALC", "REL_IMP", "GENDER", "AGE", "INCOME")]


library(MissMech)

TestMCARNormality(CCPEBaselineMissing3)
```
