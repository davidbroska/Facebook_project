# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
library(psych)
library("GPArotation")
library("sem")

library(tidyverse)
library(semPlot)
library(lavaan)
library(semPlot)
library(semptools)
library(haven)
library(labelled)
library(psych)
library(corrplot)

# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
library(kableExtra)
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels), 
         Toxicity=factor(Toxicity,levels=c("Not toxic","Maybe, not sure","Toxic","Very toxic")),
         Education = factor(Education, levels=c("High School or less","Some college","Postgraduate")))




cfa1 = '
  Politeness =~ BToxicNum01 + BRespectful +BSarcastic +BIntolerant +BHostile + BUseHateSpeechNum01
  Productive =~ BMakesEffortNum01 + BStaysOnTopicNum01 + BEmotional + ProductiveNum01 + BOpen + BObjective

'
prod = c("BToxicNum01","BRespectful","BSarcastic","BIntolerant","BHostile","BUseHateSpeechNum01") 
polite = c("BMakesEffortNum01","BStaysOnTopicNum01","BEmotional","ProductiveNum01","BOpen","BObjective")
fit1 <- dt %>% 
  select(all_of(c(prod,polite))) %>% 
  lavaan::cfa(cfa1, data = ., meanstructure = F)
standardizedsolution(fit1)
fitmeasures(fit1, fit.measures = c("chisq", "df", "pvalue", 
                                   "rmsea", "cfi", "srmr"))


  

  corrplot(lavResiduals(fit1)$cov, method = "color", tl.col = "black")
