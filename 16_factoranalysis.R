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
library(kableExtra)

# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels), 
         Toxicity=factor(Toxicity,levels=c("Not toxic","Maybe, not sure","Toxic","Very toxic")),
         Education = factor(Education, levels=c("High School or less","Some college","Postgraduate")))
set.seed(1)


# extract fit measures
get_measures = function(.fit, .name){
  measures = fitmeasures(.fit, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr")) 
  df = as_tibble(t(data.frame(measures)))
  colnames(df) = names(measures)
  df = bind_cols(tibble(Model = .name), df)
  return(df)
}

################################################################################
# Politeness 
################################################################################

# define syntax 
polite = c("BNotToxicNum","BNotHostile","BNoHateSpeechNum","BNotIntolerant")
cfa_polite_syn = paste0('Politeness =~',paste0(polite,collapse = "+"))

# fit politeness factor analysis
pol = dt %>%
  select(all_of(polite)) %>% 
  na.omit() %>% 
  lavaan::cfa(cfa_polite_syn, data = ., meanstructure = F, ordered=polite, effect.coding="loadings") 

# get fit measures
pol_measures = get_measures(pol, "Politeness")
pol_measures

  
################################################################################
# Constructive 
################################################################################
  
# define syntax 
constructive = c("BRespectful","BMakesEffortNum","BStaysOnTopicNum","BAgreement")
cfa_constructive_syn = paste0('Constructive =~',paste0(constructive,collapse = "+"))

# fit factor analysis
con = dt %>%
  select(all_of(constructive)) %>% 
  na.omit() %>% 
  lavaan::cfa(cfa_constructive_syn, data = ., meanstructure = F, ordered=constructive, effect.coding="loadings") 

# get fit measures
con_measures = get_measures(pol, "Constructive")
con_measures


################################################################################
# Politeness and Constructive 
################################################################################

# fit factor analysis
polcon_syn = paste0(
  cfa_polite_syn, "
  
  ",cfa_constructive_syn,"
  
  # Covariance
  Politeness ~~ Constructive
  BNotIntolerant ~~ BRespectful
  BNotHostile ~~ BRespectful
  BNotToxicNum ~~ BAgreement
  
")

# fit factor analysis
polcon <- dt %>%
  select(all_of(c(polite,constructive))) %>% 
  na.omit() %>% 
  lavaan::cfa(polcon_syn, data = ., meanstructure = F, ordered=c(polite,constructive), effect.coding="loadings") 

# get fit measures
polcon_measures = get_measures(polcon, "Constructive")
polcon_measures



################################################################################
# Politeness and Constructive 
################################################################################

full_syn = paste0(
  cfa_polite_syn, "
  
  ",cfa_constructive_syn,"
  
  # Covariance
  #Politeness ~~ Constructive
  BNotIntolerant ~~ BRespectful
  BNotHostile ~~ BRespectful
  BNotToxicNum ~~ BAgreement
  
  
  # direct effect
  ProductiveNum ~ c * Constructive + b * Politeness 
  
  # effect on mediator
  Politeness ~ a * Constructive 
  
  indirect := a * b
  direct := c
  total  := a + c*b
  pmediated := indirect/total
")

# fit full model
full <- dt %>%
  select(all_of(c(polite,constructive,"ProductiveNum"))) %>% 
  na.omit() %>% 
  lavaan::cfa(full_syn, data = ., meanstructure = F, ordered=c(polite,constructive,"ProductiveNum"), effect.coding="loadings") 

summary(full)

# get fit measures
full_measures = get_measures(full, "Mediation")
full_measures


################################################################################
# Data-driven ("exploratory") factor analysis

efa2 = dt %>% 
  select(all_of(c(polite,constructive))) %>% 
  na.omit() %>% 
  fa(.,
     nfactors = 2, 
     fm = "ml", 
     cor = "cor", 
     rotate = "oblimin", 
     n.iter = 50)
efa2



################################################################################
# Report results
################################################################################

measures = full_measures %>% 
  bind_rows(polcon_measures) %>% 
  bind_rows(pol_measures) %>% 
  bind_rows(con_measures)


# print table to latex
caption = "Fit measures for SEM models" %>% ifelse(all(measures$pvalue < 0.001), paste0(., ". All p-values for chi-squared test are smaller than 0.001"), .)
measures %>% 
  select(- pvalue) %>% 
  knitr::kable(format = "latex",digits = 2, label = "sem-fit-measures",booktabs=T,
               caption = caption,
               align=c("l",rep("c",ncol(.)-1))) %>% 
  kableExtra::kable_styling(latex_options = "HOLD_position") %>% 
  kableExtra::collapse_rows(columns = 1) %>% 
  writeLines("Tables/16_SemFitMeasures.tex")


# politeness
pdf("Figures/16_FA_Polite.pdf",width=6, height=4.5)
semPaths(pol, whatLabels = "std",nCharNodes=20,fade=F,curve=2,residuals=T,shapeInt="square",sizeInt=10e-200, 
         label.font=1, label.scale=T, node.width=1.5, thresholds=F,edge.label.cex = 1, what="std")
dev.off()

# constructive
pdf("Figures/16_FA_Constructive.pdf",width=6, height=4.5)
semPaths(con, whatLabels = "std",nCharNodes=20,fade=F,curve=2,residuals=T,shapeInt="square",sizeInt=10e-200, 
         label.font=1, label.scale=T, node.width=1.5, thresholds=F,edge.label.cex = 1, what="std")
dev.off()

# politeness and constructiveness
pdf("Figures/16_FA_PolitenessConstructive.pdf",width=6, height=4.5)
semPaths(polcon, whatLabels = "std",nCharNodes=20,fade=F,curve=2,residuals=T,shapeInt="square",sizeInt=10e-200, 
         label.font=1, label.scale=T, node.width=1.5, thresholds=F,edge.label.cex = 1, what="std")
dev.off()

# exploratory factor analysis
pdf("Figures/16_EFA2Factors.pdf")
fa.diagram(efa2,cut = 0.01,main = "Exploratory factor analysis")
dev.off()


# 











# SEM: Perceived constructiveness causes evaluators to see posts as Agreement, Staying on topic
# It's okay to omit covariance terms but mention them in appendix
# Add covariances to increase model fit

medpol_syn = paste0(
  cfa_polite_syn, "
  
  ",cfa_constructive_syn,"
  
  # Covariance
  #Politeness ~~ Constructive
  BNotIntolerant ~~ BRespectful
  BNotHostile ~~ BRespectful
  BNotToxicNum ~~ BAgreement
  
  
  # direct path 

  ProductiveNum ~ c*Politeness + a*Constructive 
  Politeness ~ b*Constructive
  
  indirect := c*b
  direct := a
  total := a + c*b
  pmediated := indirect/total
  
  ")


fit_medpol = sem(medpol_syn, data=dt, meanstructure=T)
summary(fit_medpol)


# sparate models for eACH , REPORT MODEL FIT 
# run the full thing
# measurment invariance
# interpre effects





######################
polite = c("BNotToxicNum","BNotHostile","BNoHateSpeechNum","BNotIntolerant")
cfa_polite_syn = paste0('Politeness =~',paste0(polite,collapse = "+"))

constructive = c("BRespectful","BMakesEffortNum","BStaysOnTopicNum","BAgreement")
cfa_constructive_syn = paste0('Constructive =~',paste0(constructive,collapse = "+"))

cfa_syn = paste0(
  cfa_polite_syn, "
  
  ",cfa_constructive_syn,"
  
  # Covariance
  Politeness ~~ Constructive
  BNotIntolerant ~~ BRespectful
  BNotHostile ~~ BRespectful
  BNotToxicNum ~~ BAgreement
  
  
  ")
cfa_syn


fit <- dt %>%
  select(all_of(c(polite,constructive))) %>% 
  na.omit() %>% 
  lavaan::cfa(cfa_syn, data = ., meanstructure = F, ordered=c(polite,constructive), effect.coding="loadings") 
summary(fit)
pdf("Figures/16_semPlot.pdf",width=6, height=4.5)
semPaths(fit, whatLabels = "std",nCharNodes=20,fade=F,curve=2,residuals=T,shapeInt="square",sizeInt=10e-200, 
         label.font=1, label.scale=T, node.width=1.5, thresholds=F,edge.label.cex = 1, what="std")
dev.off()
fitmeasures(fit, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))

rcov = lavResiduals(fit)$cov
rcov  
which(abs(rcov)>0.2)
corrplot(rcov, method = "color", tl.col = "black")

efa2 = dt %>% 
  select(all_of(c(polite,constructive))) %>% 
  na.omit() %>% 
  fa(.,
     nfactors = 2, 
     fm = "ml", 
     cor = "cor", 
     rotate = "oblimin", 
     n.iter = 50)
efa2
fa.diagram(efa2,cut = 0.01,main = "Exploratory factor analysis")


efa3 = dt %>% 
  select(all_of(c(polite,constructive))) %>% 
  na.omit() %>% 
  fa(.,
     nfactors = 3, 
     fm = "ml", 
     cor = "cor", 
     rotate = "oblimin", 
     n.iter = 50)
efa3
fa.diagram(efa3,cut = 0.01,main = "Exploratory factor analysis")

# SEM: Perceived constructiveness causes evaluators to see posts as Agreement, Staying on topic
# It's okay to omit covariance terms but mention them in appendix
# Add covariances to increase model fit

medpol_syn = paste0(
  cfa_polite_syn, "
  
  ",cfa_constructive_syn,"
  
  # Covariance
  #Politeness ~~ Constructive
  BNotIntolerant ~~ BRespectful
  BNotHostile ~~ BRespectful
  BNotToxicNum ~~ BAgreement
  
  
  # direct path 

  ProductiveNum ~ c*Politeness + a*Constructive 
  Politeness ~ b*Constructive
  
  indirect := c*b
  direct := a
  total := a + c*b
  pmediated := indirect/total
  
  ")


fit_medpol = sem(medpol_syn, data=dt, meanstructure=T)
summary(fit_medpol)


# sparate models for eACH , REPORT MODEL FIT 
# run the full thing
# measurment invariance
# interpre effects























################################################################################
# Notes 
################################################################################


polite = c("BNotIntolerant","BNotToxicNum", "BNoHateSpeechNum","BNotHostile","BRespectful")
constructive = c("BMakesEffortNum","BStaysOnTopicNum","UnderstandB")

cor(dt[,c(polite,constructive)]) %>% round(2)

tetrachoric(dt[,c("BNotIntolerant","BNotRespectful","BAgreement","BNoneabove")])

tetrachoric(dt[,c("BNotSarcastic","BNotEmotional","BObjective","BRespectful","BNotIntolerant","BOpen")])

DVs


# excluded 
# BNoneabove
# BNotSarcastic (0.26 loading)
# BOpen is weakly correlated with every measure

efa2 = dt %>% 
  select(all_of(c(polite,constructive))) %>% 
  na.omit() %>% 
  fa(.,
     nfactors = 2, 
     fm = "ml", 
     cor = "cor", 
     rotate = "oblimin", 
     n.iter = 10)
efa2

fa.diagram(efa2,cut = 0.01,main = "Exploratory factor analysis")




# Politeness --------------------------------------------------------------
cfa_polite_syn = paste0('Polite =~',paste0(polite,collapse = "+"))
cfa_polite = dt %>%
  select(all_of(polite)) %>% 
  na.omit() %>% 
  lavaan::cfa(cfa_polite_syn, data=., meanstructure=F, ordered=T, effect.coding="loadings") 
# all above 0.6
standardizedsolution(cfa_polite)

summary(cfa_polite)
fitmeasures(cfa_polite, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))
corrplot(lavResiduals(cfa_polite)$cov, method = "color", tl.col = "black")


# Constructive ----------------------------------------------------------------
cfa_constructive_syn = paste0('Constructive =~',paste0(constructive,collapse = "+"))
cfa_constructive = dt %>%
  select(all_of(constructive)) %>% 
  na.omit() %>% 
  lavaan::cfa(cfa_constructive_syn, data=., meanstructure=F, ordered=T, effect.coding="loadings") 
# all >= .59
# 
standardizedsolution(cfa_constructive)

summary(cfa_constructive)
fitmeasures(cfa_constructive, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))
corrplot(lavResiduals(cfa_constructive)$cov, method = "color", tl.col = "black")


 



cfa_syn = paste0(
  cfa_polite_syn, "
  
  ",cfa_constructive_syn,"
  # Higher order factor
  #Civility =~ Polite + Constructive
  
  # Covariance
  Polite ~~ Constructive
  BMakesEffortNum ~~ BRespectful
  #UnderstandB ~~ BRespectful
  BStaysOnTopicNum ~~ BRespectful
  #BNotIntolerant ~~ BRespectful
  #BNotHostile ~~ BRespectful
  ")
cfa_syn

fit <- dt %>%
  select(all_of(c(polite,constructive))) %>% 
  na.omit() %>% 
  lavaan::cfa(cfa_syn, data = ., meanstructure = F, ordered=c(polite,constructive), effect.coding="loadings") 
standardizedsolution(fit)
inspect(fit)


lavInspect(fit, "cov.lv")

# heywood case or 
summary(fit)
fitmeasures(fit, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))
# factor loadings 
# how to higher order concepts (put it more observed varibles)
# drop variables? e.g. Open and Objective

rcov = lavResiduals(fit)$cov
rcov  
which(abs(rcov)>0.2)
corrplot(rcov, method = "color", tl.col = "black")

#https://www.quantargo.com/help/r/latest/packages/lavaan/0.6-10/lavPredict

semPaths(fit, whatLabels = "std",
         edge.label.cex = 1, what="std")



polite = c("BNotToxicNum", "BNoHateSpeechNum","BNotHostile")
cfa_polite_syn = paste0('Polite =~',paste0(polite,collapse = "+"))

constructive = c("BRespectful","BMakesEffortNum","BStaysOnTopicNum")
cfa_constructive_syn = paste0('Constructive =~',paste0(constructive,collapse = "+"))

cfa_syn = paste0(
  cfa_polite_syn, "
  
  ",cfa_constructive_syn,"
  # Higher order factor
  #Civility =~ Polite + Constructive
  
  # Covariance
  Polite ~~ Constructive
  #BNotIntolerant ~~ BRespectful
  BNotHostile ~~ BRespectful
  #BStaysOnTopicNum ~~ BRespectful
  ")
cfa_syn

fit <- dt %>%
  select(all_of(c(polite,constructive))) %>% 
  na.omit() %>% 
  lavaan::cfa(cfa_syn, data = ., meanstructure = F, ordered=c(polite,constructive), effect.coding="loadings") 
semPaths(fit, whatLabels = "std",nCharNodes = 0,layout = "tree2",sizeMan = 11,
         edge.label.cex = 1, what="std")

fitmeasures(fit, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))
# factor loadings 
# how to higher order concepts (put it more observed varibles)
# drop variables? e.g. Open and Objective

rcov = lavResiduals(fit)$cov
rcov  
which(abs(rcov)>0.2)
corrplot(rcov, method = "color", tl.col = "black")

idx <- lavInspect(fit, "case.idx")
# fscores <- lavPredict(fit)
# for (fs in colnames(fscores)) {
#   dt[idx, fs] <- fscores[ , fs]
# }
# cor(dt[c("Constructive",constructive)])
# cor(dt[c("Polite",polite)])




summfe = dt %>%
  select(all_of(c("Constructive","Polite")),PolIdComp,all_of(Covs)) %>%
  gather(var,val,-PolIdComp,-all_of(Covs)) %>%
  group_by(var) %>%
  do(tidy(feols(fo("val",Covs[-which(Covs=="target_id")]),fixef="target_id",vcov="HC1",data = .),conf.int=T)) 
summfe


polite = c("BNotToxicNum", "BNoHateSpeechNum","BNotHostile","BNotIntolerant")
cfa_polite_syn = paste0('Polite =~',paste0(polite,collapse = "+"))

constructive = c("BRespectful","BMakesEffortNum","BStaysOnTopicNum")
cfa_constructive_syn = paste0('Constructive =~',paste0(constructive,collapse = "+"))

cfa_syn = paste0(
  cfa_polite_syn, "
  
  ",cfa_constructive_syn,"
  # Higher order factor
  #Civility =~ Polite + Constructive
  
  # Covariance
  Polite ~~ Constructive
  BNotIntolerant ~~ BRespectful
  BNotHostile ~~ BRespectful
  #BStaysOnTopicNum ~~ BRespectful
  ")
cfa_syn

fit <- dt %>%
  select(all_of(c(polite,constructive))) %>% 
  na.omit() %>% 
  lavaan::cfa(cfa_syn, data = ., meanstructure = F, ordered=c(polite,constructive), effect.coding="loadings") 
semPaths(fit, whatLabels = "std",nCharNodes=20,fade=F,curve=2,residuals=T,shapeInt="square",sizeInt=10e-200, 
         label.font=1, label.scale=T, node.width=1.5, thresholds=F,edge.label.cex = 1, what="std")

fitmeasures(fit, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))

rcov = lavResiduals(fit)$cov
rcov  
which(abs(rcov)>0.2)
corrplot(rcov, method = "color", tl.col = "black")

# constructive = engaging with the other's perpective
# showing respect, making an effort to understand, and staying on topic

# polite = absence of agitation / not-destructive




