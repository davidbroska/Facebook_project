# Setup ########################################################################
library(stringr)
library(AER)
library(lmtest)
library(broom)
library(stringr)
library(fixest)
library(corrplot)
library(multidplyr)
library(broom.mixed)
library(lme4)
library(performance)
library(sjPlot)
library(kableExtra)
library(ggplot2)
library(tidyr)
library(readr) 
library(dplyr)
theme_set(theme_bw(base_size = 10))
report_effect = function(.m, .coef_name, .one_sided=F){
  
  pval = function(p){
    if(p>= 0.001) txt = paste0("p=",substring(round(p,3),2))
    if(p < 0.001) txt = "p<.001"
    return(txt)
  }
  # subset to relevant coefficient
  s = as.data.frame(summary(.m)$coefficients)
  s$term = rownames(s)
  s = s[s$term==.coef_name,]
  # get stats
  dof = .m$df.residual
  pvalue = ifelse(.one_sided, pval(s$`Pr(>|t|)`/2), pval(s$`Pr(>|t|)`))
  # created text
  txt = paste0("(b=",round(s$Estimate,3),
               ", t(",dof,")=",round(s$`t value`,2),
               ", ",pvalue, ")")
  return(txt)
}


# function to create regression equation
fo = function(.dv, .covs) {
  .covs = paste(.covs,collapse = "+")
  formula(paste0(.dv,"~",.covs))
}

scale2 = function(.var){
  # scale by 2 standard deviations 
  # then the regression coefficient regression represents the change in the DV by 2 SDs
  (.var - mean(.var,na.rm=T)) / (2*sd(.var,na.rm=T))
}

# function to run linear probability model with robust SEs  
run_lpm = function(.formula, .data){
  
  m = lm(.formula, .data)
  tidy(coeftest(m, vcov. = vcovHC, type = "HC3"),conf.int=T)
  # https://www.econometrics-with-r.org/11.1-binary-dependent-variables-and-the-linear-probability-model.html
}

custom_fa = function(.data, .nfactors,.niter=1, .cor="cor"){
  fa(.data, 
     nfactors = .nfactors, 
     fm = "ml", 
     cor = .cor, 
     rotate = "oblimin", 
     n.iter = .niter)
}



TopicsTab = tribble(
  ~ topic, ~ label,
  "antiamerica",       "Anti-America",
  "antichristianity",  "Anti-Christianity",
  "suggestive",        "Suggestive",
  "drugs",             "Drugs",
  "antitrump",         "Anti-Trump",
  "protrump",          "Pro-Trump",
  "anticlinton",       "Anti-Clinton",
  "proclinton",        "Pro-Clinton",
  "antiimmigrant",     "Anti-Immigrant",
  "proimmigrant",      "Pro-Immigrant",
  "antiabortion",      "Anti-Abortion",
  "proabortion",       "Pro-Abortion",
  "antigun",           "Anti-Gun",
  "progun",            "Pro-Gun",
  "antiwhite",         "Anti-White"
)
topics = TopicsTab$topic
emf = c("CareNegSen","FairnessNegSen","LoyaltyNegSen","AuthorityNegSen","SanctityNegSen") %>% 
  paste0(.,"2Sd")

LiwcCats = c("tone_pos","tone_neg","emo_pos","emo_neg","swear",
             "conflict","prosocial","polite","moral","comm",
             "cogproc", 
             "politic", "ethnicity", "tech",               # culture 
             "leisure", "home", "work", "money", "relig",  # lifestyle  
             "substances","sexual", "food","death",        # physical
             "male","female",
             "shehe","they","you","i","we","Emoji",
             "power","achieve"
)

# political ideology ordered categories from right-wing to left-wing
PolIdLevels = c("Extremely liberal","Liberal","Slightly liberal","Moderate",
                "Slightly conservative","Conservative","Extremely conservative")

# party id ordered categories from right-wing to left-wing
PartisanshipLevels = c("Strong Democrat","Weak Democrat","Leaning Democrat","Independent",
                       "Leaning Republican","Weak Republican","Strong Republican")

# covariates used in regression analysis
CovsLabs = c("Political Ideology Comp.", "Age","Gender","Education","Marital status","Religion","Sexual orientation",
             "Household size","Region", "Income in tsd.", "Race", "Fb comment", "Order")
Covs = c("PolIdComp2Sd", "Age2Sd","Gender","EducationNum2Sd","MaritalStatus","Religion","SexualOrientation",
         "HhSize","Region","Income2Sd", "Race", "target_id","Order")
if(length(CovsLabs) != length(Covs)) warning("Length mismatch"); 


CovsLean = Covs[-which(Covs %in% c("target_id","Order"))]

DVsTable = tibble(DVs = c("UnderstandBNum01","BMakesEffortNum01","BStaysOnTopicNum01","BAgreesNum01",
                          "BRespectful","BOpen","BObjective","BEmotional","BSarcastic","BIntolerant","BHostile","BNoneabove",
                          "BToxicNum01","BUseHateSpeechNum01","ProductiveNum01"), 
                  BinaryDVs = c("UnderstandB","BMakesEffort","BStaysOnTopic","BAgrees",
                                "BRespectful","BOpen","BObjective","BEmotional","BSarcastic","BIntolerant","BHostile","BNoneabove",
                                "BIsToxic","BUseHateSpeech","IsProductive"),
                  CatDVs = c("UnderstandB","BMakesEffort","BStaysOnTopic","BAgrees",
                             "BRespectful","BOpen","BObjective","BEmotional","BSarcastic","BIntolerant","BHostile","BNoneabove",
                             "Toxicity","BUseHateSpeech","Productive"),
                  Labs = c("Understand B","B makes effort","B stays on topic","B agrees",
                           "B is respectul","B is open","B is objective","B is emotional","B is sarcastic","B is intolerant","B is hostile","B is none of the above",
                           "B is toxic","B uses hate speech","Conversation is productive"))
              
DVsLabs = DVsTable$Labs

DVs = DVsTable$DVs
BinaryDVs = DVsTable$BinaryDVs
CatDVs = DVsTable$CatDVs


if(length(DVsLabs) != length(DVs)) warning("Length mismatch"); 


DVsDmd = paste0(DVs,"_NonDmd")
BinaryDVsDmd = paste0(BinaryDVs, "_NonDmd")
MainDvLabs = c("Productive"="ProductiveNum01","Toxic"="BToxicityNum01")

FaPSplit = 0.75

# plotting specifications
PlotWidth=6
PlotHeight=5
StrWrap=140


