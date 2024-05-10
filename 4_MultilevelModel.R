# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
library(lme4)
library(kableExtra)
library(performance)
set.seed(04162024)
is_REML = F

fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)

dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels), 
         # subtract out grand mean
         BToxicNum01 = BToxicNum01 - mean(BToxicNum01),
         ProductiveNum01 = ProductiveNum01 - mean(ProductiveNum01),
         Order = Order+3)

vars = c("BToxicNum01","ProductiveNum01","ResponseId","target_id","PolIdComp2Sd","Age2Sd","Gender","EducationNum2Sd",
         "MaritalStatus","Religion","SexualOrientation","HhSize","Region","Income2Sd","Race","Order",
         "ideo_commenterB","target_likes_count",
         "antiamerica","antichristianity","suggestive",#"antiwhite.csv",
         "drugs",#"killing.csv"
         "antitrump","protrump","anticlinton","proclinton",#"antiobama.csv",
         "antiimmigrant","proimmigrant","antiabortion","proabortion",
         "antigun")#,"progun.csv","antirepcons.csv","antidemlib.csv")

# check for NAs
summarise(dt, across(all_of(vars), ~ sum(is.na(.)))) %>% 
  select_if(~ any(. > 0))



dt %>%
  select(all_of(c(LiwcCats,Topics))) 
#   cor(use = "complete") %>% 
#   corrplot()



###########
# TOXICITY
########### 

# Null model -------------------------------------------------------------------
# 1. There is a correlation within individuals and within comments on perceived toxicity
m0_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id), data=dt, REML=is_REML)
icc(m0_tox,by_group = T)

# Q: How large are these variances, and the icc?
# 10% enough + substantive question

# Level 1 predictors --------------------------------------------------
# 2. Liberals perceive comments overall as more toxic than conservatives
m1a_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + PolIdComp2Sd, 
               data=dt, REML=is_REML)

summary(m1a_tox)


m1b_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                 PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                 MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                 Income2Sd + Race + Order, 
               data=dt, REML=is_REML)
summary(m1b_tox)


m1a_pid = fixef(m1a_tox)["PolIdComp2Sd"]
m1b_pid = fixef(m1b_tox)["PolIdComp2Sd"]

# percent of the effect size that is left after controlling for individual level predictors
1 - ((m1a_pid - m1b_pid) / m1a_pid)


# 9% of variation in 
(0.01594 - 0.01445) / 0.01594


# Likelihood ratio test is significant but the improvement in BIC is not much
#anova(m0_tox, m1b_tox)
# Q: Again, the variance in random intercepts for individuals does not go down by much



dt$achieve
# Comment predictors ------------------------------------------------------

# Adding just comment predictors
m2a_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
               # Comment predictors
               antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
               antitrump+protrump+anticlinton+proclinton+ #"antiobama
               antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
               ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
               tone_pos+tone_neg+emo_pos+emo_neg+swear+
               conflict+prosocial+polite+moral+comm+
               cogproc+politic+ethnicity+tech+
               leisure+home+work+money+relig+
               substances+sexual+food+death+
               male+female+shehe+they+you+i+we+Emoji+
               power+achieve, 
              data=dt, REML=is_REML)
summary(m2a_tox)

m2b_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                 # Person predictors
                 PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                 MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                 Income2Sd + Race + Order + 
                   # Comment predictors  
                   antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                   antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                   antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                   ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                   tone_pos+tone_neg+emo_pos+emo_neg+swear+
                   conflict+prosocial+polite+moral+comm+
                   cogproc+politic+ethnicity+tech+
                   leisure+home+work+money+relig+  
                   substances+sexual+food+death+    
                   male+female+shehe+they+you+i+we+Emoji+
                   power+achieve, 
               data=dt, REML=is_REML)
summary(m2b_tox)

m2b_pid = fixef(m2b_tox)["PolIdComp2Sd"]

# percent of the effect size that is left after controlling for individual AND comment level predictors
1 - ((m1a_pid - m2b_pid) / m1a_pid)

(0.05408-0.04464) / 0.05408

# Q: The variance in random intercepts for comments goes down by 0.02

# Random slopes ----------------------------------------------------------------
# The relationship between perceived toxicity and political ideology is not the same for all comments
m3a_tox = lmer(BToxicNum01 ~ PolIdComp2Sd + (1|ResponseId) + 
                 (1+PolIdComp2Sd|target_id), data=dt, REML=is_REML)
#anova(m1a_tox, m3a_tox)
summary(m3a_tox)

# 95% of the slopes are between
-0.064491 - 1.96*0.08503 
-0.064491 + 1.96*0.08503
# for some comments, the association is zero


# Q: Is the conclusion that the relationship between perceived toxicity does not strongly vary political ideology
# Liberals and conservative perceive each post similarly


# do I run random slopes in the fully specified model?
m3b_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                 # Person predictors
                 PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                 MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                 Income2Sd + Race + Order + 
                   # Comment predictors  
                   antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                   antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                   antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                   ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                   tone_pos+tone_neg+emo_pos+emo_neg+swear+
                   conflict+prosocial+polite+moral+comm+
                   cogproc+politic+ethnicity+tech+
                   leisure+home+work+money+relig+  
                   substances+sexual+food+death+    
                   male+female+shehe+they+you+i+we+Emoji+
                   power+achieve+
                     # random slopes
                     (1+PolIdComp2Sd|target_id),
               data=dt, REML=is_REML)
summary(m3b_tox)
#anova(m2b_tox, m3b_tox)
#anova(m3a_tox, m3b_tox)

# Q: did not converge
colnames(dt)

# Cross-level interaction ------------------------------------------------------
m4_tox_all = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                        # Person predictors
                        PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                        MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                        Income2Sd + Race + Order + 
                          # Comment predictors  
                          antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                          antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                          antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                          ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                          tone_pos+tone_neg+emo_pos+emo_neg+swear+
                          conflict+prosocial+polite+moral+comm+
                          cogproc+politic+ethnicity+tech+
                          leisure+home+work+money+relig+  
                          substances+sexual+food+death+    
                          male+female+shehe+they+you+i+we+Emoji+
                          power+achieve+
                            # Interaction
                            (antiamerica*PolIdComp2Sd) +
                            (antichristianity*PolIdComp2Sd) +
                            (suggestive*PolIdComp2Sd) +
                            (drugs*PolIdComp2Sd) +
                            (antitrump*PolIdComp2Sd) +
                            (protrump*PolIdComp2Sd) +
                            (anticlinton*PolIdComp2Sd) +
                            (proclinton*PolIdComp2Sd) +
                            (antiimmigrant*PolIdComp2Sd) +
                            (proimmigrant*PolIdComp2Sd) +
                            (antiabortion*PolIdComp2Sd) +
                            (proabortion*PolIdComp2Sd) +
                            (antigun*PolIdComp2Sd) +
                            (progun*PolIdComp2Sd) +
                            (ideo_commenterB*PolIdComp2Sd) +
                            (target_likes_count*PolIdComp2Sd) +
                            (Age2Sd*PolIdComp2Sd) +
                            (EducationNum2Sd*PolIdComp2Sd) +
                            (Income2Sd*PolIdComp2Sd) +
                            (Order*PolIdComp2Sd),
                  data=dt, REML=is_REML)



plotint = function(.m, .xaxis, .topic,.lab) {
  lab = unlist(unname(.lab))
  topic = unlist(unname(.topic))
  
  ggp = ggpredict(.m,terms = c(.xaxis,topic))
  
  p = plot(ggp) +
    scale_color_manual(breaks = factor(c(1,0)),
                       values=c("darkgreen","grey"),
                       labels=c(lab,"Other topic            ")) +
    labs(title=lab, color="",
         x="Political ideology composite",
         y="Perceived toxicity") +
    scale_y_continuous(limits = c(-0.3,0.3)) +
    scale_x_continuous(limits = c(-0.6,1)) +
    coord_fixed()
  ggsave(paste0("Figures/",topic,"_inter.pdf"),width = 6,height = 5)
  l = list(topic = topic, data = ggp,plot = p)
  
  return(l)
}

predicted = apply(TopicsTab, 1, function(tt)  plotint(m4_tox_all,.xaxis="PolIdComp2Sd",.topic=tt[1],.lab=tt[2]))
pd = predicted %>% 
  lapply(., function(d){
    d$data %>% 
      mutate(topic = d$topic)
  }) %>% 
  do.call(bind_rows,.)
pp = lapply(predicted, function(p) p$plot)
plot(pd[[1]])
ggpubr::ggarrange(
  pp[[1]]+labs(x=""),pp[[2]]+labs(x="",y=""),
  pp[[3]]+labs(x=""),pp[[4]]+labs(x="",y=""),
  pp[[5]]+labs(x=""),pp[[6]]+labs(x="",y=""),
  pp[[7]]+labs(x=""),pp[[8]]+labs(x="",y=""),
  pp[[11]]+labs(x=""),pp[[10]]+labs(x="",y=""),
  pp[[9]],pp[[12]]+labs(y=""),
  ncol=2, nrow=6)
ggsave("Figures/topic_interactions.pdf",width=8.27, height=11.69,bg="white")

names(pp)

m=lm(mpg ~  wt*am+wt*am2, mutate(mtcars,am2=rbinom(nrow(mtcars),1,prob = .5)))
TT = tribble(
  ~topic,~label,
  "am","amer",
  "am2","amer2"
)

predicted = apply(TT, 1, function(tt)  plotint(m,.xaxis="wt",.topic=tt[1],.lab=tt[2]))
pd = predicted %>% 
  lapply(., function(d){
    d$data %>% 
      mutate(topic = d$topic)
    }) %>% 
  do.call(bind_rows,.)
pp = lapply(predicted, function(p) p$plot)





#plotlist = apply(TopicsTab,1, function(tt) plotint(m4_tox_all,.xaxis="PolIdComp2Sd",.topic=tt[1],.lab=tt[2]))





plist
m4_tox_antiamerica = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                        # Person predictors
                        PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                        MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                        Income2Sd + Race + Order + 
                          # Comment predictors  
                          antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                          antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                          antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                          ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                          tone_pos+tone_neg+emo_pos+emo_neg+swear+
                          conflict+prosocial+polite+moral+comm+
                          cogproc+politic+ethnicity+tech+
                          leisure+home+work+money+relig+  
                          substances+sexual+food+death+    
                          male+female+shehe+they+you+i+we+Emoji+
                          power+achieve+
                            # Interaction
                            (antiamerica*PolIdComp2Sd),
                      data=dt, REML=is_REML)
m4_tox_antichristianity = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                        # Person predictors
                        PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                        MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                        Income2Sd + Race + Order + 
                          # Comment predictors  
                          antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                          antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                          antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                          ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                          tone_pos+tone_neg+emo_pos+emo_neg+swear+
                          conflict+prosocial+polite+moral+comm+
                          cogproc+politic+ethnicity+tech+
                          leisure+home+work+money+relig+  
                          substances+sexual+food+death+    
                          male+female+shehe+they+you+i+we+Emoji+
                          power+achieve+
                            # Interaction
                            (antichristianity*PolIdComp2Sd),
                      data=dt, REML=is_REML)

m4_tox_suggestive = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                           # Person predictors
                           PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                           MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                           Income2Sd + Race + Order + 
                           # Comment predictors  
                           antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                           antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                           antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                           ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                           tone_pos+tone_neg+emo_pos+emo_neg+swear+
                           conflict+prosocial+polite+moral+comm+
                           cogproc+politic+ethnicity+tech+
                           leisure+home+work+money+relig+  
                           substances+sexual+food+death+    
                           male+female+shehe+they+you+i+we+Emoji+
                           power+achieve+
                           # Interaction
                           (suggestive*PolIdComp2Sd),
                         data=dt, REML=is_REML)

m4_tox_drugs = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                      # Person predictors
                      PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                      MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                      Income2Sd + Race + Order + 
                      # Comment predictors  
                      antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                      antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                      antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                      ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                      tone_pos+tone_neg+emo_pos+emo_neg+swear+
                      conflict+prosocial+polite+moral+comm+
                      cogproc+politic+ethnicity+tech+
                      leisure+home+work+money+relig+  
                      substances+sexual+food+death+    
                      male+female+shehe+they+you+i+we+Emoji+
                      power+achieve+
                      # Interaction
                      (drugs*PolIdComp2Sd),
                    data=dt, REML=is_REML)

m4_tox_antitrump = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                          # Person predictors
                          PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                          MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                          Income2Sd + Race + Order + 
                          # Comment predictors  
                          antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                          antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                          antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                          ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                          tone_pos+tone_neg+emo_pos+emo_neg+swear+
                          conflict+prosocial+polite+moral+comm+
                          cogproc+politic+ethnicity+tech+
                          leisure+home+work+money+relig+  
                          substances+sexual+food+death+    
                          male+female+shehe+they+you+i+we+Emoji+
                          power+achieve+
                          # Interaction
                          (antitrump*PolIdComp2Sd),
              data=dt, REML=is_REML)

m4_tox_protrump = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                         # Person predictors
                         PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                         MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                         Income2Sd + Race + Order + 
                         # Comment predictors  
                         antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                         antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                         antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                         ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                         tone_pos+tone_neg+emo_pos+emo_neg+swear+
                         conflict+prosocial+polite+moral+comm+
                         cogproc+politic+ethnicity+tech+
                         leisure+home+work+money+relig+  
                         substances+sexual+food+death+    
                         male+female+shehe+they+you+i+we+Emoji+
                         power+achieve+
                         # Interaction
                         (protrump*PolIdComp2Sd),
              data=dt, REML=is_REML)

m4_tox_anticlinton = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                            # Person predictors
                            PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                            MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                            Income2Sd + Race + Order + 
                            # Comment predictors  
                            antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                            antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                            antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                            ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                            tone_pos+tone_neg+emo_pos+emo_neg+swear+
                            conflict+prosocial+polite+moral+comm+
                            cogproc+politic+ethnicity+tech+
                            leisure+home+work+money+relig+  
                            substances+sexual+food+death+    
                            male+female+shehe+they+you+i+we+Emoji+
                            power+achieve+
                            # Interaction
                            (anticlinton*PolIdComp2Sd),
                          data=dt, REML=is_REML)

m4_tox_proclinton = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                           # Person predictors
                           PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                           MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                           Income2Sd + Race + Order + 
                           # Comment predictors  
                           antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                           antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                           antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                           ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                           tone_pos+tone_neg+emo_pos+emo_neg+swear+
                           conflict+prosocial+polite+moral+comm+
                           cogproc+politic+ethnicity+tech+
                           leisure+home+work+money+relig+  
                           substances+sexual+food+death+    
                           male+female+shehe+they+you+i+we+Emoji+
                           power+achieve+
                           # Interaction
                           (proclinton*PolIdComp2Sd),
                         data=dt, REML=is_REML)

m4_tox_antiimmigrant = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                           # Person predictors
                           PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                           MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                           Income2Sd + Race + Order + 
                           # Comment predictors  
                           antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                           antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                           antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                           ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                           tone_pos+tone_neg+emo_pos+emo_neg+swear+
                           conflict+prosocial+polite+moral+comm+
                           cogproc+politic+ethnicity+tech+
                           leisure+home+work+money+relig+  
                           substances+sexual+food+death+    
                           male+female+shehe+they+you+i+we+Emoji+
                           power+achieve+
                           # Interaction
                           (antiimmigrant*PolIdComp2Sd),
                         data=dt, REML=is_REML)

m4_tox_proimmigrant = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                           # Person predictors
                           PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                           MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                           Income2Sd + Race + Order + 
                           # Comment predictors  
                           antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                           antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                           antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                           ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                           tone_pos+tone_neg+emo_pos+emo_neg+swear+
                           conflict+prosocial+polite+moral+comm+
                           cogproc+politic+ethnicity+tech+
                           leisure+home+work+money+relig+  
                           substances+sexual+food+death+    
                           male+female+shehe+they+you+i+we+Emoji+
                           power+achieve+
                           # Interaction
                           (proimmigrant*PolIdComp2Sd),
                         data=dt, REML=is_REML)

m4_tox_antiabortion = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                           # Person predictors
                           PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                           MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                           Income2Sd + Race + Order + 
                           # Comment predictors  
                           antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                           antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                           antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                           ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                           tone_pos+tone_neg+emo_pos+emo_neg+swear+
                           conflict+prosocial+polite+moral+comm+
                           cogproc+politic+ethnicity+tech+
                           leisure+home+work+money+relig+  
                           substances+sexual+food+death+    
                           male+female+shehe+they+you+i+we+Emoji+
                           power+achieve+
                           # Interaction
                           (antiabortion*PolIdComp2Sd),
                         data=dt, REML=is_REML)

m4_tox_proabortion = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                           # Person predictors
                           PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                           MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                           Income2Sd + Race + Order + 
                           # Comment predictors  
                           antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                           antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                           antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                           ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                           tone_pos+tone_neg+emo_pos+emo_neg+swear+
                           conflict+prosocial+polite+moral+comm+
                           cogproc+politic+ethnicity+tech+
                           leisure+home+work+money+relig+  
                           substances+sexual+food+death+    
                           male+female+shehe+they+you+i+we+Emoji+
                           power+achieve+
                           # Interaction
                           (proabortion*PolIdComp2Sd),
                         data=dt, REML=is_REML)

m4_tox_antigun = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                        # Person predictors
                        PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                        MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                        Income2Sd + Race + Order + 
                          # Comment predictors  
                          antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                          antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                          antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                          ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                          tone_pos+tone_neg+emo_pos+emo_neg+swear+
                          conflict+prosocial+polite+moral+comm+
                          cogproc+politic+ethnicity+tech+
                          leisure+home+work+money+relig+  
                          substances+sexual+food+death+    
                          male+female+shehe+they+you+i+we+Emoji+
                          power+achieve+
                            # Interaction
                            (antigun*PolIdComp2Sd),
                      data=dt, REML=is_REML)

# m4_tox_progun = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
#                         # Person predictors
#                         PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
#                         MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
#                         Income2Sd + Race + Order + 
#                           # Comment predictors  
#                           antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
#                           antitrump+protrump+anticlinton+proclinton+ #"antiobama,
#                           antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
#                           ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
#                           tone_pos+tone_neg+emo_pos+emo_neg+swear+
#                           conflict+prosocial+polite+moral+comm+
#                           cogproc+politic+ethnicity+tech+
#                           leisure+home+work+money+relig+  
#                           substances+sexual+food+death+    
#                           male+female+shehe+they+you+i+we+Emoji+
#                           power+achieve+
#                             # Interaction
#                             (progun*PolIdComp2Sd),
#                       data=dt, REML=is_REML)




m4_tox_ideo = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                     # Person predictors
                     PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                     MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                     Income2Sd + Race + Order + 
                     # Comment predictors  
                     antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                     antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                     antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                     ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                     tone_pos+tone_neg+emo_pos+emo_neg+swear+
                     conflict+prosocial+polite+moral+comm+
                     cogproc+politic+ethnicity+tech+
                     leisure+home+work+money+relig+  
                     substances+sexual+food+death+    
                     male+female+shehe+they+you+i+we+Emoji+
                     power+achieve+
                     # Interaction
                     (ideo_commenterB*PolIdComp2Sd),
              data=dt, REML=is_REML)
m4_tox_like = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                     # Person predictors
                     PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                     MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                     Income2Sd + Race + Order + 
                     # Comment predictors  
                     antiamerica+antichristianity+suggestive+drugs+# antiwhite+killing+
                     antitrump+protrump+anticlinton+proclinton+ #"antiobama,
                     antiimmigrant+proimmigrant+antiabortion+proabortion+antigun+
                     ideo_commenterB+target_likes_count+progun+#+antirepcons+antidemlib"
                     tone_pos+tone_neg+emo_pos+emo_neg+swear+
                     conflict+prosocial+polite+moral+comm+
                     cogproc+politic+ethnicity+tech+
                     leisure+home+work+money+relig+  
                     substances+sexual+food+death+    
                     male+female+shehe+they+you+i+we+Emoji+
                     power+achieve+
                     # Interaction
                     (target_likes_count*PolIdComp2Sd),
              data=dt, REML=is_REML)


library(ggeffects)


pm = plot_model(m4_tox_anticlinton,type="int")
pm2 = plot_model(m4_tox_anticlinton,type="pred",terms = c("PolIdComp2Sd","anticlinton"))

pred_anticlinton = ggpredict(m4_tox_anticlinton,terms = c("PolIdComp2Sd","anticlinton"))
pred_proclinton = ggpredict(m4_tox_proclinton,terms = c("PolIdComp2Sd","proclinton"))

pred_antitrump= ggpredict(m4_tox_antitrump,terms = c("PolIdComp2Sd","antitrump"))
pred_protrump = ggpredict(m4_tox_protrump,terms = c("PolIdComp2Sd","protrump"))

pred_drugs = ggpredict(m4_tox_drugs,terms = c("PolIdComp2Sd","drugs"))

pred_suggestive = ggpredict(m4_tox_suggestive,terms = c("PolIdComp2Sd","suggestive"))

pred_ideo = ggpredict(m4_tox_ideo,terms = c("PolIdComp2Sd","ideo_commenterB"))
pred_like = ggpredict(m4_tox_like,terms = c("PolIdComp2Sd","target_likes_count"))

plotint = function(.m)

library(sjPlot)
plot(pred_anticlinton) + 
  scale_color_manual(values=c("1"="darkgreen","0"="grey"),labels=c("1"="yes","0"="no")) +
  labs(title="Negative comment about Hillary Clinton",
       color="Classified\nas relevant",
       x="Political ideology composite (more conservative -->)")
plot(pred_proclinton) +
  scale_color_manual(values=c("1"="darkgreen","0"="grey"),labels=c("1"="yes","0"="no")) +
  labs(title="Positive comment about Hillary Clinton",
       color="Classified\nas relevant",
       x="Political ideology composite (more conservative -->)")
plot(pred_antitrump) +
  scale_color_manual(values=c("1"="darkgreen","0"="grey"),labels=c("1"="yes","0"="no")) +
  labs(title="Negative comment about Donald Trump",
       color="Classified\nas relevant",
       x="Political ideology composite (more conservative -->)")
plot(pred_protrump)+
  scale_color_manual(values=c("1"="darkgreen","0"="grey"),labels=c("1"="yes","0"="no")) +
  labs(title="Positive comment about Donald Trump",
       color="Classified\nas relevant",
       x="Political ideology composite (more conservative -->)")
plot(pred_drugs) +
  scale_color_manual(values=c("1"="darkgreen","0"="grey"),labels=c("1"="yes","0"="no")) +
  labs(title="Comment about drugs",
       color="Classified\nas relevant",
       x="Political ideology composite (more conservative -->)")
plot(pred_suggestive) +
  scale_color_manual(values=c("1"="darkgreen","0"="grey"),labels=c("1"="yes","0"="no")) +
  labs(title="Suggestive comment",
       color="Classified\nas relevant",
       x="Political ideology composite (more conservative -->)")
plot(pred_ideo) +
  labs(title="Ideology of commenter",
       x="Political ideology composite (more conservative -->)")
plot(pred_like) +
  labs(title="Number of likes of comments",
       x="Political ideology composite (more conservative -->)")



sg = function(.m,.filename, .hide_controls=F){
  
  #add.lines = ifelse(.hide_controls, list(c("Control variables", rep("\\checkmark",10))), NULL)
  
  stargazer::stargazer(.m, digits = 2, omit.stat = c("F","rsq","ser"),
                       no.space = T, font.size = "small",
                       single.row = T, align = T,
                       report = "vc*",
                       se = NULL,
                       intercept.bottom = T, header = F,
                       dep.var.caption = "Dependent variable",
                       column.sep.width = "-11pt", 
                       #style = "asr",
                       out = paste0("Tables/",.filename)#,
                       #keep = .keep, 
                       #add.lines = add.lines
  )
  
}
sg(list(m0_tox,m1a_tox,m1b_tox,
   m2_tox),
   "tox.tex")
tab_model(m0_tox,m1a_tox,m1b_tox,m2_tox, file = "tox.docx")

2*sd(dt$PolIdComp)
range(dt$PolIdComp)

# intercept shift - what does that mean
# how much Facebook do I need to consume to become as toxic as conservatives
# moral sanction, political morality is strong
# not an echo chamber 
# political morals are rejected by conservative
# show descriptive statistics 
# placebo treatment, kitty
# are conservative numb in both direction
# multilevel depending on the number
# Dependent variable
# How do people have perceptions of 
# individual + context 
# individal: 
# context: Dont talk about politics with guests
# third: a certain type of person in particular context will react in a certain way, 
# age, exposure = liberals, is it just 
# if a college matters, then it means you have to learn
# woke = aware, how do you know it's authentic


# flip the script: do people react positive -> positive, negative -> negative
# topic shift, Takemoto 

##############
# PRODUCTIVITY
############## 

m0_prod = lmer(ProductiveNum01 ~ (1|ResponseId) + (1|target_id), data=dt)
icc(m0_prod,by_group = T)



# # ---- Productivity ----
# mlm_prod_t = lmer(ProductiveNum01 ~ (1|target_id), data=dt, REML = F)
# mlm_prod_r = lmer(ProductiveNum01 ~ (1|ResponseId), data=dt, REML = F)
# 
# icc(mlm_prod_t)
# icc(mlm_prod_r)
# 
# 
# vc_prod = VarCorr(mlm_prod) %>% 
#   as_tibble() %>% 
#   mutate(DepVar = "Productive", Demeaned = "No") %>% 
#   select(DepVar,Demeaned,Groups=grp,Std.Dev.=sdcor) 
# 
# # demeaned
# mlm_prod_TargetDmd = lmer(ProductiveNum01_TargetDmd ~ (1|target_id) + (1|ResponseId), data=dt)
# vc_prod_TargetDmd = VarCorr(mlm_prod_TargetDmd) %>% 
#   as_tibble() %>% 
#   mutate(DepVar = "Productive", Demeaned = "Yes") %>% 
#   select(DepVar,Demeaned,Groups=grp,Std.Dev.=sdcor) 
# 
# # ---- Toxicity ----
# mlm_tox_t = lmer(BToxicNum01 ~ (1|target_id), data=dt, REML = F)
# mlm_tox_r = lmer(BToxicNum01 ~ (1|ResponseId), data=dt, REML = F)
# 
# icc(mlm_tox_t)
# icc(mlm_tox_r)
# 
# mlm_tox
# vc_tox = VarCorr(mlm_tox) %>% 
#   as_tibble() %>% 
#   mutate(DepVar = "Toxic", Demeaned = "No") %>% 
#   select(DepVar,Demeaned,Groups=grp,Std.Dev.=sdcor)
# 
# # demeaned
# mlm_tox_TargetDmd = lmer(BToxicityNum01_TargetDmd ~ (1|target_id) + (1|ResponseId), data=dt)
# vc_tox_TargetDmd = VarCorr(mlm_tox_TargetDmd) %>% 
#   as_tibble() %>% 
#   mutate(DepVar = "Toxic", Demeaned = "Yes") %>% 
#   select(DepVar,Demeaned,Groups=grp,Std.Dev.=sdcor)
# 
# # ---- Summary of results ----
# tab = bind_rows(vc_tox,vc_tox_TargetDmd,vc_prod,vc_prod_TargetDmd) 
# saveRDS(tab,"fb_survey/figures/4_MultilevelVariance.RDS")
# 
# tab = readRDS("fb_survey/figures/4_MultilevelVariance.RDS")
# tab %>% 
#   kable(format = "html",digits = 2, caption = "Variance components from Y ~ (1|target_id) + (1|ResponseId)") %>%
#   kable_styling("striped") %>%
#   collapse_rows(1:2) %>% 
#   save_kable(file = "4_MultilevelVariance.png")
# 
