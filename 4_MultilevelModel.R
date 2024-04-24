# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))

fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)

dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels), 
         # subtract out grand mean
         BToxicNum01 = BToxicNum01 - mean(BToxicNum01),
         ProductiveNum01 = ProductiveNum01 - mean(ProductiveNum01))

vars = c("BToxicNum01","ProductiveNum01","ResponseId","target_id","PolIdComp2Sd","Age2Sd","Gender","EducationNum2Sd",
         "MaritalStatus","Religion","SexualOrientation","HhSize","Region","Income2Sd","Race","Order",
         "anticonservative","america","christianity","suggestive","drugs","ideo_commenterB","target_likes_count")

# check for NAs
summarise(dt, across(all_of(vars), ~ sum(is.na(.)))) %>% 
  select_if(~ any(. > 0))

library(lme4)
library(kableExtra)
library(performance)
set.seed(04162024)

###########
# TOXICITY
########### 

# Null model -------------------------------------------------------------------
# 1. There is a correlation within individuals and within comments on perceived toxicity
m0_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id), data=dt, REML=F)
summary(m0_tox)
# ICC for comments
round(0.05406 / (0.05406+0.01594+0.07015),3)
# ICC for individuals
round(0.01594 / (0.05406+0.01594+0.07015),3)
# ICC for error
round(0.07015 / (0.05406+0.01594+0.07015),3)

# Q: How large are these variances, and the icc?
# 10% enough + substantive question

# Level 1 predictors --------------------------------------------------
# 2. Liberals perceive comments overall as more toxic than conservatives
m1a_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + PolIdComp2Sd, 
               data=dt, REML=F)
summary(m1a_tox)

# Likelihood ratio test is significant but the improvement in BIC is not much
anova(m0_tox, m1a_tox)
# "same" test as for coeff

# Q: The variance in random intercepts for individuals does not go down by much


m1b_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                 PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                 MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                 Income2Sd + Race + Order, 
               data=dt, REML=F)
summary(m1b_tox)

# 9% of 11% explained
(0.01594 - 0.01445) / 0.01594


# Likelihood ratio test is significant but the improvement in BIC is not much
anova(m0_tox, m1b_tox)
# Q: Again, the variance in random intercepts for individuals does not go down by much



# Level 2 predictors --------------------------------------------------------
# 3. The following topics are perceived as more toxic than others
m2_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                Income2Sd + Race + Order + 
                  anticonservative + america + christianity + suggestive + drugs +
                  ideo_commenterB+target_likes_count, 
              data=dt, REML=F)
summary(m2_tox)


# Q: The variance in random intercepts for comments goes down by 0.02

# Random slopes ----------------------------------------------------------------
# The relationship between perceived toxicity and political ideology is not the same for all comments
m3a_tox = lmer(BToxicNum01 ~ PolIdComp2Sd + (1|ResponseId) + 
                 (1+PolIdComp2Sd|target_id), data=dt, REML=F)
anova(m1a_tox, m3a_tox)
summary(m3a_tox)

# 95% of the slopes are between
-0.064491 - 1.96*0.08503 
-0.064491 + 1.96*0.08503
# for some comments, the association is zero



# Q: Is the conclusion that the relationship between perceived toxicity does not strongly vary political ideology
# Liberals and conservative perceive each post similarly


# do I run random slopes in the fully specified model?
m3b_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
       # individual level
       PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
       MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
       Income2Sd + Race + Order + 
        # comment level
        anticonservative + america + christianity + suggestive + drugs + 
        ideo_commenterB+target_likes_count + 
         # random slopes
          (1+PolIdComp2Sd|target_id), 
     data=dt, REML=F)
summary(m3b_tox)
anova(m2_tox, m3b_tox)
anova(m3a_tox, m3b_tox)

# Q: did not converge

# Cross-level interaction ------------------------------------------------------
m4_tox = lmer(BToxicNum01 ~ (1|ResponseId) + (1|target_id) + 
                PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                Income2Sd + Race + Order +
                   # (1+PolIdComp2Sd|target_id) +
                      (anticonservative*PolIdComp2Sd)+
                      (america*PolIdComp2Sd)+
                      (christianity*PolIdComp2Sd)+
                      (suggestive*PolIdComp2Sd)+
                      (drugs*PolIdComp2Sd)+
                      (ideo_commenterB*PolIdComp2Sd)+
                      (target_likes_count*PolIdComp2Sd),
              data=dt, REML=F)
summary(m4_tox)

# ICC for comments
round(0.04455 / (0.04455+0.01448+0.06877),3)
# ICC for individuals
round(0.01448 / (0.04455+0.01448+0.06877),3)
# ICC for error
round(0.06877 / (0.04455+0.01448+0.06877),3)


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
