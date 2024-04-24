# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
fname = "dt_survey_0625_with_toxicity_clean_index.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))
library(lme4)
library(kableExtra)
Covs
fo("ProductiveNum01",Covs)
# ---- Productivity ----
mlm_prod = lmer(ProductiveNum01 ~ (1|target_id)+PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                  MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                  Income2Sd + Race + target_id + Order, data=dt)
mlm_tox = lmer(BToxicityNum01 ~ (1|target_id)+PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                  MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                  Income2Sd + Race + target_id + Order, data=dt)
mlm_app = lmer(Appropriateness ~ (1|target_id)+PolIdComp2Sd + Age2Sd + Gender + EducationNum2Sd + 
                  MaritalStatus + Religion + SexualOrientation + HhSize + Region + 
                  Income2Sd + Race + target_id + Order, data=dt)

coef(mlm_prod)$target_id$PolIdComp2Sd
dt$BToxicityNum01

library(lmerTest)
TestProd = coef(summary(as(mlm_prod,"merModLmerTest")))
TestApp = coef(summary(as(mlm_app,"merModLmerTest")))
TestTox = coef(summary(as(mlm_tox,"merModLmerTest")))

saveRDS(mlm_tox, "fb_survey/figures/9_MLM_BToxicityNum01.RDS")
#saveRDS(mlm_prod, "fb_survey/figures/9_MLM_ProductiveNum01.RDS")
#saveRDS(mlm_app, "fb_survey/figures/9_MLM_Appropriateness.RDS")
vc_prod = VarCorr(mlm_prod) %>% 
  as_tibble() %>% 
  mutate(DepVar = "Productive", Demeaned = "No") %>% 
  select(DepVar,Demeaned,Groups=grp,Std.Dev.=sdcor) 
