# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
library(nnet)
library("plot3logit")
# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))

rca = readRDS("fb_survey/5_RelationalClassAnalysis/RCA_idsDmD.RDS")
count(rca, RCA)

dt = inner_join(dt, rca, by = "ID")

lm(BToxicityNum01 ~ Order+PolIdComp,dt) %>% summary()


p_densities = dt[,c(DVsDmd,"RCA")] %>% 
  gather(variable, value, - RCA) %>% 
  mutate(variable = str_remove(variable, "_TargetDmd")) %>% 
  ggplot(aes(x= value, linetype = factor(RCA), fill = factor(RCA))) +
  geom_density(alpha=0.4, linewidth = 0.2) +
  facet_wrap(~variable, scales = "free") +   
  #scale_linetype_manual( values = c("solid", "longdash", "dotted")) +
  #theme_classic(base_size = 20) +
  labs(x="",y="Density",fill="RCA",linetype="RCA") 
p_densities
ggsave("fb_survey/figures/5_RCA_Densities.jpg",p_densities, height = PlotHeight, width = PlotWidth, device="jpg")



############### REGRESSIONS #################
#############################################
# Multinomial logistic regression in R
#############################################

#### Table 2 - BASIC MULTINOMIAL MODELS
# Neutral logic as the reference class
dt$RCA <- relevel(as.factor(dt$RCA), ref = "2")
mn <-  multinom(fo("RCA",c(DVsDmd,CovsLean)), data = dt)
tmn = tidy(mn, conf.int = T)
p_mneffects = tmn %>% 
  filter(term != "(Intercept)", term %in% DVsDmd) %>%  
  ggplot(aes(estimate,xmin=conf.low, xmax=conf.high, y = reorder(term,estimate))) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red") +
  geom_pointrange(size = 0.1) + 
  facet_wrap(~ y.level) +
  theme_bw(base_size = 8) +
  labs(title="Multinomial regression of RCA cluster", 
       x = "Cofficient estimate in logits with 95% confidence intervals", 
       y = "Predictor")
p_mneffects
ggsave("fb_survey/figures/5_MultinomEffects.jpg",p_mneffects, height = PlotHeight, width = PlotWidth, device="jpg")

refpoint <- list(c(0.5, 0.5, 0.5))
depo <- list(
  list(delta = "BToxicityNum01_TargetDmd",label="Toxic"),
  list(delta = "BUseHateSpeech_TargetDmd",label="BUseHateSpeech"),
  list(delta = "ProductiveNum01_TargetDmd",label="Productive"),
  list(delta = "BMakesEffort_TargetDmd",label="BMakesEffort"),
  list(delta = "PolIdComp",label="Pol. Ideo."),
  list(delta = "Age",label="Age"),
  list(delta = "EducationNum",label="Education")
  )
field = plot3logit::field3logit(mn, p0 = refpoint,delta = depo)
p_ternary = gg3logit(field) +
  stat_field3logit(aes(color = label), size = .3) +
  labs(color = "Variable")
ggsave("fb_survey/figures/5_Ternary.jpg",p_ternary, height = PlotHeight, width = PlotWidth, device="jpg")


