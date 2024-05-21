# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))

# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))

# fixed-effects regression 
summfe = dt %>%
  select(all_of(DVs),PolIdComp,all_of(Covs)) %>%
  gather(var,val,-PolIdComp,-all_of(Covs)) %>%
  group_by(var) %>%
  do(tidy(feols(fo("val",Covs[-which(Covs=="target_id")]),fixef="target_id",vcov="HC1",data = .),conf.int=T)) 


# Political ideology -----------------------------------------------------------

# association of political ideology with toxicity in terms of percentage points
filter(summfe,var=="BToxicityNum01",term=="PolIdComp2Sd") %>% pull(estimate) %>% {100*.} %>% round(1)
filter(summfe,var=="ProductiveNum01",term=="PolIdComp2Sd") %>% pull(estimate) %>% {100*.} %>% round(1)
sd(dt$PolIdComp2Sd)
# a 2 sd increase in political ideology corresponds to 60% of the total scale
pscale = 2*sd(dt$PolIdComp2Sd) / (max(dt$PolIdComp2Sd) - min(dt$PolIdComp2Sd))
7 * pscale

# effects plot
p_PolIdReg = summfe %>% 
  filter(stringr::str_detect(term,"PolIdComp2Sd")) %>% 
  mutate(var = factor(var,levels = DVs,labels = DVsLabs)) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,reorder(var,-estimate)))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_x_continuous(breaks = seq(-0.06,0.06,by = 0.02)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLabs,collapse=", ")),width = 110),
       subtitle="Regression on political ideology (standardized)\nLinear probability model with robust SEs and fixed-effects for conversations")
p_PolIdReg
ggsave("Figures/3_PolIdReg2Sd.png",p_PolIdReg,width = 7, height=5.5)

dvorder = summfe %>% 
  filter(stringr::str_detect(term,"PolIdComp2Sd")) %>% 
  arrange(-estimate) %>% 
  pull(var)


# Education --------------------------------------------------------------------
# association of political ideology with toxicity in terms of percentage points
filter(summfe,var=="BToxicityNum01",term=="EducationNum2Sd") %>% pull(estimate) %>% {100*.} %>% round(2)
filter(summfe,var=="ProductiveNum01",term=="EducationNum2Sd") %>% pull(estimate) %>% {100*.} %>% round(2)

p_Education = summfe %>% 
  filter(stringr::str_detect(term,"EducationNum2Sd")) %>% 
  mutate(var = factor(var,levels = dvorder)) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,var))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLabs,collapse=", ")),width = 120),
       subtitle="Regression on education (standardized)\nLinear probability model with robust SEs and fixed-effects for Fb posts")
p_Education
ggsave("fb_survey/figures/3_EducationRegSd.png",p_Education,width = 6.25, height=5.5)



# Age --------------------------------------------------------------------
# association of political ideology with toxicity in terms of percentage points
filter(summfe,var=="BToxicityNum01",term=="Age2Sd") %>% pull(estimate) %>% {100*.} %>% round(2)
filter(summfe,var=="ProductiveNum01",term=="Age2Sd") %>% pull(estimate) %>% {100*.} %>% round(2)

p_Age = summfe %>% 
  filter(stringr::str_detect(term,"Age2Sd")) %>% 
  mutate(var = factor(var,levels = dvorder)) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,var))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLabs,collapse=", ")),width = 120),
       subtitle="Regression on age (standardized)\nLinear probability model with robust SEs and fixed-effects for Fb posts")
p_Age
ggsave("fb_survey/figures/3_AgeRegSd.png",p_Age,width = 6.25, height=5.5)


# Income --------------------------------------------------------------------
# association of political ideology with toxicity in terms of percentage points
filter(summfe,var=="BToxicityNum01",term=="Income2Sd") %>% pull(estimate) %>% {100*.} %>% round(2)
filter(summfe,var=="ProductiveNum01",term=="Income2Sd") %>% pull(estimate) %>% {100*.} %>% round(2)

p_Order = summfe %>% 
  filter(stringr::str_detect(term,"Order")) %>% 
  mutate(var = factor(var,levels = dvorder)) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,var))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLabs,collapse=", ")),width = 120),
       subtitle="Regression on order (standardized)\nLinear probability model with robust SEs and fixed-effects for Fb posts")
p_Order
ggsave("fb_survey/figures/3_Order.png",p_Order,width = 6.25, height=5.5)

# Gender --------------------------------------------------------------------
# association of political ideology with toxicity in terms of percentage points
filter(summfe,var=="BToxicityNum01",term=="GenderWoman") %>% pull(estimate) %>% {100*.} %>% round(2)
filter(summfe,var=="ProductiveNum01",term=="GenderWoman") %>% pull(estimate) %>% {100*.} %>% round(2)

p_GenderWoman = summfe %>% 
  filter(stringr::str_detect(term,"GenderWoman")) %>% 
  mutate(var = factor(var,levels = dvorder)) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,var))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLabs,collapse=", ")),width = 120),
       subtitle="Regression on gender woman (vs man)\nLinear probability model with robust SEs and fixed-effects for Fb posts")
p_GenderWoman
ggsave("fb_survey/figures/3_GenderWoman.png",p_GenderWoman,width = 6.25, height=5.5)




# Interactions


# Political ideology * order --------------------------------------------------------------------
# Does exposure everyone equally?
rhsside = c(Covs[-which(Covs=="target_id")], "Order*PolIdComp2Sd")
summfe = dt %>%
  select(all_of(DVs),PolIdComp,all_of(Covs)) %>%
  gather(var,val,-PolIdComp,-all_of(Covs)) %>%
  group_by(var) %>%
  do(tidy(feols(fo("val",rhsside),fixef="target_id",vcov="HC1",data = .),conf.int=T)) 
p_PolIdxOrder = summfe %>% 
  filter(term=="PolIdComp2Sd:Order") %>% 
  mutate(var = factor(var,levels = dvorder)) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,var))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLabs,collapse=", ")),width = 120),
       subtitle="Regression on age (standardized)\nLinear probability model with robust SEs and fixed-effects for Fb posts")
p_PolIdxOrder

rhsside = c(Covs[-which(Covs=="target_id")], "Age2Sd*Order")
summfe = dt %>%
  select(all_of(DVs),PolIdComp,all_of(Covs)) %>%
  gather(var,val,-PolIdComp,-all_of(Covs)) %>%
  group_by(var) %>%
  do(tidy(feols(fo("val",rhsside),fixef="target_id",vcov="HC1",data = .),conf.int=T)) 
p_AgexOrder = summfe %>% 
  filter(term=="Age2Sd:Order") %>% 
  mutate(var = factor(var,levels = dvorder)) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,var))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLabs,collapse=", ")),width = 120),
       subtitle="Regression on age (standardized)\nLinear probability model with robust SEs and fixed-effects for Fb posts")
p_AgexOrder
ggsave("fb_survey/figures/3_AgeRegSd.png",p_Age,width = 6.25, height=5.5)




# indiffence 
# ignorance is bliss
# libraralism is anger
# content is 
# why conservatism 
# is toxic a word that liberals use
# if democrats use 
# tim hallot (public sociology ideas), 
# elective affinity (correspondence) 
# cultural resonance
# control for content, mediated/confounded by sample 
# confounded tocxicty
# timmermans chris bail,
#m = lm(BToxicityNum01 ~ PolIdComp+target_id,dt)
#saveRDS(m,"fb_survey/RegOnPolId1Item.Rds")


# regression with standardized variables age, political ideology, and (less) education
# target decomposed in qualities
# regression with random effects 
# correlational class analysis


