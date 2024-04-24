# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
fname = "dt_survey_0625_with_toxicity_clean_index.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))
library(mediation)

# check variable distribution
lapply(dt[,DVs], mean)



lm(BUseHateSpeechNum01~PolIdComp2Sd,dt) %>% summary()
lm(BToxicNum01~PolIdComp2Sd+Age2Sd,dt) %>% summary()

.mediator = "UnderstandBNum01"
.dv = "BToxicNum01"
mediators = c("UnderstandBNum01","BMakesEffortNum01","BStaysOnTopicNum01","BAgreesNum01","BRespectful","BOpen",
              "BObjective","BEmotional","BSarcastic","BIntolerant","BHostile","BNoneabove","BUseHateSpeechNum01")

fixefmed = function(.mediator,.dv,.data){
  dd = na.omit(.data[,c(.dv,.mediator,Covs)])
  
  # total effect
  mt_forml = fo(.dv,Covs[-which(Covs=="target_id")])
  mt = feols(mt_forml,fixef="target_id",vcov="HC1",data = dd)
  
  # mediation model
  mm_forml = fo(.mediator,Covs[-which(Covs=="target_id")])
  mm = feols(mm_forml,fixef="target_id",vcov="HC1",data = dd)
  
  # direct effect model
  my_forml = paste0(paste0(Covs[-which(Covs=="target_id")],collapse = "+"),"+",.mediator)
  my = feols(fo(.dv,my_forml),fixef="target_id",vcov="HC1",data = dd)
  
  # get effects
  a = coef(mm)["PolIdComp2Sd"]
  b = coef(my)[.mediator]
  te = coef(mt)["PolIdComp2Sd"]
  de = coef(my)["PolIdComp2Sd"]
  ide = coef(mt)["PolIdComp2Sd"] - coef(my)["PolIdComp2Sd"] 
  p = ide / te
  res = data.frame(te,de,p,ide,a,b,x="PolIdComp2Sd",me=.mediator,dv=.dv)
  return(res)
}

tbme = tibble()
for (m in seq_along(mediators)) {
  res = fixefmed(mediators[m],"BToxicNum01",dt)
  tbme = bind_rows(tbme,res)
}
ggplot(tbme,aes(p,reorder(me,p))) +
  geom_col() +
  labs(x="Percent of total effect mediated",y="Mediator",
       title="Mediation analysis for political ideology on perceived toxicity")






fixefmod = function(.x,.dv,.data){
  dd = na.omit(.data[,c(.dv,"PolIdComp2Sd",.x,Covs)])
  
  interac = paste0("PolIdComp2Sd*",.x)
  
  rhs = c(Covs[-which(Covs=="target_id")], interac)
  # total effect
  m_forml = fo(.dv,rhs)
  m = feols(m_forml,fixef="target_id",vcov="HC1",data = dd)
  
  res = tidy(m,conf.int = T) %>% 
    mutate(x = .x,dv=.dv,mod = "PolIdComp2Sd",
           interac=str_replace(interac,"[*]",":"))
  
  return(m)
}
library(ggeffects)
m = fixefmod("BAgreesNum01","BToxicNum01",dt) 
mydf <- ggpredict(m, terms = c("BAgreesNum01", "PolIdComp2Sd"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()

tbmo = tibble()
mmo = list(rep(NA,length(mediators)))
for (m in seq_along(mediators)) {
  l = fixefmod(mediators[m],"BToxicNum01",dt)
  res = l$res
  m = l$m
  mmo[[m]] = m
  tbmo = bind_rows(tbmo,res)
}

mm
tbmo %>% 
  filter(term == interac) %>% 
  ggplot(aes(y=reorder(x,-estimate),x=estimate,xmin=conf.low,xmax=conf.high)) +
  geom_vline(xintercept=0,linetype="dashed",alpha=0.5) +
  geom_point() +
  geom_errorbar(width=0.3) 
  labs(x="Percent of total effect mediated",y="Mediator",
       title="Mediation analysis for political ideology on perceived toxicity")

sd(dt$PolIdComp2Sd)-1
sd(dt$PolIdComp2Sd)+1

