# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))
library(mediation)

# check variable distribution
lapply(dt[,DVs], mean)

dvs = c("ProductiveNum01","BToxicNum01")
dvs_nonDmd = paste0(dvs,"_NonDmd")

mediators = c("UnderstandBNum01","BMakesEffortNum01","BStaysOnTopicNum01","BAgreesNum01","BRespectful","BOpen",
              "BObjective","BEmotional","BSarcastic","BIntolerant","BHostile","BNoneabove") %>% 
  paste0(.,"_NonDmd")

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
for (dv in seq_along(dvs_nonDmd)) {
  for (m in seq_along(mediators)) {
    res = fixefmed(mediators[m],dvs_nonDmd[dv],dt)
    tbme = bind_rows(tbme,res)
  }
}



ggplot(tbme,aes(p,reorder(me,p))) +
  geom_col() +
  labs(x="Percent of total effect mediated",y="Mediator",
       title="Mediation analysis for political ideology") +
  facet_wrap(~dv)






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
mod = fixefmod("BAgreesNum01","BUseHateSpeechNum01",dt) 
mydf <- ggpredict(mod, terms = c("BAgreesNum01", "PolIdComp2Sd"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()

tbmo = tibble()
regtab = tibble()
mmo = list(rep(NA,length(mediators)))

for(dv in seq_along(dvs)){
  for (m in seq_along(mediators)) {
    l = fixefmod(.x=mediators[m], .dv=dvs[dv], .data=dt)
  
    
    regtab = tidy(l) %>% 
      mutate(
        Predictor = mediators[m],
        dv = dvs[dv],
        siglevel = case_when(
          p.value < 0.001                   ~"(p<0.001)",
          p.value < 0.01 & p.value >= 0.001 ~ "(p<0.01)",
          p.value < 0.05 & p.value >= 0.01  ~ "(p<0.05)",
          TRUE ~ paste0("(p=",round(p.value,3),")"))
        ) %>% 
      bind_rows(regtab,.)
    
    
    p = tidy(l) %>% 
      filter(str_detect(term,"PolIdComp2Sd[:]")) %>% 
      pull(p.value) 
  
    pred = l %>% 
      ggpredict(terms = c(mediators[m], "PolIdComp2Sd[-1, 0, 1]")) %>% 
      mutate(
        PoliticalIdeologyComposite = case_when(
          group == -1 ~ "Liberal",
          group ==  0 ~ "Moderate",
          group ==  1 ~ "Conservative"),
        Predictor = mediators[m],
        dv = dvs[dv],
        siglevel = case_when(
          p < 0.001 ~ "(p<0.001)",
          p < 0.01 & p >= 0.001 ~ "(p<0.01)",
          p < 0.05 & p >= 0.01  ~ "(p<0.05)",
          TRUE ~ paste0("(p=",round(p,3),")")),
        label = paste0(Predictor," ",siglevel)) 
    tbmo = bind_rows(tbmo,pred)
  }
}

# consistently significant effects across three dvs
ce = regtab %>% 
  filter(str_detect(term,"PolIdComp2Sd[:]")) %>% 
  group_by(Predictor) %>% 
  mutate(nsig = sum(p.value < 0.01)) %>% 
  filter(nsig >= length(dvs))
unique(ce$Predictor)

arrange(term,dv) %>% View()
regtab 

tbmo %>% 
  na.omit() %>% 
  filter(Predictor %in% unique(ce$Predictor)) %>% 
  ggplot(aes(y=predicted,x=x,xmin=conf.low,xmax=conf.high,color=PoliticalIdeologyComposite)) +
  geom_line() +
  facet_wrap(~ dv+label,scales = "free_y",ncol = length(dvs),dir = "v") + 
  theme(legend.position = "bottom") +
  scale_color_manual(values= c("Liberal"="darkblue","Moderate"="darkgreen","Conservative"="darkred")) +
  labs(x="Values of predictor variable",
       y="Values of dependent variable (Toxicity, Productive)", 
       caption = "Note: Labels denote the p-values of interaction effect with political ideology")
ggsave("Figures/14_moderation.pdf",width = 5,height = 8)



lower_ci <- function(mean, se, n, conf_level = 0.83){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.83){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

dt %>% 
  #mutate(BAgreesNum = factor(BAgrees, levels = 0:1, labels=c("Does not agree","Agrees"))) %>% 
  group_by(BAgreesNum,PartyId) %>% 
  summarise(avg = mean(BToxicNum01_NonDmd), 
            se = sd(BToxicNum01_NonDmd), 
            n = n()) %>% 
  mutate(lowerCI = lower_ci(avg,se,n),
         upperCI= upper_ci(avg,se,n)) %>% 
  ggplot(aes(BAgreesNum, avg,ymin=lowerCI,ymax=upperCI)) + 
  geom_pointrange() + 
  facet_wrap(~ PartyId)+
  geom_hline(yintercept = 0,linetype = "dashed")+
  labs(y = "Toxicity")


dt %>% 
  dplyr::select(all_of(dvs_nonDmd), PolIdComp,BAgreesNum) %>% 
  pivot_longer(cols = all_of(dvs_nonDmd)) %>% 
  #mutate(BAgreesNum = factor(BAgrees, levels = 0:1, labels=c("Does not agree","Agrees"))) %>% 
  group_by(BAgreesNum,PolIdComp,name) %>% 
  summarise(avg = mean(value), 
            se = sd(value), 
            n = n()) %>% 
  mutate(lowerCI = lower_ci(avg,se,n),
         upperCI= upper_ci(avg,se,n), 
         BAgreesNum = factor(BAgreesNum, levels=-2:2,
                             labels=c("Strongly Disagrees","Disagrees","Neither","Agrees","Strongly Agrees"))) %>% 
  ggplot(aes(PolIdComp, avg,color=factor(BAgreesNum))) + 
  geom_line() + 
  labs(y = "Dependent variable", x="Political Ideology Composite", 
       color = "Agreemeent with Commenter A") +
  facet_wrap(~ name) +
  scale_x_continuous(breaks = 1:7) +
  scale_color_brewer(palette = "RdYlGn")


# Agreement ---------------------------------------------------------------
dt %>% 
  dplyr::select(all_of(dvs_nonDmd), PartisanshipNum,BAgreesNum) %>% 
  pivot_longer(cols = c(all_of(dvs_nonDmd))) %>% 
  group_by(BAgreesNum,PartisanshipNum,name) %>% 
  summarise(avg = mean(value), 
            se = sd(value), 
            n = n()) %>% 
  mutate(PartisanshipNum = factor(PartisanshipNum, levels=1:7,
                                  labels=c(c("Strong Democrat","Weak Democrat","Leaning Democrat","Independent",
                                             "Leaning Republican", "Weak Republican", "Strong Republican")))) %>% 
  ggplot(aes(BAgreesNum, avg,color=factor(PartisanshipNum))) + 
  geom_line() + 
  labs(y = "Dependent variable", color="Partisanship", 
       x = "Agreemeent with Commenter A") +
  facet_wrap(~ name) +
  scale_y_continuous(limits = c(0,0.61))+
  coord_fixed(ratio= 12) +
  scale_x_continuous(breaks = -2:2,labels=c("Strongly\nDisagrees","Disagrees","Neither","Agrees","Strongly\nAgrees"))  +
  scale_color_brewer(palette = "RdYlBu",direction = -1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 7), 
        panel.spacing.x = unit(1,"lines"))
ggsave("Figures/14_agreement_partisanship.pdf",width=6,height=5)


dt %>% 
  filter(proimmigrant==1) %>% 
  dplyr::select(all_of(dvs_nonDmd), PartyId,BAgreesNum) %>% 
  pivot_longer(cols = c(all_of(dvs_nonDmd))) %>% 
  group_by(BAgreesNum,PartyId,name) %>% 
  summarise(avg = mean(value), 
            se = sd(value), 
            n = n()) %>% 
  ggplot(aes(BAgreesNum, avg,color=factor(PartyId))) + 
  geom_line() + 
  coord_fixed(ratio= 12) +
  labs(y = "Dependent variable", color="Political Ideology", 
       x = "Agreemeent with Commenter A") +
  facet_wrap(~ name) +
  scale_y_continuous(limits = c(0,0.61))+
  scale_x_continuous(breaks = -2:2,labels=c("Strongly\nDisagrees","Disagrees","Neither","Agrees","Strongly\nAgrees"))  +
  scale_color_brewer(palette = "RdYlBu",direction = -1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 7), 
        panel.spacing.x = unit(1,"lines"))
ggsave("Figures/14_agreement_polid.pdf",width=6,height=5)





