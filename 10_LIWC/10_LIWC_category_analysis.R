source(list.files(pattern="0_Setup.R", recursive=T))

LiwcCats = c("tone_pos","tone_neg","emo_pos","emo_neg","swear",
             "conflict","prosocial","polite","moral","comm",
             "cogproc", 
             "politic", "ethnicity", "tech",               # culture 
             "leisure", "home", "work", "money", "relig",  # lifestyle  
             "substances","sexual", "food","death",        # physical
             "male","female",
             "shehe","they","you","i","we","Emoji",
             #"affiliation",
             "power","achieve"
)





# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath) %>% 
  inner_join(liwc,by = join_by(post_id, post_user_id, context_id, context_user_id, target_id, target_user_id)) %>% 
  mutate(across(all_of(LiwcCats), .fns = ~ (.-mean(.,na.rm=T)) / sd(.,na.rm=T)))
                #.fns = ~ (log(1+.) - mean(log(1+.),na.rm=T)) / sd(log(1+.),na.rm=T)))


LiwcCats = colnames(liwc)[- str_which(colnames(liwc),"_id")]
CovsLiwc = c(LiwcCats, CovsLean)
# ----------------- Main effects ----------------------------------------------
# Toxicity 
m_tox = lm(fo("BToxicNum01",CovsLiwc), dt)
m_tox_summ = tidy(coeftest(m_tox, vcov. = vcovHC(m_tox, type = "HC3", cluster = dt$ResponseId)), conf.int=T)

p_ToxLiwc = m_tox_summ %>% 
  filter(term %in% LiwcCats) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,reorder(term,estimate)))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLiwc,collapse=", ")),width = 150),
       subtitle="Linear regression of Toxicity on LIWC word count model with robust SEs")
p_ToxLiwc
ggsave("fb_survey/figures/10_ToxLiwc.png",p_ToxLiwc, width = 6, height=6)


# Productivity 
m_prod = lm(fo("ProductiveNum01",CovsLiwc), dt)
m_prod_summ = tidy(coeftest(m_prod, vcov. = vcovHC(m_prod, type = "HC3",cluster = dt$ResponseId)), conf.int=T)

p_ProdLiwc = m_prod_summ %>% 
  filter(term %in% LiwcCats) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,reorder(term,estimate)))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLiwc,collapse=", ")),width = 150),
       subtitle="Linear regression of Productive on LIWC word count model with robust SEs clustered at respondent")
ggsave("fb_survey/figures/10_ProdLiwc.png",p_ProdLiwc, width = 6, height=6)


# ----------------- Interaction effects ----------------------------------------------
CovsLiwcInt = c(paste0("PolIdComp2Sd*", LiwcCats), CovsLean)

m_tox_int = lm(fo("BToxicNum01",CovsLiwcInt), dt)
m_tox_int_summ = tidy(coeftest(m_tox_int, vcov. = vcovHC(m_tox_int, type = "HC3")), conf.int=T)


m_tox_int_summ %>% 
  filter(term != "(Intercept)", p.value <0.1,
         str_detect(term,"[:]")) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,reorder(term,estimate)))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLabs,collapse=", ")),width = 180),
       subtitle="Linear regression of Toxicity on LIWC word count model with robust SEs")

library(ggeffects)


PlotToxInt = function(.liwc_category, .title){
  
  CovsLiwcInt = c(paste0("PolIdComp2Sd*", .liwc_category), CovsLean, LiwcCats)
  m_tox_int = lm(fo("BToxicNum01",CovsLiwcInt), dt)
  
  ccolors = c("-1" = "blue", "0" = "darkgreen", "1" = "red")
  clabels = c("-1" = "Liberal","0"="Moderate","1"="Conservative")
  
  pval = function(p){
    if(p>= 0.001) txt = paste0("p=",substring(round(p,3),2))
    if(p < 0.001) txt = "p<.001"
    return(txt)
  }
  test = tidy(coeftest(m_tox_int, vcov. = vcovHC(m_tox_int, type = "HC3",cluster = dt$ResponseId)),conf.int = T)
  pvalue = filter(test,str_detect(term, paste0("[:]",.liwc_category))) %>% pull(p.value) %>% pval() 
  
  liwc = paste0(.liwc_category,"[0,2,4,6,8,10]")
  p = ggpredict(m_tox_int, terms = c(liwc,"PolIdComp2Sd[-1,0,1]")) %>% 
    ggplot(aes(x, predicted, colour = group)) + geom_line()+ 
    scale_color_manual(values = ccolors, labels = clabels) +
    labs(color="Political ideology",x="Dictionary count",y="Predicted value",
         title=.title, subtitle = paste0("Interaction between political ideology and dictionary count (",pvalue,")"),
         caption=str_wrap(paste0("Covariates: ",paste(CovsLiwcInt,collapse=", ")),width = 100))+
    scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
    scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,by=0.2))
  fname = paste0("fb_survey/figures/10_",str_replace_all(.title," +","_"),".png") 
  ggsave(fname,p, width = 6.5, height=5)
  return(p)
}
# republicans consider the following more offensive
PlotToxInt("sexual", "Toxicity on Sexual")
PlotToxInt("substances", "Toxicity on Substances")

# liberals consider the following more offensive
PlotToxInt("politic", "Toxicity on Politics")
PlotToxInt("relig", "Toxicity on Religion")
PlotToxInt("tone_neg", "Toxicity on Negative Tone") #+scale_y_continuous(limits = c(0,1.5))
PlotToxInt("conflict", "Toxicity on Conflict")
PlotToxInt("leisure", "Toxicity on Leisure")
PlotToxInt("power", "Toxicity on Power")
PlotToxInt("i", "Toxicity on Pronoun I")

# 
PlotToxInt("swear", "Toxicity on Swear") +scale_y_continuous(limits = c(0,1.3))
PlotToxInt("emo_neg", "Toxicity on Negative Emotion")
PlotToxInt("polite", "Toxicity on Polite")
PlotToxInt("prosocial", "Toxicity on Prosocial")
PlotToxInt( "ethnicity", "Toxicity on Ethnicity")






# Productivity
m_prod_int = lm(fo("ProductiveNum01",CovsLiwcInt), dt)
m_prod_int_summ = tidy(coeftest(m_prod_int, vcov. = vcovHC(m_prod_int, type = "HC3")), conf.int=T)


m_prod_int_summ %>% 
  filter(term != "(Intercept)", p.value <0.1,
         str_detect(term,"[:]")) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,reorder(term,estimate)))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Dependent variable",
       caption=str_wrap(paste0("Covariates: ",paste(CovsLabs,collapse=", ")),width = 180),
       subtitle="Linear regression of Toxicity on LIWC word count model with robust SEs")



PlotProdInt = function(.liwc_category, .title){
  
  CovsLiwcInt = c(paste0("PolIdComp2Sd*", .liwc_category), CovsLean, LiwcCats)
  m_prod_int = lm(fo("ProductiveNum01",CovsLiwcInt), dt)
  
  ccolors = c("-1" = "blue", "0" = "darkgreen", "1" = "red")
  clabels = c("-1" = "Liberal","0"="Moderate","1"="Conservative")
  
  pval = function(p){
    if(p>= 0.001) txt = paste0("p=",substring(round(p,3),2))
    if(p < 0.001) txt = "p<.001"
    return(txt)
  }
  test = tidy(coeftest(m_prod_int, vcov. = vcovHC(m_prod_int, type = "HC3",cluster = dt$ResponseId)),conf.int = T)
  pvalue = filter(test,str_detect(term, paste0("[:]",.liwc_category))) %>% pull(p.value) %>% pval() 
  
  liwc = paste0(.liwc_category,"[0,2,4,6,8,10]")
  p = ggpredict(m_prod_int, terms = c(liwc,"PolIdComp2Sd[-1,0,1]")) %>% 
    ggplot(aes(x, predicted, colour = group)) + geom_line()+ 
    scale_color_manual(values = ccolors, labels = clabels) +
    labs(color="Political ideology",x="Dictionary count",y="Predicted value",
         title=.title, subtitle = paste0("Interaction between political ideology and dictionary count (",pvalue,")"),
         caption=str_wrap(paste0("Covariates: ",paste(CovsLiwcInt,collapse=", ")),width = 100))+
    scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=0.2))
  fname = paste0("fb_survey/figures/10_",str_replace_all(.title," +","_"),".png") 
  ggsave(fname,p, width = 6.5, height=5)
  return(p)
}
PlotProdInt("swear", "Productive on Swear") 
PlotProdInt("tone_neg", "Productive on Negative Tone")
PlotProdInt("leisure", "Productive on Leisure")
PlotProdInt("cogproc", "Productive on Cognitive Processes")
PlotProdInt("food", "Productive on Food")

PlotProdInt("emo_neg", "Productive on Negative Emotions")


