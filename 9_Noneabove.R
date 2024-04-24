# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))

# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))

# Who selects none of the above? -----------------------------------------------
count(dt,BNoneabove) %>% mutate(N=sum(n),p=100*n/N)
p_Noneabove = summfe %>% 
  filter(stringr::str_detect(var,"BNoneabove")) %>% 
  ggplot(aes(x=estimate,xmin=conf.low,xmax=conf.high,var,y=reorder(term,estimate)))  +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_pointrange(size=0.2) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x="Coefficient estimate with 95% CI",
       y="Predictor variable",
       subtitle='Regression on answering "None of the above"\nLinear probability model with robust SEs and fixed-effects for Fb posts')
p_Noneabove
ggsave("fb_survey/figures/9_BNoneabove.png",p_Noneabove,width = 6.25, height=5.5)



library(quanteda)
library("quanteda.textmodels")
library("quanteda.textstats")
library(quanteda.textplots)
co = dt %>% 
  group_by(target_id) %>% 
  mutate(nNoneabove = sum(BNoneabove)) %>% 
  ungroup()

summary(co$nNoneabove)
co$AboveAvgNoneabove = ifelse(co$nNoneabove > mean(co$nNoneabove), "More likely none above" ,"Less likely none above")
co = distinct(co, target,target_id,AboveAvgNoneabove) %>% 
  corpus(docid_field = "target_id",text_field = "target")

dfmat = co %>% 
  tokens() %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_wordstem() %>% 
  dfm() %>% 
  dfm_group(groups = AboveAvgNoneabove)

dfm_sort(dfmat)
textplot_wordcloud(dfmat, comparison = T,color = c("darkred","blue"))

# comments rated as noneabove are shorter on average
rowSums(dfmat)


tstat_keyness = textstat_keyness(dfmat,target = dfmat$AboveAvgNoneabove == "More likely none above")
textplot_keyness( tstat_keyness, n = 35,labelsize = 2.5) +
  theme(legend.position = "bottom")



