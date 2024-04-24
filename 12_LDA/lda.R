source(list.files(pattern="0_Setup.R", recursive=T))
library(ggeffects)
#install.packages("arrow")
gpt_raw = readxl::read_xlsx("fb_survey/12_LDA/Topic_GPT_Human_on_posts.xlsx") 
colnames(gpt_raw)
gpt = gpt_raw %>% 
  select(topic_number = `Topic Number`, 
         topic_label = human_decision,
         topic_political = political_topic) %>% 
  mutate(topic_label = topic_label %>% str_remove_all('\\"'))

gpt1 = rename(gpt, max_1_topic_label=topic_label, max_1_topic_political=topic_political)
gpt2 = rename(gpt, max_2_topic_label=topic_label, max_2_topic_political=topic_political)
gpt3 = rename(gpt, max_3_topic_label=topic_label, max_3_topic_political=topic_political)
gpt4 = rename(gpt, max_4_topic_label=topic_label, max_4_topic_political=topic_political)
gpt5 = rename(gpt, max_5_topic_label=topic_label, max_5_topic_political=topic_political)

colnames(gpt)
colnames(topics)
topics = arrow::read_parquet("fb_survey/12_LDA/post_topic_assignments_40_topics_greater_20w.parquet") %>% 
  rename_all(~ str_replace(., "_label$","_number")) %>% 
  left_join(gpt1,by=c("max_1_topic_number"="topic_number")) %>% 
  left_join(gpt2,by=c("max_2_topic_number"="topic_number")) %>% 
  left_join(gpt3,by=c("max_3_topic_number"="topic_number")) %>% 
  left_join(gpt4,by=c("max_4_topic_number"="topic_number")) %>% 
  left_join(gpt5,by=c("max_5_topic_number"="topic_number")) %>% 
  mutate(post_id = paste0("P",post_id)) %>% 
  select(everything(),contains("max_1"),contains("max_2"),contains("max_3"),contains("max_4"),contains("max_5")) 
  

fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath) %>% 
  left_join(topics, by = "post_id")


# variables of interest 
IntCovs = c(Covs[-which(Covs=="target_id")], "PolIdComp2Sd*max_1_topic_label")
IntDVs = c("ProductiveNum01", "BToxicNum01")
m_tox_int = lm(fo("BToxicNum01",IntCovs),dt) 

# topics democrats find more toxic (p < 0.1)
tidy(m_tox_int) %>% 
  filter(str_detect(term,"[:]max_[0-9]_topic"), 
         estimate < 0, p.value < 0.1)

# topics conservatives find more toxic (note all p>0.1)
tidy(m_tox_int) %>% 
  filter(str_detect(term,"[:]max_[0-9]_topic"), 
         estimate > 0, p.value < 0.5)




# toxicity and political ideology * topic
p_tox_topic_polid = ggpredict(m_tox_int, terms = c("PolIdComp2Sd[-1,1]","max_1_topic_label")) %>% 
  ggplot(aes(x, predicted, colour = group)) + 
  geom_line() +
  theme_bw(base_size = 8)+
  #scale_color_manual(values = ccolors, labels = clabels) +
  labs(color="Topic", title = "Toxic", 
       subtitle = "Interaction of topic with political ideology",
       caption=str_wrap(paste0("Covariates: ",paste(IntCovs,collapse=", ")),width = 100),
       x="Political ideology (-1=Extremely liberal to 1=Extremely conservative)", 
       y="Toxicity of B (0=Not toxic to 1=Very toxic)")
p_tox_topic_polid
ggsave("fb_survey/figures/11_ToxPolTopicInteraction.png",p_tox_topic_polid,width = 6.5, height=5)

# only significant interaction is with immigration
tidy(coeftest(m_tox_int, vcov.=vcov(m_prod_int,type="HC3", cluster=dt$ResponseId))) %>% 
  filter(str_detect(term, "[:]"))


# productivity and political ideology * topic
m_prod_int = lm(fo("ProductiveNum01",IntCovs),data = dt)

p_prod_topic_polid = ggpredict(m_prod_int, terms = c("PolIdComp2Sd[-1,1]","max_1_topic_label"), 
                               vcov_type = "HC3") %>% 
  ggplot(aes(x, predicted, colour = group)) + 
  geom_line() +
  #scale_color_manual(values = ccolors, labels = clabels) +
  labs(color="Topic", title = "Productive",
       subtitle = "Interaction of topic with political ideology",
       caption=str_wrap(paste0("Covariates: ",paste(IntCovs,collapse=", ")),width = 100),
       x="Political ideology (-1=Extremely liberal to 1=Extremely conservative)", 
       y="Productive (0=Not productive to 1=Productive)")
p_prod_topic_polid
ggsave("fb_survey/figures/11_ProdPolTopicInteraction.png",p_prod_topic_polid,width = 6.5, height=5)


# there are no topics democrats find more productive (p < 0.5)
tidy(m_prod_int) %>% 
  filter(str_detect(term,"[:]max_[0-9]_topic"), 
         estimate < 0, p.value < 0.1)

# topics conservatives find more productive (note all p<0.1)
tidy(m_prod_int) %>% 
  filter(str_detect(term,"[:]max_[0-9]_topic"), 
         estimate > 0, p.value < 0.1)

# only significant interaction is with immigration
tidy(coeftest(m_prod_int, vcov.=vcov(m_prod_int,type="HC3", cluster=dt$ResponseId))) %>% filter(str_detect(term, "[:]"))
summary(m_prod_int)


# immigrations and guns are the slope differences 
# slurs = profanity
# conservatives are less judgemental




