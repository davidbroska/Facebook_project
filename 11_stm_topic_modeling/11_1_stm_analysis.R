source(list.files(pattern="0_Setup.R", recursive=T))
dt = read_csv("fb_survey/11_topic_modeling/dt_survey_0625_with_toxicity_clean_topics.csv", 
              col_types = c("target_id"="character", "ResponseId"="character")) %>% 
  mutate(topics = factor(topics, levels = c("Identity","GunControl","ProImmigration",
                                            "PersonalExperience")))

# variables of interest 
IntCovs = c(Covs[-which(Covs=="target_id")], "PolIdComp2Sd*topics")
IntDVs = c("ProductiveNum01", "BToxicNum01")
m_tox_int = lm(fo("BToxicNum01",IntCovs),dt) 

#m_tox_int = feols(fo("BToxicNum01",IntCovs),fixef="ResponseId",vcov="HC1",data = dt)


# target_id, post_id,  fixed effects produce collinearity with topics
# ResponseId fixed effects produce collinearity with individual covariates

library(ggeffects)

# toxicity and political ideology * topic
p_tox_topic_polid = ggpredict(m_tox_int, terms = c("PolIdComp2Sd[-1,1]","topics")) %>% 
  ggplot(aes(x, predicted, colour = group)) + 
  geom_line() +
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

p_prod_topic_polid = ggpredict(m_prod_int, terms = c("PolIdComp2Sd[-1,1]","topics"), 
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

# only significant interaction is with immigration
tidy(coeftest(m_prod_int, vcov.=vcov(m_prod_int,type="HC3", cluster=dt$ResponseId))) %>% filter(str_detect(term, "[:]"))
summary(m_prod_int)


# immigrations and guns are the slope differences 
# slurs = profanity
# conservatives are less judgemental
