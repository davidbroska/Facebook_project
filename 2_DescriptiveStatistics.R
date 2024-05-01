# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))

# Descriptive statistics #######################################################


# confirm that ID is unique
dt %>% 
  count(ID) %>% 
  filter(n > 1)

# number of respondents
length(unique(dt$ResponseId))

# number of distinct posts 
length(unique(dt$target_id))

# number of evaluations per post
dt %>% 
  count(target_id) %>% 
  summarise(min=min(n),median=median(n),mean=mean(n),max=max(n))

corrplot::corrplot(round(cor(dt[DVs]),2))

# the sample is comprised of more liberal than conservative respondents
dt %>% 
  distinct(ResponseId,PolId) %>% 
  group_by(PolId) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = 100*n/sum(n))

# we have more rating from democrats than from republicans per target_id
dt %>% 
  count(target_id, Partisanship) %>% 
  group_by(Partisanship) %>% 
  summarise(mean(n))

# because of this imbalance we need to compute the average rating for each post per party_id
p_avgp_Partisanship = dt %>% 
  select(Partisanship,target_id, all_of(DVs)) %>% 
  gather(var,val,-Partisanship,-target_id) %>% 
  group_by(var,Partisanship,target_id) %>% 
  summarise(perc=100*mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,Partisanship) %>% 
  summarise(avg = mean(perc)) %>% # avg percent agreeing per variable
  ggplot(aes(Partisanship,avg)) + 
  geom_col() + 
  facet_wrap(~ var,nrow=3) +
  theme(axis.text.x = element_text(angle=90),
        plot.caption = element_text(hjust = 0)) +
  labs(x="Partisan identity",y="Average % agreeing",
       title="Average % of partisans agreeing per comment",
       caption="Note: We take the average agreement per comment because we have more ratings from liberals than from moderates and conservatives.")
p_avgp_Partisanship
ggsave("fb_survey/figures/2_AvgPercAgreeing_Partisanship.png",p_avgp_Partisanship,width = PlotWidth, height=PlotHeight)

p_avgp_ideo = dt %>% 
  select(PolId,target_id, all_of(DVs)) %>% 
  gather(var,val,-PolId,-target_id) %>% 
  group_by(var,PolId,target_id) %>% 
  summarise(perc=100*mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,PolId) %>% 
  summarise(avg = mean(perc)) %>% # avg percent agreeing per variable
  ggplot(aes(PolId,avg)) + 
  geom_col() + 
  facet_wrap(~ var,nrow=3) +
  theme(axis.text.x = element_text(angle=90)) + 
  labs(x="Political ideology",y="Average % agreeing",
       title="Average % agreeing per comment for each level of political ideology",
       caption="Note: We take the average agreement per comment because we have more ratings from liberals than from moderates and conservatives.")
p_avgp_ideo
ggsave("fb_survey/figures/2_AvgPercAgreeing_Ideo.png",p_avgp_ideo,width = PlotWidth, height=PlotHeight)






# ------------------------------------------------------------------------------
# DEPENDENT VARIABLES ON NUMERIC SCALE
p_avg_nobinary_Partisanship = dt %>%
  select(Partisanship,target_id,ToxicityNum,ProductiveNum) %>%
  gather(var,val,-Partisanship,-target_id) %>%
  group_by(var,Partisanship,target_id) %>%
  summarise(avg=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,Partisanship) %>%
  summarise(avgavg = mean(avg)) %>% # avg percent agreeing per variable
  mutate(var=factor(var,levels=c("ToxicityNum","ProductiveNum"),
                    labels=c("B is toxic","Conversation was productive"))) %>%
  ggplot(aes(Partisanship,avgavg)) +
  geom_col() +
  facet_wrap(~ var,nrow=1) +
  theme(axis.text.x = element_text(angle=90),
        plot.caption = element_text(hjust = 0)) +
  labs(x="Partisan identity",y="Average rating",
       title="Average rating of productivity and toxicity on Facebook by partisanship",
       caption="Note: Respondents' rating of the productivity of the conversation was recoded as follows: -1='No', 0='Not sure', 1='Yes'.
Respondents' rating of the toxicity of a comment was recoded as follows: -1='Not toxic', 0='Maybe,not sure', 1='Toxic', 2='Very toxic'.
The graph shows the average rating per comment per partisan identity.")
p_avg_nobinary_Partisanship
ggsave("fb_survey/figures/2_AvgAgreement_NoBinary_Partisanship.png",p_avg_nobinary_Partisanship,width = PlotWidth, height=PlotHeight)

p_avg_nobinary_ideo = dt %>%
  select(PolId,target_id,ToxicityNum,ProductiveNum) %>%
  gather(var,val,-PolId,-target_id) %>%
  group_by(var,PolId,target_id) %>%
  summarise(avg=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,PolId) %>%
  summarise(avgavg = mean(avg)) %>% # avg percent agreeing per variable
  mutate(var=factor(var,levels=c("ToxicityNum","ProductiveNum"),
                    labels=c("B is toxic","Conversation was productive"))) %>%
  ggplot(aes(PolId,avgavg)) +
  geom_col() +
  facet_wrap(~ var,nrow=1) +
  theme(axis.text.x = element_text(angle=90),
        plot.caption = element_text(hjust = 0)) +
  labs(x="Political ideology",y="Average rating",
       title="Average rating of productivity and toxicity on Facebook by political ideology",
       caption="Note: Respondents' rating of the productivity of the conversation was recoded as follows: -1='No', 0='Not sure', 1='Yes'.
Respondents' rating of the toxicity of a comment was recoded as follows: -1='Not toxic', 0='Maybe,not sure', 1='Toxic', 2='Very toxic'.
The graph shows the average rating per comment per political ideology.")
p_avg_nobinary_ideo
ggsave("fb_survey/figures/2_AvgAgreement_NoBinary_Ideo.png",p_avg_nobinary_ideo,width = PlotWidth, height=PlotHeight)




# ------------------------------------------------------------------------------
# BINARY DEPENDENT VARIABLES
p_avg_binary_Partisanship = dt %>%
  select(Partisanship,target_id,BToxicityNum01,ProductiveNum01) %>%
  gather(var,val,-Partisanship,-target_id) %>%
  group_by(var,Partisanship,target_id) %>%
  summarise(avg=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,Partisanship) %>%
  summarise(avgavg = mean(avg)) %>% # avg percent agreeing per variable
  mutate(var=factor(var,levels=c("BToxicityNum01","ProductiveNum01"),
                    labels=c("B is toxic","Conversation was productive"))) %>%
  ggplot(aes(Partisanship,avgavg)) +
  geom_col() +
  facet_wrap(~ var,nrow=1) +
  scale_y_continuous(limits = c(0,0.5)) +
  theme(axis.text.x = element_text(angle=90),
        plot.caption = element_text(hjust = 0)) +
  labs(x="Partisan identity",y="Average rating",
       title="Average rating of productivity and toxicity on Facebook by partisanship",
       caption="Note: Respondents' rating of the productivity of the conversation was recoded as follows: -1='No', 0='Not sure', 1='Yes'.
Respondents' rating of the toxicity of a comment was recoded as follows: -1='Not toxic', 0='Maybe,not sure', 1='Toxic', 2='Very toxic'.
Both dependent variables were shifted and scaled so that their range is 0 and 1.
The graph shows the average rating per comment per partisan identity.")
p_avg_binary_Partisanship
ggsave("fb_survey/figures/2_AvgAgreement_NotBinary_Partisanship.png",p_avg_binary_Partisanship,width = PlotWidth, height=PlotHeight)

p_avg_binary_ideo = dt %>%
  select(PolId,target_id,BToxicityNum01,ProductiveNum01) %>%
  gather(var,val,-PolId,-target_id) %>%
  group_by(var,PolId,target_id) %>%
  summarise(avg=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,PolId) %>%
  summarise(avgavg = mean(avg)) %>% # avg percent agreeing per variable
  mutate(var=factor(var,levels=c("BToxicityNum01","ProductiveNum01"),
                    labels=c("B is toxic","Conversation was productive"))) %>%
  ggplot(aes(PolId,avgavg)) +
  geom_col() +
  facet_wrap(~ var,nrow=1) +
  scale_y_continuous(limits = c(0,0.5)) +
  theme(axis.text.x = element_text(angle=90),
        plot.caption = element_text(hjust = 0)) +
  labs(x="Political ideology",y="Average rating",
       title="Average rating of productivity and toxicity on Facebook by political ideology",
       caption="Note: Respondents' rating of the productivity of the conversation was recoded as follows: -1='No', 0='Not sure', 1='Yes'.
Respondents' rating of the toxicity of a comment was recoded as follows: -1='Not toxic', 0='Maybe,not sure', 1='Toxic', 2='Very toxic'.
The graph shows the average rating per comment per political ideology.")
p_avg_binary_ideo
ggsave("fb_survey/figures/2_AvgAgreement_Binary_Ideo.png",p_avg_binary_ideo,width = PlotWidth, height=PlotHeight)


