# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
library(kableExtra)
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels), 
         Toxicity=factor(Toxicity,levels=c("Not toxic","Maybe, not sure","Toxic","Very toxic")),
         Education = factor(Education, levels=c("High School or less","Some college","Postgraduate")))


# Descriptive statistics --------------------------------------------------

Dems = c("PolIdComp","PolIdComp2Sd","Man","Woman","OtherGender","EducationNum","Income2Sd",
         "Asian","Black","Hispanic","White","OtherRace",
         "Married", "Separated","Widowed",
         "MaritalNoAnswer","Divorced","NeverMarried", 
         "EvangelicalProtestant","MainlineProtestant","Mormon","Catholic","Jewish",
         "Muslim","NotReligious","ReligionNoAnswer","OtherReligion")

summ_covs = dt %>% 
  select(
    BToxicNum01,
    all_of(Dems), all_of(TopicsTab$topic), all_of(LiwcCats),
    # Other conversation-level predictors
    Order,IdeoCommenterB,TargetLikesCount) %>% 
  pivot_longer(cols = everything()) %>% 
  filter(!is.na(value)) %>% 
  group_by(Variable=name) %>% 
  summarise(Min=min(value),Q25=quantile(value,.25), 
            Mean=mean(value),Median=median(value),
            Q75=quantile(value,.75),Max=max(value)) %>% 
  mutate(Type = case_when(
    Variable %in% c("BToxicNum","BToxicNum01") ~ "Dependent Variable",
    Variable %in% Dems ~ "Demographics",
    Variable %in% TopicsTab$topic ~ "Topic",#,antirepcons,antidemlib"),
    Variable %in% LiwcCats ~ "LIWC",
    Variable %in% c("Order","IdeoCommenterB","TargetLikesCount") ~ "Other") %>% 
      factor(levels=c("Dependent Variable","Demographics","Topic","LIWC","Other")),
    Variable = factor(Variable, levels = c(DVs,Dems,TopicsTab$topic,LiwcCats,
                                           "Order","IdeoCommenterB","TargetLikesCount"))
    ) %>% 
  select(Type,everything()) %>% 
  arrange(Type,Variable)


summ_covs %>% 
  filter(Type %in% c("Dependent Variable","Demographics")) %>% 
  knitr::kable(format = "latex",digits = 2, label = "summ-tab1",booktabs=T,
               caption = "Summary statistics on characteristics of survey respondents") %>% 
  kableExtra::kable_styling(latex_options = "hold_position") %>% 
  kableExtra::collapse_rows(columns = 1) %>% 
  writeLines("Tables/summary_stats1.tex")

summ_covs %>% 
  filter(Type %in% c("Topic","Other")) %>% 
  knitr::kable(format = "latex",digits = 2,label = "summ-tab2",booktabs=T,
               caption = "Summary statistics on conversation characteristics (1)") %>% 
  kableExtra::kable_styling(latex_options = "hold_position") %>% 
  kableExtra::collapse_rows(columns = 1) %>% 
  writeLines("Tables/summary_stats2.tex")

summ_covs %>% 
  filter(Type %in% c("LIWC")) %>% 
  knitr::kable(format = "latex",digits = 2,label = "summ-tab3",booktabs=T,
               caption = "Summary statistics on conversation characteristics (2)") %>% 
  kableExtra::kable_styling(latex_options = "hold_position") %>% 
  kableExtra::collapse_rows(columns = 1) %>% 
  writeLines("Tables/summary_stats3.tex")

# confirm that ID is unique
dt %>% 
  count(ID) %>% 
  filter(n > 1)

# number of respondents
length(unique(dt$ResponseId))

# number of distinct posts 
length(unique(dt$target_id))

# respondent characteristics
anes_ed = tribble(
  ~ Education, ~ ANES,
  "High School or less", 20,
  "Some college", 60,
  "Postgraduate",20,
)
dt %>% 
  distinct(ResponseId,Education) %>% 
  count(Education) %>% 
  mutate(p = round(100*n/sum(n),1)) %>% 
  left_join(anes_ed,by="Education") %>% 
  kable()



dt %>% 
  distinct(ResponseId,Race) %>% 
  count(Race) %>% 
  mutate(p = round(100*n/sum(n),1)) %>% 
  kable()

anes_partyid = tribble(
  ~ PartyId, ~ ANES,
  "Democrat",36,
  "Independent",33,
  "Republican",31
)
dt %>% 
  distinct(ResponseId,PartyId) %>% 
  count(PartyId) %>% 
  mutate(p = round(100*n/sum(n),1)) %>% 
  left_join(anes_partyid,by="PartyId") %>% 
  kable() %>% 
  add_footnote(label = "Independents include leaners")

anes_age = tribble(
  ~ AgeCat, ~ ANES,
  "18-29", 20,
  "30-44", 28,
  "45-59", 26,
  "60+", 26,
)
dt %>% 
  distinct(ResponseId,AgeCat) %>% 
  count(AgeCat) %>% 
  mutate(p = round(100*n/sum(n),1)) %>% 
  left_join(anes_age,by="AgeCat") %>% 
  kable() 


dt %>% 
  count(Toxicity) %>% 
  mutate(p = round(100*n/sum(n),1)) %>% 
  ggplot(aes(Toxicity,p)) +
  geom_col() +
  labs(y="Percent of ratings")
  
  
pdf(file = "Figures/corrplot.pdf")
corrplot(round(cor(dt[c(DVs,"PartisanshipNum","PolIdNum","PolIdComp2Sd")]),2), method = "number",tl.col = "black",
         number.cex = 0.7,tl.cex = 0.8,number.digits = 2,type = "upper",diag = T)
dev.off()

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
  select(Partisanship,target_id, all_of(DVsDmd)) %>% 
  gather(var,val,-Partisanship,-target_id) %>% 
  group_by(var,Partisanship,target_id) %>% 
  summarise(perc=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,Partisanship) %>% 
  summarise(avg = mean(perc)) %>% # avg percent agreeing per variable
  mutate(var = factor(var,levels = DVsDmd,labels = DVsLabs)) %>% 
  ggplot(aes(Partisanship,avg)) + 
  geom_col() + 
  facet_wrap(~ var,nrow=3) +
  scale_y_continuous(breaks = seq(0,.7,.1)) + 
  theme(axis.text.x = element_text(angle=90,size = 6),
        plot.caption = element_text(hjust = 0)) +
  labs(x="Partisan identity",y="Average rating",
       caption="Note: We take the average agreement per comment because we have more ratings from liberals than from moderates and conservatives.")
p_avgp_Partisanship
ggsave("Figures/2_AvgAgreement_Partisanship.pdf",p_avgp_Partisanship,width = PlotWidth, height=PlotHeight)

p_avgp_ideo = dt %>% 
  select(PolId,target_id, all_of(DVsDmd)) %>% 
  gather(var,val,-PolId,-target_id) %>% 
  group_by(var,PolId,target_id) %>% 
  summarise(perc=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,PolId) %>% 
  summarise(avg = mean(perc)) %>% # avg percent agreeing per variable
  mutate(var = factor(var,levels = DVsDmd,labels = DVsLabs)) %>% 
  ggplot(aes(PolId,avg)) + 
  geom_col() + 
  facet_wrap(~ var,nrow=3) +
  scale_y_continuous(breaks = seq(0,.7,.1)) + 
  theme(axis.text.x = element_text(angle=90,size = 6),
        plot.caption = element_text(hjust = 0)) +
  labs(x="Political ideology",y="Average rating",
       caption="Note: We take the average agreement per comment because we have more ratings from liberals than from moderates and conservatives.")
p_avgp_ideo
ggsave("Figures/2_AvgAgreement_Ideo.pdf",p_avgp_ideo,width = PlotWidth, height=PlotHeight)




# --------------Covs# ------------------------------------------------------------------------------
# DEPENDENT VARIABLES ON NUMERIC SCALE
p_avg_nobinary_Partisanship = dt %>%
  select(Partisanship,target_id,BToxicNum,ProductiveNum) %>%
  gather(var,val,-Partisanship,-target_id) %>%
  group_by(var,Partisanship,target_id) %>%
  summarise(avg=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,Partisanship) %>%
  summarise(avgavg = mean(avg)) %>% # avg percent agreeing per variable
  mutate(var=factor(var,levels=c("BToxicNum","ProductiveNum"),
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
ggsave("Figures/2_AvgAgreement_NoBinary_Partisanship.png",p_avg_nobinary_Partisanship,width = PlotWidth, height=PlotHeight)

p_avg_nobinary_ideo = dt %>%
  select(PolId,target_id,BToxicNum,ProductiveNum) %>%
  gather(var,val,-PolId,-target_id) %>%
  group_by(var,PolId,target_id) %>%
  summarise(avg=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,PolId) %>%
  summarise(avgavg = mean(avg)) %>% # avg percent agreeing per variable
  mutate(var=factor(var,levels=c("BToxicNum","ProductiveNum"),
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
ggsave("Figures/2_AvgAgreement_NoBinary_Ideo.png",p_avg_nobinary_ideo,width = PlotWidth, height=PlotHeight)




# ------------------------------------------------------------------------------
# BINARY DEPENDENT VARIABLES
p_avg_binary_Partisanship = dt %>%
  select(Partisanship,target_id,BToxicNum01,ProductiveNum01) %>%
  gather(var,val,-Partisanship,-target_id) %>%
  group_by(var,Partisanship,target_id) %>%
  summarise(avg=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,Partisanship) %>%
  summarise(avgavg = mean(avg)) %>% # avg percent agreeing per variable
  mutate(var=factor(var,levels=c("BToxicNum01","ProductiveNum01"),
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
ggsave("Figures/2_AvgAgreement_NotBinary_Partisanship.png",p_avg_binary_Partisanship,width = PlotWidth, height=PlotHeight)

p_avg_binary_ideo = dt %>%
  select(PolId,target_id,BToxicNum01,ProductiveNum01) %>%
  gather(var,val,-PolId,-target_id) %>%
  group_by(var,PolId,target_id) %>%
  summarise(avg=mean(val)) %>% # percent of partisans agreeing per target/comment
  group_by(var,PolId) %>%
  summarise(avgavg = mean(avg)) %>% # avg percent agreeing per variable
  mutate(var=factor(var,levels=c("BToxicNum01","ProductiveNum01"),
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
ggsave("Figures/2_AvgAgreement_Binary_Ideo.png",p_avg_binary_ideo,width = PlotWidth, height=PlotHeight)


