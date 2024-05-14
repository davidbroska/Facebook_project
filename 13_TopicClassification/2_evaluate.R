library(caret)

jw = read_csv("13_TopicClassification/classified_comments.csv")
cols = colnames(jw)[- which(colnames(jw)=="target_id")]

# prepare dataset for analysis
dfl = jw %>% 
  pivot_longer(cols=cols) %>% 
  mutate(annotator = str_extract(name,"_[a-z]+$") %>% str_remove("_"),
         name = str_remove(name,"_[a-z]+")) 

dfl %>%
  group_by(name,annotator) %>% 
  summarize(nratings= sum(!is.na(value)),N=n(),p=round(nratings/N,2), ndistinctval=length(unique(value))) %>% 
  print(n=50)

dfw= dfl %>% 
  pivot_wider(id_cols = c(target_id,name),values_fill = NA,
              names_from = annotator, values_from = value) %>% 
  rename(topic=name) %>% 
  filter(!is.na(llm))
count(dfw,target_id,sort=T)


# function to compute reliability measures
get_reliability = function(.df, .pred, .truth){
  cm = caret::confusionMatrix(data=factor(.df[[.pred]],levels = 0:1),
                              reference=factor(.df[[.truth]],levels= 0:1),
                              positive ="1")
  #print(cm$table)
  tibble(comparison = paste0(.pred,"_",.truth),
         NCases = sum(cm$table),
         TruePos = sum(.df[[.truth]]==1,na.rm=T),
         TrueNeg = sum(.df[[.truth]]==0,na.rm=T),
         PredPos = sum(.df[[.pred]]==1,na.rm=T),
         PredNeg = sum(.df[[.pred]]==0,na.rm=T),
         Prevalence=cm$byClass["Prevalence"],
         Precision = cm$byClass["Precision"],
         Recall = cm$byClass["Recall"],
         F1 = cm$byClass["F1"],
         Specificity = cm$byClass["Specificity"],
         Kappa = cm$overall["Kappa"],
         Accuracy = cm$overall["Accuracy"])
}


get_reliability(dfw, "ne",  "db")
get_reliability(dfw, "llm", "db")
get_reliability(dfw, "llm", "ne")

# number of NAs in LLM annotations
dfw %>% 
  group_by(topic) %>% 
  summarize_all(~ sum(!is.na(.)))


# reliability measures per topic
summ = dfw %>% 
  group_split(topic) %>% 
  purrr::map(function(d){
    
    # only compare set of complete cases (e.g. missing LLM predictions)
    ne_db = get_reliability(na.omit(d[,c("ne","db")]),  .pred="ne",.truth="db")
    llm_db = get_reliability(na.omit(d[,c("llm","db")]),.pred="llm",.truth="db")
    db_ne = get_reliability(na.omit(d[,c("db","ne")]),  .pred="db",.truth="ne")
    llm_ne = get_reliability(na.omit(d[,c("llm","ne")]),.pred="llm",.truth="ne")
    
    # aggregate
    list(ne_db,llm_db,db_ne,llm_ne) %>% 
      do.call(bind_rows,.) %>% 
      mutate(Topic = unlist(unique(d[,"topic"])))
    
    }) %>% 
  do.call(bind_rows,.) %>% 
  arrange(Topic,comparison,-F1) 
summ %>% 
  arrange(comparison,Prevalence) %>% 
  print(n=40)

summ %>% 
  filter(F1 >= 0.6)



summ %>%
  left_join(TopicsTab,by = c("Topic"="topic")) %>% 
  #filter(comparison %in% c("llm_db","ne_db")) %>% 
  group_by(Topic) %>%
  mutate(
    Topic = ifelse(any(comparison == "llm_db"), paste0(label,"\nN=",NCases,", True Pos.=",TruePos,", True Neg.=",TrueNeg), label),
    ComparisonType = case_when(
      str_detect(comparison,"llm_") ~ "LLM predicts labels\nfrom human annotator",
      str_detect(comparison,"db_ne|ne_db") ~ "Human annotator predicts\nlabels from human annotator")) %>%  
  ungroup() %>% 
  pivot_longer(cols=c(Precision,Recall,F1,Specificity,Kappa)) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(value,name,color=comparison,shape=ComparisonType)) + 
  geom_point(size = 1,position=position_dodge(width = .35)) +
  facet_wrap(~ Topic,ncol=3) +
  scale_x_continuous(limits = c(0,1),breaks=seq(0,1,0.1)) +
  geom_vline(xintercept = .7,alpha=.4,linetype="dashed") +
  scale_color_manual(
    breaks = c("db_ne","llm_ne","ne_db","llm_db"),
    values=c("#b10026","#fc4e2a","#005a32","#78c679"),
    labels=c("Annotator DB labels predict\nannotator NE labels",
             "LLM labels predict\nannotator NE labels",
             "Annotator NE labels predict\nannotator DB labels",
             "LLM labels predict\nannotator DB labels")) +
  labs(y="",shape="Comparison Type",color="Comparison",
       x="Value of reliability statistic")
ggsave("Figures/classifier_performance.pdf",width=7,height=6,bg="white")
           







set.seed(1)
c1 = filter(lw,proclinton_llm==1) %>% sample_n(.,nrow(.)) %>% anti_join(hw,by="target_id") %>% head(50)
c2 = filter(lw,proimmigrant_llm==1) %>% sample_n(.,nrow(.)) %>% anti_join(hw,by="target_id") %>% head(40)
c3 = filter(lw,antigun_llm==1) %>% sample_n(.,nrow(.)) %>% anti_join(hw,by="target_id") %>% head(100)
c4 = filter(lw,drugs_llm==1) %>% sample_n(.,nrow(.)) %>% anti_join(hw,by="target_id") %>% head(40)
c5 = filter(lw,antichristianity_llm==1) %>% sample_n(.,nrow(.)) %>% anti_join(hw,by="target_id") %>% head(100)
c6 = filter(lw,antiabortion_llm==1) %>% sample_n(.,nrow(.)) %>% anti_join(hw,by="target_id") %>% head(70)

cc = list(c1,c2,c3,c4,c5,c6) %>% 
  do.call(bind_rows,.) %>% 
  distinct(target_id,.keep_all = T) %>% 
  anti_join(hw,by="target_id") %>% 
  left_join(distinct(dt,target_id,post,context,target),by="target_id") 
nrow(cc)  
cc %>% 
  select(target_id,post,context,target) %>% 
  write_csv("13_TopicClassification/rarepos_classified2.csv")


  

  


