# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))

# classified
fnames = c("anticonservative.csv","america.csv","christianity.csv","suggestive.csv","drugs.csv")
df = fnames %>% 
  lapply(., function(f){
    list.files(pattern = f, recursive=T) %>% 
      read_csv() %>% 
      select(-...1) %>% 
      mutate(topic = str_extract(f,"[a-z]+.csv") %>% str_remove(".csv"))
  }) %>% 
  do.call(bind_rows, .) %>% 
  mutate(relevant = label %>% 
           str_extract("(^ ?[0-1])|([0-1] ?$)")) %>% 
  filter(!is.na(relevant)) %>% 
  pivot_wider(names_from = "topic",values_from = "relevant",id_cols = "target_id") %>% 
  left_join(distinct(dt,target_id,post,context,target),by="target_id")

set.seed(0414)
df = df[sample(1:nrow(df)),]

df[which(df$target_id=="T1340233236006726"), "christianity"] = "0"
df[which(df$target_id=="T1340233236006726"), "suggestive"] = "0"
df[which(df$target_id=="T1340233236006726"), "drugs"] = "0"
df[which(df$target_id=="T10154299684276548"),"christianity"] = "0"
df[which(df$target_id=="T10154299684276548"),"suggestive"] = "0"
df[which(df$target_id=="T10153200958788836"),"suggestive"] = "0"
df[which(df$target_id=="T10153200958788836"),"christianity"] = "0"

summarise_all(df, ~ sum(is.na(.)))


write_csv(df,"fb_survey/13_TopicClassification/classified_comments.csv")




