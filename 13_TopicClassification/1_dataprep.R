# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
library(googlesheets4)


# LLM annotated data ------------------------------------------------------

# List of files with comments classified by LLM
fnames = paste0(TopicsTab$topic, ".csv") 

# Read and aggregate those files
lw = fnames %>% 
  lapply(., function(f){
    list.files(pattern = f, recursive=T) %>% 
      read_csv() %>% 
      select(-...1) %>% 
      mutate(topic = str_extract(f,"[a-z]+.csv") %>% str_remove(".csv"))
  }) %>% 
  do.call(bind_rows, .) %>% 
  mutate(relevant = label %>% str_extract("(^ ?[0-1])|([0-1] ?$)")) %>% 
  filter(!is.na(relevant)) %>% 
  pivot_wider(names_from = "topic",names_glue = "{topic}_llm",
              values_from = "relevant",id_cols = "target_id") 



# Human annotated data ----------------------------------------------------

# First set of annotations
hw1 = read_sheet("17lQDiLpRpegbIXqfgcnrXZJF9niLbi5H-NIYBl-bA0g", sheet = "all_comments") %>% 
  select(-anticonservative,-antiamerica,-antichristianity,-suggestive,-drugs,
         -post,-context,-target, 
         -contains("_db0"))

# Second set of annotations
hw2 = read_sheet("17lQDiLpRpegbIXqfgcnrXZJF9niLbi5H-NIYBl-bA0g", sheet = "comments_predicted_relevant") %>% 
  select(-post,-context,-target)  

hw1 %>% 
  gather(var,val,-target_id) %>% 
  group_by(target_id,var) %>%
  mutate(notallNA = any(!is.na(val))) %>%
  ungroup() %>%
  filter(notallNA) %>%
  filter(is.na(val))

hw2 %>% 
  gather(var,val,-target_id) %>% 
  group_by(target_id,var) %>%
  mutate(notallNA = any(!is.na(val))) %>%
  ungroup() %>%
  filter(notallNA) %>%
  filter(is.na(val))

# Append two sets of annotations 
hw = bind_rows(hw2,hw1) %>% 
  group_by(target_id) %>% 
  slice(1) %>% 
  ungroup()

# Check that there are no duplicates 
count(hw,target_id,sort=T) %>% filter(n>1) 


# Join with LLM annotated data ------------------------------------------

# joined wide format
jw = left_join(lw,hw, by = "target_id")

# check that there are no duplicates 
count(jw,target_id,sort=T) %>% filter(n>1)

# write file
write_csv(jw,"13_TopicClassification/classified_comments.csv")




