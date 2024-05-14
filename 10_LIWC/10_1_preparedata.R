source(list.files(pattern="0_Setup.R", recursive=T))
# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character"))

distinct_dt = dt %>% 
  distinct(post_id,post_user_id,post, 
           context, context_id, context_user_id,
           target_id,target_user_id,target) %>% 
  rowwise() %>% 
  mutate(PostContextTarget = paste(c(post,context,target), collapse = " ")) %>% 
  select(post_id,context_id,target_id,post,context,target,PostContextTarget)
  
write_csv(distinct_dt, "data/dt_survey_0625_with_toxicity_clean_textonly.csv")

# to reproduce the dictionary counts 
# 1. load dt_survey_0625_with_toxicity_clean_textonly.csv into LIWC-22 
# 2. for each post, context, target run LIWC and the extended moral foundation dictionary
