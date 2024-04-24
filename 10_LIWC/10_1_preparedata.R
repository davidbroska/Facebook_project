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
  mutate(PostContextTarget = paste(c(post,context,target), collapse = " "),
         PostContext       = paste(c(post,context), collapse = " "))


  
write_csv(distinct_dt, "fb_survey/10_LIWC/dt_survey_0625_with_toxicity_clean_textonly.csv")
