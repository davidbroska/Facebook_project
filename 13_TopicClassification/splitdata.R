source(list.files(pattern="0_Setup.R", recursive=T))
# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character"))

distinct_dt = dt %>% 
  distinct(post_id,post_user_id,post, 
           context, context_id, context_user_id,
           target_id,target_user_id,target) %>% 
  mutate(target_label = "", target_label2 = "", note="") %>% 
  select(post,context,target,target_label,target_label2,note,everything())

set.seed(03082024)
# 0. backup labeled data not used to train the model
backupset = distinct_dt %>% 
  slice_sample(n=250)
write_csv(backupset,"fb_survey/13_ActiveLearning/0.0_backupset_unlabeled.csv")

# 1. initial set of targets to be labeled
# based on this set, we will define the topics 
trainingset = distinct_dt %>% 
  anti_join(backupset, by = c("post_id","context_id","target_id")) %>% 
  slice_sample(n=250)
write_csv(trainingset,"fb_survey/13_ActiveLearning/1.0_trainingset_unlabeled.csv")

# 2. deploy active meachine learning with 50 batches with 2 documents per batch

# 3. plot generalization error curve and look whether it seems to reach a plateau. 
#    if the curve looks steep repeat step 2 and otherwise proceed with step 4

# 4. label a random sample of 100 documents after generalization error in active learning seems to plateau
#    this might help the active learning algorithm to get stuck in a suboptimal space

# 5. repeat steps 2 and 3 if necessary but don't repeat step 4










