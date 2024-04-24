source(list.files(pattern="0_Setup.R", recursive=T))
library(stm)
library(quanteda)
library(stmprinter)

# load data
fname = "dt_survey_0625_with_toxicity_clean_textonly.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath)

ngrams = c("illegal immigrants","legal immigrant","illegal aliens",
           "illegal people","illegal mexicans",
           "million people","legal immigration","legal immigrants",
           "black people", "black community","black president","black lives","black panther",
           "sharia law","right now","muslim immigrants",
           "muslim extremists","muslim invaders","muslim religion",
           "hillary clinton","bernie sanders",
           "white house","i voted","united states","tax payer","pay taxes",
           "donald trump","make america great again","vote trump","mr trump","ted cruz",
           "go away","go home","stupid bitch","dumb bitch","little bitch","dumb ass","look like",
           "god bless","american people","get rid","border wall", "white people", 
           "gun lobby","gun owners","gun control","gun crime","gun violence",
           "gun safety","thank god","trump supporters","trump says", 
           "drug addicts","drug violence","drug cartels","drug trafficking",
           "drup problem","drug consumer","drug dealers","drug users","drug abuse",
           "cops shoot","law enforcement", "2nd amendment", "unarmed people", "hardworking people",
           "honest people","american jobs","american flag","american government",
           "go to jail","people need","people want","people demand",
           "people like","conspiracy theories",
           "get fired","get married","get deported","get education",
           "get elected","get together","get pregnant","get sexual","get votes",
           "get vaccinated","get specific","get molested",
           "get worse","get away","get money","get covered",
           "sex offender","sex traffickers","sex workers","sex tape","involuntary sex", 
           "fox news","black lives matter", "clean water","water industry")

ngrams <- c("stupid bitch", "god bless", "donald trump", "get rid", "sharia law",
            "get away", "hillary clinton", "thank god", "dumb ass", "go away",
            "dumb bitch", "american flag", "right now", "gun control", "mr trump", "little bitch")




# use quanteda to create a subset of the dataset with sufficient document length
toks = dt %>% 
  corpus(docid_field = "target_id", text_field = "PostContextTarget") %>% 
  tokens(remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_url = TRUE,
         remove_symbols=TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("english")) %>%
  tokens_remove("[@]username|[@]webpagelink", valuetype="regex") %>%
  tokens_compound(pattern = phrase(ngrams)) %>% 
  tokens_select(max_nchar = 35, min_nchar = 2) 

kwic(x = toks, "support", window = 1)

dfmat = toks %>% 
  dfm() %>% 
  dfm_wordstem() %>% 
  dfm_trim(min_termfreq = 2, min_docfreq = 1) 

dfmat = dfmat[which(rowSums(dfmat) > 0), ]

  
dfmat[,which(colnames(dfmat) %in% str_replace(ngrams, " ","_"))] %>% colSums()
dfmat[,which(str_detect(colnames(dfmat),"_"))] %>% colSums()

dfmat %>% colnames()
topfeatures(dfmat,n = 50)

# to stm object
dfstm = convert(dfmat, to = "stm")


# preprocess with stm functionalities
# temp = textProcessor(documents=dt$target,
#               metadata = dt[, - which(names(dt) %in% c("target"))],
#               lowercase = TRUE, #*
#               removestopwords = TRUE, #*
#               removenumbers = TRUE, #*
#               removepunctuation = TRUE, #*
#               stem = TRUE, #*
#               wordLengths = c(3,Inf), #*
#               sparselevel = 1, #*
#               language = "en", #*
#               verbose = TRUE, #*
#               onlycharacter = TRUE, # not def
#               striphtml = FALSE, #*
#               customstopwords = NULL, #*
#               v1 = FALSE) #*
out = prepDocuments(dfstm$documents, dfstm$vocab, dfstm$meta)
str_subset(dfstm$vocab,"_")

# fit multiple stm models
set.seed(2024)

stm_models <- many_models(
  K = 3:10,
  documents = out$documents,
  vocab= out$vocab,
  data = out$meta,
  N = 2,
  runs = 100, 
  cores = parallel::detectCores()
)

# save models
saveRDS(stm_models, file = "fb_survey/11_topic_modeling/stm_modelsN2.RDS")

if(!is.null(out$docs.removed)){
  DocIds = names(dfstm$documents)[-c(as.integer(out$docs.removed))]
} else {
  DocIds = names(dfstm$documents)
}
text = dt %>% filter(target_id %in% DocIds) %>% pull(target)

print_models(
  stm_models, texts = text,
  file = "fb_survey/11_topic_modeling/target_stm_runsN2.pdf",
  title = "Target"
)

stm_models %>% names()

metrics = lapply(stm_models, function(m){
  tibble(k = length(m$semcoh[[1]]),
         Coherence = m$semcoh[[1]],
         Exclusivity = m$exclusivity[[1]], 
  )}) %>% 
  do.call(rbind, .) %>% 
  group_by(k) %>% 
  mutate(AvgCoherence = mean(Coherence), 
         AvgExclusivity = mean(Exclusivity))


# we want models with high exclusivity and high (less negative coherence)
p_ExclusivityCoherence = metrics %>% 
  pivot_longer(cols = c("Coherence","Exclusivity")) %>% 
  mutate(k=factor(k, levels = seq(min(metrics$k), max(metrics$k), 1))) %>% 
  ggplot(aes(k,value,group=k)) + 
  geom_jitter(width = 0.1, size = 0.7, alpha=0.5) + 
  stat_summary(fun.y = mean, color = "red", shape = 3) + 
  facet_wrap( ~ name,nrow = 2, scales="free") +
  labs(x="Number of topics",y="") +
  theme_bw(base_size = 12)
p_ExclusivityCoherence
ggsave("fb_survey/figures/11_ExclusivityCoherence.png",p_ExclusivityCoherence,width = 6.5, height=5)




#stm_models = readRDS("fb_survey/11_topic_modeling/stm_modelsN2.RDS")
tm = stm_models[["k5"]]$runout[[1]]

# assign topics to documents
theta_scores <- as.data.frame(tm$theta) 
theta_scores$target_id <- out$meta$target_id 


topics_long <- theta_scores %>%
  pivot_longer(cols = V1:V5,
               names_to = "topic",
               values_to = "theta") %>% 
  mutate(topic = topic %>% 
           str_replace("V1","GenderSexism") %>% 
           str_replace("V2","Slurs") %>% 
           str_replace("V3","GunControl") %>% 
           str_replace("V4","Identity") %>% 
           str_replace("V5","PersonalExperience") %>% 
           str_replace("V6","ProImmigration")) 

toptopics <- topics_long %>%
  group_by(target_id) %>%
  slice_max(theta)

colnames(toptopics)[1] <- "target_id"
colnames(toptopics)[2] <- "topics"
toptopics$status_id <- as.numeric(toptopics$target_id)

table(toptopics$topics) %>% as.data.frame() %>%
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity")

# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))



inner_join(dt,toptopics,by ="target_id") %>% 
  write_csv("fb_survey/11_topic_modeling/dt_survey_0625_with_toxicity_clean_topics.csv")







