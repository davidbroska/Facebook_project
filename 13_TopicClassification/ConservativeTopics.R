source(list.files(pattern="0_Setup.R", recursive=T))
library(quanteda)
library(stm)
library(stmprinter)
# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character"))


summ = dt %>% 
  mutate(PolCentered = PolIdNum - mean(PolIdNum), 
         ToxPol = PolCentered * BToxicNum01) %>% 
  group_by(target_id) %>%
  summarize(MeanTox = sum(ToxPol), n = n()) 

hist(summ$MeanTox)
mean(summ$MeanTox)
sd(summ$MeanTox)


# load data
fnameto = "dt_survey_0625_with_toxicity_clean_textonly.csv"
fpathto = list.files(pattern = fname, recursive = T)
dtto = read_csv(fpath)

cons_texts = summ %>% 
  filter(n > 3, MeanTox > mean(MeanTox) + 1 * sd(MeanTox))  %>% 
  left_join(dtto, by = "target_id") %>% 
  select(post,context,target,everything())


toks = cons_texts %>% 
  left_join(texts, by = "target_id") %>% 
  corpus(text_field = "target") %>% 
  tokens(remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_url = TRUE,
         remove_symbols=TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("english")) %>%
  tokens_remove("[@]username|[@]webpagelink", valuetype="regex") %>%
  tokens_select(max_nchar = 35, min_nchar = 2) 

dfmat = toks %>% 
  dfm() %>% 
  dfm_wordstem() %>% 
  dfm_trim(min_termfreq = 5, min_docfreq = 5) 

dfmat = dfmat[which(rowSums(dfmat) > 0), ]

dfmat %>% colnames()
topfeatures(dfmat,n = 50)
library(quanteda.textplots)
textplot_wordcloud(dfmat)


