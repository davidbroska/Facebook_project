library(googlesheets4)

# load data
df = read_sheet("17lQDiLpRpegbIXqfgcnrXZJF9niLbi5H-NIYBl-bA0g")

coded = c("anticonservative","america","christianity","suggestive","drugs")

# correlation between classifications
lm(scale(anticonservative) ~ scale(anticonservative_ann),df) #0.187161
lm(scale(america)          ~ scale(america_ann),df)          #0.24238
lm(scale(christianity)     ~ scale(christianity_ann),df)     #0.7942
lm(scale(suggestive)       ~ scale(suggestive_ann),df)       #0.69366
lm(scale(drugs)            ~ scale(drugs_ann),df)            #0.66714

# get number of coded documents
summarise(df,across(all_of(coded), ~ sum(!is.na(.))))

# get number of documents rated as relevant
summarise(df,across(all_of(coded), ~ sum(.==1,na.rm=T)))

# get number of documents rated as irrelevant
summarise(df,across(all_of(coded), ~ sum(.==0,na.rm=T)))









  


