library(googlesheets4)

# load data
df = read_sheet("17lQDiLpRpegbIXqfgcnrXZJF9niLbi5H-NIYBl-bA0g")
colnames(df)

coded = c("anticonservative_db","antiamerica_db","antichristianity_db","suggestive_db","drugs_db")


# correlation between classifications
lm(scale(anticonservative_db) ~ scale(anticonservative),df) #0.187161
lm(scale(antiamerica_db)      ~ scale(antiamerica),df)      #0.24238
lm(scale(antichristianity_db) ~ scale(antichristianity),df) #0.31065
lm(scale(suggestive_db)       ~ scale(suggestive),df)       #0.69366
lm(scale(drugs_db)            ~ scale(drugs),df)            #0.66714

# get number of coded documents
summarise(df,across(all_of(coded), ~ sum(!is.na(.))))

# get number and percent of documents rated as relevant
summarise(df,across(all_of(coded), ~ sum(.==1,na.rm=T)))
summarise(df,across(all_of(coded), ~ sum(.==1,na.rm=T) / sum(.!=1,na.rm=T)))

# get number of documents rated as irrelevant
summarise(df,across(all_of(coded), ~ sum(.==0,na.rm=T)))









  


