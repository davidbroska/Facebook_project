# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
library(psych)
library("GPArotation")
library("sem")

# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))


#AppDVs = DVs[-which(DVs%in%c("BOpen","BEmotional","BObjective","BSarcastic","BNoneabove"))]

dt$BNotToxic = 1- dt$BIsToxic 
dt$BNotToxicNum01 = 1 - dt$BToxicNum01
dt$BNoHateSpeech = 1- dt$BUseHateSpeech
dt$BNotHostile = 1 - dt$BHostile
dt$BTolerant = 1- dt$BIntolerant 


AppDVsCommon = DVs[which(!DVs %in% c("BToxicNum01","BUseHateSpeechNum01","BHostile","BIntolerant",
                                     "BNoneabove","BEmotional","BSarcastic","BOpen","BObjective"))]
AppDVsCommon = c(AppDVsCommon, c("BNotToxicNum01","BNoHateSpeech","BNotHostile","BTolerant","BRespectful"))
# Our theory contains 10 variables
length(AppDVsCommon)


# Split dataset ----------------------------------------------------------------

# Indices for EFA and CFA
set.seed(0218)

# use subset of respondents in training data
RespN = floor(FaPSplit * length(unique(dt$ResponseId)))

# get Ids of those respondents
RespId = sample(unique(dt$ResponseId), size = RespN, replace = F)

# Split the data
indices = 1:nrow(dt)
dt_efa = dt %>% filter( ResponseId %in% RespId) %>% select(all_of(AppDVsCommon))
dt_cfa = dt %>% filter(!ResponseId %in% RespId) %>% select(all_of(AppDVsCommon))

# check
nrow(dt_efa) + nrow(dt_cfa) == nrow(dt)


# Confirmatory factor analysis (CFA) -------------------------------------------
efa1 = custom_fa(dt_efa, .nfactors = 1)
efa2 = custom_fa(dt_efa, .nfactors = 2)
fa.diagram(efa1,cut = .1)

options(fit.indices = c("CFI","GFI","RMSEA","BIC"))
# setup syntax for CFA 
cfa1 = sem(structure.sem(efa1), data=dt_cfa)
cfa2 = sem(structure.sem(efa2), data=dt_cfa)


#cfa_theory = cfa(text =  paste0("Appropriate: ",paste(AppDVs,collapse = ", ")))
#cfa1 = sem(model = cfa_theory, data = dt_cfa)

# Examine loadings
efa1$loadings %>% head()
summary(cfa1)$coeff$Estimate %>% head()


# Model fit statistics 
tab = tibble(NFactors = 1:2, 
             GFI = round(c(summary(cfa1)$GFI, summary(cfa2)$GFI), 3),
             CFI = round(c(summary(cfa1)$CFI, summary(cfa2)$CFI), 3),
             RMSEA = round(c(summary(cfa1)$RMSEA[1],summary(cfa2)$RMSEA[1]), 3),
             BIC = round(c(summary(cfa1)$BIC, summary(cfa2)$BIC), 0)) %>% 
  mutate_all(as.character) %>% 
  add_row(NFactors = "Desired",
          GFI = ">0.9",
          CFI = ">0.9",
          RMSEA = "<0.05",
          BIC = "lower")
tab



# Investigate differences in factor scores -------------------------------------

# Use the wrapper function to create syntax for use with the sem() function
efa1_syn <- structure.sem(efa1)

# Run a CFA using the EFA syntax you created earlier
efa1_cfa1 <- sem(efa1_syn, data = dt_cfa)
efa1_scores = efa1$scores
cfa1_scores = fscores(efa1_cfa1,data = dt_cfa)

# The density curves will differ because the parameter values differ between efa and cfa 
# However, the curves are close
plot(density(efa1_scores[,1], na.rm = TRUE), 
     xlim = c(-3, 3), ylim = c(0, 1), col = "blue")
lines(density(cfa1_scores[,1], na.rm = TRUE), 
      xlim = c(-3, 3), ylim = c(0, 1), col = "red")



# Create composite items -------------------------------------------------------

dt$Appropriateness = rowMeans(dt[AppDVsCommon], na.rm=T)
# The output from the alpha() function will also tell you some basic stats for each item as well as how the overall alpha value would be affected if an item were dropped. 
# If dropping an item would cause alpha to increase, that's an indicator that that item isn't performing as well.
psych::alpha(dt[AppDVsCommon])[[1]][,c("raw_alpha","std.alpha","G6(smc)")]

# Reliability if item is dropped
psych::alpha(dt[AppDVsCommon])[[2]] %>% round(2)

# Split half reliability reflects how well two halves of the test relate to each other.
splitHalf(dt[AppDVsCommon])

psych::glb(dt[AppDVsCommon])

# Compare with exploratory factor analysis scores on whole data set
efa1_TrainTest = custom_fa(select(dt,all_of(AppDVsCommon)), .nfactors = 1)
dt$AppropFa1Scores = as.numeric(efa1_TrainTest$scores)
cor(dt$Appropriateness,dt$AppropFa1Scores)
fa.diagram(efa1_TrainTest,cut = .25)



length(DVs) - length(AppDVsCommon)
AppDVsCommon %>% paste(collapse = ", ")

# the reliability does not increase when positive and negative items 
AppDVsCommon
positive = c("ProductiveNum01","UnderstandB","BMakesEffort","BStaysOnTopic","BRespectful","BAgrees")
negative = c("BNotToxicNum01","BNoHateSpeech","BNotHostile","BTolerant")
alpha(dt[positive])[[1]][,c("raw_alpha","std.alpha","G6(smc)")]
alpha(dt[negative])[[1]][,c("raw_alpha","std.alpha","G6(smc)")]

dt %>% 
  select(all_of(c(colnames(read_csv(fpath)), "Appropriateness","AppropFa1Scores"))) %>% 
  write_csv(.,  file=str_replace(fpath,"clean.csv","clean_index.csv"))

