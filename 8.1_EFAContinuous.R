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

DVs = DVs[ - which(DVs %in% c("BNoneabove","UnderstandB")) ]



# Split dataset ----------------------------------------------------------------

# Indices for EFA and CFA
set.seed(1)

# use subset of respondents in training data
RespN = floor(FaPSplit * length(unique(dt$ResponseId)))

# get Ids of those respondents
RespId = sample(unique(dt$ResponseId), size = RespN, replace = F)

# Split the data
indices = 1:nrow(dt)
dt_efa = dt %>% filter( ResponseId %in% RespId) %>% select(all_of(DVs))
dt_cfa = dt %>% filter(!ResponseId %in% RespId) %>% select(all_of(DVs))

# check
nrow(dt_efa) + nrow(dt_cfa) == nrow(dt)


# Eigenvalues are calculated from matrices, so our first step is to calculate the correlation matrix.
efa_cor = cor(dt_efa, use = "pairwise.complete.obs")

# A general rule is that eigenvalues greater than 1 represent meaningful factors
# We find 5 of them but the other values are close to 1
( eigenvalues = eigen(efa_cor)$values )
sum(eigenvalues > 1)

# Use correlation matrix to create scree plot for easier visualization
scree(efa_cor, factors = F)

# Based on the empirical rule of thumb, we find 5 or more factors
# However, for more than 2 factors we get the warning that an ultra-Heywood case was detected



efa1 = custom_fa(dt_efa, .nfactors = 1)
efa2 = custom_fa(dt_efa, .nfactors = 2,.niter = 500)
efa3 = custom_fa(dt_efa, .nfactors = 3, .niter = 500)
efa4 = custom_fa(dt_efa, .nfactors = 4)
efa5 = custom_fa(dt_efa, .nfactors = 5)


# Solid black lines indicate positive significant loadings
# Dotted red lines indicate negative significant loadings
fa.diagram(efa1,cut = 0.1,main = "Exploratory factor analysis with 1 factor")
fa.diagram(efa2,cut = 0.1,main = "Exploratory factor analysis")
fa.diagram(efa3,cut = 0.1,main = "Exploratory factor analysis with 3 factors")
fa.diagram(efa4,cut = 0.3,main = "Exploratory factor analysis")
fa.diagram(efa5)



# Fit statistics ------------------------------------------------------
# Absolute fit statistics are useful for making a judgment about whether or not a model fits adequately.

tab = tibble(NFactors = 1:5, 
             TLI = round(c(efa1$TLI, efa2$TLI, efa3$TLI, efa4$TLI, efa5$TLI), 2),
             RMSEA = round(c(efa1$RMSEA[1],efa2$RMSEA[1],efa3$RMSEA[1],efa4$RMSEA[1],efa5$RMSEA[1]), 3),
             BIC = round(c(efa1$BIC, efa2$BIC, efa3$BIC, efa4$BIC, efa5$BIC), 0)) %>% 
  mutate_all(as.character) %>% 
  add_row(NFactors = "Desired",
          TLI = ">0.9",
          RMSEA = "<0.05",
          BIC = "lower")
tab

# Good model or not? Use established statistics
# chi square test should not detect a difference between the training and test data but for large dataset this often happens
# Tucker Lewis Index (penalizes more complex models) good fit to test data if TLI > 0.9
# Root mean square error of approximation good if RMSEA < 0.05

# To sum up, when you are in the process of model development, the first step is
# to make sure your model or models have adequate fit according to the absolute fit statistics. 
# If you are comparing multiple models that all have good fit, you can use relative fit 
# statistics to make an empirical determination about which model is mathematically preferred.


