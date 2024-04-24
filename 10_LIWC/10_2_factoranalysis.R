source(list.files(pattern="0_Setup.R", recursive=T))

me = read_csv("fb_survey/10_LIWC/LIWC-22_MeaningExtraction2p.csv") %>% 
  select(-c(post_id:RawTokenCount))
library(psych)
library("GPArotation")
library("sem")


# Eigenvalues are calculated from matrices, so our first step is to calculate the correlation matrix.
me_cor = cor(me, use = "pairwise.complete.obs")

# A general rule is that eigenvalues greater than 1 represent meaningful factors
# We find 5 of them but the other values are close to 1
( eigenvalues = eigen(me_cor)$values )
sum(eigenvalues > 1)

# Use correlation matrix to create scree plot for easier visualization
scree(me_cor, factors = F)

# Based on the empirical rule of thumb, we find 5 or more factors
# However, for more than 2 factors we get the warning that an ultra-Heywood case was detected



efa1 = custom_fa(me, .nfactors = 1)
efa2 = custom_fa(me, .nfactors = 2)
efa3 = custom_fa(me, .nfactors = 3)
efa4 = custom_fa(me, .nfactors = 4)
efa5 = custom_fa(me, .nfactors = 5)
efa6 = custom_fa(me, .nfactors = 6)
efa7 = custom_fa(me, .nfactors = 7)
efa8 = custom_fa(me, .nfactors = 13)



# Solid black lines indicate positive significant loadings
# Dotted red lines indicate negative significant loadings
fa.diagram(efa1,cut=.1)
fa.diagram(efa2,cut=.1)
fa.diagram(efa3,cut=.1)
fa.diagram(efa4,cut=.05)
fa.diagram(efa5,cut=.05) # most informative
fa.diagram(efa6,cut=.1)
fa.diagram(efa7,cut=.1)
fa.diagram(efa8,cut = .2)




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