# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
library(factoextra)
library(FactoMineR)
library(gplots)

# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels), 
         Toxicity = paste0("Toxicity_",Toxicity), 
         Productive = paste0("Productive_",Productive))

QualSupp = "Gender"#Partisanship"#Education"#c("Partisanship")#,"Education","AgeCat")#,"Race")
QuantSupp = c("Age2Sd","Income2Sd","PolIdComp2Sd", "Order2Sd", "EducationNum2Sd")
# subset data to dependent variables
dd = dt %>% 
  select(all_of(CatDVs)
         #,all_of(QualSupp),all_of(QuantSupp)
         ) %>% 
  mutate(across(all_of(c(CatDVs#,
                         #QualSupp
                         )), as.factor)) %>% 
  na.omit()

# plot how variables map onto dimensions
# covariates represent social structure

IndexQual = which(names(dd) %in% QualSupp)
IndexQuant = which(names(dd) %in% QuantSupp)
mca = MCA(dd,graph = F#,
          #quali.sup =  IndexQual, 
          #quanti.sup = IndexQuant
          )

p_mca = fviz_mca_var(mca, repel = T,labelsize = 3)
ggsave("fb_survey/figures/7_mca.png",p_mca,bg = "white",width = 6.25, height=5.5)

p_mca_contrib = fviz_mca_var(mca, col.ind="contrib", choice = "var",repel = TRUE)
ggsave("fb_survey/figures/7_mca_contrib.png",p_mca_contrib,bg = "white",width = 6.25, height=5.5)

# to do 
# make 4 plots and display dependent variables individually
# make DVs correct: right now you have categories that say "No" but we dont whether this productive or toxic
# note partisanship is the only supp variables for now 
# extract labels
rownames(mca$var$coord)

plot_vars1 = c(as.character(unique(dt$Partisanship)), paste0(CatDVs), paste0(CatDVs,"_0"))
plot_vars2 = c(as.character(unique(dt$AgeCat)), paste0(CatDVs,"_1"), paste0(CatDVs,"_0"))
plot_vars3 = c(as.character(unique(dt$Education)), paste0(CatDVs,"_1"), paste0(CatDVs,"_0"))
plot_vars4 = c(as.character(unique(dt$OrderCat)), paste0(CatDVs,"_1"), paste0(CatDVs,"_0"))
fviz_mca_var(mca, repel = T,labelsize = 2, select.var = list(name=plot_vars1))
fviz_mca_var(mca, repel = T,labelsize = 2, select.var = list(name=plot_vars2))
fviz_mca_var(mca, repel = T,labelsize = 2, select.var = list(name=plot_vars3))
fviz_mca_var(mca, repel = T,labelsize = 2, select.var = list(name=plot_vars3))


# eigenvalues
get_eigenvalue(mca)
fviz_screeplot(mca,ylim=c(0,25))

# biplot catgorical
fviz_mca_var(mca, repel = T,labelsize = 2)

mca$var$contrib
fviz_mca_var(mca, choice = "quanti.sup",repel = T,arrowsize = .3, labelsize = 4, 
             col.quanti.sup = QuantSupp)




