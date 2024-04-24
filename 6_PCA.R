# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
library(factoextra)

# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))

get_loadings = function(.pca,.component, .absolute=T){
  # for each variable extract loading onto a PC
  l = data.frame(variable = names(.pca$rotation[,.component]), 
                 loading = .pca$rotation[,.component])
  rownames(l) = NULL
  
  # if true then contribution to a PC is taken into account
  if (.absolute) l$loading = abs(l$loading)
  
  # arrange in decreasing order
  l_ordered = l[order(l$loading,decreasing = T),]
  return(l_ordered)
}



# Dependent variables ----------------------------------------------------------
# run principle component analysis
pca = dt %>% 
  select(all_of(DVsDmd)) %>%  
  rename_with(~ str_remove(.,"_TargetDmd"), all_of(DVsDmd)) %>% 
  prcomp(center = T, scale. = T)

p_PCAscree = fviz_eig(pca) +
  theme_bw(base_size = 9)
p_PCAscree
ggsave("fb_survey/figures/6_PcaDvScreeDV.png",p_PCAscree,bg = "white",width = 6.25, height=5.5)

p_PCAvar = fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, labelsize = 3) +
  labs(color = "Contribution") + 
  theme(legend.position = "right") + 
  theme_bw(base_size = 9) 
p_PCAvar
ggsave("fb_survey/figures/6_PcaDvVar.png",p_PCAvar,bg = "white",width = 6.25, height=5.5)


# factors that load most on PC1 in absolute terms
get_loadings(pca,"PC1")

# loading on PC1 in positive direction
head(get_loadings(pca,"PC1",.absolute=F), 3)

# loading on PC1 in negative direction
tail(get_loadings(pca,"PC1",.absolute=F), 3)

get_loadings(pca,"PC2")





# Dependent variables and covariates -------------------------------------------
# run principle component analysis
PcaVars = c(DVsDmd,"PolIdComp2Sd","Age2Sd","EducationNum2Sd","Income2Sd")
pca = dt %>% 
  select(all_of(PcaVars)) %>%  
  rename_with(~ str_remove(.,"_TargetDmd"), all_of(PcaVars)) %>% 
  prcomp(center = T, scale. = T)

p_PCAscree = fviz_eig(pca) +
  theme_bw(base_size = 9)
p_PCAscree
ggsave("fb_survey/figures/6_PcaDvScreeDvAndCovs.png",p_PCAscree,bg = "white",width = 6.25, height=5.5)

p_PCAvar = fviz_pca_var(pca, col.var = "contrib",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE, labelsize = 3) +
  labs(color = "Contribution") + 
  theme(legend.position = "right") + 
  theme_bw(base_size = 9) 
p_PCAvar
ggsave("fb_survey/figures/6_PcaDvAndCovsVar.png",p_PCAvar,bg = "white",width = 6.25, height=5.5)


# factors that load most on PC1 in absolute terms
get_loadings(pca,"PC7",.absolute = F)
get_loadings(pca,"PC4")









