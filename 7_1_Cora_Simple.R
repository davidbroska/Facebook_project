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
         PolId = factor(PolId,levels=PolIdLevels))

# subset data to dependent variables
dd = dt %>% 
  select(all_of(BinaryDVs))

p_BinaryVarsBarchart = dd %>% 
  gather(var,val) %>% 
  group_by(var) %>% 
  summarise(n= n(),
            p1 = sum(val==1) /n(), 
            p0 = sum(val==0) /n()) %>% 
  ggplot(aes(reorder(var,p1),p1)) + 
  geom_col() +
  geom_text(aes(label = n)) +
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,.2)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(x="Variable",y="Proportion of respondenses = 1")
p_BinaryVarsBarchart
ggsave("fb_survey/figures/7_BarchartBinaryVars.png",p_BinaryVarsBarchart,bg = "white",width = 6.25, height=5.5)




ca = CA(dd, graph = F)
eigenvalues = get_eigenvalue(ca)
fviz_eig(ca)

caplot = fviz_ca_biplot(ca, repel = T,labelsize =4,
                        geom.row = "point", pointsize = 0.02) +
  labs(caption=str_wrap("Note: Proximity of observations (blue) and variables (red) indicates similarity"))
caplot
ggsave("fb_survey/figures/7_CorrespondenceAnalysis.png",caplot,bg = "white",width = 6.25, height=5.5)


res.desc <- dimdesc(ca, axes = c(1,2))
head(res.desc[[1]]$col, 4)
tail(res.desc[[1]]$col, 4)
head(res.desc[[2]]$col, 4)



# 
dt %>% 
  filter(PolIdComp < median(PolIdComp)-sd()) %>% 
  select(all_of(BinaryDVs)) %>% 
  CA(graph=F) %>% 
  fviz_ca_biplot(repel = T,labelsize =4,
                 geom.row = "point", pointsize = 0.02)

# Education
dt %>% 
  filter(EducationNum < median(EducationNum)-1) %>% 
  select(all_of(BinaryDVs)) %>% 
  CA(graph=F) %>% 
  fviz_ca_biplot(repel = T,labelsize =4,
                 geom.row = "point", pointsize = 0.02)

# Age
dt %>% 
  filter(Age < median(Age)-1) %>% 
  select(all_of(BinaryDVs)) %>% 
  CA(graph=F) %>% 
  fviz_ca_biplot(repel = T,labelsize =4,
                 geom.row = "point", pointsize = 0.02)


  

ca = CA(dd, graph = F)
eigenvalues = get_eigenvalue(ca)
fviz_eig(ca)

caplot = fviz_ca_biplot(ca, repel = T,labelsize =4,
                        geom.row = "point", pointsize = 0.02) +
  labs(caption=str_wrap("Note: Proximity of observations (blue) and variables (red) indicates similarity"))
caplot
ggsave("fb_survey/figures/7_CorrespondenceAnalysis.png",caplot,bg = "white",width = 6.25, height=5.5)


res.desc <- dimdesc(ca, axes = c(1,2))
head(res.desc[[1]]$col, 4)
tail(res.desc[[1]]$col, 4)
head(res.desc[[2]]$col, 4)

