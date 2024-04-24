# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))

# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))


## CONSTRUCTION OF SCALES
# Variables to visualize
vars_to_plot <- c("ID", "RCA_new", "attitude_AsianAmericans", "attitude_Blacks", "attitude_Hispanics", "attitude_Whites", "attitude_Jews", "attitude_Muslims", "attitude_Christians", "attitude_ChristianFundamentalists",
                  "attitude_IllegalImmigrants", "attitude_RichPeople", "attitude_PoorPeople", "attitude_Liberals", "attitude_Conservatives", "attitude_GayLesbians", "attitude_Transgender",
                  "attitude_Scientists", "attitude_Feminists")

# Names of relevant variables
labels = c("ID", "RCA_new", c("AsianAmericans", "Blacks", "Hispanics", "Whites", "Jews", "Muslims", "Christians", "ChristianFundamentalists",
                              "IllegalImmigrants", "RichPeople", "PoorPeople", "Liberals", "Conservatives", "GayLesbians", "Transgender",
                              "Scientists", "Feminists"))

vars_to_plot[which(!vars_to_plot %in% colnames(anes_w_classes))]
anes2016 = anes_w_classes[,vars_to_plot] # subset to relevant variables
#colnames(anes2016) = labels # replace names

# Clean data (drop year and ID, make matrix, set all values > 110 or < 0 to NA, add 1 to every value (so that no variable has 0))
anes_run = anes2016[,3:length(anes2016)]
anes_run = as.matrix(anes_run)
anes_run[] = ifelse(anes_run[] < 0 | anes_run > 110, NA, anes_run[])
anes_run[] = anes_run[] + 1

# Add cleaned data back to regular data (basically preserving year and id which were dropped as part of cleaning stage)
anes2016[,3:length(anes2016)] = anes_run

# Sub to complete sets
anes2016 = na.omit(anes2016)

# Apply rescale and categorize function dataset
anes2016_cat = anes2016
anes2016_cat[,3:length(anes2016_cat)] = rescale_cat(anes2016_cat[,3:length(anes2016_cat)])
anes2016_cat[,3:length(anes2016_cat)] = sapply(anes2016_cat[,3:length(anes2016_cat)], FUN = function(x) as.numeric(as.character(x)))

# Subset to RCA classes
rca1 <- subset(anes2016_cat, RCA_new == 1)
rca2 <- subset(anes2016_cat, RCA_new == 2)
rca3 <- subset(anes2016_cat, RCA_new == 3)

# Produce scale for each class
rca1_for_pca <- rca1[,3:length(rca1)]
rca1_pca <- prcomp(rca1_for_pca, scale = T)

rca2_for_pca <- rca2[,3:length(rca2)]
rca2_pca <- prcomp(rca2_for_pca, scale = T)

rca3_for_pca <- rca3[,3:length(rca3)]
rca3_pca <- prcomp(rca3_for_pca, scale = T)

library(factoextra)

p1 = fviz_eig(rca1_pca, barfill = "grey60", barcolor = "grey30") + ggtitle("Partisans") + theme(text = element_text(size = 15))
# ggsave("rca1_pca_plot.jpeg", plot = last_plot(), device = "jpeg", height = 5, width = 5)

p2 = fviz_eig(rca2_pca, barfill = "grey60", barcolor = "grey30") + ggtitle("Racials") + theme(text = element_text(size = 15))   
# ggsave("rca2_pca_plot.jpeg", plot = last_plot(), device = "jpeg", height = 5, width = 5)

p3 = fviz_eig(rca3_pca, barfill = "grey60", barcolor = "grey30") + ggtitle("Neutrals") + theme(text = element_text(size = 15))   
# ggsave("rca3_pca_plot.jpeg", plot = last_plot(), device = "jpeg", height = 5, width = 5)

# All together
#library(cowplot)
#plot_grid(p1, p2, p3, labels = c('A', 'B', 'C'), label_size = 20)
#ggsave("eigenvalues_plot.tiff", plot = last_plot(), device = "tiff", height = 10, width = 10)


pca_rca1_1_2 = rbind(data.frame(rotation = rca1_pca$rotation[,"PC1"], PC = "PC1"))
pca_rca1_1_2$Social_Group = rownames(rca1_pca$rotation)
pca_rca2_1_2 = rbind(data.frame(rotation = rca2_pca$rotation[,"PC1"], PC = "PC1"))
pca_rca2_1_2$Social_Group = rownames(rca2_pca$rotation)
pca_rca3_1_2 = rbind(data.frame(rotation = rca3_pca$rotation[,"PC1"], PC = "PC1"))
pca_rca3_1_2$Social_Group = rownames(rca3_pca$rotation)

att_names = c("attitude_AsianAmericans", "attitude_Blacks", "attitude_Hispanics", "attitude_Whites",  "attitude_Jews", "attitude_Muslims", "attitude_Christians", "attitude_ChristianFundamentalists",
              "attitude_IllegalImmigrants", "attitude_RichPeople", "attitude_PoorPeople", "attitude_Liberals", "attitude_Conservatives", "attitude_GayLesbians", "attitude_Transgender",
              "attitude_Scientists", "attitude_Feminists")

att_names_fixed <- gsub("attitude_", "", att_names)
att_names_fixed <-  ifelse(att_names_fixed == "RichPeople", "Rich People", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "PoorPeople", "Poor People", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "IllegalImmigrants", "Illegal Immigrants", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "GayLesbians", "Gays and\nLesbians", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "Transgender", "Transgender\nPeople", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "ChristianFundamentalists", "Christian\nFundamentalists", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "AsianAmericans", "Asian Americans", att_names_fixed)

pca_rca1_1_2$Social_Group = att_names_fixed[match(att_names, pca_rca1_1_2$Social_Group)] 
pca_rca2_1_2$Social_Group = att_names_fixed[match(att_names, pca_rca2_1_2$Social_Group)] 
pca_rca3_1_2$Social_Group = att_names_fixed[match(att_names, pca_rca3_1_2$Social_Group)] 

variable_order <- c("Asian Americans", "Blacks", "Hispanics", "Whites", "Illegal Immigrants", "Jews", "Muslims", 
                    "Gays and\nLesbians", "Transgender\nPeople", "Feminists", "Scientists",  "Liberals",   
                    "Conservatives", "Christians", "Christian\nFundamentalists", "Rich People", "Poor People")

pca_rca1_1_2$Social_Group <- factor(pca_rca1_1_2$Social_Group, levels = variable_order)
pca_rca2_1_2$Social_Group <- factor(pca_rca2_1_2$Social_Group, levels = variable_order)
pca_rca3_1_2$Social_Group <- factor(pca_rca3_1_2$Social_Group, levels = variable_order)


library(ggplot2)
# Basic barplot
p1<-ggplot(data=pca_rca1_1_2, aes(x=Social_Group, y=rotation, color = PC, fill = PC)) +
  geom_bar(position="dodge", stat="identity", alpha = 0.5)
p1 = p1 + ylim(c(-0.5, 0.5)) + xlab("") + ylab("Variable Loading, PC 1") + ggtitle("Partisans") + theme_classic(base_size = 17) + scale_colour_grey() + scale_fill_grey()  +
  theme(legend.position="none")
# ggsave("rca1_rotation_plot.jpeg", plot = last_plot(), device = "jpeg", height = 7, width = 14)

p2<-ggplot(data=pca_rca2_1_2, aes(x=Social_Group, y=rotation, color = PC, fill = PC)) +
  geom_bar(position="dodge", stat="identity", alpha = 0.5)
p2 = p2 + ylim(c(-0.5, 0.5)) + xlab("Social Group") + ggtitle("Racials") + ylab("Variable Loading, PC 1") + theme_classic(base_size = 17) + scale_colour_grey() + scale_fill_grey()  +
  theme(legend.position="none")
# ggsave("rca2_rotation_plot.jpeg", plot = last_plot(), device = "jpeg", height = 7, width = 14)

p3<-ggplot(data=pca_rca3_1_2, aes(x=Social_Group, y=rotation, color = PC, fill = PC)) +
  geom_bar(position="dodge", stat="identity", alpha = 0.5)
p3 = p3 + ylim(c(-0.5, 0.5)) + xlab("") + ggtitle("Neutrals") + ylab("Variable Loading, PC 1") + theme_classic(base_size = 14) + scale_colour_grey() + scale_fill_grey()  +
  theme(legend.position="none")
# ggsave("rca3_rotation_plot.jpeg", plot = last_plot(), device = "jpeg", height = 7, width = 14)

library(cowplot)
plot_grid(p1, p2, labels = c('A', 'B'), label_size = 20, ncol = 1)
# ggsave("pca_rotation_plot.tiff", plot = last_plot(), device = "tiff", height = 10, width = 20)


# Identify positions on scales
anes2016_cat$Scale1 <- as.numeric(rca_scale(anes2016_cat[,3:19], rca1_pca))
anes2016_cat$Scale2 <- as.numeric(rca_scale(anes2016_cat[,3:19], rca2_pca))
anes2016_cat$Scale3 <- as.numeric(rca_scale(anes2016_cat[,3:19], rca3_pca))

# Rescale to make more interpretable
anes2016_cat$Scale1_scaled <- scale(anes2016_cat$Scale1)
anes2016_cat$Scale2_scaled <- scale(anes2016_cat$Scale2)
anes2016_cat$Scale3_scaled <- scale(anes2016_cat$Scale3)

id_to_scales <- anes2016_cat[,c("ID",
                                "Scale1",
                                "Scale2",
                                "Scale3", 
                                "Scale1_scaled",
                                "Scale2_scaled",
                                "Scale3_scaled")]

saveRDS(id_to_scales, "Brensinger2022/output/ids_to_scales.RDS")
