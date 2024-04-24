### Final Black and White Plots

## FIGURE 1 -- Example Schema Figure
# Hypothetical data
example_df = data.frame(Conservatives = c(100, 70, 10, 100),
                        Blacks = c(50, 60, 80, 30),
                        Muslims = c(10, 30, 90, 20),
                        `Gays.Lesbians` = c(10, 30, 90, 80),
                        Group = c("A", "B", "C", "D"),
                        stringsAsFactors = F)

# Convert from wide to long form
example_df <- melt(example_df, id.vars = "Group")

# Make sure all of the variables are ready for plotting
save_group_names <- as.character(example_df$variable)
save_group_names <- ifelse(save_group_names == "Gays.Lesbians", "Gays and \nLesbians", save_group_names)
example_df$variable <- as.numeric(example_df$variable)
example_df$value <- as.numeric(example_df$value)

example_df1 <- subset(example_df, Group %in% c("A", "B", "C", "D"))
# Plot schemas
ggplot(data = example_df1, aes(x = variable, y = value, color = Group, shape = Group)) +
  geom_point(size = 4) +
  geom_line(lwd = 1.5) +
  theme_classic(base_size = 18) +
  ylab("Attitude") +
  xlab("Feeling Thermometer") +
  xlim(c(0,5))+
  guides(color=guide_legend(title=""), shape = guide_legend(title = "")) +
  scale_x_continuous(labels=unique(save_group_names)) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 30)) +
  theme(legend.position="bottom", axis.title.y=element_blank(), axis.title.x=element_blank()) +
  scale_color_grey() +
  scale_shape_manual(values = c(15:18))

ggsave(filename = "Brensinger2022/output/Figure 1.tiff", height = 7, width = 10, device = "tiff")

## FIGURE 3 -- Heatmaps and Network Plots
## Note: igraph has changed how its network layout algorithms handle edge weights since we wrote the paper,
# As a result, the network plots will look different than those in the paper, but the differences are only do to the underlying layout algorithm and not reflective of a change in the data. 
# Using a layout algorithm which repels nodes with negative weights will fix the problem. Revert to igraph version 0.8.0 for best results. 

# load data and fix variable names
anes_w_classes = readRDS("Brensinger2022/output/anes_w_classes_w_covars_and_dks.RDS")
id_to_scales <- readRDS("Brensinger2022/output/ids_to_scales.RDS")

anes_w_classes <- merge(anes_w_classes, id_to_scales, by = "ID")

# Split Class 1 into two poles at 0 and create variables accordingly
pole_mid <- 0
anes_w_classes$Scale1_pole1 <- ifelse(anes_w_classes$Scale1_scaled>=mean(anes_w_classes$Scale1_scaled), 1, 0)
anes_w_classes$Scale1_pole2 <- ifelse(anes_w_classes$Scale1_scaled<mean(anes_w_classes$Scale1_scaled), 1, 0)
anes_w_classes$RCA_1_1 = ifelse(anes_w_classes$RCA_new == 1, anes_w_classes$Scale1_pole1, 0)
anes_w_classes$RCA_1_2 = ifelse(anes_w_classes$RCA_new == 1, anes_w_classes$Scale1_pole2, 0)

anes_w_classes$Scale2_pole1 <- ifelse(anes_w_classes$Scale2_scaled>=mean(anes_w_classes$Scale2_scaled), 1, 0)
anes_w_classes$Scale2_pole2 <- ifelse(anes_w_classes$Scale2_scaled<mean(anes_w_classes$Scale2_scaled), 1, 0)
anes_w_classes$RCA_2_1 = ifelse(anes_w_classes$RCA_new == 2, anes_w_classes$Scale2_pole1, 0)
anes_w_classes$RCA_2_2 = ifelse(anes_w_classes$RCA_new == 2, anes_w_classes$Scale2_pole2, 0)

# Fix attitude names
att_names = c("attitude_AsianAmericans", "attitude_Blacks", "attitude_Hispanics", "attitude_Whites",  "attitude_Jews", "attitude_Muslims", "attitude_Christians", "attitude_ChristianFundamentalists",
              "attitude_IllegalImmigrants", "attitude_RichPeople", "attitude_PoorPeople", "attitude_Liberals", "attitude_Conservatives", "attitude_GayLesbians", "attitude_Transgender",
              "attitude_Scientists", "attitude_Feminists")

att_names_fixed <- gsub("attitude_", "", att_names)
att_names_fixed <-  ifelse(att_names_fixed == "RichPeople", "Rich People", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "PoorPeople", "Poor People", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "IllegalImmigrants", "Illegal Immigrants", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "GayLesbians", "Gays and Lesbians", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "Transgender", "Transgender \nPeople", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "ChristianFundamentalists", "Christian \nFundamentalists", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "AsianAmericans", "Asian Americans", att_names_fixed)
colnames(anes_w_classes)[match(att_names, colnames(anes_w_classes))] <- att_names_fixed

pole_mid = 0
anes_w_classes$Scale2_pole1 <- ifelse(anes_w_classes$Scale2_scaled>=pole_mid, 1, 0)
anes_w_classes$Scale2_pole2 <- ifelse(anes_w_classes$Scale2_scaled<pole_mid, 1, 0)
anes_w_classes$RCA_2_1 = ifelse(anes_w_classes$RCA_new == 2, anes_w_classes$Scale2_pole1, 0)
anes_w_classes$RCA_2_2 = ifelse(anes_w_classes$RCA_new == 2, anes_w_classes$Scale2_pole2, 0)

anes_w_classes$classes_w_poles_new = ifelse(anes_w_classes$Scale1_pole1 == 1 & anes_w_classes$RCA_1_new == 1, 
                                            "Conservative", 
                                            "Neutral")
anes_w_classes$classes_w_poles_new = ifelse(anes_w_classes$Scale1_pole1 == 0 & anes_w_classes$RCA_1_new == 1, 
                                            "Progressive", 
                                            anes_w_classes$classes_w_poles_new)
anes_w_classes$classes_w_poles_new <- ifelse(anes_w_classes$Scale2_pole1 == 0 & anes_w_classes$RCA_2_new == 1, 
                                             "Race-Opposing", 
                                             anes_w_classes$classes_w_poles_new)

anes_w_classes$classes_w_poles_new <- ifelse(anes_w_classes$Scale2_pole1 == 1 & anes_w_classes$RCA_2_new == 1, 
                                             "Race-Affirming", 
                                             anes_w_classes$classes_w_poles_new)

# Apply rescale and categorize function dataset
to_plot <- anes_w_classes
to_plot[,att_names_fixed] = rescale_cat(to_plot[,att_names_fixed])
to_plot[,att_names_fixed] = sapply(to_plot[,att_names_fixed], FUN = function(x) as.numeric(as.character(x)))

# Subset to RCA + feeling thermometers
anes_net <- to_plot[,c("RCA_new_cat", att_names_fixed)]

# Use plot.group function to get the network plots (see functions.R for more info)
plot_out <- plot.groups(anes_net, variables = att_names_fixed, group.ids = "RCA_new_cat", sig.level = .05, type = "net", bw = T)

# Set seed so graph is reproducable
# set.seed(100)

# # Set network graph attributes
# plot_out[[1]]$layout <- layout_with_fr(plot_out[[1]])
# V(plot_out[[1]])$label <- V(plot_out[[1]])$name
# V(plot_out[[1]])$label.cex <- 1
# V(plot_out[[1]])$size <- 10

# # Reduce node overlap
# plot_out[[1]]$layout <- layout_rotate(plot_out[[1]]$layout, -90)
# plot_out[[1]] <- reduceLabelOverlap(plot_out[[1]])

# plot_out[[2]]$layout <- layout_with_fr(plot_out[[2]])
# V(plot_out[[2]])$label <- V(plot_out[[2]])$name
# V(plot_out[[2]])$label.cex <- 2
# V(plot_out[[2]])$size <- 10
# plot_out[[2]] <- reduceLabelOverlap(plot_out[[2]])

# plot_out[[3]]$layout <- layout_with_fr(plot_out[[3]])
# V(plot_out[[3]])$label <- V(plot_out[[3]])$name
# V(plot_out[[3]])$label.cex <- 2
# V(plot_out[[3]])$size <- 10
# plot_out[[3]] <- reduceLabelOverlap(plot_out[[3]])

# tiff("Figure_3_neutral_net.tiff", height = 800, width = 800)
# plot(plot_out[[1]], edge.width = abs(E(plot_out[[1]])$weight) * 4,
#      vertex.label.color = "black", vertex.label.cex = 1, vertex.color = "white")
# dev.off()

# tiff("Figure_3_partisan_net.tiff", height = 800, width = 800)
# plot(plot_out[[2]], edge.width = abs(E(plot_out[[1]])$weight) * 4, vertex.label.color = "black", vertex.label.cex = 1, vertex.color = "white")
# dev.off()

# tiff("Figure_3_racial_net.tiff", height = 800, width = 800)
# plot(plot_out[[3]], edge.width = abs(E(plot_out[[1]])$weight) * 4, vertex.label.color = "black", vertex.label.cex = 1, vertex.color = "white")
# dev.off()

# Order the categories using factor
anes_w_classes_for_heatmap <- to_plot
anes_w_classes_for_heatmap$RCA_new_cat <- factor(anes_w_classes_for_heatmap$RCA_new_cat, levels = c("Partisan", "Racial", "Neutral"))

# Plot using heatmap (plots individually and together)
sig_val = 0.05
class_p <- class_heatmaps(anes_w_classes_for_heatmap,
                            variables = att_names_fixed,
                            class.ids = "RCA_new_cat",
                            file_name = "Brensinger2022/output/class_heatmaps_sig_bw",
                            sig_level = sig_val,
                            label_type = "correlations")

# pdf(paste0("class_plots_sig_bw", sig_val, ".pdf"), width = 16, height = 6)
# do.call("grid_arrange_shared_legend", c(class_p, COLNUM = 3))
# dev.off()

tiff("Brensinger2022/output/Figure 3.tiff", width = 1600, height = 600)
do.call("grid_arrange_shared_legend", c(class_p, COLNUM = 3))
dev.off()

## FIGURE 4 - Correlations between Party ID and Attitudes
# Set of variables to be correlated
to_corr = c("party_id", att_names_fixed)

# Subset data to each of the classes
rca1 = subset(anes_w_classes, RCA_new == 1)
rca2 = subset(anes_w_classes, RCA_new == 2)
rca3 = subset(anes_w_classes, RCA_new == 3)

# Run within-class correlations
corr_c1 = cor(rca1[,to_corr], use = "complete.obs")[,"party_id"]
corr_c2 = cor(rca2[,to_corr], use = "complete.obs")[,"party_id"]
corr_c3 = cor(rca3[,to_corr], use = "complete.obs")[,"party_id"]

# Construct as long-form data.frames
df1 = data.frame(Variable = names(corr_c1), Corr = as.numeric(corr_c1), stringsAsFactors = F)
df2 = data.frame(Variable = names(corr_c2), Corr = as.numeric(corr_c2), stringsAsFactors = F)
df3 = data.frame(Variable = names(corr_c3), Corr = as.numeric(corr_c3), stringsAsFactors = F)

# Add in class names
df1$Class = "Partisan"
df2$Class = "Racial"
df3$Class = "Neutral"

# Bind all of them together
to_plot = rbind(df1, df2, df3)
to_plot = subset(to_plot, Variable != "party_id")
to_plot$Class = factor(to_plot$Class, levels = c("Partisan", "Racial", "Neutral"))

ggplot(to_plot, aes(x = Corr, y = reorder(Variable, desc(Variable)) , shape = Class, color = Class)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_vline(aes(xintercept = 0), linetype = "dotted", color = "grey70") +
  scale_linetype_manual(values=c("dashed")) +
  scale_colour_grey() +
  theme_bw(base_size = 14) +
  xlab("Correlation with Party Identification") +
  ylab("") +
guides(linetype = F)

ggsave("Brensinger2022/output/Figure 4.tiff", height=8, width = 8, device = "tiff")


## FIGURE 5 -- Density Plots
att = c("RCA_new_cat", att_names_fixed)
att_df <- anes_w_classes[,att]
att_df_melted <- melt(att_df, id.vars = "RCA_new_cat")
att_df_melted$RCA <- as.character(att_df_melted$RCA_new_cat)

tiff(height = 1000, width = 1600, filename = "Brensinger2022/output/Figure 5.tiff")

ggplot(att_df_melted,
       aes(x=value, linetype = RCA, fill = RCA)) +
  geom_density(alpha=0.5) +
  facet_wrap(~variable, scales = "free") +  scale_fill_grey() + scale_linetype_manual( values = c("solid", "longdash", "dotted")) +
  theme_classic(base_size = 20) +
  xlab("") +
  ylab("Density")

dev.off()


## FIGURE 6 -- Principal Component Scales
g1 <- pca_plot_hist_version(anes_w_classes, 1, title = "Partisan")
g2 <- pca_plot_hist_version(anes_w_classes, 2, title = "Racial")
g3 <- pca_plot_hist_version(anes_w_classes, 3, title = "Neutral")

# pdf("pca_pole_plots_hist_norm.pdf", height = 4, width = 12)
# grid_arrange_shared_legend(g1, g2, g3, COLNUM = 3)
# dev.off()

tiff("Brensinger2022/output/Figure 6.tiff", height = 400, width = 1200)
grid_arrange_shared_legend(g1, g2, g3, COLNUM = 3)
dev.off()


## FIGURE 7 -- COMPARING MEANS USING Z-SCORE
# Get mean responses per class
mrp_att <- mean_response_pattern(anes_w_classes$classes_w_poles_new, anes_w_classes[,att_names_fixed], min_size = 5, zscore = F, byclass = F)


overall_means <- colMeans(anes_w_classes[, att_names_fixed])
overall_sds <- sapply(anes_w_classes[, att_names_fixed], sd)
overall_means <- data.frame(variable = names(overall_means),
                           means = overall_means,
                           sds = overall_sds,
                           stringsAsFactors = F)

# Reshape from wide to long
melted_mrp_att <- melt(mrp_att, id.vars = c("poles", "class"))

melted_mrp_att$SampleMean = overall_means$means[match(melted_mrp_att$variable, overall_means$variable)]
melted_mrp_att$SampleSD = overall_means$sds[match(melted_mrp_att$variable, overall_means$variable)]
melted_mrp_att$final_value = (melted_mrp_att$value-melted_mrp_att$SampleMean)/melted_mrp_att$SampleSD

# Re-order class variable using factor
melted_mrp_att$class <- ifelse(melted_mrp_att$class == "Supportive", "Race-Affirming", melted_mrp_att$class)
melted_mrp_att$class <- ifelse(melted_mrp_att$class == "Critical", "Race-Opposing", melted_mrp_att$class)

melted_mrp_att$class <- factor(melted_mrp_att$class, levels = c("Conservative", "Progressive", "Race-Opposing", "Race-Affirming", "Neutral"))

melted_mrp_att$variable <- as.character(melted_mrp_att$variable)

variable_order <- c("Asian Americans", "Blacks", "Hispanics", "Whites", "Illegal Immigrants", "Jews", "Muslims", 
                    "Gays and Lesbians", "Transgender \nPeople", "Feminists", "Scientists",  "Liberals",   
                    "Conservatives", "Christians", "Christian \nFundamentalists", "Rich People", "Poor People")


all(variable_order %in% melted_mrp_att$variable)

melted_mrp_att$variable <- factor(melted_mrp_att$variable, levels = variable_order)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
color_vals = gg_color_hue(5)

library(cowplot)

partisans = subset(melted_mrp_att, poles %in% c("Conservative", "Progressive"))

p1 = ggplot(partisans, aes(x=variable, y=final_value, fill=class)) + 
  geom_col(width=0.5,    
           position=position_dodge(0.5)) +
  #scale_fill_manual(values=c(color_vals[1], color_vals[2]), name = "Pole") + 
  labs(x="", y = "Average Attitude (Z score)") +
  theme_classic(base_size = 24) +
  scale_fill_grey(name = "Pole") +
  ylim(-1.1, 1.1) + 
  theme(axis.text.x = element_text(size = 16, angle = 90, vjust = 0.5, hjust=1),
        panel.spacing.y = unit(1.5, "lines")) 

# ggsave("Brensinger2022/output/Figure_7_partisans.tiff", width = 16, height = 8, device = "tiff")

racials = subset(melted_mrp_att, poles %in% c("Race-Opposing", "Race-Affirming"))

p2 = ggplot(racials, aes(x=variable, y=final_value, fill=class)) + 
  geom_col(width=0.5,
           position=position_dodge(0.5)) +
  # scale_fill_manual(values=c(color_vals[3], color_vals[4]), name = "Pole") + 
  labs(x="", y = "Average Attitude (Z score)")+
  theme_classic(base_size = 24) +
  scale_fill_grey(name = "Pole") +
  ylim(-1, 1) + 
   theme(axis.text.x = element_text(size = 16, angle = 90, vjust = 0.5, hjust=1),
        panel.spacing.y = unit(1.5, "lines")) 

# ggsave("Brensinger2022/output/Figure_7_racials.tiff", width = 16, height = 8, device = "tiff")


plot_grid(p1, p2, labels = c('Panel A: Partisans', 'Panel B: Racials'), label_size = 30, ncol = 1, align = "v", hjust=c(-0.35, -0.4), vjust=1, scale = 0.95)
ggsave("Brensinger2022/output/Figure 7.tiff", width = 16, height = 16, device = "tiff")

# For Figure 7 footnote:
# function that compares means between two classes or poles
compare_means <- function(df, var, class, class1 = "Pole1", class2 = "Pole2"){
  
  var1 <- df[df[,class] == class1, var]
  var2 <- df[df[,class] == class2, var]
  
  return(t.test(var1, var2))
}

# example for list of vars
# lapply to apply compare means to each variable in the set
t_test_outcomes <- lapply(att_names_fixed, 
                          FUN = function(x) compare_means(anes_w_classes, x, "classes_w_poles_new", "Progressive", "Conservative"))

# change names to reflect variable
names(t_test_outcomes) <- att_names_fixed

# lapply to apply compare means to each variable in the set
t_test_outcomes <- lapply(att_names_fixed, 
                          FUN = function(x) compare_means(anes_w_classes, x, "classes_w_poles_new", "Race-Affirming", "Race-Opposing"))

# change names to reflect variable
names(t_test_outcomes) <- att_names_fixed



## Figure 8: Race by race attitude averages for racial logic
## These plots examine the distribution of feelings of members of various races towards their own and other races using density plots
anes_w_scaled = anes_w_classes
anes_w_scaled[,att_names_fixed] <- sapply(anes_w_scaled[,att_names_fixed], FUN = function(x) scale(x))

# limit data to RCA class 2
class2 <- subset(anes_w_scaled, anes_w_scaled$RCA_new == 2)

# This function allows one to grab the legend of a ggplot graph
grab_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Subset class 2 to each pole
class2_pole1 <- subset(class2, Scale2_pole1 == 0)
class2_pole2 <- subset(class2, Scale2_pole1 == 1)

# FOR POLE 1
# limit the data to relevant variables
att_df <- class2_pole1[,c("racial_cat", att_names_fixed)]
# fix variable names
colnames(att_df)[1] <- c("Race")
# limit further to only race attitudes
att_df <- att_df[,c("Race", "Asian Americans", "Blacks", "Hispanics", "Whites")]
# convert from long to wide form
att_df_melted <- melt(att_df, id.vars = "Race")
# ensure the variable from respondent's own race is a character vector
att_df_melted$Race <- as.character(att_df_melted$Race)
# drop NAs
att_df_melted <- na.omit(att_df_melted)
# take average feeling thermometer towards each race by respondent's own race (we will use this later)
p1_race_means <- aggregate(value ~ Race + variable, data = att_df_melted, FUN = function(x) mean(x, na.rm = T))
p1_race_ses <- aggregate(value ~ Race + variable, data = att_df_melted, FUN = function(x) sd(x, na.rm = T)/sqrt(length(x)))
p1_race_means$se <- p1_race_ses$value
p1_race_means$ci_term <- p1_race_means$se * qnorm(.975)
p1_race_means$UpperCI <- p1_race_means$value + p1_race_means$ci_term
p1_race_means$LowerCI <- p1_race_means$value - p1_race_means$ci_term
p1_race_means$Pole = "Race-Opposing"

# FOR POLE 2
# limit the data to relevant variables
att_df <- class2_pole2[,c("racial_cat", att_names_fixed)]
# fix variable names
colnames(att_df)[1] <- c("Race")
# limit further to only race attitudes
att_df <- att_df[,c("Race", "Asian Americans", "Blacks", "Hispanics", "Whites")]
# convert from long to wide form
att_df_melted <- melt(att_df, id.vars = "Race")
# ensure the variable from respondent's own race is a character vector
att_df_melted$Race <- as.character(att_df_melted$Race)
# drop NAs
att_df_melted <- na.omit(att_df_melted)
# take average feeling thermometer towards each race by respondent's own race (we will use this later)
p2_race_means <- aggregate(value ~ Race + variable, data = att_df_melted, FUN = function(x) mean(x, na.rm = T))
p2_race_ses <- aggregate(value ~ Race + variable, data = att_df_melted, FUN = function(x) sd(x, na.rm = T)/sqrt(length(x)))
p2_race_means$se <- p2_race_ses$value
p2_race_means$ci_term <- p2_race_means$se * qnorm(.975)
p2_race_means$UpperCI <- p2_race_means$value + p2_race_means$ci_term
p2_race_means$LowerCI <- p2_race_means$value - p2_race_means$ci_term
p2_race_means$Pole = "Race-Affirming"

race_means_class2 <- rbind(p1_race_means, p2_race_means)
race_means_class2$Pole = factor(race_means_class2$Pole, levels = c("Race-Opposing", "Race-Affirming"))

tiff("Brensinger2022/output/Figure 8.tiff", height = 600, width = 1200)
p <- ggplot(race_means_class2,
            aes(y=value, x = Race, color = Pole, shape = Pole)) +
      geom_point(size = 5) +   
      facet_grid(Race~variable, scales = "free", switch="y") +  
      scale_color_manual(values = c("black", "grey60")) + 
      geom_errorbar(aes(ymin=LowerCI, ymax=UpperCI), width=0.3, size = 1) + 
      theme_classic(base_size = 20) +
      coord_flip() +
      xlab("") +
      ylab("Feeling Thermometer (Standardized)")  + 
      theme(legend.position="bottom", 
          axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(), 
            panel.spacing.x = unit(2, "lines"))

final_legend <- grab_legend(p)

y.grob <- textGrob("Own Race", 
                   gp=gpar(col="black", fontsize=18), rot=90)

x.grob <- textGrob("Racial Group Thermometer", 
                   gp=gpar(fcol="black", fontsize=18))

grid.arrange(arrangeGrob(p + theme(legend.position="none"), left = y.grob, top = x.grob), final_legend, nrow=2,heights=c(10, 1))

dev.off()


## FIGURE 9 -- Interaction Plot of Race x Strength of Partisanship
plot_df = readRDS("Brensinger2022/output/figure_8_data.RDS")

# Plot results
library(ggplot2)
library(scales)
ggplot(data = plot_df, aes(x = strength_party_id, 
                           y = mean,
                           ymin = lower, ymax = upper,
                           group = as.factor(In),
                           linetype = as.factor(In))) +
  geom_ribbon(alpha = 0.1) +
  geom_line() +
  facet_grid(race ~ RCA_new_3ref) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # % labels
  scale_x_continuous(breaks = c(0:10),
                     minor_breaks = FALSE) +
  scale_linetype_manual(name = "",
                          values = c(1, 2),
                          breaks = c(1,0),
                          labels = c("Members of this Race", "Rest of Sample")) +
  theme_bw(base_size = 18) +
  labs(y = "Predicted Probabilities",
       x = "Strength of Party Identification") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("Brensinger2022/output/Figure 9.tiff", device = "tiff", width = 12, height = 9)
