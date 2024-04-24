# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))

# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))





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
att_names_fixed <- ifelse(att_names_fixed == "GayLesbians", "Gays/Lesbians", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "ChristianFundamentalists", "Christian Fundamentalists", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "Transgender", "Transgenders", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "AsianAmericans", "Asian Americans", att_names_fixed)

colnames(anes_w_classes)[match(att_names, colnames(anes_w_classes))] <- att_names_fixed

##### Table A1 - Descriptive Statistics
anes_w_classes$homsexual_or_bisexual <- ifelse(anes_w_classes$homosexual == 1 | anes_w_classes$bisexual == 1,
                                               1, 0)
vars_to_mean <- c("democrat", "independent", "republican", 
                  "midwest", "northeast", "southern", "west", 
                  "asian", "black", "hispanic", "white", "other_racial",
                  "female", "homsexual_or_bisexual", "age",                        
                  "less_high_school", "high_school_some_college",
                  "bachelors_degree", "advanced_degree",
                  "income_q_1", "income_q_2", "income_q_3",
                  "income_q_4", "income_q_5",
                  "protestant", "black_protestant", "evangelical",                        
                  "catholic", "other_christian", "jewish", 
                  "other_religion", "not_religious")

rca_1_sub = subset(anes_w_classes, anes_w_classes$RCA_new == 1)
mrp_att_class_1 = round(colMeans(rca_1_sub[,vars_to_mean], na.rm = T),2)

rca_1_sub_cons = subset(anes_w_classes, anes_w_classes$RCA_1_1 == 1)
mrp_att_class_1_cons = round(colMeans(rca_1_sub_cons[,vars_to_mean], na.rm = T),2)

rca_1_sub_lib = subset(anes_w_classes, anes_w_classes$RCA_1_2 == 1)
mrp_att_class_1_lib = round(colMeans(rca_1_sub_lib[,vars_to_mean], na.rm = T),2)

rca_2_sub = subset(anes_w_classes, anes_w_classes$RCA_new == 2)
mrp_att_class_2 = round(colMeans(rca_2_sub[,vars_to_mean], na.rm = T),2)

rca_2_sub_egal = subset(anes_w_classes, anes_w_classes$RCA_2_1 == 1)
mrp_att_class_2_egal  = round(colMeans(rca_2_sub_egal [,vars_to_mean], na.rm = T),2)

rca_2_sub_antag = subset(anes_w_classes, anes_w_classes$RCA_2_2 == 1)
mrp_att_class_2_antag  = round(colMeans(rca_2_sub_antag[,vars_to_mean], na.rm = T),2)

rca_3_sub = subset(anes_w_classes, anes_w_classes$RCA_new == 3)
mrp_att_class_3 = round(colMeans(rca_3_sub[,vars_to_mean], na.rm = T),2)

# Include sample mean
mrp_att_round_sample = round(colMeans(anes_w_classes[,vars_to_mean], na.rm = T),2)

mrp_all <- rbind(mrp_att_class_1, mrp_att_class_1_cons, mrp_att_class_1_lib, 
                 mrp_att_class_2, mrp_att_class_2_egal, mrp_att_class_2_antag,
                 mrp_att_class_3, mrp_att_round_sample)

mrp_all <- t(mrp_all)

write.csv(mrp_all, "Brensinger2022/output/TableA1.csv")

#### Table 1 -- SD and Attitude averages
averages_sd = NA
averages_sd$rca1_average = round(mean(colMeans(rca_1_sub[,att_names_fixed]), na.rm = T), 2)
averages_sd$rca2_average = round(mean(colMeans(rca_2_sub[,att_names_fixed]), na.rm = T), 2)
averages_sd$rca3_average = round(mean(colMeans(rca_3_sub[,att_names_fixed]), na.rm = T), 2)
averages_sd$full_average = round(mean(colMeans(anes_w_classes[,att_names_fixed]), na.rm = T), 2)

# SD of attitudes in class
averages_sd$class_1_att_sd = round(sd(colMeans(rca_1_sub[,att_names_fixed])), 2)
averages_sd$class_2_att_sd = round(sd(colMeans(rca_2_sub[,att_names_fixed])), 2)
averages_sd$class_3_att_sd = round(sd(colMeans(rca_3_sub[,att_names_fixed])), 2)
averages_sd$class_sample_att_sd = round(sd(colMeans(anes_w_classes[,att_names_fixed])), 2)

# Within-SD
averages_sd$class_1_within_sd = round(mean(rca_1_sub$within_ind_sd, na.rm = T), 2)
averages_sd$class_2_within_sd = round(mean(rca_2_sub$within_ind_sd, na.rm = T), 2)
averages_sd$class_3_within_sd = round(mean(rca_3_sub$within_ind_sd, na.rm = T), 2)
averages_sd$class_sample_within_sd = round(mean(anes_w_classes$within_ind_sd, na.rm = T), 2)

mean_sd = data.frame(Sample = c("Partisans", "Racials", "Neutrals", "Full Sample"), Averages = c(averages_sd$rca1_average, averages_sd$rca2_average, averages_sd$rca3_average, averages_sd$full_average), SD = c(averages_sd$class_1_att_sd, averages_sd$class_2_att_sd, averages_sd$class_3_att_sd, averages_sd$class_sample_att_sd), Within_In_SD = c(averages_sd$class_1_within_sd, averages_sd$class_2_within_sd, averages_sd$class_3_within_sd, averages_sd$class_sample_within_sd))
write.csv(mean_sd, "Brensinger2022/output/Table1.csv")

############### REGRESSIONS #################
#############################################
# Multinomial logistic regression in R
#############################################

#### Table 2 - BASIC MULTINOMIAL MODELS
# Neutral logic as the reference class
anes_w_classes$RCA_new_3ref <- relevel(as.factor(anes_w_classes$RCA_new), ref = 3)

class_mlogit_rca_3ref <-  multinom(RCA_new_3ref ~  strength_party_id + party_id +
  political_interest + attention_to_news + personal_discrim  +
  church_freq + southern +
  black + asian + hispanic + other_racial +
  non_hetero + female + age + college_grad  +
  income_q_2 + income_q_3 + income_q_4 + income_q_5 +
  black_protestant + evangelical + catholic + jewish + not_religious +
  other_religion + other_christian,
  data = anes_w_classes)

z_3ref <- summary(class_mlogit_rca_3ref)$coefficients/summary(class_mlogit_rca_3ref)$standard.errors
se_3ref <- summary(class_mlogit_rca_3ref)$standard.errors
p_3ref <- (1 - pnorm(abs(z_3ref), 0, 1))*2

# Comparing neutral to partisan logic
partisan_neutral_sum = round(cbind(OR = exp(coef(class_mlogit_rca_3ref))[1,], se = se_3ref[1,], zscore = z_3ref[1,], pvalue = p_3ref[1,]), 3)
partisan_neutral_sum = rbind(c("Pr(Partisan)/Pr(Neutral)","Pr(Partisan)/Pr(Neutral)", "Pr(Partisan)/Pr(Neutral)", "Pr(Partisan)/Pr(Neutral)"), partisan_neutral_sum)

# Comparing neutral to racial logic
racial_neutral_sum = round(cbind(OR = exp(coef(class_mlogit_rca_3ref))[2,], se = se_3ref[2,], zscore = z_3ref[2,], pvalue = p_3ref[2,]), 3)
racial_neutral_sum = rbind(c("Pr(Racial)/(Neutral)","Pr(Racial)/(Neutral)", "Pr(Racial)/(Neutral)", "Pr(Racial)/(Neutral)"), racial_neutral_sum)


# Same steps as above but with racial logic as the reference class
anes_w_classes$RCA_new_2ref <- relevel(as.factor(anes_w_classes$RCA_new), ref = 2)

class_mlogit_rca_2ref <-  multinom(RCA_new_2ref ~ strength_party_id + party_id +
  political_interest + attention_to_news + personal_discrim +
  church_freq + southern +
  black + asian + hispanic + other_racial +
  non_hetero + female + age + college_grad  +
  income_q_2 + income_q_3 + income_q_4 + income_q_5 +
  black_protestant + evangelical + catholic + jewish +
  not_religious + other_religion + other_christian,
  data = anes_w_classes)

z_2ref <- summary(class_mlogit_rca_2ref)$coefficients/summary(class_mlogit_rca_2ref)$standard.errors
se_2ref <- summary(class_mlogit_rca_2ref)$standard.errors
p_2ref <- (1 - pnorm(abs(z_2ref), 0, 1))*2

# Comparing racial to partisan logic
partisan_racial_sum = round(cbind(OR = exp(coef(class_mlogit_rca_2ref))[1,], se = se_2ref[1,], zscore = z_2ref[1,], pvalue  = p_2ref[1,]), 3)
partisan_racial_sum = rbind(c("Pr(Partisan)/Pr(Racial)","Pr(Partisan)/Pr(Racial)", "Pr(Partisan)/Pr(Racial)", "Pr(Partisan)/Pr(Racial)"), partisan_racial_sum)


## Summary statistics (they are the same for both sets of regressions, but just to make sure, we run both)
# AIC
summary(class_mlogit_rca_3ref)$AIC
summary(class_mlogit_rca_2ref)$AIC

# Log likelihood
logLik(class_mlogit_rca_3ref)
logLik(class_mlogit_rca_2ref)

# Number of observations
length(residuals(class_mlogit_rca_3ref))
length(residuals(class_mlogit_rca_2ref))

write.csv(cbind(partisan_racial_sum, partisan_neutral_sum, racial_neutral_sum), "Brensinger2022/output/Table2.csv")


##### Figure 8 - Multinomial logits with strength_party_id x race interaction
# Neutral logic as the reference class
class_mlogit_rca_3ref_pxr <-  multinom(RCA_new_3ref ~ strength_party_id + party_id +
  political_interest + attention_to_news + personal_discrim +
  church_freq + southern +
  black + asian + hispanic + other_racial +
  non_hetero + female + age + college_grad  +
  income_q_2 + income_q_3 + income_q_4 + income_q_5 +
  black_protestant + evangelical + catholic + jewish +
  not_religious + other_religion + other_christian +
  strength_party_id * black + strength_party_id * hispanic +
  strength_party_id * asian + strength_party_id * other_racial,
  data = anes_w_classes,
  Hess = T)

z_3ref_pxr <- summary(class_mlogit_rca_3ref_pxr)$coefficients/summary(class_mlogit_rca_3ref_pxr)$standard.errors
se_3ref_pxr <- summary(class_mlogit_rca_3ref_pxr)$standard.errors
p_3ref_pxr <- (1 - pnorm(abs(z_3ref_pxr), 0, 1))*2

# Comparing neutral to partisan logic
partisan_neutral_sum = round(cbind(OR = exp(coef(class_mlogit_rca_3ref_pxr))[1,],
            se = se_3ref_pxr[1,],
            zscore = z_3ref_pxr[1,],
            pvalue = p_3ref_pxr[1,]), 3)

partisan_neutral_sum = rbind(c("Pr(Partisan)/Pr(Neutral)","Pr(Partisan)/Pr(Neutral)", "Pr(Partisan)/Pr(Neutral)", "Pr(Partisan)/Pr(Neutral)"), partisan_neutral_sum)


# Comparing neutral to racial logic
racial_neutral_sum = round(cbind(OR = exp(coef(class_mlogit_rca_3ref_pxr))[2,],
            se =se_3ref_pxr[2,],
            zscore = z_3ref_pxr[2,],
            pvalue = p_3ref_pxr[2,]), 3)

racial_neutral_sum = rbind(c("Pr(Racial)/(Neutral)","Pr(Racial)/(Neutral)", "Pr(Racial)/(Neutral)", "Pr(Racial)/(Neutral)"), racial_neutral_sum)


# Same steps with racial logic as the reference class
class_mlogit_rca_2ref_pxr <-  multinom(RCA_new_2ref ~ strength_party_id + party_id +
  political_interest + attention_to_news + personal_discrim  +
  church_freq + southern +
  black + asian + hispanic + other_racial +
  non_hetero + female + age + college_grad  +
  income_q_2 + income_q_3 + income_q_4 + income_q_5 +
  black_protestant + evangelical + catholic + jewish +
  not_religious + other_religion + other_christian +
  strength_party_id * black + strength_party_id * hispanic +
  strength_party_id * asian + strength_party_id * other_racial,
  data = anes_w_classes)

z_2ref_pxr <- summary(class_mlogit_rca_2ref_pxr)$coefficients/summary(class_mlogit_rca_2ref_pxr)$standard.errors
se_2ref_pxr <- summary(class_mlogit_rca_2ref_pxr)$standard.errors
p_2ref_pxr <- (1 - pnorm(abs(z_2ref_pxr), 0, 1))*2

# Comparing racial to partisan logic
partisan_racial_sum = round(cbind(OR = exp(coef(class_mlogit_rca_2ref_pxr))[1,],
            se = se_2ref_pxr[1,],
            zscore = z_2ref_pxr[1,],
            pvalue = p_2ref_pxr[1,]), 3)

partisan_racial_sum = rbind(c("Pr(Partisan)/Pr(Racial)","Pr(Partisan)/Pr(Racial)", "Pr(Partisan)/Pr(Racial)", "Pr(Partisan)/Pr(Racial)"), partisan_racial_sum)


# AIC
summary(class_mlogit_rca_3ref_pxr)$AIC
summary(class_mlogit_rca_2ref_pxr)$AIC

# Log likelihood
logLik(class_mlogit_rca_3ref_pxr)
logLik(class_mlogit_rca_2ref_pxr)

# Number of observations
length(residuals(class_mlogit_rca_3ref_pxr))
length(residuals(class_mlogit_rca_2ref_pxr))

write.csv(cbind(partisan_racial_sum, partisan_neutral_sum, racial_neutral_sum), "Brensinger2022/output/TableA2.csv")


### Grab coefficients and generate predictions for Figure 8

## Comparing neutral to partisan logic
# Grab intercept and race coefficients
int1 <- exp(coef(class_mlogit_rca_3ref_pxr))[1,c(1,9:12)]

# Create intercepts (by race)
white_int1 <- int1[1]
black_int1 <- int1[1]*int1[2]
asian_int1 <- int1[1]*int1[3]
hisp_int1 <- int1[1]*int1[4]
other_int1 <- int1[1]*int1[5]

int1 <- coef(class_mlogit_rca_3ref_pxr)[1,c(1,9:12)]

# Create intercepts (by race)
white_int1 <- int1[1]
black_int1 <- int1[1] + int1[2]
asian_int1 <- int1[1] + int1[3]
hisp_int1 <- int1[1] + int1[4]
other_int1 <- int1[1] + int1[5]

strength_coef1 <- coef(class_mlogit_rca_3ref_pxr)[1,c(2, 28:31)]

white_str_coef1 <- strength_coef1[1]
black_str_coef1 <- strength_coef1[2]
asian_str_coef1 <- strength_coef1[4]
hisp_str_coef1 <-  strength_coef1[3]
other_str_coef1 <- strength_coef1[5]


coef_vals <- coef(class_mlogit_rca_3ref_pxr)[1,c(1,9:12, 2, 28:31)]

df_start <- matrix(c(1, 0, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0, 
  1, 0, 1, 0, 0, 0,
  1, 0, 0, 1, 0, 0,
  1, 0, 0, 0, 1, 0), nrow = 5, byrow = T)

df_start2 <- df_start
df_start2[,ncol(df_start2)] = 1
df_start3 <- df_start
df_start3[,ncol(df_start3)] = 2
df_start4 <- df_start
df_start4[,ncol(df_start4)] = 3
final_df <- rbind(df_start, df_start2, df_start3, df_start4)
final_df <- as.data.frame(final_df)
colnames(final_df) = c("int", "black", "hispanic", "asian", "other", "strength")

final_df$strengthxblack <- final_df$strength * final_df$black
final_df$strengthxhispanic <- final_df$strength * final_df$hispanic
final_df$strengthxasian <- final_df$strength * final_df$asian
final_df$strengthxother <- final_df$strength * final_df$other


probs <- c()
for(i in 1:nrow(final_df)){
  probs <- c(probs, inv.logit(sum(final_df[i,] * coef_vals)))
}
matrix(probs, ncol = 5, byrow = T)

# Create predictions
ind_coef1 <- c(white_int1, black_int1, asian_int1, hisp_int1, other_int1)
ind_lean_coef1 <- c(white_int1 + white_str_coef1, black_int1*black_str_coef1, asian_int1*asian_str_coef1, hisp_int1*hisp_str_coef1, other_int1*other_str_coef1)
not_very_coef1 <- c(white_int1*white_str_coef1^2, black_int1*black_str_coef1^2, asian_int1*asian_str_coef1^2, hisp_int1*hisp_str_coef1^2, other_int1*other_str_coef1^2)
strong_coef1 <- c(white_int1*white_str_coef1^3, black_int1*black_str_coef1^3, asian_int1*asian_str_coef1^3, hisp_int1*hisp_str_coef1^3, other_int1*other_str_coef1^3)



# Grab strength_party_id and race interaction coefficients
strength_coef1 <- exp(coef(class_mlogit_rca_3ref_pxr))[1,c(2, 28:31)]

# Create strength_party_id coefficients (by race)
white_str_coef1 <- strength_coef1[1]
black_str_coef1 <- strength_coef1[1]*strength_coef1[2]
asian_str_coef1 <- strength_coef1[1]*strength_coef1[4]
hisp_str_coef1 <- strength_coef1[1]*strength_coef1[3]
other_str_coef1 <- strength_coef1[1]*strength_coef1[5]

# Create predictions
ind_coef1 <- c(white_int1, black_int1, asian_int1, hisp_int1, other_int1)
ind_lean_coef1 <- c(white_int1*white_str_coef1, black_int1*black_str_coef1, asian_int1*asian_str_coef1, hisp_int1*hisp_str_coef1, other_int1*other_str_coef1)
not_very_coef1 <- c(white_int1*white_str_coef1^2, black_int1*black_str_coef1^2, asian_int1*asian_str_coef1^2, hisp_int1*hisp_str_coef1^2, other_int1*other_str_coef1^2)
strong_coef1 <- c(white_int1*white_str_coef1^3, black_int1*black_str_coef1^3, asian_int1*asian_str_coef1^3, hisp_int1*hisp_str_coef1^3, other_int1*other_str_coef1^3)

# Create data frame
strength_x_race_df1 = data.frame('Independent' = ind_coef1,
                                'Independent leaning' = ind_lean_coef1,
                                'Not very strong ID' = not_very_coef1,
                                'Strong ID' = strong_coef1,
                                Race = c("Whites", "Blacks", "Asians", "Hispanics", "Other"),
                                stringsAsFactors = F)

# trial
coef(class_mlogit_rca_3ref_pxr)

df <- data.frame()

pred1 <- mnl_pred_ova(model = class_mlogit_rca_3ref_pxr,
                      data = anes_w_classes,
                      x = "black",
                      by = 1,
                      seed = "random", # default
                      nsim = 100, # faster
                      probs = c(0.025, 0.975)) # default


fdif2 <- mnl_fd_ova(model = class_mlogit_rca_3ref_pxr,
                    data = anes_w_classes,
                    x = "strength_party_id",
                    by = 1,
                    z = "black",
                    z_values = c(0,1),
                    seed = 68159,
                    nsim = 100)

fdif2_black <- mnl_fd_ova(model = class_mlogit_rca_3ref_pxr,
                    data = anes_w_classes,
                    x = "strength_party_id",
                    by = 1,
                    z = "black",
                    z_values = c(0,1),
                    seed = 68159,
                    nsim = 100)

fdif2_hispanic <- mnl_fd_ova(model = class_mlogit_rca_3ref_pxr,
                    data = anes_w_classes,
                    x = "strength_party_id",
                    by = 1,
                    z = "hispanic",
                    z_values = c(0,1),
                    seed = 68159,
                    nsim = 100)


fdif2_asian <- mnl_fd_ova(model = class_mlogit_rca_3ref_pxr,
                    data = anes_w_classes,
                    x = "strength_party_id",
                    by = 1,
                    z = "asian",
                    z_values = c(0,1),
                    seed = 68159,
                    nsim = 100)


fdif2_other <- mnl_fd_ova(model = class_mlogit_rca_3ref_pxr,
                    data = anes_w_classes,
                    x = "strength_party_id",
                    by = 1,
                    z = "other_racial",
                    z_values = c(0,1),
                    seed = 68159,
                    nsim = 100)

black_df <- fdif2_black$plotdata
black_df$race <- "Black"
colnames(black_df)[3] = "In"

hispanic_df <- fdif2_hispanic$plotdata
hispanic_df$race <- "Hispanic"
colnames(hispanic_df)[3] = "In"

asian_df <- fdif2_asian$plotdata
asian_df$race <- "Asian"
colnames(asian_df)[3] = "In"

other_df <- fdif2_other$plotdata
other_df$race <- "Other"
colnames(other_df)[3] = "In"

plot_df <- rbind(black_df, 
      hispanic_df,
      asian_df,
      other_df)

plot_df$RCA_new_3ref <- as.character(plot_df$RCA_new_3ref)

plot_df$RCA_new_3ref <- ifelse(plot_df$RCA_new_3ref == "3", "Neutrals", plot_df$RCA_new_3ref)
plot_df$RCA_new_3ref <- ifelse(plot_df$RCA_new_3ref == "2", "Racials", plot_df$RCA_new_3ref)
plot_df$RCA_new_3ref <- ifelse(plot_df$RCA_new_3ref == "1", "Partisans", plot_df$RCA_new_3ref)

plot_df$RCA_new_3ref <- factor(plot_df$RCA_new_3ref, levels = c("Partisans",
                                                                "Racials",
                                                                "Neutrals"))

saveRDS(plot_df, "Brensinger2022/output/figure_8_data.RDS")

strength_x_race_df1 <- gather(strength_x_race_df1, Strength_ID, OR, -Race)

## Comparing racial to partisan logic
# Grab intercept and race coefficients
int2 <- exp(coef(class_mlogit_rca_2ref_pxr))[1,c(1,9:12)]

# Create intercepts (by race)
white_int2 <- int2[1]
black_int2 <- int2[1]*int2[2]
asian_int2 <- int2[1]*int2[3]
hisp_int2 <- int2[1]*int2[4]
other_int2 <- int2[1]*int2[5]

# Grab strength_party_id and race interaction coefficients
strength_coef2 <- exp(coef(class_mlogit_rca_2ref_pxr))[1,c(2, 28:31)]

# Create strength_party_id coefficients (by race)
white_str_coef2 <- strength_coef2[1]
black_str_coef2 <- strength_coef2[1]*strength_coef2[2]
asian_str_coef2 <- strength_coef2[1]*strength_coef2[4]
hisp_str_coef2 <- strength_coef2[1]*strength_coef2[3]
other_str_coef2 <- strength_coef2[1]*strength_coef2[5]

# Create predictions
ind_coef2 <- c(white_int2, black_int2, asian_int2, hisp_int2, other_int2)
ind_lean_coef2 <- c(white_int2*white_str_coef2, black_int2*black_str_coef2, asian_int2*asian_str_coef2, hisp_int2*hisp_str_coef2, other_int2*other_str_coef2)
not_very_coef2 <- c(white_int2*white_str_coef2^2, black_int2*black_str_coef2^2, asian_int2*asian_str_coef2^2, hisp_int2*hisp_str_coef2^2, other_int2*other_str_coef2^2)
strong_coef2 <- c(white_int2*white_str_coef2^3, black_int2*black_str_coef2^3, asian_int2*asian_str_coef2^3, hisp_int2*hisp_str_coef2^3, other_int2*other_str_coef2^3)

# Create data frame
strength_x_race_df2 = data.frame('Independent' = ind_coef2,
                                 'Independent leaning' = ind_lean_coef2,
                                 'Not very strong ID' = not_very_coef2,
                                 'Strong ID' = strong_coef2,
                                 Race = c("Whites", "Blacks", "Asians", "Hispanics", "Other"),
                                 stringsAsFactors = F)

strength_x_race_df2 <- gather(strength_x_race_df2, Strength_ID, OR, -Race)

# Combine DFs from neutral and racial models into single DF for Figure 8
strength_x_race_df <- bind_rows("Panel A: Partisan vs. Racial Logic" = strength_x_race_df2, "Panel B: Partisan vs. Neutral Logic" = strength_x_race_df1, .id = "Class")


##### Table 3 -- Predicting poles within classes
class1 <- subset(anes_w_classes, anes_w_classes$RCA_new == 1)
class2 <- subset(anes_w_classes, anes_w_classes$RCA_new == 2)

# Logits
# Function to extract proper ses from glm output
se.coef <- function(glm.output){
  sqrt(diag(vcov(glm.output)))
}

m1_log <- glm(Scale1_pole1 ~ strength_party_id + party_id + 
                political_interest + attention_to_news + personal_discrim  + 
                church_freq + southern + black + asian + hispanic + other_racial +
                non_hetero + female + age + college_grad  + income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                black_protestant + evangelical + catholic + jewish + not_religious + other_religion + other_christian, 
              data = class1, family = binomial(link = "logit"))


#Get the odds ratios
ors_m1_log <- exp(coef(m1_log))

# Get the ses for the ors by multiplying ors by se.coef output
or_ses_m1_log <-  ors_m1_log * se.coef(m1_log)

# calculate p-values using log-odds (inapproriate to do so using odds)
pvalue_m1_log <- 2*pnorm(abs(coef(m1_log)/se.coef(m1_log)), lower.tail = F)

m2_log <- glm(Scale2_pole1 ~ strength_party_id + party_id + 
                political_interest + attention_to_news + personal_discrim  + 
                church_freq + southern + black + asian + hispanic + other_racial +
                non_hetero + female + age + college_grad  + income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                black_protestant + evangelical + catholic + jewish + not_religious + other_religion + other_christian, 
              data = class2, family = binomial(link = "logit"))

# Get the odds ratios
ors_m2_log <- exp(coef(m2_log))

# Get the ses for the ors by multiplying ors by se.coef output
or_ses_m2_log <-  ors_m2_log * se.coef(m2_log)

# calculate p-values using log-odds (inapproriate to do so using odds)
pvalue_m2_log <- 2*pnorm(abs(coef(m2_log)/se.coef(m2_log)), lower.tail = F)

stargazer(m1_log, m2_log, coef=list(ors_m1_log, ors_m2_log),  se = list(or_ses_m1_log, or_ses_m2_log), p = list(pvalue_m1_log, pvalue_m2_log), t.auto=F, p.auto=F, type = "text",  no.space = T, out = "Brensinger2022/output/Table3.txt")
