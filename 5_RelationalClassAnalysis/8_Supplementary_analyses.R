### Supplementary Analyses

cca <- function (dtf, filter.significance = F,
                 filter.value = 0.01,
                 zero.action = c("ownclass", "drop")) {
  require(igraph)
  cormat <- .make.cormat(dtf, zero.action)

  if (filter.significance == TRUE) {
    cormat <- .filter.insignif (cormat, ncol(dtf), pcutoff = filter.value)
  }

  graph <- .cormat.to.igraph (cormat, absolute.value = TRUE)
  comm <- leading.eigenvector.community(graph)

  modules <- .separate(attr(cormat, "dtf"), comm$membership)

  val <- list (membership = comm$membership, modules = modules, cormat = cormat)
  class (val) <- "cca"

  return (invisible(val))
}

.make.cormat <- function (dtf, zero.action) {
  # Helper function.  Make a correlation matrix from data frame.
  if (!all(sapply(dtf, is.numeric))) dtf2 <- data.frame(sapply(dtf, as.numeric))
  else dtf2 <- dtf

  # Floating point imprecision may make 0-variance rows appear to have variance slightly higher than 0.
  zeros <- which(apply(dtf2, 1, var) <= 0.000000001)

  if (zero.action[1] == "drop" & (length(zeros) > 0)) {
    dtf2 <- dtf2[-zeros,]
  }

  rv <- abs(cor(t(dtf2)))

  attributes(rv)$dtf <- dtf2

  if ((zero.action[1] == "ownclass") & length(zeros) > 0) {
    rv[zeros,] <- 0
    rv[,zeros] <- 0
    rv[zeros,zeros] <- 1
  }

  diag(rv) <- 0

  return (rv)
}

.filter.insignif <- function (corr, N.vars, pcutoff = 0.05) {

  corr <- abs(corr)

  tvalues <- corr * sqrt ((N.vars-2) / (1 - corr^2))
  if (any(is.infinite(tvalues))) {
    tvalues[is.infinite(tvalues)] <- 9999 # a very big number
  }
  cutoff <- abs(qt(pcutoff / 2, N.vars))

  corr[tvalues < cutoff] <- 0

  return (corr)
}

.cormat.to.igraph <- function (corr, absolute.value = TRUE) {
  if (absolute.value)
    corr <- abs(corr)
  diag(corr) <- 0
  graph <- graph.adjacency(corr, mode="undirected", weighted = TRUE, diag = FALSE)
  return(graph)
}

.separate <- function (dtf, membership) {
  ids <- sort(unique(membership))
  modules <- list()
  if (class(dtf) == "matrix") {
    rownames(dtf) <- NULL
    dtf <- data.frame(dtf)
  }

  for (i in 1:length(ids)) {
    curmod <- list()
    curmod$dtf <- dtf[membership == ids[i],]
    curmod$cormat <- cor(curmod$dtf)
    curmod$degenerate <- any(is.na(curmod$cormat))
    modules[[i]] <- curmod
  }

  return (modules)
}

# Read in data
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
att_names_fixed <- ifelse(att_names_fixed == "GayLesbians", "Gays/Lesbians", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "ChristianFundamentalists", "Christian Fundamentalists", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "Transgender", "Transgenders", att_names_fixed)
att_names_fixed <- ifelse(att_names_fixed == "AsianAmericans", "Asian Americans", att_names_fixed)

colnames(anes_w_classes)[match(att_names, colnames(anes_w_classes))] <- att_names_fixed

#### Table S5: LPMS without interactions
class_lpm_rca_1 <-  lm(RCA_1_new ~  strength_party_id + party_id +
                         political_interest + attention_to_news + personal_discrim  +
                         church_freq + southern +
                         black + asian + hispanic + other_racial +
                         non_hetero + female + age + college_grad  +
                         income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                         black_protestant + evangelical + catholic + jewish + not_religious +
                         other_religion + other_christian,
                       data = anes_w_classes)

class_lpm_rca_2 <-  lm(RCA_2_new ~  strength_party_id + party_id +
                         political_interest + attention_to_news + personal_discrim  +
                         church_freq + southern +
                         black + asian + hispanic + other_racial +
                         non_hetero + female + age + college_grad  +
                         income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                         black_protestant + evangelical + catholic + jewish + not_religious +
                         other_religion + other_christian,
                       data = anes_w_classes)

class_lpm_rca_3 <-  lm(RCA_3_new ~  strength_party_id + party_id +
                         political_interest + attention_to_news + personal_discrim  +
                         church_freq + southern +
                         black + asian + hispanic + other_racial +
                         non_hetero + female + age + college_grad  +
                         income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                         black_protestant + evangelical + catholic + jewish + not_religious +
                         other_religion + other_christian ,
                       data = anes_w_classes)

##### LPMs with strength_party_id x race interaction
class_lpm_rca_1_pxr <-  lm(RCA_1_new ~  strength_party_id + party_id +
                             political_interest + attention_to_news + personal_discrim  +
                             church_freq + southern +
                             black + asian + hispanic + other_racial +
                             non_hetero + female + age + college_grad  +
                             income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                             black_protestant + evangelical + catholic + jewish + not_religious +
                             other_religion + other_christian +
                             strength_party_id * black + strength_party_id * hispanic +
                             strength_party_id * asian + strength_party_id * other_racial,
                           data = anes_w_classes)

class_lpm_rca_2_pxr <-  lm(RCA_2_new ~  strength_party_id + party_id +
                             political_interest + attention_to_news + personal_discrim  +
                             church_freq + southern +
                             black + asian + hispanic + other_racial +
                             non_hetero + female + age + college_grad  +
                             income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                             black_protestant + evangelical + catholic + jewish + not_religious +
                             other_religion + other_christian +
                             strength_party_id * black + strength_party_id * hispanic +
                             strength_party_id * asian + strength_party_id * other_racial,
                           data = anes_w_classes)

class_lpm_rca_3_pxr <-  lm(RCA_3_new ~  strength_party_id + party_id +
                             political_interest + attention_to_news + personal_discrim  +
                             church_freq + southern +
                             black + asian + hispanic + other_racial +
                             non_hetero + female + age + college_grad  +
                             income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                             black_protestant + evangelical + catholic + jewish + not_religious +
                             other_religion + other_christian +
                             strength_party_id * black + strength_party_id * hispanic +
                             strength_party_id * asian + strength_party_id * other_racial,
                           data = anes_w_classes)



stargazer(class_lpm_rca_1, class_lpm_rca_2, class_lpm_rca_3,
          class_lpm_rca_1_pxr, class_lpm_rca_2_pxr, class_lpm_rca_3_pxr, 
          type = "text", out = "Brensinger2022/output/TableS5.txt")


##### Table S7: Linear Probability Models Predicting Pole Membership within Partisan and Racial Logics
# Load in stargazer
library(stargazer)

# Function to extract proper ses from glm output
se.coef <- function(glm.output){
  sqrt(diag(vcov(glm.output)))
}

## 1. run descriptive regressions predicting pole membership as a function of demographic variables using an LPM
## Within Class 1
m1_lpm <- lm(Scale1_pole1 ~ strength_party_id + party_id + 
           political_interest + attention_to_news + personal_discrim  + 
           church_freq + southern + black + asian + hispanic + other_racial +
           non_hetero + female + age + college_grad  + income_q_2 + income_q_3 + income_q_4 + income_q_5 +
           black_protestant + evangelical + catholic + jewish + not_religious + other_religion + other_christian, 
         data = rca_1_sub)

m1_lpm_pxr <- lm(Scale1_pole1 ~  strength_party_id + party_id +
                             political_interest + attention_to_news + personal_discrim  +
                             church_freq + southern +
                             black + asian + hispanic + other_racial +
                             non_hetero + female + age + college_grad  +
                             income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                             black_protestant + evangelical + catholic + jewish + not_religious +
                             other_religion + other_christian +
                             strength_party_id * black + strength_party_id * hispanic +
                             strength_party_id * asian + strength_party_id * other_racial, data = rca_1_sub)

## Within Class 2
m2_lpm <- lm(Scale2_pole1 ~ strength_party_id + party_id + 
           political_interest + attention_to_news + personal_discrim  + 
           church_freq + southern + black + asian + hispanic + other_racial +
           non_hetero + female + age + college_grad  + income_q_2 + income_q_3 + income_q_4 + income_q_5 +
           black_protestant + evangelical + catholic + jewish + not_religious + other_religion + other_christian, 
         data = rca_2_sub)

m2_lpm_pxr <- lm(Scale2_pole1 ~ strength_party_id + party_id +
                             political_interest + attention_to_news + personal_discrim  +
                             church_freq + southern +
                             black + asian + hispanic + other_racial +
                             non_hetero + female + age + college_grad  +
                             income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                             black_protestant + evangelical + catholic + jewish + not_religious +
                             other_religion + other_christian +
                             strength_party_id * black + strength_party_id * hispanic +
                             strength_party_id * asian + strength_party_id * other_racial,
                             data = rca_2_sub)

# Report results
stargazer(m1_lpm, m1_lpm_pxr, m2_lpm, m2_lpm_pxr,  type = "text",   no.space = T, out = "TableS7.txt")

##### TABLE A3: Running multinomial logit with strength of party ID x race interaction (NON-REPUBLICANS ONLY)
# Subset data to include only Non-Republicans
ind_dem_sub = subset(anes_w_classes, anes_w_classes$party_id < 5)

# Neutral logic as the reference class
ind_dem_sub$RCA_new_3ref <- relevel(as.factor(ind_dem_sub$RCA_new), ref = 3)

class_mlogit_rca_3ref_ind_dem <- multinom(RCA_new_3ref ~ strength_party_id + party_id +
  political_interest + personal_discrim + attention_to_news +
  black + asian + hispanic + other_racial +
  southern + non_hetero + college_grad +
  church_freq + female + age +
  income_q_2 + income_q_3 + income_q_4 + income_q_5 +
  black_protestant + evangelical + catholic + jewish +
  not_religious + other_religion + other_christian +
  strength_party_id * black + strength_party_id * hispanic +
  strength_party_id * asian + strength_party_id * other_racial,
  data = ind_dem_sub)

z_3ref_ind_dem <- summary(class_mlogit_rca_3ref_ind_dem)$coefficients/summary(class_mlogit_rca_3ref_ind_dem)$standard.errors
se_3ref_ind_dem <- summary(class_mlogit_rca_3ref_ind_dem)$standard.errors
p_3ref_ind_dem <- (1 - pnorm(abs(z_3ref_ind_dem), 0, 1))*2

# Comparing neutral to partisan logic
partisan_neutral_sum = round(cbind(OR = exp(coef(class_mlogit_rca_3ref_ind_dem))[1,], se =se_3ref_ind_dem[1,],  zscore = z_3ref_ind_dem[1,], pvalue = p_3ref_ind_dem[1,]), 3)
partisan_neutral_sum = rbind(c("Pr(Partisan)/Pr(Neutral)","Pr(Partisan)/Pr(Neutral)", "Pr(Partisan)/Pr(Neutral)", "Pr(Partisan)/Pr(Neutral)"), partisan_neutral_sum)

# Comparing neutral to racial logic
racial_neutral_sum = round(cbind(OR = exp(coef(class_mlogit_rca_3ref_ind_dem))[2,], se =se_3ref_ind_dem[2,], zscore = z_3ref_ind_dem[2,], pvalue = p_3ref_ind_dem[2,]), 3)
racial_neutral_sum = rbind(c("Pr(Racial)/(Neutral)","Pr(Racial)/(Neutral)", "Pr(Racial)/(Neutral)", "Pr(Racial)/(Neutral)"), racial_neutral_sum)

# Same as above, but with racial logic as reference class
ind_dem_sub$RCA_new_2ref <- relevel(as.factor(ind_dem_sub$RCA_new), ref = 2)

class_mlogit_rca_2ref_ind_dem <-  multinom(RCA_new_2ref ~ strength_party_id + party_id +
  political_interest + personal_discrim + attention_to_news +
  black + asian + hispanic + other_racial +
  southern + non_hetero + college_grad +
  church_freq + female + age +
  income_q_2 + income_q_3 + income_q_4 + income_q_5 +
  black_protestant + evangelical + catholic + jewish +
  not_religious + other_religion + other_christian +
  strength_party_id * black + strength_party_id * hispanic +
  strength_party_id * asian + strength_party_id * other_racial,
  data = ind_dem_sub)

z_2ref_ind_dem <- summary(class_mlogit_rca_2ref_ind_dem)$coefficients/summary(class_mlogit_rca_2ref_ind_dem)$standard.errors
se_2ref_ind_dem <- summary(class_mlogit_rca_2ref_ind_dem)$standard.errors
p_2ref_ind_dem <- (1 - pnorm(abs(z_2ref_ind_dem), 0, 1))*2

# Comparing racial to partisan logic
partisan_racial_sum = round(cbind(OR = exp(coef(class_mlogit_rca_2ref_ind_dem))[1,], se = se_2ref_ind_dem[1,], zscore = z_2ref_ind_dem[1,], pvalue = p_2ref_ind_dem[1,]), 3)
partisan_racial_sum = rbind(c("Pr(Partisan)/Pr(Racial)","Pr(Partisan)/Pr(Racial)", "Pr(Partisan)/Pr(Racial)", "Pr(Partisan)/Pr(Racial)"), partisan_racial_sum)

# AIC
summary(class_mlogit_rca_3ref_ind_dem)$AIC
summary(class_mlogit_rca_2ref_ind_dem)$AIC

# Log likelihood
logLik(class_mlogit_rca_3ref_ind_dem)
logLik(class_mlogit_rca_2ref_ind_dem)

# Number of observations
length(residuals(class_mlogit_rca_3ref_ind_dem))
length(residuals(class_mlogit_rca_2ref_ind_dem))

write.csv(cbind(partisan_racial_sum, partisan_neutral_sum, racial_neutral_sum), "TableA3.csv")


#### Table A5 - Multinomial Logistic Regression Models Predicting RCA Class Assignment as a Function of “Don’t Know” Responses
# Neutral logic as the reference class
anes_w_classes$RCA_new_3ref <- relevel(as.factor(anes_w_classes$RCA_new), ref = 3)

#########//////// add in exactly fifty
class_mlogit_rca_3ref <-  multinom(RCA_new_3ref ~  total_dks_all_qs + 
                                     pol_dk_qs +
                                     strength_party_id + party_id +
                                     political_interest + 
                                     attention_to_news + personal_discrim  +
                                     southern + 
                                     black + asian + hispanic + other_racial +
                                     female + non_hetero + age + college_grad  +
                                     income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                                     church_freq + black_protestant + evangelical + catholic + jewish + not_religious +
                                     other_religion + other_christian,
                                   data = anes_w_classes)

z_3ref <- summary(class_mlogit_rca_3ref)$coefficients/summary(class_mlogit_rca_3ref)$standard.errors
se_3ref <- summary(class_mlogit_rca_3ref)$standard.errors
p_3ref <- (1 - pnorm(abs(z_3ref), 0, 1))*2

# Comparing neutral to partisan logic
partisan_neutral_sum = round(cbind(OR = exp(coef(class_mlogit_rca_3ref))[1,], se = se_3ref[1,], zscore = z_3ref[1,], pvalue = p_3ref[1,]), 3)
partisan_neutral_sum = rbind(c("Pr(Partisan)/Pr(Neutral)","Pr(Partisan)/Pr(Neutral)", "Pr(Partisan)/Pr(Neutral)", "Pr(Partisan)/Pr(Neutral)"), partisan_neutral_sum)

# Comparing neutral to racial logic
racialist_neutral_sum = round(cbind(OR = exp(coef(class_mlogit_rca_3ref))[2,], se = se_3ref[2,], zscore = z_3ref[2,], pvalue = p_3ref[2,]), 3)
racialist_neutral_sum = rbind(c("Pr(Racialist)/(Neutral)","Pr(Racialist)/(Neutral)", "Pr(Racialist)/(Neutral)", "Pr(Racialist)/(Neutral)"), racialist_neutral_sum)

# Same steps as above but with racial logic as the reference class
anes_w_classes$RCA_new_2ref <- relevel(as.factor(anes_w_classes$RCA_new), ref = 2)

class_mlogit_rca_2ref <-  multinom(RCA_new_2ref ~ total_dks_all_qs + 
                                     pol_dk_qs +
                                     strength_party_id + party_id +
                                     political_interest + 
                                     attention_to_news + personal_discrim  +
                                     southern + 
                                     black + asian + hispanic + other_racial +
                                     female + non_hetero + age + college_grad  +
                                     income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                                     church_freq + black_protestant + evangelical + catholic + jewish + not_religious +
                                     other_religion + other_christian,
                                   data = anes_w_classes)

z_2ref <- summary(class_mlogit_rca_2ref)$coefficients/summary(class_mlogit_rca_2ref)$standard.errors
se_2ref <- summary(class_mlogit_rca_2ref)$standard.errors
p_2ref <- (1 - pnorm(abs(z_2ref), 0, 1))*2

# Comparing racial to partisan logic
partisan_racialist_sum = round(cbind(OR = exp(coef(class_mlogit_rca_2ref))[1,], se = se_2ref[1,], zscore = z_2ref[1,], pvalue  = p_2ref[1,]), 3)
partisan_racialist_sum = rbind(c("Pr(Partisan)/Pr(Racialist)","Pr(Partisan)/Pr(Racialist)", "Pr(Partisan)/Pr(Racialist)", "Pr(Partisan)/Pr(Racialist)"), partisan_racialist_sum)

## Summary statistics (they are the same for both sets of regressions, but just to make sure, we run both)
# AIC
summary(class_mlogit_rca_3ref)$AIC
summary(class_mlogit_rca_2ref)$AIC

# Log likelihood
logLik(class_mlogit_rca_3ref)
logLik(class_mlogit_rca_2ref)

# Number of observations
length(residuals(class_mlogit_rca_3ref)[,1])
length(residuals(class_mlogit_rca_2ref)[,1])

write.csv(cbind(partisan_racialist_sum, partisan_neutral_sum, racialist_neutral_sum), "TableA5.csv")


#### Table S5: LPMS without interactions
class_lpm_rca_1_dk <-  lm(RCA_1_new ~  total_dks_all_qs + pol_dk_qs +
                         strength_party_id + party_id +
                         political_interest + attention_to_news + personal_discrim  +
                         church_freq + southern +
                         black + asian + hispanic + other_racial +
                         non_hetero + female + age + college_grad  +
                         income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                         black_protestant + evangelical + catholic + jewish + not_religious +
                         other_religion + other_christian,
                       data = anes_w_classes)

class_lpm_rca_2_dk <-  lm(RCA_2_new ~  total_dks_all_qs + pol_dk_qs +
                         strength_party_id + party_id +
                         political_interest + attention_to_news + personal_discrim  +
                         church_freq + southern +
                         black + asian + hispanic + other_racial +
                         non_hetero + female + age + college_grad  +
                         income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                         black_protestant + evangelical + catholic + jewish + not_religious +
                         other_religion + other_christian,
                       data = anes_w_classes)

class_lpm_rca_3_dk <-  lm(RCA_3_new ~  total_dks_all_qs + pol_dk_qs +
                         strength_party_id + party_id +
                         political_interest + attention_to_news + personal_discrim  +
                         church_freq + southern +
                         black + asian + hispanic + other_racial +
                         non_hetero + female + age + college_grad  +
                         income_q_2 + income_q_3 + income_q_4 + income_q_5 +
                         black_protestant + evangelical + catholic + jewish + not_religious +
                         other_religion + other_christian ,
                       data = anes_w_classes)

stargazer(class_lpm_rca_1_dk, class_lpm_rca_2_dk, class_lpm_rca_3_dk,
          type = "text", out = "TableS6.txt")


## Robustness check: CCA Analyses
# Read data
anes2016 = read.dta13("anes_timeseries_2016_Stata13.dta")

# List of relevant variables (ANES code)
vars = c("V160001", c("V162095", "V162096", "V162097", "V162098", "V162099", "V162100", "V162101",  "V162102", "V162103", "V162104", "V162105", "V162106", "V162107", "V162108", "V162109", "V162110", "V162111", "V162112", "V162113", "V162310", "V162311", "V162312", "V162313", "V162314", "V161095", "V161096"))

# Names of relevant variables
labels = c("ID", c("ChristianFundamentalists", "Feminists", "Liberals", "LaborUnions", "PoorPeople", "BigBusiness", "Conservatives", "SupremeCourt", "GayLesbians", "Congress", "RichPeople", "Muslims", "Christians", "Jews", "TeaParty", "Police", "Transgender", "Scientists", "BlackLivesMatter", "AsianAmericans", "Hispanics", "Blacks", "IllegalImmigrants", "Whites", "DemocraticParty", "RepublicanParty"))

# Subset to relevant variables
anes2016 = anes2016[,vars]

# Replace names
colnames(anes2016) = labels

# Clean data (drop year and id, make matrix, set all values > 110 or < 0 to NA, add 1 to every value (so that no variable has 0))
anes_run = anes2016[,2:length(anes2016)]
anes_run = as.matrix(anes_run)
anes_run[] = ifelse(anes_run[] < 0 | anes_run > 110, NA, anes_run[])
anes_run[] = anes_run[] + 1

# Add cleaned data back to regular data (basically preserving year and id which were dropped as part of cleaning stage)
anes2016[,2:length(anes2016)] = anes_run

# Identify attitudes towards social groups
anes2016 = subset(anes2016, select = c(ID, Jews, Muslims, Christians, ChristianFundamentalists,
                                        Blacks, Whites, Hispanics, IllegalImmigrants, AsianAmericans,
                                        PoorPeople, RichPeople, Liberals, Conservatives, Scientists,
                                        Transgender, GayLesbians, Feminists))

anes2016 <- na.omit(anes2016)

# Apply rescale and categorize function dataset (saving as new name so i can run RCA on each)
anes2016_cat = anes2016
anes2016_cat[,2:length(anes2016_cat)] = rescale_cat(anes2016_cat[,2:length(anes2016_cat)])
anes2016_cat[,2:length(anes2016_cat)] = sapply(anes2016_cat[,2:length(anes2016_cat)], FUN = function(x) as.numeric(as.character(x)))

# Improved RCA
RNGkind(sample.kind = "default")
set.seed(1000)
rca_out_2016 = rca.full.3(as.matrix(anes2016_cat[,2:length(anes2016_cat)]), amirsd = F)

anes2016_cat_new <- anes2016_cat
anes2016_cat_new$RCA_improved <- rca_out_2016$membership

# Original RCA
original_RCA <- RCA(as.matrix(anes2016_cat[,2:length(anes2016_cat)]), alpha = 1)
anes2016_cat_new$RCA_original <- original_RCA$membership

# CCA
cca_out <- cca(as.matrix(anes2016_cat[,2:length(anes2016_cat)]))
anes2016_cat_new$CCA <- cca_out$membership

# Apply rescale and categorize function dataset
vars <- colnames(anes2016_cat)[2:length(anes2016_cat)]
to_plot <- anes2016_cat_new
to_plot[,vars] = rescale_cat(to_plot[,vars])
to_plot[,vars] = sapply(to_plot[,vars], FUN = function(x) as.numeric(as.character(x)))

# Subset to RCA + feeling thermometers
anes_net <- to_plot[,c("CCA", vars)]

# Use plot.group function to get the network plots (see functions.R for more info)
plot_out <- plot.groups(anes_net, variables = vars, group.ids = "CCA", sig.level = .05, type = "matrix", bw = T)

# Order the categories using factor
anes_w_classes_for_heatmap <- to_plot

cormats = class_cormats(anes_w_classes_for_heatmap,
                            variables = vars,
                            class.ids = "CCA",
                            file_name = "class_heatmaps_sig_bw_cca",
                            sig_level = 0.05,
                            label_type = "correlations")

## For CCA, make heatmaps
sig_val = 0.05
class_p <- class_heatmaps(anes2016_cat_new, variables = colnames(anes2016_cat[,2:length(anes2016_cat)]),
                            class.ids = "CCA",
                            file_name = "cca_class_heatmaps_sig", sig.level = sig_val)
  tiff(paste0("cca_class_heatmaps_no_filter", sig_val, ".tiff"), units = "in", width = 1600, height = 600, res = 160, compression = "lzw")
  do.call("grid_arrange_shared_legend", c(class_p, COLNUM = 3))
  dev.off()

  
  # pdf(paste0("cca_class_heatmaps_no_filter", sig_val, ".pdf"), width = 16, height = 6)
  
  
# Using SEM to compare models' fit
social_groups = c("Jews", "Muslims", "Christians", "ChristianFundamentalists",
                  "Blacks", "Whites", "Hispanics", "IllegalImmigrants", "AsianAmericans",
                  "PoorPeople", "RichPeople", "Liberals", "Conservatives", "Scientists",
                  "Transgender", "GayLesbians", "Feminists")

var_combs <- t(combn(social_groups, 2))
var_combs <- subset(var_combs, var_combs[,1] != var_combs[,2])

covariance_model <- paste(var_combs[,1], ' ~~ ', var_combs[,2], ';', sep='' )

overall_model <- sem(model=covariance_model,
                     data = anes2016_cat_new,
                     check.gradient = FALSE)

rca_impr_model <- try(sem(model= covariance_model,
                        data = anes2016_cat_new,
                        group = "RCA_improved",
                        check.gradient = FALSE))

rca_orig_model <- try(sem(model= covariance_model,
                        data = anes2016_cat_new,
                        group = "RCA_original",
                        check.gradient = FALSE))

cca_model <- sem(model=covariance_model,
                     data = anes2016_cat_new,
                     group = "CCA",
                     check.gradient = FALSE)

# TABLE S2: SEM results
summary(rca_impr_model, fit.measures = T)
summary(rca_orig_model, fit.measures = T)
summary(cca_model, fit.measures = T)


### EXCLUDING VARIABLES ROBUSTNESS CHECK
### Running RCA
source("Packages.R")
source("Functions.R")

# Read data
anes2016 = read.dta13("anes_timeseries_2016_Stata13.dta")

# List of relevant variables (ANES code)
vars <- c("V160001",
        c("V162095", "V162096", "V162097",
          "V162099", "V162101", "V162103",
          "V162105", "V162106", "V162107",
          "V162108", "V162111", "V162112",
          "V162310", "V162311", "V162312",
          "V162313", "V162314"
          )
        )

# Names of relevant variables
labels <- c("ID",
          c("ChristianFundamentalists",
            "Feminists", "Liberals", "PoorPeople",
            "Conservatives", "GayLesbians", "RichPeople",
            "Muslims", "Christians", "Jews", "Transgender",
            "Scientists", "AsianAmericans", "Hispanics",
            "Blacks", "IllegalImmigrants", "Whites"
            )
          )

# Subset to relevant variables
anes2016 <- anes2016[, vars]

# Replace names
colnames(anes2016) <- labels

# Clean data (drop year and ID, make matrix, 
# set all values > 110 or < 0 to NA, add 1 
# to every value (so that no variable has 0))
anes_run <- anes2016[,2:length(anes2016)]
anes_run <- as.matrix(anes_run)
anes_run[] <- ifelse(anes_run[] < 0 | anes_run > 110, NA, anes_run[])
anes_run[] <- anes_run[] + 1

# Add cleaned data back to regular data (basically 
# preserving year and id which were dropped as part of cleaning stage)
anes2016[, 2:length(anes2016)] <- anes_run

anes2016 <- na.omit(anes2016)

# Apply rescale and categorize function dataset 
# (saving as new name so RCA can be run on each)
anes2016_cat <- anes2016
anes2016_cat[, 2:length(anes2016_cat)] <- rescale_cat(
                                        anes2016_cat[,2:length(anes2016_cat)]
                                        )

anes2016_cat[,2:length(anes2016_cat)] <- sapply(
                                        anes2016_cat[,2:length(anes2016_cat)], 
                                        FUN = function(x) 
                                        as.numeric(as.character(x))
                                        )

# Run RCA on each (starting with categories)
set.seed(1000)
for_rca <- as.matrix(anes2016_cat[, 2:length(anes2016_cat)])

# single exclusion
membership_ids <- list()
for (to_drop in seq_len(NCOL(for_rca))) {
    temp_df <- for_rca[, -to_drop]

    rca_out_temp <- rca.full.3(temp_df,
                                amirsd = F)

    membership_ids[[length(membership_ids) + 1]] <- rca_out_temp$membership
}

names(membership_ids) <- colnames(for_rca)

single_exclusion <- do.call("cbind", membership_ids)
single_exclusion <- as.data.frame(single_exclusion)
colnames(single_exclusion) <- paste0("exclude", colnames(single_exclusion))

single_exclusion$ID <- anes2016_cat$ID

# double exlusion
ex2_membership_ids <- list()

two_combs <- combn(NCOL(for_rca), 2)
for (i in 28:ncol(two_combs)) {
    out1 <- two_combs[1, i]
    out2 <- two_combs[2, i]
    temp_df <- for_rca[, -out1]
    temp_df <- temp_df[, -out2]

    rca_out_temp <- rca.full.3(temp_df,
                                amirsd = F)

    ex2_membership_ids[[length(ex2_membership_ids) + 1]] <- rca_out_temp$membership
}

two_combs_names = combn(colnames(for_rca), 2)
double_names <- paste0(two_combs_names[1, ], "_", two_combs_names[2, ])
names(ex2_membership_ids) <- double_names

double_exclusion <- do.call("cbind", ex2_membership_ids)
double_exclusion <- as.data.frame(double_exclusion)
colnames(double_exclusion) <- paste0("exclude", colnames(double_exclusion))
double_exclusion$ID <- anes2016_cat$ID


# Use rand to compare single and double exclusion membership vectors against original membership vector 
library(fossil)
original_ids <- readRDS("RCA_robustness_ids.RDS")

# For single exclusion
# loop through id sets and add in proper column names
for (i in 4:1) {
    if (i == 1) {
        colnames(original_ids[[i]]) <- c("ID", "RCA_Original")
    } else if (i == 2) {
        colnames(original_ids[[i]]) <- c("ID", "RCA_NoLibCons")
    } else if (i == 3) {
        colnames(original_ids[[i]]) <- c("ID", "RCA_DemRep")
    } else if (i == 4) {
        colnames(original_ids[[i]]) <- c("ID", "RCA_NoSci")
    }
    single_exclusion <- merge(original_ids[[i]], single_exclusion, by = "ID")
}

single_rand_vals <- c()
for (i in 3:length(single_exclusion)) {
    rand_out <- rand.index(single_exclusion[, 2], single_exclusion[, i])
    single_rand_vals <- c(single_rand_vals, rand_out)
}

# For double exclusion
# loop through id sets and add in proper column names
for (i in 4:1) {
    if (i == 1) {
        colnames(original_ids[[i]]) <- c("ID", "RCA_Original")
    } else if (i == 2) {
        colnames(original_ids[[i]]) <- c("ID", "RCA_NoLibCons")
    } else if (i == 3) {
        colnames(original_ids[[i]]) <- c("ID", "RCA_DemRep")
    } else if (i == 4) {
        colnames(original_ids[[i]]) <- c("ID", "RCA_NoSci")
    }
    double_exclusion <- merge(original_ids[[i]], double_exclusion, by = "ID")
}

double_rand_vals <- c()
for (i in 3:length(double_exclusion)) {
    rand_out <- rand.index(double_exclusion[, 2], double_exclusion[, i])
    double_rand_vals <- c(double_rand_vals, rand_out)
}

# Put them all together
final_rand_vals <- c(single_rand_vals, double_rand_vals)
final_rand_names <- c(names(single_exclusion[3:length(single_exclusion)]),
                      names(double_exclusion[3:length(double_exclusion)]))

order_decreasing = order(final_rand_vals, decreasing = F)
final_rand_vals[order_decreasing]
final_rand_names[order_decreasing][3:12]

# Convert to DF for visualizing with ggplot2
library(ggplot2)
df = data.frame(Exclusion = final_rand_names, Rand = final_rand_vals)
df <- na.omit(df)
df <- unique(df)
df = subset(df, !grepl("\\.y", df$Exclusion))
df$Exclusion = gsub("\\.x", "", df$Exclusion)

# Basic histogram
ggplot(df, aes(x = Rand)) +
    geom_histogram(aes(y=..density..), colour="black", fill="white") +
    theme_bw(base_size = 15) +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("Rand Index") +
    ylab("Frequency")

ggsave("Brensinger2022/output/exclusion_Rand_distribution.tiff", width = 8, height = 8, device = "tiff")

# Save results
write.csv(df[1:3,], "Brensinger2022/output/TableS3.csv")
write.csv(df[4:nrow(df),], "Brensinger2022/output/TableS4.csv")

### Permutation tests
adjRand_test(double_exclusion$RCA_Original, double_exclusion$RCA_NoLibCons, perm = 2000)
adjRand_test(double_exclusion$RCA_Original, double_exclusion$RCA_DemRep, perm = 2000)
adjRand_test(double_exclusion$RCA_Original, double_exclusion$RCA_NoSci, perm = 2000)

### BOOTSTRAPPING SAMPLE SELECTION ROBUSTNESS CHECK
# Read data
anes2016 <- read.dta13("Brensinger2022/anes_timeseries_2016_Stata13.dta")

# List of relevant variables (ANES code)
vars = c("V160001",
        c("V162095",
        "V162096", "V162097", "V162099",
        "V162101", "V162103", "V162105",
        "V162106", "V162107", "V162108", "V162111",
        "V162112", "V162310", "V162311",
        "V162312", "V162313", "V162314"))

# Names of relevant variables
labels <- c("ID",
            c("ChristianFundamentalists",
            "Feminists", "Liberals", "PoorPeople",
            "Conservatives", "GayLesbians", "RichPeople",
            "Muslims", "Christians", "Jews", "Transgender",
            "Scientists", "AsianAmericans", "Hispanics",
            "Blacks", "IllegalImmigrants", "Whites"))

# Subset to relevant variables
anes2016 <- anes2016[, vars]

# Replace names
colnames(anes2016) <- labels

# Clean data
# (drop year and ID, make matrix, set all values > 110 or < 0 to NA,
# add 1 to every value (so that no variable has 0))
anes_run <- anes2016[, 2:length(anes2016)]
anes_run <- as.matrix(anes_run)
anes_run[] <- ifelse(anes_run[] < 0 | anes_run > 110, NA, anes_run[])
anes_run[] <- anes_run[] + 1

# Add cleaned data back to regular data
# (basically preserving year and id
# which were dropped as part of cleaning stage)
anes2016[, 2:length(anes2016)] <- anes_run

anes2016 <- na.omit(anes2016)

# Apply rescale and categorize function dataset
# (saving as new name so RCA can be run on each)
anes2016_cat <- anes2016
anes2016_cat[, 2:length(anes2016_cat)] <- rescale_cat(
                                          anes2016_cat[, 2:length(anes2016_cat)]
                                          )

anes2016_cat[, 2:length(anes2016_cat)] <- sapply(
                                        anes2016_cat[,2:length(anes2016_cat)],
                                        FUN = function(x) as.numeric(
                                            as.character(x))
                                        )

# Run RCA on each (starting with categories)
# set.seed(1000)
library(parallel)

bootstrap_Rand <- function(x){
    row_set <- seq_len(NROW(x))
    sample_rows <- sample(row_set, NROW(x), replace = T)
    sub_df <- x[sample_rows, ]
    rca_out <- rca.full.3(as.matrix(sub_df[, 2:length(sub_df)]), amirsd = F)
    output <- data.frame(ids = sub_df$ID, class = rca_out$membership)
    return(output)
}

id_set <- list()
id_set <- mclapply(1:800, FUN = function(x) bootstrap_Rand(anes2016_cat), mc.cores = 40)

saveRDS(id_set, "Brensinger2022/output/rca_bootstrap_results_w_replace.RDS")

library(tidyverse)
library(fossil)
library(entropy)

bootstrap_results <- readRDS("Brensinger2022/output/rca_bootstrap_results_w_replace.RDS")

bootstrap_results = bootstrap_results[1:1000]

robust_ids = readRDS("Brensinger2022/output/RCA_robustness_ids.RDS")
orig_ids = robust_ids[[1]]
colnames(orig_ids) = c("ids", "original")

compare_rca = function(orig_ids, bootstrap_out){
    final_df <- merge(orig_ids, bootstrap_out, by = "ids")
    final_df <- na.omit(final_df)
    norm_rand_out <- rand.index(final_df$original, final_df$class)
    return(norm_rand_out)
}

library(parallel)

rca_out = mclapply(1:length(bootstrap_results), FUN = function(x) compare_rca(orig_ids, bootstrap_results[[x]]), mc.cores = 40)


rca_out_unlisted = unlist(rca_out)
quantile(rca_out_unlisted, c(0.25, 0.75))


library(ggplot2)
df = data.frame(Run = 1:length(rca_out_unlisted), Rand = rca_out_unlisted)
df = subset(df, df$Rand <= 1)
df <- na.omit(df)
mean(df$Rand, na.rm = T)
# Basic histogram
ggplot(df, aes(x = Rand)) +
    geom_histogram(aes(y=..density..), colour="black", fill="white") +
    theme_minimal(base_size = 15) +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("Rand Index") +
    ylab("Frequency")

ggsave("Brensinger2022/output/bootstrap_Rand_distribution.tiff", width = 8, height = 8, device = "tiff")


### Table S8: JACKKNIFE ANALYSIS
# Read in data
anes_w_classes = readRDS("Brensinger2022/output/anes_w_classes_w_covars_and_dks.RDS")
id_to_scales <- readRDS("Brensinger2022/output/ids_to_scales.RDS")

anes_w_classes <- merge(anes_w_classes, id_to_scales, by = "ID")

# Split Class 1 into two poles at 0 and create variables accordingly
pole_mid <- 0
anes_w_classes$Scale1_pole1 <- ifelse(anes_w_classes$Scale1_scaled>=pole_mid, 1, 0)
anes_w_classes$Scale1_pole2 <- ifelse(anes_w_classes$Scale1_scaled<pole_mid, 1, 0)
anes_w_classes$RCA_1_1 = ifelse(anes_w_classes$RCA_new == 1, anes_w_classes$Scale1_pole1, 0)
anes_w_classes$RCA_1_2 = ifelse(anes_w_classes$RCA_new == 1, anes_w_classes$Scale1_pole2, 0)

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

anes_w_classes$homsexual_or_bisexual <- ifelse(anes_w_classes$homosexual == 1 | anes_w_classes$bisexual == 1,

# Neutral logic as the reference class
anes_w_classes$RCA_new_3ref <- relevel(as.factor(anes_w_classes$RCA_new), ref = 3)

# core_vars <- c("strength_party_id", "party_id", "political_interest", "attention_to_news", "personal_discrim", 
#               "black", "asian", "hispanic", "other_racial", "strength_party_id * black", "strength_party_id * asian",
#               "strength_party_id * hispanic", "strength_party_id * other_racial")

core_vars <- c("strength_party_id * black", "strength_party_id * asian",
              "strength_party_id * hispanic", "strength_party_id * other_racial")

covars <- c("strength_party_id", "party_id",
              "political_interest", "attention_to_news", "personal_discrim",
              "church_freq", "southern",
              "black", "asian", "hispanic", "other_racial",
              "non_hetero", "female", "age", "college_grad",
              "income_q_2", "income_q_3", "income_q_4", "income_q_5",
              "black_protestant", "evangelical", "catholic", "jewish", "not_religious",
              "other_religion", "other_christian", "strength_party_id * black", "strength_party_id * asian",
            "strength_party_id * hispanic", "strength_party_id * other_racial")

core_var_dists <- list()
for(i in 1:length(core_vars)){
  sig_save_1 <- c()
  sig_save_2 <- c()
  coef_save_1 <- c()
  coef_save_2 <- c()
  for(j in 1:length(covars)){
    final_vars <- unique(c(core_vars[i], covars[-j]))
    final_vars_form <- paste(final_vars, collapse = " + ")
    form_string <- paste("RCA_new_2ref ~", final_vars_form)
    to_regress <- as.formula(form_string)
    multinom_out <- multinom(to_regress, data = anes_w_classes)
    coefs_ref <- summary(multinom_out)$coefficients
    to_sub <- gsub(" \\* ", ":", core_vars[i])
    coef_vals <- exp(coefs_ref[,to_sub])
    z_ref <- coefs_ref/summary(multinom_out)$standard.errors
    p_ref <- (1 - pnorm(abs(z_ref), 0, 1))*2
    p_vals <- p_ref[,to_sub]
    sig_save_1 <- c(sig_save_1, p_vals[1])
    sig_save_2 <- c(sig_save_2, p_vals[2])
    coef_save_1 <- c(coef_save_1, coef_vals[1])
    coef_save_2 <- c(coef_save_2, coef_vals[2])
  }
  direction_save_1 <- ifelse(coef_save_1 >= 0, 1, -1)
  direction_save_2 <- ifelse(coef_save_2 >= 0, 1, -1)
  core_var_dists[[length(core_var_dists) + 1]] <- list(Sigs1 = sig_save_1, 
                                                       Sigs2 = sig_save_2,
                                                       Coefs1 = coef_save_1,
                                                       Coefs2 = coef_save_2,
                                                       Direct1 = direction_save_1,
                                                       Direct2 = direction_save_2)
}

# Table S8 output 
jackknife_results = lapply(core_var_dists, FUN = function(x) lapply(x, FUN = function(x) paste0("[", round(range(x),2)[1], ", ", round(range(x),2)[2], "]")))
saveRDS(jackknife_results, "Brensinger2022/output/TableS8.RDS")
