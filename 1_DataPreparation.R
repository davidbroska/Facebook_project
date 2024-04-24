source(list.files(pattern="0_Setup.R", recursive=T))

# Data preparation 
fname = "dt_survey_0625_with_toxicity.csv"
fpath = list.files(pattern = fname, recursive = T)

# Exclude participants with missing values on the main DVs and IVs
dt = read_csv(fpath) %>% 
  rename(Order=order, 
         Toxicity=toxicity, 
         Productive=productive) %>% 
  filter(!is.na(Toxicity),
         !is.na(Productive),
         ! r_partyid %in% c(NA, "Something else"),
         ! r_ideo %in% c(NA, "I don't think of myself that way")) 


dt$target_id = paste0("T",dt$target_id)
dt$post_id = paste0("P",dt$post_id)


# Order: nth commment rated
table(dt$Order,useNA = "always")
dt$Order2Sd = scale2(dt$Order)

dt = dt %>% 
  mutate(OrderCat = case_when(Order %in% 1:2 ~ "Order12",
                              Order %in% 3:5 ~ "Order345",
                              Order %in% 6:7 ~ "Order67"))
table(dt$OrderCat, useNA = "always")


# Age: create variable and replace missing values by median
table(dt$r_bornyear, useNA = "always")
dt$Age = 2023 - as.numeric(dt$r_bornyear)
dt$Age2Sd = scale2(dt$Age)
dt = dt %>%
  mutate(AgeCat = case_when(Age >= 18 & Age <= 29 ~ "18-29", 
                            Age >= 30 & Age <= 44 ~ "30-44",
                            Age >= 45 & Age <= 64 ~ "45-64",
                            Age >= 65 ~ "65+"))
table(dt$AgeCat, useNA = "always")

# Gender: replace missing or low frequency values by mode
table(dt$r_gender, useNA = "always")
dt$Gender = ifelse(!dt$r_gender %in% c("Man","Woman"), "Other", dt$r_gender)
table(dt$Gender, useNA = "always")


# Race
table(dt$r_race_asian)
table(dt$r_race_black)
table(dt$r_race_hispanic)
table(dt$r_race_white)
table(dt$r_race_other)
table(dt$r_race_refuse)
dt = dt %>% 
  mutate(Race = case_when(r_race_asian == 1 ~ "Asian",
                          r_race_black == 1 ~  "Black",
                          r_race_hispanic == 1 ~ "Hispanic",
                          r_race_white == 1 ~ "White",
                          r_race_other == 1 ~ "Other",
                          r_race_refuse == 1 ~ "Other"))
table(dt$Race, useNA = "always")


# Marital status
table(dt$r_marstat, useNA = "always") 
dt$MaritalStatus = dt$r_marstat


# Religion
table(dt$r_religion,useNA = "always")
dt = dt %>% 
  mutate(Religion = case_when(r_religion %in% c("Atheist or Agnostic","No Religion") ~ "Not religious",
                              r_religion %in% c(NA, "Prefer not to answer") ~ "Not available", 
                              TRUE ~ r_religion))
table(dt$Religion,useNA = "always")


# Sexual orientation
table(dt$r_sexorient,useNA = "always")
dt = dt %>% 
  mutate(SexualOrientation = case_when(r_sexorient %in% c(NA,"Other","Prefer not to answer") ~ "Other", 
                                    TRUE ~ r_sexorient))
table(dt$SexualOrientation,useNA = "always")


# Household size 
table(dt$r_householdsize,useNA = "always")
dt = dt %>% 
  mutate(HhSize = case_when(r_householdsize %in% c("Prefer not to answer",NA) ~ "Other",
                            r_householdsize == "more than 5" ~ "6 or more",
                            TRUE ~ r_householdsize) %>% 
           str_c("Hh",.))
table(dt$HhSize,useNA = "always")


# Region

dt = dt %>% 
  mutate(UsState = ifelse(r_state=="Prefer not to answer",NA,r_state),
    Region = case_when(
    r_state %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", 
                   "South Carolina", "Virginia", "West Virginia", "Alabama", 
                   "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", 
                   "Oklahoma", "Texas") ~ "South",
    r_state %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "District of Columbia",
                   "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania") ~ "Northeast",
    r_state %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", 
                   "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", 
                   "North Dakota", "South Dakota") ~ "Midwest",
    r_state %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", 
                   "New Mexico", "Utah", "Wyoming", "Alaska", "California", 
                   "Hawaii", "Oregon", "Washington") ~ "West"
  ))
table(dt$UsState,useNA = "always")
table(dt$Region,useNA = "always")

# replace missing values with mode
dt$Region = ifelse(is.na(dt$Region), names(sort(-table(dt$Region))[1]), dt$Region)
table(dt$Region,useNA = "always")




# Education: replace missing with mode
names(sort(-table(dt$r_educ))[1])

dt$r_educ = ifelse(dt$r_educ == "Prefer not to answer", NA, dt$r_educ)
dt$r_educ = ifelse(is.na(dt$r_educ), names(sort(-table(dt$r_educ))[1]), dt$r_educ)
table(dt$r_educ,useNA = "always")

dt = dt %>% mutate(Education = case_when(
  r_educ %in% c("Less than high school","High school graduate") ~ "High School or less",
  r_educ %in% c("2 year degree (e.g., Associate's degree or vocational training)",
                "4 year degree (e.g., Bachelor's degree)",
                "Some college") ~ "Some college", 
  r_educ %in% c("Doctorate","Professional / master's degree") ~ "Postgraduate"))
table(dt$Education, useNA = "always")

dt = dt %>% 
  mutate(EducationNum = case_when(r_educ == "Less than high school"~ 1, 
                                  r_educ == "High school graduate" ~ 2,
                                  r_educ %in% c("2 year degree (e.g., Associate's degree or vocational training)", 
                                                "Some college", "4 year degree (e.g., Bachelor's degree)") ~ 3,
                                  r_educ == "Professional / master's degree" ~ 4,
                                  r_educ == "Doctorate"~ 5))
dt$EducationNum2Sd = scale2(dt$EducationNum)
table(dt$EducationNum)



# Create continuous income variable
table(dt$r_famincome, useNA = "always")
dt$r_famincome = ifelse(dt$r_famincome %in% c("Prefer not to answer", "6859"),NA,dt$r_famincome)

# covert income brackets into continuous variable
m = dt$r_famincome %>% 
  str_remove("(Less than )?[$]") %>%
  str_remove_all(",") %>% 
  str_remove_all(" or more$") %>% 
  str_split(" to [$]?") %>% 
  lapply(function(s) data.frame(lower=as.numeric(s[1]), upper=as.numeric(s[length(s)]))) %>% 
  do.call(bind_rows, .) 
table(m, useNA = "always")

# define midpoint of lowest income category as mean of 0 and its upper limit
mmin = 15000
m[which(m$lower == mmin & m$upper == mmin), "lower"] = 0

# define midpoint of upper category with the method by Hout (2004) if possible
mmax = 200000
m[which(m$upper == mmax & m$upper == mmax), "upper"] = NA

# mean for all other values
m$M = rowMeans(m)

income_Mtop = function(.Ltop,.Ltopm1,.ftop,.ftopm1,.impute=T){
  # Ltop   : lower limit of the top category
  # Ltopm1 : lower limit of the category before the top one
  # ftop   : frequency in the top category
  # ftopm1 : frequency in the category before the top one
  V = (log(.ftopm1+.ftop)-log(.ftop)) / (log(.Ltop)-log(.Ltopm1))
  Mtop = .Ltop * (V / (V-1))
  
  # The function described in Hout (2004) is not always well defined, e.g. if ftopm1=0, ftop=0, ...
  if(.impute & (is.na(Mtop) | Mtop<Ltop | is.infinite(abs(Mtop))))  Mtop = .Ltop
  
  Mtop = round(Mtop, 1)
  
  print(paste0("The value for the midpoint of the top category is ", Mtop))
  return(Mtop)
}

cnt = count(m,lower) %>% arrange(desc(lower))
ftop = cnt[1,"n"]
ftopm1 = cnt[2,"n"]
Ltop = cnt[1,"lower"]
Ltopm1 = cnt[2,"lower"]
Mtop = income_Mtop(.Ltop = Ltop,.Ltopm1 = Ltopm1, .ftop = ftop,.ftopm1 = ftopm1)
m[which(m$lower==mmax & is.na(m$upper)), "M"] = Mtop
dt$Income = m$M
# estimated midpoint of top category
Mtop
# impute missing incomes by median
dt$Income = ifelse(is.na(dt$Income), median(dt$Income,na.rm=T), dt$Income)
# rescale income to make regression coefficients more interpretable
dt$Income1k = dt$Income / 1000
dt$Income2Sd = scale2(dt$Income)
table(dt$Income,useNA = "always")


# Income quintiles
# Following Baldassarri & Goldberg (2014), I divide income into 5 categories that correspond to 
# 0-17, 17-33, 33-67, 67-83 and 83-100 percentiles (this also allows for standardization across years)
IncomeCats <- quantile(dt$Income, c(0, .17, .33, .67, .83, 1), na.rm = TRUE) 
dt$IncomeQuantiles <- cut(dt$Income, breaks = IncomeCats, labels = c(1:5),
                          include.lowest = TRUE, right = TRUE, ordered_result = TRUE)



# Political ideology
table(dt$r_ideo, useNA = "always")
dt = dt %>% mutate(PolIdNum = case_when(r_ideo == "Extremely liberal"        ~ 1,
                                        r_ideo == "Liberal"                  ~ 2,
                                        r_ideo == "Slightly liberal"         ~ 3,
                                        r_ideo == "Moderate"                 ~ 4,
                                        r_ideo == "Slightly conservative"    ~ 5,
                                        r_ideo == "Conservative"             ~ 6,
                                        r_ideo == "Extremely conservative"   ~ 7))

# Partisanship and part Id
table(dt$r_partyid, useNA = "always")
dt = dt %>% mutate(Partisanship = str_remove(r_partyid," toward"),
                   PartyId = case_when(Partisanship == "Strong Democrat"    ~ "Democrat",
                                       Partisanship == "Weak Democrat"      ~ "Democrat",
                                       Partisanship == "Leaning Democrat"   ~ "Democrat",
                                       Partisanship == "Independent"        ~ "Independent",
                                       Partisanship == "Leaning Republican" ~ "Republican",
                                       Partisanship == "Weak Republican"    ~ "Republican",
                                       Partisanship == "Strong Republican"  ~ "Republican"),
                   PartisanshipNum = case_when(Partisanship == "Strong Democrat"    ~ 1,
                                               Partisanship == "Weak Democrat"      ~ 2,
                                               Partisanship == "Leaning Democrat"   ~ 3,
                                               Partisanship == "Independent"        ~ 4,
                                               Partisanship == "Leaning Republican" ~ 5,
                                               Partisanship == "Weak Republican"    ~ 6,
                                               Partisanship == "Strong Republican"  ~ 7))
table(dt$PartyId, useNA = "always")
table(dt$Partisanship, useNA = "always")
table(dt$PartisanshipNum, useNA = "always")

# create composite of political ideology
dt$PolIdComp = rowMeans(dt[,c("PolIdNum","PartisanshipNum")],na.rm = T)
cor(dt$PolIdNum,dt$PartisanshipNum,use = "complete.obs")
dt$PolIdComp2Sd = scale2(dt$PolIdComp)


# Dependent variables
dt = dt %>% 
  mutate(across(c(hatespeech_commenterA,hatespeech_author,hatespeech_others,hatespeech_no,hatespeech_notsure), ~ifelse(.==-99,0,.))) %>% 
  mutate(
    # 1. Can you understand the point that Commenter B is trying to make?
    UnderstandB = ifelse(understand=="Yes",1,0),
    UnderstandBNum = case_when(understand=="Yes"      ~ 1, 
                               understand=="Not sure" ~ 0, 
                               understand=="No"       ~-1),
    UnderstandBNum01 = (UnderstandBNum + 1) / 2,
    # 2. Do you think Commenter B is making an effort to understand Commenter A's point of view? 
    BMakesEffort = ifelse(effort=="Yes",1,0),
    BMakesEffortNum = case_when(effort=="Yes"      ~ 1,
                                effort=="Not sure" ~ 0, 
                                effort=="No"       ~-1),
    BMakesEffortNum01 = (BMakesEffortNum + 1) / 2,
    # 3. Do you think that Commenter B is staying on topic in their response to Commenter A?
    BStaysOnTopic = ifelse(stayontopic=="Yes",1,0),
    BStaysOnTopicNum = case_when(stayontopic=="Yes"     ~ 1,
                                stayontopic=="Not sure" ~ 0, 
                                stayontopic=="No"       ~-1),
    BStaysOnTopicNum01 = (BStaysOnTopicNum + 1) / 2,
    # 4. In your view, does Commenter B agree or disagree with Commenter A?
    BAgrees = ifelse(disagree %in% c("Strongly Agrees","Agrees"),1,0),
    BAgreesNum = case_when(disagree=="Strongly Agrees"     ~ 2,
                           disagree=="Agrees"              ~ 1,
                           disagree %in% c("Not sure","Neither Agrees nor Disagrees") ~ 0, 
                           disagree=="Disagrees"           ~-1,
                           disagree=="Strongly Disagrees"  ~-2),
    BAgreesNum01 = (BAgreesNum + 2) / 4,
    # 5. to 12. Which of the following words would you use to describe Commenter B's attitude towards Commenter A? Please check all that apply.
    BRespectful = attitude_respectful,
    BOpen = attitude_open,
    BObjective = attitude_objective,
    BEmotional = attitude_emotional,
    BSarcastic = attitude_sarcastic,
    BIntolerant = attitude_intolerant,
    BHostile = attitude_hostile,
    BNoneabove = attitude_noneabove,
    # 13. Rate the toxicity of Commenter B's comment.
    # Very toxic: A comment that is very hateful, aggressive, disrespectful, or otherwise very likely to make a user leave a discussion or give up on sharing their perspective.
    # Toxic: A comment that is rude, disrespectful, unreasonable, or otherwise somewhat likely to make a user leave a discussion or give up on sharing their perspective.
    BIsToxic = ifelse(Toxicity %in% c("Toxic","Very toxic"),1,0), 
    BToxicNum = case_when(Toxicity=="Not toxic" ~ -1,
                          Toxicity=="Maybe, not sure"~0,
                          Toxicity=="Toxic"~1, 
                          Toxicity=="Very toxic"~2),
    BToxicNum01 = (BToxicNum + 1) / 3,
    # 14. In your opinion, does Commenter B use hate speech? If so, who are the targets? Check all that apply.  
    # Hate speech: A comment that uses pejorative/discriminatory language or attacks persons/groups on the basis of who they are,
    # such as because of their race, ethnicity, religion, disability, sexual orientation, or gender identity. 
    BUseHateSpeech = ifelse(hatespeech_author==1|hatespeech_commenterA==1|hatespeech_others==1,1,0), 
    BUseHateSpeechNum = hatespeech_commenterA + hatespeech_author + hatespeech_others - hatespeech_no,
    BUseHateSpeechNum01 = (BUseHateSpeechNum + 1) / 4,
    # Do you think that this was a productive conversation?
    ProductiveNum = case_when(Productive=="No" ~ -1,
                              Productive=="Not sure"~0,
                              Productive=="Yes"~1),
    IsProductive = ifelse(Productive=="Yes",1,0),
    ProductiveNum01 = (ProductiveNum + 1) / 2, 
    # Other variables
    CharCountText = nchar(productive_why_text,keepNA = F) +
      nchar(productive_whynot_text,keepNA = F) +
      nchar(productive_whynotsure_text,keepNA = F),
    StrCountProductive = str_count(replace_na(productive_why_text,"NA"),"[Pp]roductive") +
      str_count(replace_na(productive_whynot_text,"NA"),"[Pp]roductive") +
      str_count(replace_na(productive_whynotsure_text,"NA"),"[Pp]roductiv"),
    StrCountToxic = str_count(replace_na(productive_why_text,"NA"),"[Tt]oxic") +
      str_count(replace_na(productive_whynot_text,"NA"),"[Tt]oxic") +
      str_count(replace_na(productive_whynotsure_text,"NA"),"[Tt]oxic"),
    PercProductive = 100*StrCountProductive/CharCountText,
    PercToxic      = 100*StrCountToxic/CharCountText)


lapply(dt[DVs], range)


# Create ID that is unique for each row
dt$ID = str_c(dt$ResponseId,"_",dt$target_id)
dt %>% 
  count(ID) %>% 
  filter(n > 1)

# grand mean center variables 
numeric_vars = c(DVs,Covs)[ sapply(dt[c(DVs,Covs)], is.numeric) ] 
dt = dt %>% 
  mutate(across(all_of(numeric_vars), ~.-mean(.)))


# created demeaned binary dependent variables
dt_DvsDmd = dt %>% 
  select(ID,target_id, all_of(unique(c(DVs, BinaryDVs)))) %>% 
  group_by(target_id) %>% 
  mutate(across(all_of(unique(c(DVs, BinaryDVs))), ~ (. - mean(.,na.rm=T)) )) %>% 
  rename_with(~str_c(.,"_TargetDmd"), all_of(unique(c(DVs, BinaryDVs)))) 

dt = left_join(dt,dt_DvsDmd,by = c("ID","target_id"))


 
create_dummies = function(.data, .id, .col){
  # Note that the matrix with dummies will not have full rank. Either:
  # 1) drop a dummy or
  # 2) add intercept and drop a dummy
  prefix = names(select(.data,{{.col}}))
  
  .data %>%
    select({{.id}}, {{.col}}) %>% 
    arrange({{.col}}) %>% 
    mutate(value=1) %>% 
    pivot_wider(id_cols = {{.id}},
                names_from = {{.col}},
                names_prefix = prefix,
                values_from= value, 
                values_fill = 0) 
}






dt_clean = dt %>% 
  select(ID,ResponseId:inc_prob,
         Order,Order2Sd,OrderCat,
         PartyId,
         PolId = r_ideo, PolIdNum, 
         Partisanship, PartisanshipNum, 
         PolIdComp, PolIdComp2Sd,
         Age, Age2Sd, AgeCat, Gender, 
         Education, EducationNum,EducationNum2Sd,
         Race, MaritalStatus, Religion, SexualOrientation, HhSize, Region, 
         Income, Income1k, Income2Sd,
         Toxicity,BToxicNum,Productive,ProductiveNum,
         all_of(DVs), all_of(DVsDmd), 
         all_of(BinaryDVs),all_of(BinaryDVsDmd),
         CharCountText:PercToxic) %>% 
  left_join(create_dummies(dt,ID, Gender), by = "ID") %>% 
  left_join(create_dummies(dt,ID, Education), by = "ID") %>% 
  left_join(create_dummies(dt,ID, Race), by = "ID") %>% 
  left_join(create_dummies(dt,ID, MaritalStatus), by = "ID") %>% 
  left_join(create_dummies(dt,ID, Religion), by = "ID") %>% 
  left_join(create_dummies(dt,ID, SexualOrientation), by = "ID") %>% 
  left_join(create_dummies(dt,ID, HhSize), by = "ID") %>% 
  left_join(create_dummies(dt,ID, Region), by = "ID") %>% 
  left_join(create_dummies(dt,ID, IncomeQuantiles), by = "ID") 



# join categorized data
dc = read_csv(list.files(pattern="classified_comments.csv",recursive = T)) 



dt_clean = dc %>% 
  select(target_id,anticonservative,america,christianity,suggestive,drugs) %>% 
  right_join(dt_clean,by="target_id")

# lead data on join ideology of author of comment 
di = read_csv(list.files(pattern="target_ideology.csv",recursive=T)) %>% 
  distinct() %>% 
  rename(target_id=comment_id, target_user_id=user_id, ideo_commenterB=ideology) %>% 
  mutate(target_id = paste0("T",target_id)) %>% 
  group_by(target_id,target_user_id) %>%
  summarise(ideo_commenterB=mean(ideo_commenterB)) %>% 
  ungroup() %>% 
  mutate(ideo_commenterB = ideo_commenterB %>% ifelse(is.na(.),mean(.,na.rm=T),.))
  
all(count(di,target_id)$n==1)

# lead data on ideology of those who liked comments
dl = read_csv(list.files(pattern="target_like_ideology.csv",recursive=T)) %>% 
  distinct() %>% 
  rename(target_id=comment_id, ideo_like=ideology) %>% 
  mutate(target_id = paste0("T",target_id)) %>% 
  group_by(target_id) %>%
  summarise(ideo_like=mean(ideo_like,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(ideo_like = ideo_like %>% ifelse(is.na(.), mean(.,na.rm=T),.))
all(count(dl,target_id,sort=T)$n==1)

# join data
dt_clean = dt_clean %>% 
  left_join(di,by=c("target_id","target_user_id")) %>% 
  left_join(dl,by=c("target_id")) 

sum(is.na(dt_clean$ideo_commenterB))
sum(is.na(dt_clean$ideo_like))

# export data
fpath_export = strsplit(fpath, fname)[[1]]
write_csv(dt_clean, paste0(fpath_export, "dt_survey_0625_with_Toxicity_clean.csv"))


hist(dt_clean$ideo_commenterB)
hist(dt_clean$ideo_like)

#dt[c(Covs,DVs)]

# The broader problem with the data is that the 15 variables are on different scales. We have the following: 
#   -	4 items with 3 categories (Yes, No, Not Sure) transformed to a 0-1 items with values (0, 0.5, 1) 
# o	Understand B: Can you understand the point that Commenter B is trying to make?
#   o	B Makes Effort
# o	B Stay on topic
# o	Conversation is productive
# -	1 likert scale item 1-5 (Strongly Agree to Disagree) transformed to 0-1 item with values 0,0.25,0.5,1
# o	Agree with A: In your view, does Commenter B agree or disagree with Commenter A? 
#   -	7 Binary variables (Which of the following words would you use to describe Commenter B's attitude towards Commenter A? Please check all that apply.)
# o	BRespectful
# o	BOpen
# o	BObjective
# o	BEmotional
# o	BSarcastic
# o	BIntolerant
# o	BHostile
# o	BNoneabove
# -	B is Toxic (Not toxic, Maybe not sure, Toxic, Very toxic):  Rate the toxicity of Commenter B's comment. Transformed to 0-1 scale (0,.33,.66,1)
#                         