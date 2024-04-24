# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))

# load data
RCA = readRDS("fb_survey/5_RelationalClassAnalysis/RCA_ids.RDS")

fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels)) %>% 
  left_join(RCA,by = "ID")


count(dt, RCA)
