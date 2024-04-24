# run setup script to load data and functions
source(list.files(pattern="0_Setup.R", recursive=T))
source("fb_survey/5_RelationalClassAnalysis/2_Functions.R")

# load data
fname = "dt_survey_0625_with_toxicity_clean.csv"
fpath = list.files(pattern = fname, recursive = T)
dt = read_csv(fpath, col_types = c("target_id"="character")) %>% 
  mutate(Partisanship = factor(Partisanship, levels=PartisanshipLevels), 
         PolId = factor(PolId,levels=PolIdLevels))

# List of relevant variables (ANES code)
vars = c("ID", DVsDmd)

# Names of relevant variables
labels = c("ID", DVsDmd)

dt = dt[, vars]

# Replace names
colnames(dt) = labels

# Clean data (drop year and ID, make matrix, set all values > 110 or < 0 to NA, add 1 to every value (so that no variable has 0))
dt_run = dt[,2:length(dt)]
dt_run = as.matrix(dt_run)
#dt_run[] = ifelse(dt_run[] < 0 | dt_run > 110, NA, dt_run[])
#dt_run[] = dt_run[] + 1

# Add cleaned data back to regular data (basically preserving year and id which were dropped as part of cleaning stage)
dt[,2:length(dt)] = dt_run

dt <- na.omit(dt)

# Apply rescale and categorize function dataset (saving as new name so RCA can be run on each)
dt_cat = dt
dt_cat[,2:length(dt_cat)] = rescale_cat(dt_cat[,2:length(dt_cat)])
dt_cat[,2:length(dt_cat)] = sapply(dt_cat[,2:length(dt_cat)], FUN = function(x) as.numeric(as.character(x)))

# Run RCA on each (starting with categories)
set.seed(1016)
rca_out_2016 = rca.full.3(as.matrix(dt_cat[,2:length(dt_cat)]), amirsd = F)

# Make a data.frame with the class and pole assignments
ids_to_classes = data.frame(ID = dt_cat$ID,
                            RCA = rca_out_2016$membership,
                            stringsAsFactors = F)


saveRDS(ids_to_classes, "fb_survey/5_RelationalClassAnalysis/RCA_idsDmd.RDS")
