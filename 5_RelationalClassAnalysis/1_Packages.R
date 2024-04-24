### Preparing Packages
# List of packages for session
packages_to_load = c("proxy",
					"DescTools",
					"mfe",
					"minerva",
					"intrinsicDimension",
					"Rfast",
					"readstata13",
					"purrr",
					"dplyr",
					"scales",
					"matrixStats",
					"stargazer",
					"ggplot2",
					"corclass",
					"reshape2",
					"grid",
					"gridExtra",
					"wordcloud",
					"graphlayouts",
					"nnet",
					"readr",
					"broom",
					"corrplot",
					"ggthemes",
					"lsa",
					"viridis",
					"tidyr",
					"RCA",
					"lavaan", 
					"devtools",
					"MNLpred",
					"factoextra")

# Install CRAN packages (if not already installed)
already_installed <- packages_to_load %in% installed.packages()
if(length(packages_to_load[!already_installed]) > 0) install.packages(packages_to_load[!already_installed])

# Load packages into session 
lapply(packages_to_load, require, character.only = TRUE)

