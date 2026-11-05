## -------------------------------------------------- ##
# Alias Finding
## -------------------------------------------------- ##
# PURPOSE
## Core to all taxonomy problems is finding the aliases for the taxon of interest
## Figuring out how to do that is the purpose of this script

# Load libraries
# install.packages("tidyverse")
librarian::shelf(tidyverse, taxize)

# Get set up
source(file = file.path("-setup.r"))

# Clear environment
rm(list = ls()); gc()

# Identify the current (correct) name for the taxon of interest
focal_tax <- "Poliocitellus franklinii"

## ---------------------------------- ##
# MDD Species ----
## ---------------------------------- ##

# Load MDD taxonomy authority
mdd_df <- read.csv(file.path("data", "mdd", "MDD_v2.5_6904species.csv"))

# Check structure
dplyr::glimpse(mdd_df) 

# Subset down to just the taxon of interest
mdd_sub <- mdd_df %>% 
  dplyr::filter(sciName == gsub(pattern = " ", replacement = "_", x = focal_tax))

# Check structure
dplyr::glimpse(mdd_sub) 




# Parse the 'nominalNames' column
## Split just this off
test <- dplyr::select(mdd_sub, nominalNames)
# Remove anything in parentheses or brackets (and leading whitespace of either)
test$nominalNames <- gsub("\\s*\\([^)]*\\)|\\s*\\[[^)]*\\]", "", test$nominalNames)
## Count number of variants
(var_ct <- stringr::str_count(string = test$nominalNames, pattern = "\\|"))
## Actually split up the info
test %>% 
  tidyr::separate_wider_delim(data = ., cols = nominalNames, delim = "|",
    names = paste0("alias_", 1:ifelse(var_ct == 0, yes = 1, no = var_ct + 1)), 
    too_few = "align_start")

mdd_sub$originalNameCombination
mdd_sub$nominalNames
mdd_sub$CMW_sciName
mdd_sub$MSW3_sciName


purrr::map(.x = c("itis", "nbn", "worms"),
  .f = ~ taxize::synonyms_df(x = taxize::synonyms(sci_id = focal_tax, db = .x)))


## ---------------------------------- ##
# MDD Synonymy ----
## ---------------------------------- ##

# Read in MDD's synonymy file
mdd_syn <- read.csv(file.path("data", "mdd", "Species_Syn_Current_v2.5.csv"))

# Check structure
dplyr::glimpse(mdd_syn)

# End ----



