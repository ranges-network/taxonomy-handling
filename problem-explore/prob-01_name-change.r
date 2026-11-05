## -------------------------------------------------- ##
# Taxonomy Problem #1: Name Changes
## -------------------------------------------------- ##
# PURPOSE
## Explore solutions to a particular taxonomic name change
## This category is a simple, 1:1 change, _not_ a lump or split

# Load libraries
# install.packages("tidyverse")
librarian::shelf(tidyverse, taxize)

# Get set up
source(file = file.path("-setup.r"))

# Clear environment
rm(list = ls()); gc()

# Load network trait data
rng_df <- read.csv(file.path("data", "Ranges_Baseline_Traits_Jan-Feb2024.csv"))

# Check structure
dplyr::glimpse(rng_df)

# Identify the current (correct) name for the taxon of interest
focal_tax <- "Poliocitellus franklinii"

## ---------------------------------- ##
# Prepare Taxonomic Authority ----
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





# End ----
