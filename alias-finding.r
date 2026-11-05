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
# MDD Synonymy ----
## ---------------------------------- ##

# Read in MDD's synonymy file
mdd <- read.csv(file.path("data", "mdd", "Species_Syn_Current_v2.5.csv"))

# Check structure
dplyr::glimpse(mdd)

# Grab the rows relevant to the focal taxon
syn_sub <- mdd %>% 
  dplyr::filter(MDD_species == focal_tax) %>% 
  dplyr::select(MDD_species, MDD_normalized_original_combination, MDD_nomenclature_status)

# Check structure
dplyr::glimpse(syn_sub)
## view(syn_sub)

## ---------------------------------- ##
# Function Explore ----
## ---------------------------------- ##

# Write custom function
find_alias <- function(mdd_syn = NULL, sp = NULL){

  # Errors for 'mdd_syn'
  if(is.null(mdd_syn) || "data.frame" %in% class(mdd_syn) != TRUE ||
    all(c("MDD_species", "MDD_normalized_original_combination", "MDD_nomenclature_status") %in% names(mdd_syn)) != TRUE)
    stop("'mdd_syn' must be data.frame-like and include 'MDD_species', 'MDD_normalized_original_combination', and 'MDD_nomenclature_status' columns")

  # Errors for 'sp'
  if(is.null(sp) || is.character(sp) != TRUE || length(sp) > 1 || 
      stringr::str_count(string = sp, pattern = " ") != 1)
    stop("'sp' must be a single species name with genus and specific epithet separated by a space")

  # Pare down to just the bits of interest
  syn_sub <- mdd_syn %>% 
    dplyr::filter(MDD_species == sp) %>%
    dplyr::filter(MDD_species != MDD_normalized_original_combination) %>% 
    dplyr::select(MDD_species, MDD_normalized_original_combination, MDD_nomenclature_status)

  # Grab out the 'bad' names
  syn_vec <- syn_sub$MDD_normalized_original_combination

  # Warn if there's nothing
  if(length(syn_vec) == 0)
    warning("No aliases discovered.")
  
  # Return the vector either way though
  return(syn_vec)
}

find_alias(mdd_syn = mdd, sp = focal_tax)


## ---------------------------------- ##
# MDD Species ----
## ---------------------------------- ##

# # Load MDD taxonomy authority
# mdd_df <- read.csv(file.path("data", "mdd", "MDD_v2.5_6904species.csv"))

# # Check structure
# dplyr::glimpse(mdd_df) 

# # Subset down to just the taxon of interest
# mdd_sub <- mdd_df %>% 
#   dplyr::filter(sciName == gsub(pattern = " ", replacement = "_", x = focal_tax))

# # Check structure
# dplyr::glimpse(mdd_sub) 




# # Parse the 'nominalNames' column
# ## Split just this off
# test <- dplyr::select(mdd_sub, nominalNames)
# # Remove anything in parentheses or brackets (and leading whitespace of either)
# test$nominalNames <- gsub("\\s*\\([^)]*\\)|\\s*\\[[^)]*\\]", "", test$nominalNames)
# ## Count number of variants
# (var_ct <- stringr::str_count(string = test$nominalNames, pattern = "\\|"))
# ## Actually split up the info
# test %>% 
#   tidyr::separate_wider_delim(data = ., cols = nominalNames, delim = "|",
#     names = paste0("alias_", 1:ifelse(var_ct == 0, yes = 1, no = var_ct + 1)), 
#     too_few = "align_start")

# mdd_sub$originalNameCombination
# mdd_sub$nominalNames
# mdd_sub$CMW_sciName
# mdd_sub$MSW3_sciName


# purrr::map(.x = c("itis", "nbn", "worms"),
#   .f = ~ taxize::synonyms_df(x = taxize::synonyms(sci_id = focal_tax, db = .x)))


# End ----



