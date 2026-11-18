## -------------------------------------------------- ##
# MDD Synonym Data Preparation
## -------------------------------------------------- ##
# PURPOSE
## Pare down the Mammal Diversity Database (MDD) synonymy file to only the useful bits

# Load libraries
# install.packages("tidyverse")
librarian::shelf(tidyverse, supportR)

# Get set up
source(file = file.path("-setup.r"))

# Clear environment
rm(list = ls()); gc()

# Identify the relevant file
mdd_synfile <- "Species_Syn_Current_v2.5.csv"

# Load MDD data
mdd_v01 <- read.csv(file.path("data", "mdd", mdd_synfile))

# Check structure
dplyr::glimpse(mdd_v01)

## ---------------------------------- ##
# Pare Down MDD Info ----
## ---------------------------------- ##

# Get only needed columns
mdd_v02 <- mdd_v01 %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "MDD_", replacement = "", x = .)) %>% 
  dplyr::select(species, normalized_original_combination,
    validity, order, family, genus, specificEpithet, author, year)

# Check structure
dplyr::glimpse(mdd_v02)

# Match names to what's in baseline taxonomy columns
mdd_v03 <- mdd_v02 %>% 
  dplyr::rename(scientificName = species) %>% 
  dplyr::relocate(order:specificEpithet,
    .before = dplyr::everything()) %>% 
  dplyr::mutate(nameAuthority = paste(author, year),
    .after = specificEpithet) %>% 
  dplyr::select(-author, -year)

# Check structure
dplyr::glimpse(mdd_v03)

# Get only unique rows (should be only a few)
mdd_v04 <- dplyr::distinct(mdd_v03)

# Check structure
dplyr::glimpse(mdd_v04)

## ---------------------------------- ##
# Export ----
## ---------------------------------- ##

# Make a final object
mdd_v99 <- mdd_v04

# Check its structure
dplyr::glimpse(mdd_v99)

# Get the version number from the filename
(mdd_ver <- stringr::str_extract(string = mdd_synfile, pattern="v\\d*\\.\\d*"))

# Export it
write.csv(x = mdd_v99, row.names = FALSE, na = "",
  file = file.path("data", "mdd", paste0("mdd_", mdd_ver, "_synonyms.csv")))

# End ----
