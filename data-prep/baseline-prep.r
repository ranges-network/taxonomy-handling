## -------------------------------------------------- ##
# 'Baseline' Data Preparation
## -------------------------------------------------- ##
# PURPOSE
## Taxonomic fixes will also be released with some other quality of life changes to the full Ranges structure
## These include:
### 1. Separation of taxonomic and trait data for ease of future maintenance

# Load libraries
# install.packages("tidyverse")
librarian::shelf(tidyverse, supportR)

# Get set up
source(file = file.path("-setup.r"))

# Clear environment
rm(list = ls()); gc()

# Load Ranges data
baseline_v01 <- read.csv(file.path("data", "Ranges_Baseline_Traits_Jan-Feb2024.csv"))

# Check structure
dplyr::glimpse(baseline_v01)

## ---------------------------------- ##
# Remove Unidentified Taxa ----
## ---------------------------------- ##

# Remove any rows where a species is not known
baseline_df <- baseline_v01 %>% 
  dplyr::filter(nchar(scientificName) > 0) %>% 
  dplyr::filter(scientificName %in% c("1969-03-13", "1973-03-07", "1979-04-28") != TRUE)

# Check lost rows
message(nrow(baseline_v01) - nrow(baseline_df), " rows lost.")

## ---------------------------------- ##
# Separate Trait Data ----
## ---------------------------------- ##

# Make a traits-only data object
trait_v01 <- baseline_df %>% 
  dplyr::select(-scientificName:-specificEpithet)

# Check structure
dplyr::glimpse(trait_v01)

# Look for non-number latitude (class of that columns suggests a problem there)
supportR::num_check(data = trait_v01, col = "decimalLatitude")

# Ditch the non-number latitude value(s)
trait_v02 <- trait_v01 %>% 
  dplyr::mutate(decimalLatitude = as.numeric(dplyr::case_when(
    decimalLatitude == "decimal degrees" ~ NA,
    TRUE ~ decimalLatitude)))

# Check that worked and general structure
supportR::num_check(data = trait_v02, col = "decimalLatitude")
dplyr::glimpse(trait_v02)

# Make all empty cells into true `NA` cells
## NOTE: final data drops 147 MB if we do this (706 MB -> 559 MB)
trait_v03 <- trait_v02 %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .)))

# Check structure
dplyr::glimpse(trait_v03)

## ---------------------------------- ##
# Separate Taxonomy Data ----
## ---------------------------------- ##

# Make a taxonomy-only data object too
tax_v01 <- baseline_df %>% 
  dplyr::select(occurrenceID, scientificName:specificEpithet)

# Check structure
dplyr::glimpse(tax_v01)

# Collapse occurrence within a given taxon to remove 'duplicate' rows
tax_v02 <- tax_v01 %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(tax_v01), y = "occurrenceID")))) %>% 
  dplyr::summarize(occurrenceIdentifiers = paste(occurrenceID, collapse = " ;; "),
    .groups = "drop")

# Check structure
dplyr::glimpse(tax_v02)

# Do some useful wrangling of the taxonomy data now
tax_v03 <- tax_v02 %>%
  dplyr::mutate(nameAuthority = stringr::str_extract(string = scientificName, pattern = "\\s*\\([^)]*\\)")) %>% 
  dplyr::mutate(nameAuthority = gsub(pattern = "\\s*\\(|\\)", replacement = "", x = nameAuthority)) %>% 
  dplyr::mutate(scientificName = gsub("\\s*\\([^)]*\\)", "", scientificName)) %>% 
  dplyr::relocate(scientificName, occurrenceIdentifiers,
    .after = dplyr::everything())

# Check structure
dplyr::glimpse(tax_v03)

## ---------------------------------- ##
# Quality Control Check ----
## ---------------------------------- ##

# Confirm that no columns were lost
## Should only be the new taxonomy columns
supportR::diff_check(old = names(baseline_df), new = c(names(trait_v03), names(tax_v03)))

# See what was in baseline but not in trait data
## Should just be taxonomic columns
supportR::diff_check(old = names(baseline_df), new = names(trait_v03))

# See what was in baseline but not in taxonomy data
## Should be all trait columns (again, plus the new tax column)
supportR::diff_check(old = names(baseline_df), new = names(tax_v03))

## ---------------------------------- ##
# Export ----
## ---------------------------------- ##

# Export trait data
write.csv(x = trait_v03, row.names = FALSE, na = "",
  file = file.path("data", "ranges_traits-only.csv"))

# Export taxonomic data
write.csv(x = tax_v03, row.names = FALSE, na = "",
  file = file.path("data", "ranges_taxonomy-only.csv"))

# End ----
