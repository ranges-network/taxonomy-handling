## -------------------------------------------------- ##
# Taxonomy Problem #1: Name Changes
## -------------------------------------------------- ##
# PURPOSE
## Explore solutions to a particular taxonomic name change
## This category is a simple, 1:1 change, _not_ a lump or split

# Load libraries
# install.packages("tidyverse")
librarian::shelf(tidyverse)

# Get set up
source(file = file.path("-setup.r"))

# Clear environment
rm(list = ls()); gc()

# Load custom functions
purrr::walk(.x = dir(file.path("tools"), pattern = "*.r"),
  .f = ~ source(file.path("tools", .x)))

# Identify the current (correct) name for the taxon of interest
focal_tax <- "Poliocitellus franklinii"

## ---------------------------------- ##
# Find Aliases ----
## ---------------------------------- ##

# Read in MDD's synonymy file
mdd_db <- read.csv(file.path("data", "mdd", "Species_Syn_Current_v2.5.csv"))

# Check structure
dplyr::glimpse(mdd_db)

# Find the syonyms/aliases of the provided taxon
(syns <- find_alias(mdd = mdd_db, sp = focal_tax))

## ---------------------------------- ##
# Find Focal Taxon's Trait Data ----
## ---------------------------------- ##

# Load network trait data
rng_df <- read.csv(file.path("data", "Ranges_Baseline_Traits_Jan-Feb2024.csv"))

# Check structure
dplyr::glimpse(rng_df)

# Do needed wrangling
rng_df_v2 <- rng_df %>% 
  dplyr::mutate(
    sciTemp = paste(genus, specificEpithet),
    sciNoAuth = gsub(pattern = "\\s*\\([^)]*\\)", replacement = "", scientificName),
    .before = dplyr::everything())

# Identify matches to any synonyms of the focal taxon
rng_sub <- rng_df_v2 %>% 
  dplyr::filter(sciTemp %in% c(focal_tax, syns) |
    sciNoAuth %in% c(focal_tax, syns))

# Check some key columns
unique(rng_sub$scientificName)
unique(rng_sub$sciNoAuth)
unique(rng_sub$sciTemp)

# Check structure
dplyr::glimpse(rng_sub)

## ---------------------------------- ##
# Diagnose Severity of Change ----
## ---------------------------------- ##

# Quantify number of traits/institutions that would be affected by a change to the name
rng_qc <- rng_sub %>% 
  ## Can exclude rows where the focal taxon is already correct
  dplyr::filter(sciNoAuth != focal_tax) %>% 
  ## Flip to long format in order to count traits with observations
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>% 
  tidyr::pivot_longer(cols = -sciTemp:-fieldNotes,
    names_to = "trait", values_to = "value") %>% 
  dplyr::filter(!is.na(value) & nchar(value) > 0) %>% 
  ## Remove traits where changes would be low severity
  dplyr::filter(!trait %in% c("sex") & 
    stringr::str_detect(string = trait, pattern = "_inferred") != TRUE)

# Check structure
dplyr::glimpse(rng_qc)
unique(rng_qc$sciNoAuth)

# Message how many traits are affected
message(nrow(rng_qc), " observations of ", length(unique(rng_qc$trait)), " traits would be affected by changing this species name (out of ", nrow(rng_sub), " observations of this taxon by any name with data for at least one trait)")

# If there's anything here, export it as a diagnostic
if(nrow(rng_qc) != 0){
  write.csv(x = rng_qc, row.names = FALSE, na = '',
    file = file.path("data", "diagnostic", paste0("type-1-name-change-effects_", 
      gsub(" ", "-", tolower(focal_tax)), ".csv")))
}

## ---------------------------------- ##
# Replace Synonyms ----
## ---------------------------------- ##

# Identify the taxonomic authority of the correct name
tax_auth <- rng_sub %>% 
  dplyr::filter(sciNoAuth == focal_tax) %>% 
  dplyr::mutate(taxAuthority = gsub(pattern = focal_tax, replacement = "", x = scientificName)) %>% 
  dplyr::pull(taxAuthority) %>% 
  unique()

# Append it to the focal taxon vector
(focal_w_tax <- paste0(focal_tax, tax_auth))

# Split off rows that won't be affected
rng_df_nochange <- rng_df_v2 %>% 
  dplyr::filter(sciNoAuth %in% syns != TRUE)

# Actually do the replacing of the taxonomic name where it's needed
rng_df_change <- rng_df_v2 %>% 
  dplyr::filter(sciNoAuth %in% syns) %>% 
  dplyr::mutate(change_status = "species name updated",
    .before = dplyr::everything()) %>% 
  dplyr::mutate(scientificName = focal_w_tax[1])

# Make sure that split doesn't lose any rows
nrow(rng_df_v2) == nrow(rng_df_nochange) + nrow(rng_df_change)

# Recombine
rng_df_revized <- dplyr::bind_rows(rng_df_change, rng_df_nochange) %>% 
  dplyr::select(-sciTemp, -sciNoAuth)

# Make sure we didn't gain/lose any columns (other than 'change_status')
supportR::diff_check(old = names(rng_df), new = names(rng_df_revized))

# Check structure
dplyr::glimpse(rng_df_revized)

# Export it locally
write.csv(rng_df_revized, row.names = FALSE, na = "",
  file.path("data", "revision-quarantine", paste0("Ranges_Baseline_Traits_Jan-Feb2024", "_updated-on-", Sys.Date(), ".csv")))

# End ----
