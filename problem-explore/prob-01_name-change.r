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




# End ----
