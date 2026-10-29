## -------------------------------------------------- ##
# Setup
## -------------------------------------------------- ##
# PURPOSE
## Do generally-useful setup operations (that multiple other scripts will likely want to invoke)

## ---------------------------------- ##
# Make Folders ----
## ---------------------------------- ##

# Create necessary data-related folders
dir.create(path = file.path("data", "mdd"), showWarnings = FALSE, recursive = TRUE)
dir.create(path = file.path("data", "diagnostic"), showWarnings = FALSE)

# Create non-data folders
dir.create(path = file.path("graphs"), showWarnings = FALSE)

# End ----
