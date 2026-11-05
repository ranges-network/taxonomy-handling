#' @title Find Mammal Diversity Database (MDD) Aliases for Focal Taxon
#' 
#' @description When provided an MDD synonymy dataframe and a single focal species (genus and specific epithet), returns a vector of MDD-recognized "aliases" (a.k.a. "synonyms") for the focal species.
#' 
#' @param mdd (data.frame) MDD-style synonym dataframe. Must be data.frame-like (i.e., can be a tibble), and have the following columns: "MDD_species", "MDD_normalized_original_combination", "MDD_nomenclature_status"
#' @param sp (character) Single species for which to find synonyms. Scientific name components (genus and specific epithet) must be separated by a space.
#' 
#' @importFrom magrittr %>%
#' 
#' @return (character) Vector of MDD-recognized aliases/synonyms for the provided taxon
#' 
find_alias <- function(mdd = NULL, sp = NULL){

  # Errors for 'mdd'
  if(is.null(mdd) || "data.frame" %in% class(mdd) != TRUE ||
    all(c("MDD_species", "MDD_normalized_original_combination", "MDD_nomenclature_status") %in% names(mdd)) != TRUE)
    stop("'mdd' must be data.frame-like and include 'MDD_species', 'MDD_normalized_original_combination', and 'MDD_nomenclature_status' columns")

  # Errors for 'sp'
  if(is.null(sp) || is.character(sp) != TRUE || length(sp) > 1 || 
      stringr::str_count(string = sp, pattern = " ") != 1)
    stop("'sp' must be a single species name with genus and specific epithet separated by a space")

  # Pare down to just the bits of interest
  syn_sub <- mdd %>% 
    dplyr::filter(MDD_species == sp) %>%
    dplyr::filter(MDD_species != MDD_normalized_original_combination) %>% 
    dplyr::select(MDD_species, MDD_normalized_original_combination, MDD_nomenclature_status)

  # Grab out the 'bad' names
  syn_vec <- syn_sub$MDD_normalized_original_combination

  # Warn if there's nothing
  if(length(syn_vec) == 0)
    warning("No aliases discovered.")
  
  # Return the vector either way though
  return(syn_vec) }

# End ----
