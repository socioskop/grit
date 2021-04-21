
#' key dictionary
#'
#' a list with two vectors. One vector store column names for possible column names for personal ids.
#' The other vector is column names for the record id column labels. These are aligned, so they are rename to 'pnr' or 'rid' when pulling data
#' @format just two character vectors in a list
#'
#' @source keys are added manually when they are encountered in source data.
pnrs <- c("v_pnr_encrypted", "personnummer_encrypted", "patient_cpr_encrypted", "cpr_encrypted", "v_cpr_encrypted", "k_cprnr_encrypted")
rids <- c("k_recnum", "v_recnum", "kontakt_id_encrypted", "kontakt_id")

key.dict <- list(pnrs=pnrs, rids=rids)
usethis::use_data(key.dict, overwrite = T)
