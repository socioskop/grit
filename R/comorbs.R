#' Generate indicators for Charlson Index, Elixhauser & custom diagnosis classifications
#'
#' Labels ICD 8 and 10 codes based on Elixhauser, Charlson and custom categories by
#' taking a column of ICD08/09/10 diagnoses and mapping those to Elixhauser, Charlson or a third
#' custom defined grouping (can be overwritten)
#'
#' @param data a data frame containing a column with ICD-codes. 10 and 08/09 are accepted.
#' @param diag.col name of column in data frame, that contains ICD codes. ICD10 codes should have a 'D' prefix.
#' @param types takes a character vector as input, containing one or more of the following values: 'elixh', 'charl' and 'custm'.
#' @param comorbs is a list corresponding to the output of the 'comorbs' function. It maks diagnoses to diagnosis groups
#'
#' @return the function returns the input dataframe, but adding the columns corresponding to each diagnosis group defined in the Elixhauser, Charlson or custom definitions.
#'
#' @export
encode.icd <- function(data, diag.col, types = c("elixh", "charl", "custm"), comorbs) {

  data <- as.data.frame(data)

  if (sum(paste0(paste0(types, "_names")) %in% names(comorbs))<length(types)){stop("scoring needs to be contained in the list comorbs and correspond to 'types' of encodings")}

  # list of ICD09/10 scoring keys
  for (type in types){

    # placeholders
    data[[paste0("diag_", type)]] <- NA

    #print(scoring[[type]])
    for (i in 1:nrow(comorbs[[paste0(type, "_names")]])){
      name <- comorbs[[paste0(type, "_names")]]$names [i]
      labl <- comorbs[[paste0(type, "_names")]]$labels[i]
      print(paste0(name, "/", labl))
      matchlist <- paste0(paste0("^", na.omit(
        if(type=="charl"){
          c(
            if(!is.null(comorbs$charl_icd09[[paste0(labl)]])){comorbs$charl_icd09[[paste0(labl)]]},
            if(!is.null(comorbs$charl_icd10[[paste0(labl)]])){paste0("D", comorbs$charl_icd10[[paste0(labl)]])}
          )
        } else if (type=="elixh") {
          c(
            if(!is.null(comorbs$elixh_icd09[[paste0(labl)]])){comorbs$elixh_icd09[[paste0(labl)]]},
            if(!is.null(comorbs$elixh_icd10[[paste0(labl)]])){paste0("D", comorbs$elixh_icd10[[paste0(labl)]])},
            if(!is.null(comorbs$elixh_resid[[paste0(labl)]])){paste0("D", comorbs$elixh_resid[[paste0(labl)]])}
          )
        } else if (type=="custm") {
          c(
            if(!is.null(comorbs$custm_icd09[[paste0(labl)]])){comorbs$custm_icd09[[paste0(labl)]]},
            if(!is.null(comorbs$custm_icd10[[paste0(labl)]])){paste0("D", comorbs$custm_icd10[[paste0(labl)]])}
          )
        }
      ),
      collapse = "|"), "")
      print(paste0(labl, ": ", matchlist))

      # indicate if diagnosis is present
      data[[paste0("diag_", type)]][is.na(data[[paste0("diag_", type)]]) & grepl(matchlist, data[[diag.col]])] <- name
    }
  }
  return(data)
}

#' charlson index scoring function
#'
#' function takes a data frame with Charlson diagnosis groups as columns and generates a Charlson comorbidity index score for each row
#'
#' @param x is a data frame with columns corresponding to the names defined in comorbs$charl_names
#' @param cats is a logical indication of whether output should be the categorical 0, 1, 2 score or just the default count variable
#'
#'  @return returns a vector of length = nrow(x) with charlson scores
#'
#'  @export
charl_score <- function(x, cats=F) {
  x[is.na(x)] <- 0
  x[x>1] <- 1
  charl <- with(x, minfar + heartf + pervas + cerebr + dement +
                  copdis + rheuma + peptic + liverm * ifelse(liverd >= 1, 0, 1) +
                  diabeu * ifelse(diabec >= 1, 0, 1) +
                  diabec * 2 + parapl * 2 + renald * 2 + cancer * ifelse(metaca == 1, 0, 2) +
                  liverd * 3 + metaca * 6 + aidshv * 6)
  if (cats==T){
    charl <- ifelse(charl==0, "0", ifelse(charl %in% c(1, 2), "1-2", ifelse(charl >= 3, "3+", as.chr(charl))))
  }
  return(charl)
}
