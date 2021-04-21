#' observe date of incidence for any value (qualitative code, e.g. ICD-10 codes or derivates)
#'
#' a long dataset (repeated measures within id) with a set of timestamped qualitative codes will be reduced to just the date of first occurence each code before the deadline (index date)
#'
#' @param data a data frame with a 'date' column, an index.date column, an id column, a value column to be reduced to one row per id with the timestamp of the earliest observation of any valid value.
#' @param id name of id column.
#' @param index.date name of index date column; used for relative measures of incidence
#' @param value name of column with actual value corresponding to the label of metric contained in the code column
#' @param valid.values optional list of valid codes to weed out irrelevant measures
#' @param relative logical indicating whether or not incidence dates are number of days relative to index.date
#' @return a data frame with one row per id and columns for each valid type of value containing the timestamp (date) of incidence
#'
#' @export
observe.incid <- function(data, id, index.date, value, valid.values, relative=F){
  d <- as.data.frame(data)[,c(id, index.date, value, "date")]

  # ignoring values from invalid codes
  if (!is.null(valid.values)){
    d <- d[d[[value]] %in% valid.values,]
  }

  # picking rows with date equal to incid date
  d <- data.table::data.table(d)
  d[, incid := min(date, na.rm=T), by=c("pnr_enc", paste(value)) ]
  d <- d[date==incid]

  # reshaping all last observations to wide (each code has its own column)
  d <- unique(d)
  e <- reshape::cast(d, paste0(id, "+", index.date, "~", value))
  e <- dplyr::mutate_if(e, is.ok.date, lubridate::as_date)
  if (relative==T){
    for (col in colnames(e)[colnames(e) %in% valid.values]){
      e[[col]] <- as.num(e[[col]]-as.Date(e[[index.date]]))
    }
  }
  try(e[["NA"]] <- NULL, silent=T)
  return(e)
}
