#' observe the last numeric value observed for any id in any interval
#'
#' a long dataset (repeated measures within id) with a qualitative code (metric id) and its coresponding value will be reduced to just the last value of any set of codes before the deadline (index date)
#'
#' @param data a data frame with a 'date' column, an index.date column, an id column, a code (metric id) column and a value column to be reduced to one row per id with the latest observation of any valid code.
#' @param id name of id column.
#' @param index.date name of index date column
#' @param code name of column containing the metric id (when several metrics are just stacked in a long dataframe)
#' @param value name of column with actual value corresponding to the label of metric contained in the code column
#' @param intervel the limits of an interval expressed as integer days relative to the index date
#' @param valid.codes optional list of valid codes to weed out irrelevant measures
#' @param text a boolean indicator of whether values are categorical / contains letters.
#'
#' @return a data frame with one row per id and columns with counts of input vars within interval
#'
#' @export
observe.last <- function(data, id, index.date, code, value, interval, valid.codes, text=F){
  d <- data[!is.na(data[[value]]),c(id, index.date, code, value, "date")]; rm("data")

  # subsetting on specified date interval
  d$time <- as.numeric(difftime(d$date, d[[index.date]], units = "days"))

  # drop rows with missing timing or timestamps outside of accepted range
  d <- d[(d$time>=interval[1] & d$time<=interval[2]) & !is.na(d$time),]

  # ignoring values from invalid codes
  if (!is.null(valid.codes)){
    d <- d[d[[code]] %in% valid.codes,]
  }

  # picking last non-NA
  data.table::setDT(d)
  d <- d[order(get(id), date),]
  d[ !is.na(get(value)), n := 1:.N, by=c(id, code)]
  d[ !is.na(get(value)), N :=   .N, by=c(id, code)]

  ## ...by wiping non-last values
  d[ n!=N , (value) := NA ]
  d <- as.data.frame(d)[!is.na(d[[value]]),c(id, index.date, code, value)]

  # reshaping all last observations to wide (each code has its own column)
  e <- list()
  for (c in unique(d[[code]])){
    print(paste0("building counts for ", c, "..."))
    if (text==T){
      e[[c]] <- reshape::cast(d[d[[code]]==c & !is.na(d[[value]]),], paste0(id,"~",code), function(x) names(sort(table(x), T))[1], value=value)
    } else {
      e[[c]] <- reshape::cast(d[d[[code]]==c & !is.na(d[[value]]),], paste0(id,"~",code), function(x) mean(as.numeric(x), na.rm=T), value=value)
    }
  }
  try(e[["NA"]] <- NULL, silent=T)
  return(purrr::reduce(e, dplyr::full_join, by=id))
}
