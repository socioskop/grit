#' count observations from one 'value' variable containing qualitative codes of any kind
#'
#' @param data is a dataframe with an id column, an index.date column, a value column and a date column ('date')
#' @param id name of id column (multiple instances within id are aggregated to one row)
#' @param index.date name of column containing and index/reference date
#' @param value name of column with values to be counted and aggregated by id
#' @param interval two outer limits as days (integer) relative to index. Positive integers look ahead, negative integers look back, (-10, +10) will observe 10 days around (pre/post) the index date
#'
#' @return returns a data frame of one row per id, that counts all incidences of all valid values within interval of days relative to index date
#'
#' @export
observe.count <- function(data, id, index.date, value, interval, valid.values, bin=F){
  d <- as.rdf(data)[,c(id, index.date, value, "date")]; data <- NULL
  d <- d[!is.na(d[[value]]),]

  # subsetting on specified date interval
  d$time <- as.numeric(difftime(d$date, d[[index.date]], units = "days"))
  d[[value]][(d$time<interval[1] | d$time>interval[2])] <- NA
  d$time[d$time<interval[1] | d$time>interval[2]] <- NA
  d <- d[(d$time>=interval[1] & d$time<=interval[2]) | is.na(d$time),]

  # ignoring values from invalid codes
  if (!is.null(valid.values)){
    d[[value]][!d[[value]] %in% valid.values] <- NA
    d <- d[!is.na(d[[value]]),]
  }

  # counting observations of all relevant values per person in interval
  d$k <- as.numeric(!is.na(d[[value]]))
  d <- data.table::data.table(d)
  d[ , k := sum(k), by=c(id, value)]
  d <- as.data.frame(d)[,c(id, index.date, value, "k")]
  d <- unique(d) ## sorting out two registrations on same date

  # if binary indicators should replace counts:
  if (bin==T){d$k <- as.numeric(d$k>=1)}

  # reshaping counts of observations to wide (each value has its own column)
  e <- list()
  for (v in unique(d[[value]])){
    print(paste0("building counts for ", v, "..."))
    e[[v]] <- reshape::cast(d[d[[value]]==v,], paste0(id,"~",value), sum, value="k")
  }
  try(e[["NA"]] <- NULL, silent=T)
  e <- purrr::reduce(e, dplyr::full_join, by=id)
  return(e)
}
