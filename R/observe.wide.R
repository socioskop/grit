#' count observations from several columns in some interval
#'
#' reduces repeated measures (for id) of a set of count varaibles
#'
#' @param data a data frame with a 'date' column, an index.date column, an id column and a set of count variables to be reduced to one row per id (n = length(unique(ids))).
#' @param id name of id column.
#' @param index.date name of index date column
#' @param vars vector of names of the count variables to be reduced
#' @param intervel the limits of an interval expressed as integer days relative to the index date
#'
#' @return a data frame with one row per id and columns with counts of input vars within interval
#'
#' @export
observe.wide <- function(data, id, index.date, vars, interval){
  d <- as.data.frame(data)[,c(id, index.date, vars, "date")]

  # subsetting on specified date interval
  d$time <- as.numeric(difftime(as.Date(d$date), as.Date(d[[index.date]]), units = "days"))

  # drop rows with missing timing
  d <- d[(d$time>=interval[1] & d$time<=interval[2]) & !is.na(d$time),]

  # blanking out values outside of accepted interval
  for (i in vars){
    print(paste0("removing invalid values for ", i))
    d[[i]][d$time<interval[1] | d$time>interval[2]] <- NA
    #d$time[d$time<interval[1] | d$time>interval[2]] <- NA
  }

  # counting observations of all relevant values per person in interval
  data.table::setDT(d)
  for (i in vars){
    print(paste0("counting values for ", i))
    d[ , (i) := sum(get(i)), by=id]
  }
  d <- unique(as.data.frame(d)[,c(id, vars)])
  return(d)
}
