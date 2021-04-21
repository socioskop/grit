
#' get cumulative distribution func
#' @return transforms a vector to its' quantiles
#' @export
get.q <- function(x) {
  efunc <- ecdf(x)
  return(efunc(x))
}

#' plot 2d
#' @export
xgGeom2d <- function(data, x, y, log=T, log.X=F){
  data <- na.omit(d[,c(x, y)])
  if (log==T){data[[y]] <- log(data[[y]])}
  if (length(unique(data[[x]]))<=2) {data[[x]] <- data[[x]]+rnorm(nrow(data), 0, 0.1*sd(data[[x]]))}
  if (log.X==T) {data[[x]] <- log(data[[x]]-min(data[[x]])+1.001)}
  p <- ggplot(data=data)+
    geom_point(mapping=aes_string(paste0(x), paste0(y)), alpha=0.33, size=0.25)
  print(p)
}

#' plot 3d with time as x
#' @export
xgGeom3d <- function(data, x, y, z, log=T, log.X=F, log.Z=F){
  data <- na.omit(d[,c(x, y, z)])
  if (log==T){data[[y]] <- log(data[[y]])}
  if (length(unique(data[[x]]))<=2) {
    data[["xx"]] <- data[[x]]
    data[[x]] <- data[[x]]+rnorm(nrow(data), 0, 0.1*sd(data[[x]]))
  } else {
    steps <- round(seq(min(data[[x]]), max(data[[x]]), 2*sd(data[[x]])),1)
    steps <- unique(c(median(d[[x]]), quantile(d[[x]][d[[x]]!=median(d[[x]])], seq(0, 1, (1/3)))));steps <- steps[order(steps)]
    data[["xx"]] <- as.numeric(as.character(cut(data[[x]], seq(min(data[[x]]), max(data[[x]]), 2*sd(data[[x]])), labels=steps[1:(length(steps)-1)])))
    data[["xx"]] <- as.numeric(as.character(cut(data[[x]], breaks = c(-Inf, steps), right = F, include.lowest = T, labels=steps)))
    #data[["xx"]] <- ((data[["xx"]]+min(data[[x]])-1)/(max(d[[x]])-min(d[[x]])))
  }
  if (log.X==T) {data[[x]] <- log(data[[x]]-min(data[[x]])+1.001)}
  if (log.X==T) {data[[z]] <- log(data[[z]]-min(data[[z]])+1.001)}
  p <- ggplot(data=data)+
    geom_point(mapping=aes_string(paste0(x), paste0(y), colour=paste0(z)), alpha=0.33, size=0.25)+
    scale_color_gradient2(low = "blue", high = "red", mid="purple")+
    stat_summary(data=data, fun=median, mapping=aes_string(x = "xx", y=paste0(y)),
                 geom = "crossbar", color="black", size=.33, linetype="solid", width=sd(data[[x]])*.66)
  print(p)
}

#' plot AUC/ROC
#' @export
roc <- function(actual, predicted){
  actual <- actual[order(predicted, decreasing=T)]
  data.frame(TPR=cumsum(actual)/sum(actual), FPR=cumsum(!actual)/sum(actual))
}

#' check string data before converting to numeric
#'
#' returning true is character column can be converted to numeric without any losses
#' @export
is.ok <- function(x){
  (sum(!is.na(x))==sum(!is.na(as.numeric(as.character(x)))) & length(x[grepl(paste0(c(letters, "\\-"), collapse="|"), x)])==0)
}

#' check date column
#'
#' returning true is character column can be converted to date without any lost observations
#' @export
is.ok.date <- function(x){
  x <- x[!is.nan(x)]
  (sum(!is.na(x))==sum(!is.na(lubridate::as_date(x))))
}

#' short for conversion to character
#'
#' @export
as.chr <- function(x){as.character(x)}
is.chr <- function(x){is.character(x)}

#' short for conversion to numeric
#'
#' @export
as.num <- function(x){as.numeric(x)}
is.num <- function(x){is.numeric(x)}

#' short for conversion to dataframe
#'
#' @export
as.rdf <- function(x){as.data.frame(x)}

#' formats a numeric vector to a fixed string format with digits
#' @export
form.fix <- function(x, digits=3, percent=F){
  ifelse(percent==T, factor <- 100, factor <- 1)
  x <- sprintf(paste0("%.", digits, "f"), round(as.numeric(x*factor), digits)) #, nsmall=digits)
  return(x)
}

#' formats a numeric vector to a fixed string format with digits
#' @export
form.it <- function(x, digits=3, perc=F, max=NULL){
  if (!is.null(max)){
    x[x>max] <- Inf
  }
  x <- format(round(x, digits), nsmall=digits)
  if (perc==T){x <- paste0(x, "%")}
  return(x)
}

#' Inspection of big dictionaries
#'
#' @parm x is a data frame (or coercible) with ...
#' @parm col ...as the lookup column and...
#' @parm key ...as the keywords to look for.
#' @parm illegal are keywords, that can not be present (grep-like) in col entry
#' @parms subs are substring position 1 and 2, if only fixed parts of 'col' are relevant
#'
#' @return returns the 'rows of 'col' that are identified using the 'key' and the optional 'illegal' keywords.
#'
#' @export
view.it <- function(x, col, key, illegal=NULL, subs=NULL) {
  x <- as.rdf(x)
  if (!is.null(subs)){x[[col]] <- substr(x[[col]], subs[1], subs[2])}
  if (!is.null(illegal)){
    subject <- x[grepl(paste0(key, collapse="|"), x[[col]]) & !grepl(paste0(illegal, collapse="|"), x[[col]]),]
  } else {
    subject <- x[grepl(paste0(key, collapse="|"), x[[col]]),]
  }
  return(subject)
  View(subject)
}
