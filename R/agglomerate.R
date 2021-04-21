### Initial processing of raw data
#' agglomerates hierarchically nested codes (ICD10, ATC, etc.) to fewer, but more widespread categories than the initial input
#'
#' @parm data is a
#'
#' @export
agglomerate <- function(data, domain, id="pnr_enc", selection=NULL, skip=0, levels, threshold=0.01, censor.time=NULL, selection.pos=1, free=F, fixed=NULL){
  d <- as.data.frame(data); rm("data")

  # Settings & check
  if (is.null(d[["date"]])){stop("missing date column in data")}
  if (is.null(d[[domain]])){stop("missing domain column in data")}
  if (is.null(censor.time)){
    censor.time <- max(d[["date"]], na.rm=T)
  }

  # make sure levels are sorted
  levels <- levels[order(levels)]

  # store highest level of detail
  level.upper <- levels[length(levels)]

  # convert relative threshold to integer
  if (grepl("\\.", as.chr(threshold))){
    threshold <- ceiling(length(unique(d[[id]]))*as.num(threshold))
  }

  # Formatting according to skip and selection
  d <- d[!is.na(d[[domain]]) & !is.na(d$date),]
  if (!is.null(selection)){d <- d[substr(d[[domain]], selection.pos, selection.pos+nchar(selection)-1)==selection,]}
  if (skip>0){d[[domain]] <- substr(d[[domain]], skip+1, nchar(d[[domain]]))}

  # Generating hierarchical observations in separate columns
  for (level in levels){
    d[[paste0(domain, level)]] <- substr(d[[domain]], 1, level)
  }

  # Counting number of individuals per diagnosis subgroup
  data.table::setDT(d)
  d[ date<=censor.time[length(censor.time)] & !is.na(get(paste0(domain, level.upper))), paste0("affected", level.upper) := length(unique(get(id))), by=eval(paste0(domain, level.upper))]
  if (length(levels)>1){
    for (i in (length(levels)-1):1){
      level <- levels[i]
      level.above <- levels[i+1]
      if (length(censor.time)==2) {
        d[ date>=censor.time[1] &  date<=censor.time[2] & !is.na(paste0(domain, level)) & get(paste0("affected", level.above)) < threshold, paste0("affected", level) := length(unique(get(id))), by=eval(paste0(domain, level))]
      } else {
        d[ date<=censor.time & !is.na(paste0(domain, level)) & get(paste0("affected", level.above)) < threshold, paste0("affected", level) := length(unique(get(id))), by=eval(paste0(domain, level))]
      }
    }
  }

  # Organizing observations in agglomerated categories
  d[ get(paste0("affected", level.upper)) >= threshold, x := get(paste0(domain, level.upper))]
  if (length(levels)>1){
    for (i in (length(levels)-1):1){
      level <- levels[i]
      level.above <- levels[i+1]
      d[ get(paste0("affected", level)) >= threshold & get(paste0("affected", level.above)) < threshold , x := get(paste0(domain, level))]
    }
  }

  # allow for certain fixed code to pass, even if coverage is below threshold (keep it empirical, though! - or find another package for that sort of thing)
  if (!is.null(fixed)){
    for (f in fixed){
      d[ substr(get(domain), 1, nchar(f))==f , fix := f]
    }

    check <- list(pre.fix=table(d$x, d$fix))
    d$x[!is.na(d$fix)] <- d$fix[!is.na(d$fix)]
    check$post.fix <- table(d$x[!is.na(d$fix)], d$fix[!is.na(d$fix)])
    d$fix <- NULL
  }

  # retrieve codes
  o <- list()
  if (free==T){
    o[["d"]] <- as.data.frame(d)[,c(id, "date", "x")]
    o[[domain]] <- sort(table(d$x), decreasing = T)
    if (length(censor.time)==2) {
      o[[paste0(domain, ".free")]] <- unique(d[[id]][d[["date"]]>=censor.time[1] & d[["date"]]<=censor.time[2]])
    } else {
      o[[paste0(domain, ".free")]] <- unique(d[[id]][d[["date"]]<=censor.time])
    }
  } else {
    o[["d"]] <- as.data.frame(d)[,c(id, "date", "x")]
    o[[domain]] <- sort(table(d$x), decreasing = T)
  }

  if (!is.null(fixed)){o$fixed.check <- check}

  return(o)
}
