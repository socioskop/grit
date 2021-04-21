#' pulling data from local RDS database
#'
#' @param path path to local project-specific database (db_build output folder)
#' @param vw name of view file / SQL table to pull
#' @param ids optional vector of ids to pull (ignoring all non-matching rows). If ids are record ids, specify id.type as 'rid'.
#' @param vars optional list of variables to pull.
#' @param id.type 'pnr' for personal id number, 'pid' for using record ids as key.
#' @return returns an RDS file containing specified source data. If ids are specified, only rows matching an id are returned.
#'
#' @export
# SQL pulling from local csv db
local.pull <- function(path, vw, ids=NULL, vars=NULL, id.type="pnr"){

  # avoid integers to chr errors
  options(scipen=999)

  # print status
  print(paste0("pulling local ", vw, " at ", Sys.time()))

  # map sources
  if (file.exists(paste0(path, "/", toupper(vw), ".RDS"))){
    sources <- paste0(path, "/", toupper(vw), ".RDS")
  } else {
    sources <- list.files(paste0(path, "/", toupper(vw)))
  }

  # stop if no data exists as expected
  if (length(sources)==0) {
    stop("data not there")
  }

  # pick id type's names
  if (id.type %in% c("pnr")){
    id.ref <- key.dict$pnrs
  } else if (id.type %in% c("rid")){
    id.ref <- key.dict$rids
  } else if (!is.null(ids) | !is.null(id.type)){
    stop("id.type should be either pnr or rid")
  }

  # read column names
  if (length(sources)==1){
    tmp <- readRDS(sources)
    vw.cols <- colnames(tmp)

    # identify id columns
    id.col <- vw.cols[vw.cols %in% id.ref]
    if (length(id.col)==0){
      return(tmp)
    } else if (length(id.col)>1){
      stop("multiple possible id cols in source")
    } else {
      colnames(tmp)[colnames(tmp)==id.col] <- paste0(id.type, "_enc")
      if (is.null(vars)){vars <- colnames(tmp)}
      return(tmp[,colnames(tmp)[colnames(tmp) %in% c(paste0(id.type, "_enc"), vars)]])
    }
  } else if (length(sources)>1) {

    # collect data and return as one object
    if (is.null(ids)){
      tmp <- list()
      for (s in sources){
        print(paste0("reading ", s, " (", which(sources==s), "/", length(sources), ") at ", Sys.time()))
        tmp[[s]] <- readRDS(paste0(path, "/", vw, "/", s))
        colnames(tmp[[s]])[colnames(tmp[[s]]) %in% id.ref] <- paste0(id.type, "_enc")
        if (is.null(vars)){vars <- colnames(tmp[[s]])}
        tmp[[s]] <- tmp[[s]][,colnames(tmp[[s]])[colnames(tmp[[s]]) %in% c(paste0(id.type, "_enc"), vars)]]
      }
      return(dplyr::bind_rows(tmp))
    }

    # only keep data where ids match input ids
    if (!is.null(ids)){

      # split ids
      #if (id.type=="rid"){
      #  end <- ifelse(length(unique(substr(ids, 1, 1)))==1, 2, 1) # split at 2nd character if 1st are identical
      #} else {
        end <- max(nchar(gsub("\\..*", "", sources)))
      #}
      ids <- ids[order(ids)]
      ids <- split(as.character(ids), factor(substr(ids, 1, end), levels=unique(substr(ids, 1, end))))

      # force length of source names to be equal to 'end' parameter
      names(ids) <- stringr::str_pad(names(ids), end, pad="0")

      # collect data and return as one object
      tmp <- list()
      for (s in names(ids)){
        print(paste0("reading ", s, " (", which(names(ids)==s), "/", length(ids), ") at ", Sys.time()))
        try(tmp[[s]] <- readRDS(paste0(path, "/", vw, "/", s, ".RDS")))
        if (is.null(tmp[[s]])){print(paste0("no data exists for id subset ", s, ". skipping.")); next}
        vw.col <- colnames(tmp[[s]])
        id.col <- vw.col[vw.col %in% id.ref]
        colnames(tmp[[s]])[vw.col==id.col] <- paste0(id.type, "_enc")
        if (is.null(vars)){vars <- colnames(tmp[[s]])}
        tmp[[s]] <- tmp[[s]][tmp[[s]][[paste0(id.type, "_enc")]] %in% unlist(ids[[s]]),colnames(tmp[[s]])[colnames(tmp[[s]]) %in% c(paste0(id.type, "_enc"), vars)]]
      }
      return(dplyr::bind_rows(tmp))
    }
  }
}
