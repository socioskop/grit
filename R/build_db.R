#' building a RDS database from SQL source data
#'
#' The health authority uses access-restricted SQL tables to deliver data to users.
#' This is probably a link to underlying SAS data. But access is also restricted by server side memory.
#' So pulling big data chunks often gives a 'java heap memory' error.
#' To overcome this, we slice big data-sets in chunks by 1- or 2-level prefixes of table keys (cpr or record id)
#' and then store these chunks in a local database made up of RDS files in a structure resembling
#' the original SQL table structure. This solves the problem with java heap memory limits and
#' it also enables us to have parallel access to data (which is not really feasible with one SQL connection).
#'
#' @param projct 4-digit project number, assuming first four are zeros
#' @param db_dir path to where local db should be written. Will overwrite db_dir
#' @param prefix the initial number of characters to use for data slicing. Will be extended by one, if a first character in a key is constant across the table
#' @param prefix.add add names of any tables that will need an extra character on the slicing prefix
#' @param mode 'clear' will overwrite everything again, 'fill' will fill those chunks that are expected based on SQL source, but missing in local db. If really big data generates java errors even when slicing, then this option will work for iterative db-building in a loop. Check log and data index carefully when using 'fill' option.
#' @param drv Driver for the sql connection. Is a long URL with MS SQL parameters you get with data delivery.
#' @param conn_def the 'conn' parameter in DBI-requests. This is the definition of the SQL connection. Also acquired when you get data from the Health Data Authority
#'
#' @return this function will return a RDS database in a folder-file structure resembling the original SQL tables, but with all data content slices. Will also add a log file in the db_dir.
#'
#' @export
build_db <- function(projct, db_dir, prefix, prefix.add=NULL, mode,
                     drv=RJDBC::JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","c:/sqljdbc42.jar"),
                     conn_def="jdbc:sqlserver://fm-sql;databaseName=Forsker;integratedSecurity=true"){

  # make sure output dir exists
  dir.create(db_dir, recursive = T)

  # logging
  sink(paste0(db_dir, "/00_build_rdb.log"))
  print(paste0("building db, slicing on first ", prefix, " characters"))

  # get list of data sources accessed by project
  con <- DBI::dbConnect(drv, conn_def)
  vws <- DBI::dbGetQuery(con, paste0("select name from sys.objects where type in('V','U') and schema_id=Schema_ID('FSEID0000", projct, "')"))[,1]
  try(DBI::dbDisconnect(con))

  # exclude or push to last iterations the biggest chunks not relevant for population definition/matching
  last <- c("LMS_EPIKUR", "DS_LMS_EPIKUR", "LAB_DM_FORSKER", "DS_LAB_DM_FORSKER", "LAB_LAB_DM_FORSKER")
  last <- last[last %in% vws]

  frst <- vws[!vws %in% last]
  vws <- c(frst, last[last %in% vws])

  aux <- as.character()

  for (vw in vws){
    print(paste0(vw, " in progress at ", Sys.time(), "..."))

    # open connection, read table, close connection
    con <- DBI::dbConnect(drv, conn_def)
    try(tmp <- DBI::dbGetQuery(con, paste0("Select top ", 5, " * from FSEID0000", projct, ".", vw)))
    try(DBI::dbDisconnect(con))

    if (is.null(tmp)){next} # skip if data not read

    # clear existing
    if (mode=="clear"){
      try(unlink(paste0(db_dir, "/", vw), recursive = T))
      try(file.remove(paste0(db_dir, "/", vw, ".RDS"), recursive = T))
    }

    # identify index columns to use for slicing
    index <- as.character()
    t.names <- tolower(colnames(tmp))
    if (sum(t.names %in% key.dict$pnrs)>0){
      index <- t.names[t.names %in% pnrs][1]
    } else if (sum(t.names %in% key.dict$rids)>0){
      index <- t.names[t.names %in% rids][1]
    }

    # read and write in slices to reduce load on sql/odbc
    if (length(na.omit(index))==1){

      # find source's indexes
      con <- DBI::dbConnect(drv, conn_def)
      try(indxs <- DBI::dbGetQuery(con, paste0("Select DISTINCT LEFT(", toupper(index), ", ", prefix, ") from FSEID0000", projct, ".", vw)))
      if (length(unique(na.omit(indxs[,1])))==1 | vw %in% prefix.add) {
        try(indxs <- DBI::dbGetQuery(con, paste0("Select DISTINCT LEFT(", toupper(index), ", ", prefix+1, ") from FSEID0000", projct, ".", vw)))
      }
      try(DBI::dbDisconnect(con))

      # convert index list to vector
      indxs <- indxs[,1][order(indxs[,1])]
      indxs <- stringr::str_pad(indxs, prefix, pad="0")

      # identify, which index subsets are already available
      if (dir.exists(paste0(db_dir, "/", vw))) {
        local_db <- gsub(".RDS", "", list.files(paste0(db_dir, "/", vw)))

        # only fill with indexes that are still missing in table
        if (exists("local_db")){if (length(local_db)>0) {indxs <- indxs[!indxs %in% substr(local_db, 1, prefix)]}}
      } else (dir.create(paste0(db_dir, "/", vw)))

      for (s in indxs){
        print(paste0("reading subset ", s, " (", which(indxs==s), "/", length(indxs), ") of ", vw, " from remote db at ", Sys.time()))

        # connect, read, disconnect
        con <- DBI::dbConnect(drv, conn_def)
        try(tmp <- DBI::dbGetQuery(con, paste0("Select * from FSEID0000", projct, ".", vw, " WHERE LEFT(", toupper(index), ", ", prefix, ") like '", s, "'")))
        try(DBI::dbDisconnect(con))

        # skip if data not read
        if (!exists("tmp")){print(paste0(s, " for ", vw, " not saved to local db! refill to complete data transfer.")); next}

        # show top of data and write to local db
        print(paste0("writing ", vw, " (", which(vws==vw), "/", length(vws), ") to local db..."))
        colnames(tmp) <- tolower(colnames(tmp))

        # write to db
        colnames(tmp)[colnames(tmp) %in% key.dict$pnrs] <- "pnr_enc"
        colnames(tmp)[colnames(tmp) %in% key.dict$rids] <- "rid_enc"
        saveRDS(tmp, paste0(db_dir, "/", vw, "/", s, ".RDS"))

        # placeholder, wipe and clean
        rm(list=c("tmp", "con"))
        gc()
      }
    } else {

      # store information on tables with no pnrs/rids indices
      aux <- c(aux, vw)

      # no index available, pull all data in one chunk
      print(paste0("index var not there, pulling whole table at ", Sys.time()))

      # connect, read, disconnect
      con <- DBI::dbConnect(drv, conn_def)
      try(tmp <- DBI::dbGetQuery(con, paste0("Select * from FSEID0000", projct, ".", vw)))
      try(DBI::dbDisconnect(con))

      # skip if data not read
      if (is.null(tmp)){print(paste0(vw, " not read")); next}

      # show top of data and write to local db
      print(paste0("writing ", vw, " to local db..."))
      colnames(tmp) <- tolower(colnames(tmp))

      # write to db
      colnames(tmp)[colnames(tmp) %in% key.dict$pnrs] <- "pnr_enc"
      colnames(tmp)[colnames(tmp) %in% key.dict$rids] <- "rid_enc"
      saveRDS(tmp, paste0(db_dir, "/", vw, ".RDS"))

      # placeholder, wipe and clean
      rm(list=c("tmp", "con"))
      gc()
    }
  }

  # saving names of db tables with no indexing (definitions, labels, etc)
  saveRDS(aux, paste0(db_dir, "/aux_sources.RDS"))

  print("00_build_rdb.R done!")
  sink()
}
