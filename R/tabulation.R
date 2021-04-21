

#' twoway table of categorical data
#' @export
twoway.chi <- function(data, x, group, bin=F, cens, force.two=F, show.na=F) {
  # get data
  d <- data[!is.na(data[[group]]),]

  if (force.two==T){
    d[[x]] <- as.numeric(d[[x]]>=1)
  }

  if (is.null(cens)){cens <- -Inf}

  # generate table, p.value and labels
  tab <- table(as.character(d[[x]]), d[[group]], useNA="ifany")

  groups <- colnames(tab)
  if (nrow(tab)<2){stop("too few levels")}
  p <- form.it(chisq.test(tab)$p.value, 3)
  levels <- rownames(tab)

  # handle missings explicitly
  if (show.na==T){
    if (is.na(levels[length(levels)]) & sum(is.na(levels))==1) {levels[length(levels)] <- "N/A"}
  }

  # combine n and %
  tab <- matrix(rbind(tab, form.it(prop.table(tab, 2)*100, 1)), nrow=nrow(tab))

  if (sum(as.numeric(tab[,1])<=cens)>=1 | sum(as.numeric(tab[,3])<=cens)>=1){
    p <- paste0("n<", cens)
  }

  # add p value
  if (nrow(tab)>=2) {filling <- c(rep(NA, nrow(tab)-1), p)} else {filling <- p}
  if (nrow(tab)>=2 & bin==F){varname <- c(x, rep(NA, nrow(tab)-1))} else {varname <- rep(x, nrow(tab))}

  for (i in 1:nrow(tab)){
    tab[i,2][as.numeric(tab[i,1])<=cens] <- paste0("n<", cens)
    tab[i,4][as.numeric(tab[i,3])<=cens] <- paste0("n<", cens)
    tab[i,1][as.numeric(tab[i,1])<=cens] <- paste0("n<", cens)
    tab[i,3][as.numeric(tab[i,3])<=cens] <- paste0("n<", cens)
  }

  # wrap up, force regular colnames
  tab  <- as.data.frame(cbind(varname, levels, tab, filling))
  colnames(tab) <- c("var", "level", rbind(paste0(groups, ".mean/n"), paste0(groups, ".sd/%")), "p.val")

  # pick last row if bin
  if (bin==T){tab <- tab[nrow(tab),]}
  return(tab)
}

#' twoway table of n indicator data
#' @export
twoway.n <- function(data, x, group){
  d <- data[!is.na(data[[group]]),]
  tab <- table(d[[x]], d[[group]], useNA="ifany")
  groups <- colnames(tab)
  if (nrow(tab)>1){stop("too many levels for n-count")}
  tab <- c(rbind(tab, form.it(prop.table(tab, 2)*100, 1)))
  tab <- c(x, NA, tab, NA)
  tab <- as.data.frame(t(tab))
  colnames(tab) <- c("var", "level", rbind(paste0(groups, ".mean/n"), paste0(groups, ".sd/%")), "p.val")
  return(tab)
}

#' twoway table of numeric data
#' @export
twoway.num <- function(data, x, group, digit.m=1, digit.sd=1, inf=FALSE, cal.date=F){
  d <- data[!is.na(data[[group]]),]
  groups <- unique(na.omit(d[[group]]))
  tab <- rep(NA, 2*length(groups)+2)
  tab[1] <- x
  tab[2] <- NA
  k <- 3
  for (i in 1:length(groups)){
    if (cal.date==F){tab[k+0] <- form.it(mean(d[[x]][d[[group]]==groups[i]], na.rm=T), digit.m)
    } else {
      tab[k+0] <- as.character(lubridate::as_date(round(mean(d[[x]][d[[group]]==groups[i]], na.rm=T))))
    }
    tab[k+1] <- form.it(  sd(d[[x]][d[[group]]==groups[i]], na.rm=T), digit.sd)
    k <- k+2
  }
  #tab[length(tab)+1] <- tryCatch({form.it(ordinal:::anova.clm(ordinal::clm(paste0("as.factor(", x, ")~as.factor(", group, ")",), data=d, link="logit"))$`Pr(>Chisq)`, 3)}, error=function(err) NA)
  if (length(groups)>2){
    tab[length(tab)+1] <- tryCatch({form.it(pnorm(abs(coef(summary(MASS::polr(paste0("as.factor(", x, ")~as.factor(", group, ")"), data=d)))[1,3]), lower.tail = F)*2, 3)}, error=function(err) NA)
  } else {
    tab[length(tab)+1] <- form.it(wilcox.test(as.formula(paste0("as.numeric(", x, ")~as.factor(", group, ")")), data=na.omit(d[,c(x, group)]))$p.value, 3)
  }
  tab <- as.data.frame(t(tab))
  colnames(tab) <- c("var", "level", rbind(paste0(groups, ".mean/n"), paste0(groups, ".sd/%")), "p.val")
  return(tab)
}

#' wraps up (align & stack)  a set of twoway tables
#' @export
tabulate <- function(data, xs, treat, num=NA, cat=NA, bin=NA, dichotomize=NA, cal.date=NA, cens=5, show.na=F){
  data <- as.rdf(data)
  t <- data.frame()
  for (x in xs){
    print(paste0("working on ", x))
    if (x %in% "n"){try(t <- dplyr::bind_rows(t, twoway.n  (data=data, x, treat)))}
    if (x %in% num){try(t <- dplyr::bind_rows(t, twoway.num(data=data, x, treat, digit.m = 2, digit.sd = 2)))}
    if (x %in% cal.date){try(t <- dplyr::bind_rows(t, twoway.num(data=data, x, treat, digit.m = 2, digit.sd = 2, cal.date==T)))}
    if (x %in% cat){try(t <- dplyr::bind_rows(t, twoway.chi(data=data, x, treat, cens=cens, show.na=show.na)))}
    if (x %in% dichotomize & !x %in% bin){try(t <- dplyr::bind_rows(t, twoway.chi(data=data, x, treat, cens=cens, force.two=T, show.na=show.na)))}
    if (x %in% bin &         !x %in% dichotomize){try(t <- dplyr::bind_rows(t, twoway.chi(data=data, x, treat, cens=cens, bin=T, show.na=show.na)))}
    if (x %in% bin &          x %in% dichotomize){try(t <- dplyr::bind_rows(t, twoway.chi(data=data, x, treat, cens=cens, force.two=T, bin=T, show.na=show.na)))}
  }
  t <- t %>% dplyr::mutate_if(is.ok, function(x) as.numeric(as.character(x)))
  t <- t %>% dplyr::mutate_if(is.factor, function(x) as.character(x))
  return(t)
}

#' wraps up (align & stack)  a set of twoway tables
#' @export
tab.it <- function(data, case, xs, ctns, bins=NULL, cats=NULL, low.cens=10, decimals.p=1, decimals.sd=1){
  d <- data
  stats <- data.frame(matrix(NA, length(xs), 6))
  for (i in xs){
    stats[which(xs==i),1] <- i
    if (i %in% bins){
      d[[i]][!is.na(d[[i]])] <- as.numeric(d[[i]][!is.na(d[[i]])]>=1)
      stats[which(xs==i),2] <-  sum(d[[i]][d[[case]]==0], na.rm=T)
      stats[which(xs==i),3] <- form.fix(mean(d[[i]][d[[case]]==0], na.rm=T), decimals.p, percent=T)
      stats[which(xs==i),4] <-  sum(d[[i]][d[[case]]==1], na.rm=T)
      stats[which(xs==i),5] <- form.fix(mean(d[[i]][d[[case]]==1], na.rm=T), decimals.p, percent=T)
      try(stats[which(xs==i),6] <- form.fix(chisq.test(d[[case]], d[[i]])$p.value, 3), silent=T)
      stats[which(xs==i),3][as.numeric(stats[which(xs==i),2])<=low.cens] <- paste0("n<", low.cens)
      stats[which(xs==i),5][as.numeric(stats[which(xs==i),4])<=low.cens] <- paste0("n<", low.cens)
      stats[which(xs==i),6][as.numeric(stats[which(xs==i),2])<=low.cens | as.numeric(stats[which(xs==i),4])<=low.cens] <- paste0("n<", low.cens)
      stats[which(xs==i),2][as.numeric(stats[which(xs==i),2])<=low.cens] <- paste0("n<", low.cens)
      stats[which(xs==i),4][as.numeric(stats[which(xs==i),4])<=low.cens] <- paste0("n<", low.cens)
    }
    if (i %in% ctns){
      stats[which(xs==i),2] <- form.fix(mean(d[[i]][d[[case]]==0], na.rm=T), 2)
      stats[which(xs==i),3] <- form.fix(  sd(d[[i]][d[[case]]==0], na.rm=T), decimals.sd)
      stats[which(xs==i),4] <- form.fix(mean(d[[i]][d[[case]]==1], na.rm=T), 2)
      stats[which(xs==i),5] <- form.fix(  sd(d[[i]][d[[case]]==1], na.rm=T), decimals.sd)
      try(stats[which(xs==i),6] <- form.fix(wilcox.test(formula(paste0(i, "~case")), d)$p.value, 3), silent=T)
    }
    if (i %in% cats){
      tab <- table(d[[i]], d[[case]])
      p <- form.it(chisq.test(tab)$p.value, 3)
      varname <- rep(x, nrow(tab))
      tab <- matrix(rbind(tab, form.it(prop.table(tab, 2)*100, 1)), nrow=nrow(tab))
      if (nrow(tab)>=2) {filling <- c(rep(NA, nrow(tab)-1), p)} else {filling <- p}

      # wrap up, force regular colnames
      tab  <- as.data.frame(cbind(varname, tab, filling))
      colnames(tab) <- paste0("X", 1:6) #c("var", "n.ctrl", "%.ctrl", "n.case", "%.case", "p.val")
      stats <- dplyr::bind_rows(stats[1:which(xs==i)-1,], tab, stats[(which(xs==i)+1):nrow(stats),])
    }
    if ((which(xs==i)/10)==round(which(xs==i)/10)){print(paste0("i ", which(xs==i), " of ", length(xs)))}
  }
  colnames(stats) <- c("var", "n.ctrl", "%.ctrl", "n.case", "%.case", "p.val")
  return(stats)
}
