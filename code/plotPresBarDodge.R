plotPresBarDodge<-function (x, start = NULL, end = NULL, bin = "hour/day", by = NULL, 
          title = TRUE, fill = "grey35", format = c("%m/%d/%Y %H:%M:%S", 
                                                    "%m-%d-%Y %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%d %H:%M:%S"), 
          plotTz = "UTC") 
{
  binChoice <- c("call", "minute", "hour", "day", "week", 
                 "month")
  binSplit <- strsplit(bin, "/")[[1]]
  binSplit <- gsub("s$", "", binSplit)
  switch(length(binSplit), `1` = {
    type <- "density"
    timeBin <- match.arg(binSplit, binChoice)
  }, `2` = {
    if (binSplit[1] == "call") {
      type <- "density"
      timeBin <- match.arg(binSplit[2], binChoice)
    } else {
      type <- "presence"
      timeBin <- match.arg(binSplit[2], binChoice)
      presBin <- match.arg(binSplit[1], binChoice)
    }
  })
  if (!"UTC" %in% colnames(x)) {
    stop("\"x\" must have column UTC")
  }
  if (is.character(x$UTC) || is.factor(x$UTC)) {
    x$UTC <- parseToUTC(as.character(x$UTC), format = format, 
                        tz = "UTC")
  }
  if (!plotTz %in% OlsonNames()) {
    stop("Specified timezone is invalid, check \"OlsonNames()\" for accepted names.")
  }
  x$UTC <- with_tz(x$UTC, tzone = plotTz)
  x$timeBin <- floor_date(x$UTC, unit = timeBin)
  if (is.null(start)) {
    start <- min(x$UTC)
    start <- start - period(1, units = timeBin)
  }
  if (is.null(end)) {
    end <- max(x$UTC)
    end <- end + period(1, units = timeBin)
  }
  start <- floor_date(start, unit = timeBin)
  end <- floor_date(end, unit = timeBin) + period(1, timeBin)
  if (type == "presence") {
    if (period(1, presBin)/period(1, timeBin) >= 1) {
      stop("Cannot create count of ", presBin, " per ", 
           timeBin)
    }
    x$presBin <- floor_date(x$UTC, unit = presBin)
    x <- select(x, any_of(c("presBin", "timeBin", by)))
    x <- distinct(x)
    ylab <- paste0(oneUp(presBin), "s")
  }
  if (type == "density") {
    ylab <- "Calls"
  }
  tlab <- paste0(ylab, "/", oneUp(timeBin))
  g <- ggplot() + labs(x = "Date", y = paste0("Count (", ylab, 
                                              ")")) + scale_y_continuous(expand = expansion(mult = c(0, 
                                                                                                     0.05))) + scale_x_datetime(limits = c(start, end), expand = c(0, 
                                                                                                                                                                   0))
  if (is.null(by)) {
    g <- g + geom_bar(data = x, aes_string(x = "timeBin"), 
                      fill = fill,position='dodge')
  }
  else {
    g <- g + geom_bar(data = x, aes_string(x = "timeBin", 
                                           fill = by,position='dodge'))
  }
  if (isTRUE(title)) {
    title <- paste0("Call ", oneUp(type), " (", tlab, ")")
    if (!is.null(by)) {
      title <- paste0(title, " by \"", by, "\"")
    }
  }
  g + ggtitle(title)
}
