plotDataSimple2<-function(x,effortStartDate=as.POSIXct('2023-03-01',tz='UTC'), 
                          effortEndDate = as.POSIXct('2023-03-31',tz='UTC'), by_var = NULL){
  
  # Function to plot simplified data and effort using ggplot and facet wrap this 
  # is intended for use with single deployments where a full year of effort
  # occludes the output. This is intended to be used with data (x = binned
  # detections) that have already been cleaned. For instance, as single Morro
  # Bay deployment. 'by_var' is the facet_wrap variable and effortBufferDays
  # is the number of days to pad the plot with for your own taste
  barPosition='stack'
  x <- markNumEffort2(x, keepCols=c('species', 'call'))
  effort <- bind_rows(lapply(split(x$effort, x$effort$year), function(y) {
    if(nrow(y) <= 1) {
      # print(z)
      return(NULL)
    }
    shortenOffs(y, nMax=364)
  })
  )
  data <- x$data
  
  #KJP edit
  # effortdataout = subset(effort, 
  #                        binDate> (min(x$data$UTC)-period(effortBufferDays, 'day')) &
  #                          binDate<(max(x$data$UTC)+period(effortBufferDays, 'day')))
  
  effortdataout = subset(effort, 
                         binDate>= effortStartDate &
                           binDate<=effortEndDate)
  
  if(x$data$DriftName[1]=="MB05"){effortdataout$nEffort<-24}
  # joint the data and the labels for plotting
  data <- left_join(data, effortdataout[c('plotX', 'binDate', 'nEffort')],
                    by=join_by(binDate))
  
  p<-ggplot() +
    geom_bar(data =data, aes(x=plotX), position=barPosition, width=1,fill='deeppink1')
   
  
  
  # Create the label list and tick locations, format to date
  labs <- list(ix = seq(from=min(effortdataout$plotX), 
                        to=max(effortdataout$plotX), length.out=5),
               label = seq(from=min(effortdataout$binDate), 
                           to=max(effortdataout$binDate), length.out=5))
  labs$label <- format(labs$label, '%b-%d')
  
  
  # Add the updated scale and scale limits
  p<-p+
    geom_path(data=effortdataout, aes(x=plotX, y=nEffort),size=1) +
    theme_bw()+
    scale_x_continuous(limits=c(min(effortdataout$plotX),
                                max(effortdataout$plotX)), 
                       breaks=labs$ix, labels=labs$label, name=NULL)
  
  # If you want a facet wrap, then use the 'by_var' col
  if(!is.null(by_var)){(p =p+ facet_wrap(~ .data[[by_var]]))}
  # return(list(p))
  return(p)
}


markNumEffort2 <- function(x, 
                          by='DriftName',
                          bin='hour/day', 
                          keepCols=c('species', 'call')) {
  bin <- strsplit(bin, '/')[[1]]
  bin <- gsub('s$', '', bin)
  if(length(bin) == 1) {
    bin <- c(bin, bin)
  }
  x$UTC <- floor_date(x$UTC, unit=bin[1])
  x$binDate <- floor_date(x$UTC, unit=bin[2])
  # dateSeq <- seq(from=min(x$binDate), to=max(x$binDate), by=bin)
  dateSeq <- seq(from=floor_date(min(x$binDate), unit='year'),
                 to = ceiling_date(max(x$binDate), unit='year')-period(1, 'day'),
                 by=bin[2])
  if(!'pctEff' %in% colnames(x)) {
    x$pctEff <- 1
  }
  effort <- x %>%
    dplyr::select(all_of(c('UTC', 'binDate', 'pctEff', by))) %>%
    distinct() %>%
    group_by(binDate) %>%
    summarise(nEffort=sum(pctEff)) %>%
    ungroup()
  missDates <- !dateSeq %in% effort$binDate
  effort <- bind_rows(effort, data.frame(binDate=dateSeq[missDates], nEffort=0))
  effort <- arrange(effort, binDate)
  effort$group <- FALSE
  effort$off <- effort$nEffort == 0
  effort$offGroup <- FALSE
  effort$offGroup[1] <- effort$off[1]
  effort$group[1] <- TRUE
  for(i in 2:nrow(effort)) {
    effort$group[i] <- effort$nEffort[i-1] == 0 & effort$nEffort[i] != 0
    effort$offGroup[i] <- isFALSE(effort$off[i-1]) & isTRUE(effort$off[i])
  }
  effort$offGroup <- cumsum(effort$offGroup)
  effort$nGroup <- cumsum(effort$group)
  effort$season <- markSeason(effort$binDate)
  
  # effort$year <- year(effort$binDate)
  x <- distinct(dplyr::select(x, any_of(c('UTC', 'binDate','year',  by, keepCols))))
  x$season <- markSeason(x$binDate)
  if(!'year' %in% colnames(x)) {
    years <- unique(year(effort$binDate))
    x$year <- factor(year(x$binDate), levels=min(years):max(years))
  }
  effort$year <- factor(year(effort$binDate), levels=levels(x$year))
  # x$year <- year(x$binDate)
  list(dates=dateSeq, data=x, effort=effort)
}
