GlobalSimStats<-function(path='.', pattern='[0-9]*\\.csv') {
  #   Computes maximum delivery ratio, average delivery cost, 
  #   variance of delivery cost, and time to deliver on multiple
  #   simulations run output
  #
  # Args:
  #   path: the base directory where data files are stored 
  #   pattern: regex that match a valid csv data file name
  #
  # Returns:
  #   A data frame which contains maximum delivery ratio, average 
  #   delivery cost, variance of delivery cost, and time to deliver for
  #   each data file (one row per file/run)
  #
  files<-list.files(path=path, pattern=pattern)
  slist<-data.frame()
  for (f in files ){
    s<-SimStats(paste(path,f,sep=""))
    slist<-rbind(slist,s)  # Pretty 
  }
  return(slist)
}
SimStats<-function(file){
  #   Computes maximum delivery ratio, average delivery cost, 
  #   variance of delivery cost, and time to deliver on a
  #   simulations run output
  #
  # Args:
  #   file: file where simulation data are stored
  #
  # Returns:
  #   A data frame which contains maximum delivery ratio, average 
  #   delivery cost, variance of delivery cost, and time to deliver for
  #   the specified data file (just one row)
  #
  d<-read.csv(file)
  tmp<-max(max(d$delivery_ratio))
  return(data.frame(max_delivery_ratio=tmp, 
           avg_delivery_cost=mean(d$delivery_cost), 
           var_delivery_cost=var(d$delivery_cost), 
           time_to_deliver=d[d$delivery_ratio==tmp,][1,]$time))
}
PlotStats<-function(){
  # Graph global simulation statistics
  #
  par(mfrow=c(2,2))
  boxplot( g1$max_delivery_ratio~g1$alg,
           ylab="Max Delivery Ratio",xlab="Algorithm",
           main="Max Delivery Ratio per Algorithm (Run#1)" )
  boxplot( g1$avg_delivery_cost~g1$alg,
           ylab="Average Delivery Cost",xlab="Algorithm",
           main="Average Delivery Cost per Algorithm (Run#1)" )
  boxplot( g1$var_delivery_cost~g1$alg,
           ylab="Variance on Delivery Cost",xlab="Algorithm",
           main="Variance on Delivery Cost per Algorithm (Run#1)" )
  boxplot( g1$time_to_deliver~g1$alg,
           ylab="Time to Deliver",xlab="Algorithm",
           main="Time to Deliver per Algorithm (Run#1)" )
  par(mfrow=c(2,2))
  boxplot( g2$max_delivery_ratio~g2$alg,
           ylab="Max Delivery Ratio",xlab="Algorithm",
           main="Max Delivery Ratio per Algorithm (Run#2)" )
  boxplot( g2$avg_delivery_cost~g2$alg,
           ylab="Average Delivery Cost",xlab="Algorithm",
           main="Average Delivery Cost per Algorithm (Run#2)" )
  boxplot( g2$var_delivery_cost~g2$alg,
           ylab="Variance on Delivery Cost",xlab="Algorithm",
           main="Variance on Delivery Cost per Algorithm (Run#2)" )
  boxplot( g2$time_to_deliver~g2$alg,
           ylab="Time to Deliver",xlab="Algorithm",
           main="Time to Deliver per Algorithm (Run#2)" )
}
PlotAggregated<-function(){
  #   Plot aggregated data for the two experimental sessions
  #
  PlotGraphs(a1b,a1l,a1r,a1f)
  PlotGraphs(a2b,a2l,a2r,a2f)
}
PlotGraphs<-function(ab,al,ar,af){
  #   Plot aggregated data
  #
  par(mfrow=c(2,2))
  PlotDelRatio(ab,al,ar,af)
  PlotDelCost(ab,al,ar,af)
  PlotDelRatioVsDelCost(ab,al,ar,af)
}
PlotDelRatioVsDelCost<-function(ab,al,ar,af){
  #   Plot delivery ratio vs delivery cost
  #
  plot(y=range(0,10),x=range(0.0,1.0), 
       type="n", xlab="Delivery ratio", ylab="Delivery Cost",
       main="Delivery ratio vs Delivery cost")
  #d=ab[1:length((which(ab$delivery_ratio<max(ab$delivery_ratio))==TRUE)),]
  d=ab
  lines(y=d$delivery_cost, x=d$delivery_ratio,col=1,pch=1, type="l")
  #d=al[1:length((which(al$delivery_ratio<max(al$delivery_ratio))==TRUE)),]
  d=al
  lines(y=d$delivery_cost, x=d$delivery_ratio,col=2,pch=2, type="l")
  #d=ar[1:length((which(ar$delivery_ratio<max(ar$delivery_ratio))==TRUE)),]
  d=ar
  lines(y=d$delivery_cost, x=d$delivery_ratio,col=3,pch=3, type="l")
  #d=af[1:length((which(af$delivery_ratio<max(af$delivery_ratio))==TRUE)),]
  #d=af
  lines(y=d$delivery_cost, x=d$delivery_ratio,col=4,pch=4, type="l")
  legend("topleft", c("BUBBLE","LABEL","RANK","FLOOD"),fill=c(1,2,3,4))
}
PlotDelRatio<-function(ab,al,ar,af){
  #   Plot delivery ratio
  #
  plot(x=af$time,y=af$delivery_ratio, 
       type="n", ylab="Delivery ratio", xlab="Time",
       main="Delivery ratio in time")
  lines(ab$time, y=ab$delivery_ratio,col=1,pch=1, type="l")
  lines(al$time, y=al$delivery_ratio,col=2,pch=2, type="l")
  lines(ar$time, y=ar$delivery_ratio,col=3,pch=3, type="l")
  lines(af$time, y=af$delivery_ratio,col=4,pch=4, type="l")
  legend("topleft", c("BUBBLE","LABEL","RANK","FLOOD"),fill=c(1,2,3,4))
}
PlotDelCost<-function(ab,al,ar,af){
  #   Plot delivery cost
  #
  plot(x=af$time,y=af$delivery_cost,
       type="n", ylab="Delivery cost", xlab="Time",
       main="Delivery cost int time")
  lines(ab$time, y=ab$delivery_cost,col=1,pch=1, type="l")
  lines(al$time, y=al$delivery_cost,col=2,pch=2, type="l")
  lines(ar$time, y=ar$delivery_cost,col=3,pch=3, type="l")
  lines(af$time, y=af$delivery_cost,col=4,pch=4, type="l")
  legend("topleft", c("BUBBLE","LABEL","RANK","FLOOD"),fill=c(1,2,3,4))
  
}
AggregateData<-function(path='.', pattern='[0-9]*\\.csv', timeslices=6,maxinterval=20000,mininterval=0){
  #   Computes average delivery cost and delivery ratio aggregated by time slice 
  #   for each simulation data files. Then calculate the average of theese values, 
  #   aggregated by time slice.
  #
  # Args:
  #   path: the base directory where data files are stored 
  #   pattern: regex that match a valid csv data file name
  #   timeslices: number of slices
  #   maxinterval: upper bound of the considered time interval
  #   mininterval: lower bound of the considered time interval
  #
  # Returns:
  #   A data frame which contains avg delivery ratio and average 
  #   delivery cost per time slice
  #
  files<-list.files(path=path, pattern=pattern)
  slist<-data.frame()
  for (f in files ){
    s<-AggregateSim(file=paste(path,f,sep=""),timeslices=timeslices,maxinterval=maxinterval,mininterval=mininterval)
    slist<-rbind(slist,s)  # Pretty inefficient
  }
  t<-unique(slist$time)
  dr<-tapply(slist$delivery_ratio,slist$time,mean)
  dc<-tapply(slist$delivery_cost,slist$time,mean)
  drv<-tapply(slist$delivery_ratio_var,slist$time,mean)
  dcv<-tapply(slist$delivery_cost_var,slist$time,mean)
  return(data.frame(time=t,delivery_ratio=dr,delivery_cost=dc,
                    delivery_ratio_var=drv,delivery_cost_var=dcv))
}
AggregateSim<-function(file, timeslices=6, maxinterval=20000,mininterval=0){
  #   Computes average delivery cost and delivery ratio aggregated by time slice
  #
  # Args:
  #   file: file where simulation data are stored
  #   time.cut: time intervals which are used for aggregation
  #   timeslices: number of slices
  #   maxinterval: upper bound of the considered time interval
  #   mininterval: lower bound of the considered time interval
  #
  # Returns:
  #   A data frame which contains avg delivery ratio and average 
  #   delivery cost per time slice
  #
  d<-read.csv(file)
  tround= round(maxinterval/timeslices)
  brseq<-seq(mininterval, maxinterval, by=tround)
  timecut=cut(d$time,breaks=brseq,labels=sprintf("%.0f",brseq[-1]))
  agr<-aggregate(d$delivery_ratio, list(timecut), mean)
  agrv<-aggregate(d$delivery_ratio, list(timecut), var)
  agc<-aggregate(d$delivery_cost, list(timecut), mean)
  agcv<-aggregate(d$delivery_cost, list(timecut), var)
  return(data.frame(time=agr$Group.1,delivery_ratio=agr$x,delivery_cost=agc$x,
                    delivery_ratio_var=agrv$x,delivery_cost_var=agcv$x))
}
# Load Simulation Data
if ( !exists('loaded') || !loaded){
  # Reading simulations data
  g1b<-GlobalSimStats(path='60_6_16/bubble/')
  g1l<-GlobalSimStats(path='60_6_16/label/')
  g1r<-GlobalSimStats(path='60_6_16/rank/')
  g1f<-GlobalSimStats(path='60_6_16/flood/')
  g2b<-GlobalSimStats(path='60_6_100/bubble/')
  g2l<-GlobalSimStats(path='60_6_100/label/')
  g2r<-GlobalSimStats(path='60_6_100/rank/')
  g2f<-GlobalSimStats(path='60_6_100/flood/')
  # Prepare aggregated data
  timesl<-40
  maxint1<-18000
  a1b<-AggregateData(path='60_6_16/bubble/',timeslice=timesl,maxinterval=maxint1)
  a1l<-AggregateData(path='60_6_16/label/',timeslice=timesl,maxinterval=maxint1)
  a1r<-AggregateData(path='60_6_16/rank/',timeslice=timesl,maxinterval=maxint1)
  a1f<-AggregateData(path='60_6_16/flood/',timeslice=timesl,maxinterval=maxint1)
  maxint2<-23000
  a2b<-AggregateData(path='60_6_100/bubble/',timeslice=timesl,maxinterval=maxint2)
  a2l<-AggregateData(path='60_6_100/label/',timeslice=timesl,maxinterval=maxint2)
  a2r<-AggregateData(path='60_6_100/rank/',timeslice=timesl,maxinterval=maxint2)
  a2f<-AggregateData(path='60_6_100/flood/',timeslice=timesl,maxinterval=maxint2)
  # Global Simulation Statistics Data
  g1<-data.frame()
  g1<-rbind(g1, data.frame(max_delivery_ratio=g1b$max_delivery_ratio,
                         avg_delivery_cost=g1b$avg_delivery_cost,
                         var_delivery_cost=g1b$var_delivery_cost,
                         time_to_deliver=g1b$time_to_deliver,
                         alg='bubble'))
  g1<-rbind(g1, data.frame(max_delivery_ratio=g1l$max_delivery_ratio,
                         avg_delivery_cost=g1l$avg_delivery_cost,
                         var_delivery_cost=g1l$var_delivery_cost,
                         time_to_deliver=g1l$time_to_deliver,
                         alg='label'))
  g1<-rbind(g1, data.frame(max_delivery_ratio=g1r$max_delivery_ratio,
                         avg_delivery_cost=g1r$avg_delivery_cost,
                         var_delivery_cost=g1r$var_delivery_cost,
                         time_to_deliver=g1r$time_to_deliver,
                         alg='rank'))
  g1<-rbind(g1, data.frame(max_delivery_ratio=g1f$max_delivery_ratio,
                         avg_delivery_cost=g1f$avg_delivery_cost,
                         var_delivery_cost=g1f$var_delivery_cost,
                         time_to_deliver=g1f$time_to_deliver,
                         alg='flood'))
  g2<-data.frame()
  g2<-rbind(g2, data.frame(max_delivery_ratio=g2b$max_delivery_ratio,
                         avg_delivery_cost=g2b$avg_delivery_cost,
                         var_delivery_cost=g2b$var_delivery_cost,
                         time_to_deliver=g2b$time_to_deliver,
                         alg='bubble'))
  g2<-rbind(g2, data.frame(max_delivery_ratio=g2l$max_delivery_ratio,
                         avg_delivery_cost=g2l$avg_delivery_cost,
                         var_delivery_cost=g2l$var_delivery_cost,
                         time_to_deliver=g2l$time_to_deliver,
                         alg='label'))
  g2<-rbind(g2, data.frame(max_delivery_ratio=g2r$max_delivery_ratio,
                         avg_delivery_cost=g2r$avg_delivery_cost,
                         var_delivery_cost=g2r$var_delivery_cost,
                         time_to_deliver=g2r$time_to_deliver,
                         alg='rank'))
  g2<-rbind(g2, data.frame(max_delivery_ratio=g2f$max_delivery_ratio,
                         avg_delivery_cost=g2f$avg_delivery_cost,
                         var_delivery_cost=g2f$var_delivery_cost,
                         time_to_deliver=g2f$time_to_deliver,
                         alg='flood'))
  loaded<-TRUE
}

