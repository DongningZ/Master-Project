df1 = read.csv("data/1_electric.csv", header = T)
df = df1
aggregation = "day"
# need to choose time variable and response (generlise)



data.time = as.POSIXct(paste(df$DATE, df$START.TIME, sep = " "), format = "%d/%m/%Y %H:%M", tz = "UTC")

#data.time.lt = as.POSIXlt(data.time)

data = data.frame(time = data.time, y = df[,"USAGE"])

if(!is.null(unlist(input))){
  
  if(!is.null(input$year) & !is.null(input$month & is.null(input$day))){
  
  if(!is.null(input$year) & !is.null(input$month)){
ind = sprintf(rep(input$year,each = length(input$month)),input$month, fmt = "%02.0f-%02.0f")
}

  if(!is.null(input$year) & is.null(input$month)){
    ind = input$year
  }}
  
  input$month = rep(input$month, each = length(input$day))
  ind = sprintf(rep(input$year,each = length(input$month)),input$month, input$day,fmt = "%02.0f-%02.0f-%02.0f")
}



if(is.null(aggregation)){
  
  data.f = data
  ## generate a new dataframe cover the starting time to the end time by the frequency of the data
  ## merge this new dataframe with the data
  ## this would include the missing date happens in the data for ploting porpose
  
  ## here the 1800 is for half an hour, need to generalize
  time.seq = data.frame(time = seq(data.f[1,1], data.f[(dim(data)[1]),1],by = 1800), y = NA)
  aggregation = 1900
  
  
} else if ( aggregation == "hour" ){
  
  
  data.h = aggregate(data$y, list(time=cut(data$time, "hour")),sum)
  
  data.f = data.frame(time = as.POSIXct(as.character(data.h$time), tz = "UTC"), y = data.h$x )
  
  time.seq = data.frame(time = seq(data.f[1,1], data.f[(dim(data.f)[1]),1],by = "hour"), y = NA)
  
  
}  else if( aggregation == "day" ){
  
  
  data.h = aggregate(data$y, list(time=cut(data$time, "day")),sum)
  
  data.f = data.frame(time = as.POSIXct(as.character(data.h$time), tz = "UTC"), y = data.h$x )
  
  time.seq = data.frame(time = seq(data.f[1,1], data.f[(dim(data.f)[1]),1],by = "day"), y = NA)
  
  
}  else if( aggregation == "week" ){
  
  
  data.h = aggregate(data$y, list(time=cut(data$time, "week")),sum)
  
  data.f = data.frame(time = as.POSIXct(as.character(data.h$time), tz = "UTC"), y = data.h$x )
  
  time.seq = data.frame(time = seq(data.f[1,1], data.f[(dim(data.f)[1]),1],by = "week"), y = NA)
  
  
}  else if( aggregation == "month" ){
  
  
  data.h = aggregate(data$y, list(time=cut(data$time, "month")),sum)
  
  data.f = data.frame(time = as.POSIXct(as.character(data.h$time), tz = "UTC"), y = data.h$x )
  
  time.seq = data.frame(time = seq(data.f[1,1], data.f[(dim(data.f)[1]),1],by = "month"), y = NA)
  
  
}  else if( aggregation == "quarter" ){
  
  
  data.h = aggregate(data$y, list(time=cut(data$time, "quarter")),sum)
  
  data.f = data.frame(time = as.POSIXct(as.character(data.h$time), tz = "UTC"), y = data.h$x )
  
  time.seq = data.frame(time = seq(data.f[1,1], data.f[(dim(data.f)[1]),1],by = "quarter"), y = NA)
  
  
}  else if( aggregation == "year" ){
  
  
  data.h = aggregate(data$y, list(time=cut(data$time, "year")),sum)
  
  data.f = data.frame(time = as.POSIXct(as.character(data.h$time), tz = "UTC"), y = data.h$x )
  
  time.seq = data.frame(time = seq(data.f[1,1], data.f[(dim(data.f)[1]),1],by = "year"), y = NA)
  
} 

data.full.seq = merge(time.seq, data.f, by = "time", all.x = TRUE)

data.final = list(data = data.frame(time = data.full.seq$time,   
                                    y= data.full.seq$y.y, y.log = log(data.full.seq$y.y)), frequency = aggregation)
if(scale == "month"){

data.final$data$month.year = format(data.final$data$time,"%Y-%m")
data.final$data$month = as.POSIXlt(data.final$data$time)$mon+1

data.final$data$wday =  as.POSIXlt(data.final$data$time)$wday

data.final$data$year = as.POSIXlt(data.final$data$time)$year+1900

data.bymonth = split(data.final$data,data.final$data$month.year)




#### make sure the xlimit of each plot is the same
firstmonth = unique(data.final$data$month.year)[1]

lastmonth = tail(unique(data.final$data$month.year), n = 1)

expectday.f = monthdays(data.final$data$year[1], data.final$data$month[1])

if ((length(as.POSIXlt(data.bymonth[[1]][,1])$mday+1)) != expectday.f){
  
  full.seq.firstm = data.frame(time = seq(
    as.POSIXct(paste(firstmonth, 1 , sep = "-"), format = "%Y-%m-%d", tz = "UTC"), 
    by = "1 day", 
    length.out = expectday.f),
    y = NA, y.log = NA, month.year = NA, month = NA, year = NA)
  
  merge.first.m = merge(full.seq.firstm,data.bymonth[[1]] ,by = "time", all.x = TRUE)
  
  
  data.bymonth[[1]] = data.frame(time = merge.first.m$time, y = merge.first.m$y.y, 
                                y.log = merge.first.m$y.log.y, month.year = firstmonth,
                                month = data.final$data$month[1], year = data.final$data$year[1],
                                wday = as.POSIXlt(merge.first.m$time)$wday)
}

if(firstmonth!=lastmonth ){
  
  lastv = length(data.bymonth)
  
  expectday.l = monthdays(data.bymonth[[lastv]]$year[1], data.bymonth[[lastv]]$month[1])

  
  if (!(length(as.POSIXlt(data.bymonth[[lastv]][,1])$mday+1)) %in% c(28,29,30,31)){
    
    full.seq.lastm = data.frame(time = seq(
      as.POSIXct(paste(lastmonth, 1 , sep = "-"), format = "%Y-%m-%d", tz = "UTC"), 
      by = "1 day", 
      length.out = expectday.l),
      y = NA, y.log = NA, month.year = NA, month = NA, year = NA)
    
    merge.last.m = merge(full.seq.lastm,data.bymonth[[lastv]] ,by = "time", all.x = TRUE)
    
    
    data.bymonth[[lastv]] = data.frame(time = merge.last.m$time, y = merge.last.m$y.y, 
                                   y.log = merge.last.m$y.log.y, month.year = lastmonth,
                                   month = data.bymonth[[lastv]]$month[1], year = data.bymonth[[lastv]]$year[1],
                                   wday = as.POSIXlt(merge.last.m$time)$wday)
  }}

ylimt = range(data.final$data$y, na.rm = TRUE)



if(is.null(unlist(input))){
if(separate = TRUE){
  for (i in (1:length(data.bymonth))){
  
    
    sundays = data.bymonth[[i]][with(data.bymonth[[i]], wday == 0 ),1]
    
    ## subset the saturdays data 
    saturdays = data.bymonth[[i]][with(data.bymonth[[i]], wday == 6 ),1]
    
   
    
plot(data.bymonth[[i]]$time, data.bymonth[[i]]$y, type = "n",
     xaxt = "n", ylim = ylimt,
     ylab = ylab, xlab = data.bymonth[[i]]$month.year[1] , 
     main = main)
    
    axis.POSIXct(1, at = seq(data.bymonth[[i]]$time[1], tail(data.bymonth[[i]]$time, n =1), by = "week"),
                 format = ticklabfmt, cex.axis = 0.7, 
                 las = las,tck = tck1)
    axis.POSIXct(1, at =seq(data.bymonth[[i]]$time[1], tail(data.bymonth[[i]]$time, n =1), by = "day"),
                 labels = FALSE,format = ticklabfmt, 
                 cex.axis = 0.7, las = las, tck = tck2)
    # 
     # rect(saturdays, ylimt[1], sundays, ylimt[2],
     #      border = NA, col = "pink")
    
    abline(v=saturdays, col = "red", lty = 2)
    # 
    abline(v=sundays, col = "red", lty = 2)
    # 
    lines(data.bymonth[[i]]$time, data.bymonth[[i]]$y)
  }
}
  
  if(separate = FALSE){ 
    
    index = which.max(sapply(data.bymonth, dim)[1,])
    
    plot(1: length(data.bymonth[[index]]$y), data.bymonth[[index]]$y, type = "n",
         xaxt = "n", ylim = ylimt,
         ylab = ylab, xlab = "" , 
         main = main)
    
    axis(1, at = seq(1, length(data.bymonth[[index]]$y), by = 7),
         labels = seq(1, length(data.bymonth[[index]]$y), by = 7), cex.axis = 0.7, 
                 las = las,tck = tck1)
    axis(1, at = seq(1, length(data.bymonth[[index]]$y) , by = 1),
                 labels = FALSE, 
                 cex.axis = 0.7, las = las, tck = tck2)
    # 
    # rect(saturdays, ylimt[1], sundays, ylimt[2],
    #      border = NA, col = "pink")
    for (i in (1:length(data.bymonth))){
      
    #   
    #   sundays = (1:length(data.bymonth[[i]]$y))[with(data.bymonth[[i]], wday == 0 )]
    #   
    #   ## subset the saturdays data 
    #   saturdays = (1:length(data.bymonth[[i]]$y))[with(data.bymonth[[i]], wday == 0 )]
    #   
    # abline(v=saturdays, col = "red", lty = 2)
    # # 
    # abline(v=sundays, col = "red", lty = 2)
    # # 
    lines(1:length(data.bymonth[[i]]$y), data.bymonth[[i]]$y)}
    
}
}
  if(length(input$month) == 1){
    if(consecutive.month == TRUE){
     
      
   index =  which(names(data.bymonth) %in% ind)
   
   wday.m = data.bymonth[[index]]$wday[1]
   
   index.all = seq(index -n, index+n, by = 1)
   
   for (i in 1:length(index.all)){
     
     first.macth[i] = which(data.bymonth[[index.all[i]]]$wday == wday.m)[1] 
     
   
  
   }
   
   x.extend = max(first.macth)
   
   plot(c(1,(length(data.bymonth[[index]]$y) + x.extend -1)), ylimt, type = "n",
        xaxt = "n", ylim = ylimt,
        ylab = ylab, xlab = ind , 
        main = main)
   
   axis(1, at = seq(x.extend, length(data.bymonth[[index]]$y)+x.extend, by = 7),
        labels = seq(1, length(data.bymonth[[index]]$y), by = 7), cex.axis = 0.7, 
        las = las,tck = tck1)
   axis(1, at = seq(x.extend, length(data.bymonth[[index]]$y+x.extend) , by = 1),
        labels = FALSE, 
        cex.axis = 0.7, las = las, tck = tck2)
   
   # 
   # rect(saturdays, ylimt[1], sundays, ylimt[2],
   #      border = NA, col = "pink")
   
    
    
   for (i in seq(index -n, index+n, by = 1)){
     
     first.macth.e = which(data.bymonth[[i]]$wday == wday.m)[1] 
     
    start.x = x.extend-first.macth.e+1
     lines(start.x:(length(data.bymonth[[i]]$y) + start.x -1), data.bymonth[[i]]$y
         #  col = c("darkblue","red", "orange")[i-index +n + 1])
     }
   
   sundays = (1:length(data.bymonth[[index]]$y))[with(data.bymonth[[index]], wday == 0 )]
   
   ## subset the saturdays data
   saturdays = (1:length(data.bymonth[[index]]$y))[with(data.bymonth[[index]], wday == 6 )]
   
   abline(v=saturdays, col = "red", lty = 2)
   #
   abline(v=sundays, col = "red", lty = 2)
   
   lines(x.extend:(length(data.bymonth[[index]]$y)+x.extend-1), data.bymonth[[index]]$y, col = "red")
   
    # legend("topright", col = c("darkblue","red", "orange"), 
    #       legend = names(data.bymonth)[seq(index -n, index+n, by = 1)],cex = 0.6, lty = c(1,1,1))
    # 
   
    }
    
    
    if(consecutive.year == TRUE){
      
    
      years = c(input$year - n, input$year+n)
      
      index =  which(names(data.bymonth) == ind)
      
      requried.momth = sprintf(years, input$month, fmt = "%02.0f-%02.0f")
      
      indexall = which(names(data.bymonth) %in% requried.momth )
      
      for (i in 1:length(index.all)){
        
        first.macth[i] = which(data.bymonth[[index.all[i]]]$wday == wday.m)[1] 
        
        
        
      }
      
      x.extend = max(first.macth)
      
      plot(c(1,(length(data.bymonth[[index]]$y) + x.extend -1)), ylimt, type = "n",
           xaxt = "n", ylim = ylimt,
           ylab = ylab, xlab = ind , 
           main = main)
      
      axis(1, at = seq(x.extend, length(data.bymonth[[index]]$y)+x.extend, by = 7),
           labels = seq(1, length(data.bymonth[[index]]$y), by = 7), cex.axis = 0.7, 
           las = las,tck = tck1)
      axis(1, at = seq(x.extend, length(data.bymonth[[index]]$y+x.extend) , by = 1),
           labels = FALSE, 
           cex.axis = 0.7, las = las, tck = tck2)
      
      # 
      # rect(saturdays, ylimt[1], sundays, ylimt[2],
      #      border = NA, col = "pink")
      wday.m = data.bymonth[[index]]$wday[1]
      
      
      for (i in 1:length(indexall)){
        
      
      first.macth.e = which(data.bymonth[[indexall[i]]]$wday == wday.m)[1] 
      
      start.x = x.extend-first.macth.e+1
      
      lines(start.x:(length(data.bymonth[[indexall[i]]]$y) + start.x -1), data.bymonth[[indexall[i]]]$y
            )
    }
    
    sundays = (1:length(data.bymonth[[index]]$y))[with(data.bymonth[[index]], wday == 0 )]
    
    ## subset the saturdays data
    saturdays = (1:length(data.bymonth[[index]]$y))[with(data.bymonth[[index]], wday == 6 )]
    
    abline(v=saturdays, col = "red", lty = 2)
    #
    abline(v=sundays, col = "red", lty = 2)
    
    lines(x.extend:(length(data.bymonth[[index]]$y)+x.extend-1), data.bymonth[[index]]$y, col = "red")
    
    # legend("topright", col = c("darkblue","orange", "red"), 
    #        legend = names(data.bymonth)[c(indexall, index)],cex = 0.6, lty = 1)
    #  
      
    }

}
}

?POSIXct
xlab = ""
ylab = ""
main = "2014-02 with 2014-01 and 2014-03"
i = 2
las = 1
tck1 = -0.01 
tck2 = -0.005

ticklabfmt = "%b %d"
n =1
input = list(year=2013, month=NULL, day =  NULL, start1=NULL, end = NULL)

year = c(2012,2013)
month = c(1,2,3)

display (df1, xlab = "", ylab = "", main = "2014-02 with 2013-02 and 2015-02", consecutive.year == TRUE,
                   aggregation = "day", scale = "month" )
##########
monthdays =
  local({
    ## set the number of days of month for leap years and non-leap years
    ## in two vectors
    stddays  = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    leapdays = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    function(year, month) {
      ## if the year is a multiple of 400, then it is a leap year
      ## subset the number of month for that year
      if (year %% 400 == 0) 
        leapdays[month]
      ## if the year is a multiple of 4 and not a multiple of 100
      ## then it is a leap year, subset the number of month of that year
      else if (year %% 4==0 & year %% 100 != 0)
        leapdays[month]
      ## else it will be a non-leap year
      else
        stddays[month]
    }
  })












#######may out of bound, the concetive month options