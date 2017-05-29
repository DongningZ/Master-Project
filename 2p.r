### pad.start - A function to convert a start time input to posixct form and 
###             round down the input date to the 1st month, 1st day, 1st hour
###             etc.
###
### Argument:
### datatime -  A numeric vector representing date 

pad.start =
  local({
    ## set the format of the time
    fmt = "%02.0f/%02.0f/%.0f %02.0f:%02.0f:%02.0f"
    function(datetime) {
      
      ##calculate the length of the input vector
      nfields = length(datetime)
      ##converting the input to posixct form by its length
      as.POSIXct(if (nfields == 1)
        ## if the length is 1, then round it to the 1st day of the year
        sprintf(fmt, 1, 1, datetime, 0, 0, 0)
        else if (nfields == 2)
          ## if the length is 2, then round it to the 1st day of the month
          sprintf(fmt, 1, datetime[2], datetime[1], 0, 0, 0)
        ## if the length is 3, round it to the 1st hour of the day
        else if (nfields == 3)
          sprintf(fmt,datetime[3], datetime[2], datetime[1], 0, 0, 0)
        ## if the length is 4, round it to the 1st minute of the hour
        else if (nfields == 4)
          sprintf(fmt,datetime[3], datetime[2], datetime[1], datetime[4],
                  0, 0)
        else if (nfields == 5)
          ## if the length is 5, round it to the 1st second of the minute
          sprintf(fmt,datetime[3], datetime[2], datetime[1], datetime[4], 
                  datetime[5], 0)
        ## if the length is not smaller than 6, then stop 
        else if (nfields != 6)
          stop("invalid number of start fields")
        , format = "%d/%m/%Y %H:%M", tz="UTC")
      
    }
  })

### pad.start - A function to convert a start time input to posixct form and 
###             round up the input date to the next year,month,day etc.
###
### Argument:
### datatime -  A numeric vector representing date 
pad.end =
  local({
    fmt = "%02.0f/%02.0f/%.0f %02.0f:%02.0f:%02.0f"
    function(datetime) {
      
      nfields = length(datetime)
      ## Checking if the input including december, if yes round up to next year
      if (nfields == 2 && datetime[2] == 12) {datetime = c(datetime[1] + 1, 0)}
      
      ## Checking if the input including the last day of that month
      ## using monthday function , if yes round up to next month
      
      if (nfields == 3 && datetime[3] == monthdays(datetime[1], datetime[2]) 
          && datetime[2] != 12) {datetime = c(datetime[1], datetime[2] + 1 , 0)}
      
      ## Chcking if the input including the last day of yeat, if yes round
      ## up to the next year 1st day
      if (nfields == 3 && datetime[3] == monthdays(datetime[1], datetime[2]) 
          && datetime[2] == 12) {datetime = c(datetime[1] + 1, 1, 0)}
      
      
      as.POSIXct(if (nfields == 1)
        ## if the length is 1, round it to 1st day in next year
        sprintf(fmt, 1, 1, datetime + 1, 0, 0, 0)
        else if (nfields == 2)
          ## if the length is 2, round it to the 1st day in next month
          sprintf(fmt, 1,datetime[2] + 1, datetime[1], 0, 0, 0)
        else if (nfields == 3)
          ## if the length is 3, round it to the next day
          sprintf(fmt,datetime[3] + 1,datetime[2], datetime[1], 0, 0, 0)
        else if (nfields == 4)
          ## if the length is 4, round it to the next hour
          sprintf(fmt,datetime[3],datetime[2], datetime[1], datetime[4] + 1,
                  0, 0)
        else if (nfields == 5)
          ## if the length is 5, round it to the next minus
          sprintf(fmt,datetime[3],datetime[2], datetime[1], datetime[4], 
                  datetime[5]
                  + 1, 0)
        ## if the length is bigger than 6, stop
        else if (nfields != 6)
          stop("invalid number of start fields")
        , format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
      
    }
  })





### window.POSIXct - a function generate a indicator variable that help
###                  subset the interval of data from given date
### Arguments:
### posixct  - the time variable of the series
### start, end   - the start date and end is for end date
window.POSIXct =
  function(posixct, start = NULL, end = NULL) {
    ## use the start and end function to convert input variable
    start = pad.start(start)
    end = pad.end(end)
    ## create a indicater variable to subset the interval between the 
    ## start date and the end date
    (posixct >= start) & (posixct < end) 
  }





## Type = inone, yearly, monthly
## aggregation = hour, day, month, quarter, year.
## scale = total, year, month, quarter, month, week, day
## separate = FALSE, the one plot contains single value,
##            TRUE, the plot contains multiple value, such as multiple month in a year.

display = function(df, xlab = "", ylab = "", main = "",
                   aggregation = NULL, scale = "total" ){
  
  # need to choose time variable and response (generlise)
  
  
  
  data.time = as.POSIXct(paste(df$DATE, df$START.TIME, sep = " "), format = "%d/%m/%Y %H:%M", tz = "UTC")
  
  #data.time.lt = as.POSIXlt(data.time)
  
  data = data.frame(time = data.time, y = df[,"USAGE"])
  
  
  
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
  
  if( scale == "total"){
    
    
  
    plot(y~time  , data= data.final$data, type = "l",xlab = xlab, ylab = ylab, main = main,
         sub = paste("All in one", "by",aggregation, sep = " "))
    
  }
    
  if( scale == "year"){
    
    data.final$data$year =  as.POSIXlt(data.final$data$time)$year+ 1900
    
    data.byyear = split(data.final$data,data.final$data$year)
    
    firstyear = unique(data.final$data$year)[1]
    
    lastyear = tail(unique(data.final$data$year), n = 1)
    
    if (length(unique(as.POSIXlt(data.byyear[[1]][,1])$mon+1)) != 12){
      
      full.seq.firsty = data.frame(time = seq(
        as.POSIXct(paste(firstyear, 1 , 1 , sep = "-"), format = "%Y-%m-%d", tz = "UTC"), 
        as.POSIXct(paste(firstyear, 12 , 31 , sep = "-"), format = "%Y-%m-%d", tz = "UTC"),by = aggregation),
        y = NA, y.log = NA, year = NA)
      
      merge.first = merge(full.seq.firsty,data.byyear[[1]] ,by = "time", all.x = TRUE)
      
      
      data.byyear[[1]] = data.frame(time = merge.first$time, y = merge.first$y.y, 
                                    y.log = merge.first$y.log.y,year = firstyear)
    }
    
    if(firstyear!=lastyear ){
      
      lastv = length(data.byyear)
      if (length(unique(as.POSIXlt(data.byyear[[lastv]][,1])$mon+1)) != 12){
        
        full.seq.lasty = data.frame(time = seq(
          as.POSIXct(paste(lastyear, 1 , 1 , sep = "-"), format = "%Y-%m-%d", tz = "UTC"), 
          as.POSIXct(paste(lastyear, 12 , 31 , sep = "-"), format = "%Y-%m-%d", tz = "UTC"),by = aggregation),
          y = NA, y.log = NA, year = NA)
        
        merge.last = merge(full.seq.lasty,data.byyear[[lastv]] ,by = "time", all.x = TRUE)
        
        
        data.byyear[[lastv]] = data.frame(time = merge.last$time, y = merge.last$y.y, 
                                          y.log = merge.last$y.log.y,year = lastyear)
      }}
    
    for (i in 1:length(data.byyear)){
      plot(y ~ time, data = data.byyear[[i]], xlab = xlab, ylab = ylab, xaxt = "n", 
           main = main, sub = paste(data.byyear[[i]]$year[1], "by" ,aggregation, sep = " "), type = "n")
      
      lines(y ~ time, data = data.byyear[[i]])
      
      axis.POSIXct(1, seq(data.byyear[[i]][1,1], data.byyear[[i]][dim(data.byyear[[i]])[1],1], by = "month"),
                   format = "%b", cex.axis = 0.7, 
                   las = 1,tck = -0.01)
      
      # axis.POSIXct(1,  seq(data.byyear[[i]][1,1], data.byyear[[i]][dim(data.byyear[[1]])[1],1], by = "day"),
      #              labels = FALSE,format = "%d", 
      #              cex.axis = 0.7, las = 1, tck = -0.005)
      
    }
    
  }
  
  if(scale == "month"){
    
  }
  
  
}  
pdf("Dataset1-year.pdf")

display(df1,ylab = "Usage", aggregation = "hour", scale = "year",main = "Data set 1")
display(df1,ylab = "Usage", aggregation = "day", scale = "year",main = "Data set 1")

display(df1,ylab = "Usage", aggregation = "month", scale = "year",main = "Data set 1")

display(df1,ylab = "Usage", aggregation = "quarter", scale = "year",main = "Data set 1")


dev.off()

display(df2,ylab = "Usage",aggregation = "month",scale = "year", main = "Data set 2")
display(df3,ylab = "Usage",aggregation = "month", scale = "year",main = "Data set 3")
display(df4,ylab = "Usage", aggregation = "month",scale = "year",main = "Data set 4 - The building")
display(df5,ylab = "Usage", aggregation = "month",scale = "year",main = "Data set 5")
display(df6,ylab = "Usage", aggregation = "month",scale = "year",main = "Data set 6")


df = df1
separate = FALSE
separate = TRUE

