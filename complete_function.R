###############1. input period functions ##############

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




################### 2. aggregation functions #################
#### freq.r is a function that translate the aggregation level input into 
#### character strings that could be recongized by the POSIXct class functions
#### most of the aggregation levels could be eaily done by using as.character
#### however, there are some irregular aggregation level, such as half an hour need to
#### be corrected. New aggregation levels could be added in this function easily.
#### for now, the function supports min, half an hour, day, hour, week, month, quarter, year.
#### New aggregation levels could be added in this function easily.
#### Arguments 
#### x               The aggregation level
freq.r = function(x){
  switch(x,
         min = "min",
         halfanhour = "30 mins",
         hour = "hour",
         day = "day",
         week = "week",
         month = "month",
         quarter = "quarter",
         year = "year") }


#### aggre.na is a function that aggregate the data set by the aggregation level input.
#### default aggregation function is sum which could be changed.
#### This function detects missing time creates the missing entry with NA as response
#### Arguments:
#### data           The data set
#### aggregation    The aggregation level
#### fun            The aggregation function
aggre.na = function(data, aggregation, fun = sum){
  
  data.h = aggregate(data$y, list(time=cut(data$time, freq.r(aggregation))),fun)
  
  data.f = data.frame(time = as.POSIXct(as.character(data.h$time), tz = "UTC"),
                      y = data.h$x )
  
  time.seq = data.frame(time = seq(data.f[1,1], data.f[(dim(data.f)[1]),1],
                                   by = freq.r(aggregation)), y = NA)
  
  
  data.full.seq = merge(time.seq, data.f, by = "time", all.x = TRUE)
  
  data.final =  data.frame(time = data.full.seq$time, y= data.full.seq$y.y)
}

################### 3. aggregation functions #################

