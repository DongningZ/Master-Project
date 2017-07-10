display = function(time, response, freq = NULL ,start, end = start, aggregation = NULL, fun, 
                   scale = NULL, comparsion = FALSE, no.com = 2, separate = FALSE, 
                   main = NULL, ylab = NULL, ){
  
  
  xlab = 1
  ylab = 2
  main = 3
  data.1  = aggre.na(data, aggregation)
  
  if( scale == "total"){
    
    
    
    plot(y ~ time  , data= data.1, type = "l",xlab = xlab, ylab = ylab, main = main,
         sub = paste("All in one", "by",aggregation, sep = " "))
    
  }
  
  if( scale == "year"){
    
    data.1$year =  as.POSIXlt(data.1$time)$year+ 1900
    
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

aggregation = NULL


### read-in format function
### 