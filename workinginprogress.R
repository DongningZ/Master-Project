display = function(time, response, freq = NULL ,start, end = start, aggregation = NULL, fun, 
                   scale = NULL, comparsion = FALSE, no.com = 2, separate = FALSE, 
                   main = NULL, ylab = NULL, ){
  
  
  xlab = ""
  ylab = ""
  main = ""
  aggregation = "quarter"
  data.1  = aggre.na(data, aggregation)
  
  if( scale == "total"){
    
    
    
    plot(y ~ time  , data= data.1, type = "l",xlab = xlab, ylab = ylab, main = main,
         sub = paste("All in one", "by",aggregation, sep = " "))
    
  }
  
  if( scale == "year"){
    data.1$year =  as.POSIXlt(data.1$time)$year+ 1900
    
    data.byyear = split(data.1,data.1$year)
    
    if(separate == TRUE){
      
    for (i in 1:length(data.byyear)){
      
      xlimx =c(as.POSIXct(paste(names(data.byyear)[i], 1 , 1 ,sep = "-"), format = "%Y-%m-%d", tz = "UTC"),
               as.POSIXct(paste(names(data.byyear)[i], 12 , 31 , sep = "-"), format = "%Y-%m-%d", tz = "UTC"))
      
      plot(y ~ time, data = data.byyear[[i]], xlab = xlab, ylab = ylab, xaxt = "n", 
           main = main, sub = paste(data.byyear[[i]]$year[1], "by" ,aggregation, sep = " "), type = "n",
           xlim = xlimx)
      
      lines(y ~ time, data = data.byyear[[i]])
      
      axis.POSIXct(1, seq(xlimx[1],xlimx[2], by = "month"),
                   format = "%b", cex.axis = 0.7, 
                   las = 1,tck = -0.01)
      
      axis.POSIXct(1, seq(xlimx[1],xlimx[2], by = "day"),
                   labels = FALSE,format = "%d", 
                   cex.axis = 0.7, las = 1, tck = -0.005)
    } } else {
      
      xlimx =c(as.POSIXct(paste(1 , 1 ,sep = "-"), format = "%m-%d", tz = "UTC"),
               as.POSIXct(paste(12 , 31 , sep = "-"), format = "%m-%d", tz = "UTC"))
      
      plot(y ~ time, data = data.byyear[[i]], xlab = xlab, ylab = ylab, xaxt = "n", 
           main = main, sub = paste(data.byyear[[i]]$year[1], "by" ,aggregation, sep = " "), type = "n",
           xlim = xlimx)
      
      axis.POSIXct(1, seq(xlimx[1],xlimx[2], by = "month"),
                   format = "%b", cex.axis = 0.7, 
                   las = 1,tck = -0.01)
      
      axis.POSIXct(1, seq(xlimx[1],xlimx[2], by = "day"),
                   labels = FALSE,format = "%d", 
                   cex.axis = 0.7, las = 1, tck = -0.005)
      for (i in 1:length(data.byyear)){
        
      lines(y ~ time, data = data.byyear[[i]])
      }
      
    }
    
  }

### read-in format function
### 


#### detect frequency or not?
## if the aggregation is zero