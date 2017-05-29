df1 = read.csv("data/1_electric.csv", header = T)
df = df1
aggregation = NULL


if(scale = "day"){
 
  
  data.final$data$date = format(data.final$data$time,"%Y-%m-%d")


  data.byday = split(data.final$data,data.final$data$date)
  
  
  ylimt = range(data.final$data$y, na.rm = TRUE)
  if(is.null(unlist(input))){
    if(separate = TRUE){
      for (i in (1:length(data.byday))){
        
        plot(data.byday[[i]]$time, data.byday[[i]]$y, type = "l",
             xaxt = "n", ylim = ylimt,
             ylab = ylab, sub = data.byday[[i]]$date[1] , xlab = "Hour",
             main = main)
        
        axis.POSIXct(1, at = seq(data.byday[[i]]$time[1], tail(data.byday[[i]]$time, n =1), by = "3 hour"),
                     format = ticklabfmt, cex.axis = 0.7,
                     las = las,tck = tck1)
  
      }}
  
     if(separate = FALSE){
      
         
         plot(1:length(data.byday[[1]]$y), data.byday[[1]]$y, type = "n",
              xaxt = "n", ylim = ylimt,
              ylab = ylab, sub = NULL , xlab = "Hour",
              main = main)
       
          for (i in (1:length(data.byday))){
          lines(1:length(data.byday[[i]]$y), data.byday[[i]]$y)  
            
          }}}
       
       
       if(!is.null(unlist(input))){
         
         index =  which(names(data.byday) %in% ind)
         
         index =  which (as.POSIXct(names(data.byday)) >= start & as.POSIXct(names(data.byday)) < end)
         
         if(separate == TRUE){
           for (i in (1:length(data.byday[index]))){
             
             plot(data.byday[index][[i]]$time, data.byday[index][[i]]$y, type = "l",
                  xaxt = "n", ylim = ylimt,
                  ylab = ylab, sub = data.byday[index][[i]]$date[1] , xlab = "Hour",
                  main = main)
             
             axis.POSIXct(1, at = seq(data.byday[index][[i]]$time[1], tail(data.byday[index][[i]]$time, n =1), by = "3 hour"),
                          cex.axis = 0.7,
                          las = las,tck = tck1)
             
           }}
       
       if(separate == FALSE){
         
        
         plot(1:length(data.byday[index][[1]]$y), data.byday[index][[1]]$y, type = "n",
              xaxt = "n", ylim = ylimt,
              ylab = ylab, sub = NULL , xlab = "Hour",
              main = main)
         
         for (i in (1:length(data.byday[index]))){
           
           lines(1:length(data.byday[index][[i]]$y), data.byday[index][[i]]$y)  
           
         }
       
      axis(1, at = seq(1, length(data.byday[index][[1]]$y) ,by = 3),
                       cex.axis = 0.7,
                      las = las,tck = tck1)
      
      # legend("topright", col =  c("red", "lightblue", "orange","black",
      #                             "darkblue"), lty = 1, legend = names(data.byday)[index])
      # dev.off()
      }
       }
}
  
  
  index =  which(names(data.bymonth) == ind)
  
  
       i =1
       xlab = ""
       ylab = ""
       main = ""
       i = 3
       las = 1
       tck1 = -0.01 
       tck2 = -0.005
       
       ticklabfmt = "%H"
       year = c(2012,2013)
       month = c(1,2,3)