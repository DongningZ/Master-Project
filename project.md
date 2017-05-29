# Master-Project
## Update on 30/05/2017
Now the function is done.
It required a input data set, frequency and a specified date or a interval. Here is how it works.
1. Choose aggregation level from day, week, month, quarter, year.
Data could be aggregated to the above level.
Missing dates will be created and assigned with response of NA.

2. Choose scale from dayily, monthly, weekly, quarterly, yearly
Choose the scale of the plotting

3. Input interval or specified date
There are two ways of input the plotting interval: a specified date(month, year, day etc.), an interval of dates.
The specified date is for the daily, yearly, monthly, hourly data. For example, to plot the daily data of May 2014, the input will be 2014 May.
The interval of dates is for the daily data in weekly plots or monthly plots. For example, input (02-05-2017 to 02-06-2017) will generate plot(s) for this time interval in weekly or monthly.

The begining of each plot is taken care of so that the monthly plot always start on 1st of the month, the weekly always start on sunday.

5. For specified data input, Choose the way of  comparison 
For specified data input, say daily in a specified month. There are two ways of comparsion are available: compare the same months from different years or compare the adjacent months from the same year. 
The number of months could be further specified.

6. Choose separate plots or one plot
Comparison or interval of dates could be drawn on separate plots or one plot.
If the one plot is chosen, the different month will match on the day of the week.

Examples:
Two date input of half-hourly data in daily plot.
![alt text](https://github.com/DongningZ/Master-Project/blob/master/Rplot.png)

Interval input from 2014-03-02 to 2014-03-07
![alt text](https://github.com/DongningZ/Master-Project/blob/master/Rplot02.png)

Single input month with adjacent 2 months from same year
![alt text](https://github.com/DongningZ/Master-Project/blob/master/Rplot06.png)

Single input month with 2 same months from same year
![alt text](https://github.com/DongningZ/Master-Project/blob/master/Rplot05.png)


###Things to do: possible shift on the hourly/half-hourly data on dayily plot.

## Things to discuss on 09/05/2017
1. look at the electricity data sets.

The households are fine.

The building data set is bit weird and not as stable as expected.



## update on 02/05/2017 - 07/05/2017
I received total 5 household electricity data and 1 building data.

By testing these data, I found many errors with the functions.

More time needed to refine and debug these functions.

There are mainly 3 functions:
1. Simply display plot function

Current status : basic structure done, need to add more features.

2. Comparing plots function

Current status :still working on basic structure as many levels of aggregation invovled.

3. Partten of certain period 

Current status : need to rewirte.

## Things to discuss on 02/05/2017
1. Ideas of how to use average/smooth to reduce noise
  * average the data at the same time in a year or week (if we have enough data).
  * smooth over aggregated data
  * average the smooth obtained at low aggregated levels
  * aggregate the smooth obtained at low aggregated levels?
2. How to deal with missing values or extreme low value
![alt text](https://github.com/DongningZ/Master-Project/blob/master/plot1.JPG)
![alt text](https://github.com/DongningZ/Master-Project/blob/master/plot2.JPG)
  * imputation
  1. last value carry forward, easy and fast
  2. multiple imputation, reliable but slow


## update on 28/04/2017
I was asking a new data set since this data set is with way too many noises and incredibly hard to present and model.

Chris decided this project should have a review and restart.

Before anything fancy the basic presentation of the data should be the first thing to do.

The basic presentation of this type data mainly are two approaches: aggregation and average/smooth. 

More plot types such as bar plot could be considered.

### Aggregation
Data could be aggregated to different frequency.

In this data, by aggregation we can obtain :
1. yearly
2. monthly
3. quarterly
4. daily in month
5. daily in week
6. hourly in day


There are mainly 3 types of plots considering:
1. Simple display plots with different plot type for any given time period
Bar plots and more should be consider.

2. Comparing plots with lines
 * different aggregation levels
 * input time period should be flexible. For example, choosing June only to compare.

3. To extract certain patterns
  * daily pattern over a week
  * hourly pattern over a day



##### Things need to consider:
1. The input of time period of all the plots above should be flexible
2. The label and axis should response to the aggregation level and input time period.

