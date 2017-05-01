# Master-Project
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

