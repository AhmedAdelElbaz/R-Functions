######### Euclidean Distance per column function #########



###############################################
#Purpose: function returns the Euclidean Distance
#between columns as vectors
#Date: 20 June 2022                           #
#Author Name: A.Elbaz                         #
##Session info:                               #
# R version 4.2.0 (2022-04-22 ucrt)           #
# Platform: x86_64-w64-mingw32/x64 (64-bit)   #
# Running under: Windows 10 x64 (build 19043) #            
###############################################
#libraries:
  #microbenchmark
#Used to estimate the time of the function
install.packages("microbenchmark")
library("microbenchmark")
#The 1st function is ##euclidean## which calculates the euclidean distance between two variables
euclidean = function(a,b){
  results = sqrt(sum((a - b)^2))
  return(results)
}
#The Complete function to calculate the euclidean distances 
####### Make Sure the DataFrame has numerical values and right column names ########
full.euclidean = function(dataFrame){
  results = list()#Created a list to be filled with euclidean distances between columns
  for (i in 1:dim(dataFrame)[2]){
    result = sapply(dataFrame,euclidean, b = dataFrame[,i])#apply the 1st func. to the Whole data
    results[[i]] = result#fille each element of the list with the output of the apply function
  }
  results = as.data.frame(results)#converts the final list into a dataframe
  colnames(results)= c(colnames(dataFrame))#assign column names to the data frame
  row.names(results)= c(colnames(dataFrame))#assign row names to the data frame
  return(results)
}

full.euclidean_ROW = function(dataFrame){
  results = list()#Created a list to be filled with euclidean distances between ROWS
  for (i in 1:dim(dataFrame)[1]){
    result = apply(dataFrame,MARGIN=1, euclidean, b = dataFrame[i,])#apply the 1st func. to the Whole data
    results[[i]] = result#fille each element of the list with the output of the apply function
  }
  results = as.data.frame(results)#converts the final list into a dataframe
  colnames(results)= c(row.names(dataFrame))#assign column names to the data frame
  row.names(results)= c(row.names(dataFrame))#assign row names to the data frame
  return(results)
}

my_results = full.euclidean(dataFrame = genes_expression.data)
heatmap(as.matrix(my_results))
microbenchmark(full.euclidean(dataFrame = data))
my_results1 = full.euclidean_ROW(genes_expression.data[1:20,])
