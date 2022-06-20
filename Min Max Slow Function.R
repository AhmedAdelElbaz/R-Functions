######### Max and Min function #########



###############################################
#Purpose: function returns the maximum and 
#         Minimum values of a vector as a list
#Date: 08 June 2022                           #
#Author Name: A.Elbaz                         #
##Session info:                               #
# R version 4.2.0 (2022-04-22 ucrt)           #
# Platform: x86_64-w64-mingw32/x64 (64-bit)   #
# Running under: Windows 10 x64 (build 19043) #            
###############################################
#No used libraries


#Reading the expression matrix as a table
expr_matrix = read.table("D:/MAIN/STUDY/PROGRAMMING/R/Course/gene_expression.csv", header = TRUE, sep = ",")
row.names(expr_matrix) = expr_matrix$time        #name rows by column $time
expr_matrix = expr_matrix[2:dim(expr_matrix)[2]] #removing the column $time


##Assignment2 Part:

minV = expr_matrix[1,1]       #default value to be compared with each element
maxV = expr_matrix[1,1]       #default value to be compared with each element
#looping by each cell in the data frame in a row fashion
for (i in 1:dim(expr_matrix)[1]){
  for(j in 1:dim(expr_matrix)[2]){
  #Compare each cell with the minV and overwrite it if the value of the cell is lower
    if (expr_matrix[i,j]< minV) minV = expr_matrix[i,j] 
  #Compare each cell with the maxV and overwrite it if the value of the cell is higher
    else if (expr_matrix[i,j]> maxV) maxV = expr_matrix[i,j]
  }
}

vector = expr_matrix[1,] #Sub-setting the first vector of the expr_matrix
maxV = vector[1]              #default value to be compared with each element
minV = vector[1]              #default value to be compared with each element
for (i in vector){
  if (i< minV) minV = i
  else if (i > maxV) maxV = i
}


########### Function to get min and max values ##############
#This function takes any data type and converts it to a data frame
min.max = function(data){  #define the function with one parameter (data)
    data = as.data.frame(data);
    minV = data[1,1]
    maxV = data[1,1]
    for (i in 1:dim(data)[1]){
      for(j in 1:dim(data)[2]){
        if (data[i,j]< minV) minV = data[i,j]
        else if (data[i,j]> maxV) maxV = data[i,j]
      }
  }
  
  return(list(minimum.value = minV , maximum.value = maxV))
}
min.max(expr_matrix)


